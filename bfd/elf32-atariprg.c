/* Support for Atari TOS PRG/ELF binaries.
   ELF-specific code written by Vincent Riviere, 2023.
   Based on the original a.out patch by Guido Flohr, 1998.
   Copyright (C) 1998-2023 Free Software Foundation, Inc.

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include "bfd.h"
#include "libbfd.h"
#include "elf32-atariprg.h"
#include "elf-bfd.h"
#include "elf/m68k.h"

/* Set this to 1 to enable debug traces.  */
#if 0
# define TRACE(fmt, ...) printf (fmt, __VA_ARGS__)
#else
# define TRACE(fmt, ...)
#endif
#define TRACE0(msg) TRACE ("%s", msg)

/* To display well-formatted addresses.  */
#define ADR_F "0x%08" PRIx32
typedef uint32_t adr_t;

/* Test if an integer is even.  */
#define IS_EVEN(x) (((x) & 1) == 0)

/* The main idea here is to reuse the elf32-m68k target, but with a few
   overrides to add a PRG extended header before ELF data, and a PRG relocation
   table just after.  So the ELF executable is actually embedded into a standard
   PRG executable.  ELF Program Headers are specially tailored to fit the GEMDOS
   process TEXT/DATA/BSS segments.  TEXT segment starts with an extra header
   to support custom entry point and adjustable stack size.  Next comes the ELF
   File Header followed by the ELF Program Headers and the rest of the TEXT
   segment.  DATA and BSS segments are used normally.  Then non-alloc sections
   and ELF Section Headers are stored in the PRG symbols table area, so they
   aren't loaded by TOS at run-time.  Finally, the standard PRG relocation table
   is appended after ELF data.  */

/* Target vector of our base implementation, for inheritance.  */
extern const bfd_target m68k_elf32_vec;

/* Our own target vector.  */
bfd_target m68k_elf32_atariprg_vec;
static struct elf_backend_data m68k_elf32_atariprg_bed;
static struct elf_size_info m68k_elf32_atariprg_size_info;

/* Data structure that holds some private information for us.  */
struct mint_internal_info
{
  bfd_byte	*tparel;	     /* Data for TPA relative relocation
					information.  */
  file_ptr	nonload_pos;	     /* File position of the first non-load
					section.  */
  file_ptr	tparel_pos;	     /* File position of TPA relative
					relocation information.  */
  bfd_size_type tparel_size;	     /* Size of TPA relative relocation
					information.  */
#define MINT_RELOC_CHUNKSIZE 0x1000
  bfd_vma	*relocs;	     /* Array of address relocations.  */
  unsigned int  relocs_used;	     /* Number of relocation entries
					already used up.  */
  unsigned int  relocs_allocated;    /* Number of relocation entries
					allocated.  */

  file_ptr	stkpos; 	     /* File offset of _stksize variable.  */

  uint32_t	prg_flags;	     /* Standard GEMDOS flags.  */

  bool		override_stack_size; /* true if the executable stack size
					must be overridden with stack_size.  */
  int32_t	stack_size;
};

/* Standard PRG header, external format.  */
typedef struct {
  unsigned char magic[2];	/* Magic number */
  unsigned char text[4];	/* Size of TEXT segment */
  unsigned char data[4];	/* Size of DATA segment */
  unsigned char bss[4];		/* Size of BSS segment */
  unsigned char symbols[4];	/* Size of the symbols table */
  unsigned char reserved[4];	/* Used as file-format identifier */
  unsigned char flags[4];	/* Program flags */
  unsigned char absflag[2];	/* Must be 0 for relocatable PRG */
} PRG_HEADER;

/* Extended PRG/ELF header, external format.  */
typedef struct {
  /* Standard PRG header */
  PRG_HEADER prg_header;
  /* Extra PRG header */
  unsigned char trampoline[4][2];	/* Jump to entry point */
  unsigned char g_stkpos[4];		/* File offset of stack size variable */
} PRGELF_HEADER;

#define PRG_MAGIC 0x601a 		/* PRG file identifier */

/* Nice macros to determine various offsets.  */
#define PRGELF_PHNUM 3 /* Number of ELF Program Headers */
#define FILE_OFFSET_PRGELF_HEADER 0
#define FILE_OFFSET_TEXT sizeof (PRG_HEADER)
#define VMA_TEXT 0
#define SIZEOF_PRG_EXTRA_HEADER (sizeof (PRGELF_HEADER) - sizeof (PRG_HEADER))
#define FILE_OFFSET_ELF_HEADER (FILE_OFFSET_PRGELF_HEADER + sizeof (PRGELF_HEADER))
#define PRGELF_RESERVED (0x454c4600 /* ELF0 */ + FILE_OFFSET_ELF_HEADER)
#define FILE_OFFSET_E_ENTRY (FILE_OFFSET_ELF_HEADER + offsetof(Elf32_External_Ehdr, e_entry))
#define FILE_OFFSET_TRAMPOLINE (FILE_OFFSET_PRGELF_HEADER + offsetof(PRGELF_HEADER, trampoline))
#define E_ENTRY_PCREL (FILE_OFFSET_E_ENTRY - (FILE_OFFSET_TRAMPOLINE + 2))
#define TEXT_PCREL (FILE_OFFSET_TEXT - (FILE_OFFSET_TRAMPOLINE + 6))

/* We need to store extra information in a bfd. As this target will never be
   used for core dumps, just hijack the core pointer for us.  */

static void
set_mint_internal_info (bfd *abfd, struct mint_internal_info *myinfo)
{
  BFD_ASSERT (elf_tdata (abfd)->core == NULL);
  elf_tdata (abfd)->core = (struct core_elf_obj_tdata *) myinfo;
}

static struct mint_internal_info *
get_mint_internal_info_maybe_null (bfd *abfd)
{
  struct mint_internal_info *myinfo;

  if (abfd->xvec != &m68k_elf32_atariprg_vec)
    return NULL;
  if (elf_tdata (abfd) == NULL)
    return NULL;
  myinfo = (struct mint_internal_info *) elf_tdata (abfd)->core;

  return myinfo;
}

static struct mint_internal_info *
get_mint_internal_info (bfd *abfd)
{
  struct mint_internal_info *myinfo = get_mint_internal_info_maybe_null (abfd);

  BFD_ASSERT (myinfo != NULL);

  return myinfo;
}

/* Allocate a new bfd_object for output or input.  */

static bool
m68k_elf32_atariprg_make_object (bfd *abfd)
{
  struct mint_internal_info *myinfo;

  TRACE ("m68k_elf32_atariprg_make_object %s %s\n", abfd->xvec->name, abfd->filename);

  if (! m68k_elf32_vec._bfd_set_format[bfd_object] (abfd))
    return false;

  /* Allocate our private BFD data.  */
  myinfo = bfd_zalloc (abfd, sizeof (struct mint_internal_info));
  if (myinfo == NULL)
    return false;

  set_mint_internal_info (abfd, myinfo);

  return true;
}

/* Can we read this input file?
   If not, other targets will be asked.  **/

static bfd_cleanup
m68k_elf32_atariprg_object_p (bfd *abfd)
{
  PRGELF_HEADER ph_ext;
  bfd_size_type amt;
  uint16_t magic;
  uint32_t reserved;
  struct mint_internal_info *myinfo;
  bfd_cleanup ret;
  uint32_t text, data, symbols;

  TRACE ("m68k_elf32_atariprg_object_p %s %s\n", abfd->xvec->name, abfd->filename);

  /* Read the PRG extended header from the file.  */
  amt = sizeof (PRGELF_HEADER);
  if (bfd_bread (&ph_ext, amt, abfd) != amt)
    {
      if (bfd_get_error () != bfd_error_system_call)
	bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }

  /* Is this a PRG?  */
  magic = H_GET_16 (abfd, ph_ext.prg_header.magic);
  if (magic != PRG_MAGIC)
    {
      bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }

  /* Is this a PRG/ELF?  */
  reserved = H_GET_32 (abfd, ph_ext.prg_header.reserved);
  if (reserved != PRGELF_RESERVED)
    {
      bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }

  /* Seek to the ELF File Header position.  */
  if (bfd_seek (abfd, FILE_OFFSET_ELF_HEADER, SEEK_SET) != 0)
    {
      bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }

  /* Continue with the standard ELF implementation.  */
  ret = m68k_elf32_vec._bfd_check_format[bfd_object] (abfd);
  if (ret == NULL)
    return NULL;

  /* The base implementation has called m68k_elf32_atariprg_make_object (),
     so our internal info has just been allocated.  */
  myinfo = get_mint_internal_info (abfd);

  /* Store extra header information.  */
  myinfo->prg_flags = H_GET_32 (abfd, ph_ext.prg_header.flags);
  myinfo->stkpos = H_GET_32 (abfd, ph_ext.g_stkpos);
  /* Other header fields will be regenerated on write.  */

  /* We read these values just to determine the TPA relocation position.  */
  text = H_GET_32 (abfd, ph_ext.prg_header.text);
  data = H_GET_32 (abfd, ph_ext.prg_header.data);
  symbols = H_GET_32 (abfd, ph_ext.prg_header.symbols);
  myinfo->tparel_pos = sizeof (PRG_HEADER) + text + data + symbols;

  return ret;
}

/* Determine the size of the file headers for the linker SIZEOF_HEADERS.  */

static int
m68k_elf32_atariprg_sizeof_headers (bfd *abfd, struct bfd_link_info *info)
{
  int ret = SIZEOF_PRG_EXTRA_HEADER
	    + m68k_elf32_vec._bfd_sizeof_headers (abfd, info);

  TRACE ("m68k_elf32_atariprg_sizeof_headers %s %s %d\n", abfd->xvec->name, abfd->filename, ret);

  return ret;
}

/* Set the GEMDOS executable flags.
   Called by the linker emulation script.  */

bool
bfd_elf32_atariprg_set_extended_flags (bfd *abfd, uint32_t prg_flags)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);

  TRACE ("bfd_elf32_atariprg_set_extended_flags %s %s " ADR_F "\n", abfd->xvec->name, abfd->filename, (adr_t) prg_flags);

  myinfo->prg_flags = prg_flags;

  return true;
}

/* Override the stack size.
   Called by the linker emulation script.  */

bool
bfd_elf32_atariprg_set_stack_size (bfd *abfd, int32_t stack_size)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);

  TRACE ("bfd_elf32_atariprg_set_stack_size %s %s " ADR_F "\n", abfd->xvec->name, abfd->filename, (adr_t) stack_size);

  myinfo->stack_size = stack_size;
  myinfo->override_stack_size = true;

  return true;
}

/* Determine extra header information before the ELF File Header.
   Called from elf.c.  */

void
bfd_elf32_atariprg_get_extra_header_info (bfd *abfd ATTRIBUTE_UNUSED,
					  file_ptr *vma_offp,
					  bfd_size_type *sizeof_extra_headerp)
{
  /* File offset of memory start.  */
  *vma_offp = FILE_OFFSET_TEXT;

  /* Size of extra header before ELF header in segment.  */
  *sizeof_extra_headerp = SIZEOF_PRG_EXTRA_HEADER;

  TRACE ("bfd_elf32_atariprg_get_extra_header_info %s %s " ADR_F " %lu\n", abfd->xvec->name, abfd->filename, (adr_t) *vma_offp, *sizeof_extra_headerp);
}

/* Record the file offset of the first non-load section.
   This will be the beginning of the PRG symbol table.
   Called from elf.c.  */

void
bfd_elf32_atariprg_set_nonload_pos (bfd *abfd, file_ptr nonload_pos)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);

  TRACE ("bfd_elf32_atariprg_set_nonload_pos %s %s " ADR_F "\n", abfd->xvec->name, abfd->filename, (adr_t) nonload_pos);

  myinfo->nonload_pos = nonload_pos;
}

static unsigned int
bfd_read_4byte_int (bfd *abfd, file_ptr pos)
{
  bfd_byte buffer[4];
  file_ptr offset;
  unsigned int val;

  offset = bfd_tell(abfd);
  bfd_seek(abfd, pos, SEEK_SET);
  if (bfd_bread (buffer, (bfd_size_type) 4, abfd) != 4)
    {
      return -1;
    }
  val = bfd_get_32 (abfd, buffer);
  bfd_seek(abfd, offset, SEEK_SET);
  return val;
}

/* Add a TPA relocation entry.
   Called for each absolute address in TEXT/DATA segments.
   Actual relocation will be performed by the OS at load time.  */

static bool
add_tpa_relocation_entry (bfd *abfd, bfd *input_bfd, bfd_vma address)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);

  if (address & 1)
    {
      _bfd_error_handler ("%pB(%pB): TPA relocation at odd address: " ADR_F,
			  abfd, input_bfd, (adr_t) address);
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* Enlarge the buffer if necessary.  */
  if (myinfo->relocs_used * sizeof (bfd_vma) >= myinfo->relocs_allocated)
    {
      bfd_vma *newbuf;
      myinfo->relocs_allocated += MINT_RELOC_CHUNKSIZE;
      newbuf = bfd_realloc (myinfo->relocs, myinfo->relocs_allocated);
      if (newbuf == NULL)
	return false;

      myinfo->relocs = newbuf;
    }

  /* The TPA relative relocation actually just adds the address of
     the text segment (i. e. beginning of the executable in memory)
     to the addresses at the specified locations.  This allows an
     executable to be loaded everywhere in the address space without
     memory management.  */
  myinfo->relocs[myinfo->relocs_used++] = address;

  return true;
}

/* The RELOCATE_SECTION function is called by the ELF backend linker
   to handle the relocations for a section.  */

static int
m68k_elf32_atariprg_relocate_section (bfd *output_bfd,
				      struct bfd_link_info *info,
				      bfd *input_bfd,
				      asection *input_section,
				      bfd_byte *contents,
				      Elf_Internal_Rela *relocs,
				      Elf_Internal_Sym *local_syms,
				      asection **local_sections)
{
  int ret;
  Elf_Internal_Rela *rel;
  Elf_Internal_Rela *relend;

  TRACE ("m68k_elf32_atariprg_relocate_section %s %s %s %s\n", output_bfd->xvec->name, output_bfd->filename, input_bfd->filename, input_section->name);

  /* First, call the base implementation.  */
  ret = xvec_get_elf_backend_data (&m68k_elf32_vec)->elf_backend_relocate_section (output_bfd,
    info, input_bfd, input_section, contents, relocs, local_syms, local_sections);
  if (! ret)
    return ret;

  /* Non-load sections have no TPA relocations.  */
  if (! (input_section->output_section->flags & SEC_LOAD))
    return ret;

  /* Walk all ELF relocations to determine if a TPA relocation is needed.  */
  rel = relocs;
  relend = relocs + input_section->reloc_count;
  for (; rel < relend; rel++)
    {
      int r_type;

      r_type = ELF32_R_TYPE (rel->r_info);
      switch (r_type)
	{
	  case R_68K_32:
	    {
	      /* Absolute 32-bit address.  */
	      bfd_vma relocation = input_section->output_section->vma + input_section->output_offset + rel->r_offset;
	      if (! add_tpa_relocation_entry (output_bfd, input_bfd, relocation))
		return false;
	    }
	    break;

	  case R_68K_16:
	  case R_68K_8:
	    /* PRG format doesn't support short absolute relocations.  */
	    _bfd_error_handler("%pB: invalid relocation type %d for target %s",
			       output_bfd, r_type, output_bfd->xvec->name);
	    bfd_set_error (bfd_error_bad_value);
	    return false;

	  default:
	    /* Ignore PC-relative relocations.  */
	    break;
	}
    }

  return ret;
}

/* Do a link based on the link_order structures attached to each
   section of the BFD.
   After calling the base implementation, this is a good place to examine the
   contents of the newly linked executable.
   The symbol lookup is inspired from elf_link_output_extsym ().  */

static bool
m68k_elf32_atariprg_final_link (bfd *abfd, struct bfd_link_info *info)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (info->output_bfd);
  struct elf_link_hash_entry *h;

  TRACE ("m68k_elf32_atariprg_final_link %s %s\n", abfd->xvec->name, abfd->filename);

  /* First, call the base implementation.  */
  if (! m68k_elf32_vec._bfd_final_link (abfd, info))
    return false;

  TRACE ("m68k_elf32_atariprg_final_link END %s %s\n", abfd->xvec->name, abfd->filename);

  /* Remember the address of the stack size variable.  */
  h = (struct elf_link_hash_entry *) bfd_hash_lookup (&info->hash->table, "_stksize", false, false);
  if (h != NULL)
    {
      asection *input_sec;
      bfd_vma vma;

      BFD_ASSERT (h->root.type == bfd_link_hash_defined);
      BFD_ASSERT (h->type == STT_OBJECT);

      input_sec = h->root.u.def.section;
      vma = input_sec->output_section->vma + input_sec->output_offset + h->root.u.def.value;
      TRACE ("h=%p %s %s " ADR_F "\n", h, input_sec->name, h->root.root.string, (adr_t) vma);
      myinfo->stkpos = FILE_OFFSET_TEXT + vma;
    }

  return true;
}

/* Check and adjust the ELF Program Headers.
   - Ensure that the p_vaddr, p_paddr and p_offset fields are always set.
   - Ensure that segments are contiguous.
   - Ensure that addresses are consistent with offsets.
   This is a requirement for write_prgelf_header ().  */

static bool
fix_phdrs (bfd *abfd)
{
  Elf_Internal_Ehdr *i_ehdrp = elf_elfheader (abfd);
  unsigned int phnum = i_ehdrp->e_phnum;
  struct elf_obj_tdata *tdata = elf_tdata (abfd);
  Elf_Internal_Phdr *phdr_text, *phdr_data, *phdr_bss;
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);
  bfd_vma real_vaddr_data_end;
  const struct elf_backend_data *bed = get_elf_backend_data (abfd);
  struct elf_segment_map *mtext = elf_seg_map (abfd);

  TRACE ("fix_phdrs %s %s\n", abfd->xvec->name, abfd->filename);

  if (phnum != PRGELF_PHNUM)
    {
      _bfd_error_handler ("%pB: number of Program Headers %u must be exactly %u for segments TEXT, DATA and BSS",
			  abfd, phnum, PRGELF_PHNUM);
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  BFD_ASSERT(mtext->idx == 0); /* TEXT */
  phdr_text = &tdata->phdr[0];
  phdr_data = &tdata->phdr[1];
  phdr_bss  = &tdata->phdr[2];

  /* Fix TEXT segment address.  */
  if (phdr_text->p_memsz == 0)
    {
      phdr_text->p_vaddr = 0;
      phdr_text->p_paddr = phdr_text->p_vaddr;
      phdr_text->p_offset = FILE_OFFSET_TEXT + phdr_text->p_vaddr;
    }

  /* Check TEXT segment address.  */
  if (mtext->includes_filehdr && phdr_text->p_vaddr != VMA_TEXT)
    {
      _bfd_error_handler ("%pB: TEXT segment start address " ADR_F " must be " ADR_F,
			  abfd, (adr_t) phdr_text->p_vaddr, (adr_t) VMA_TEXT);
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* Fix DATA segment address.  */
  BFD_ASSERT (myinfo->nonload_pos > 0);
  real_vaddr_data_end = myinfo->nonload_pos - FILE_OFFSET_TEXT;
  if (phdr_data->p_memsz == 0)
    {
      phdr_data->p_vaddr = real_vaddr_data_end;
      phdr_data->p_paddr = phdr_data->p_vaddr;
      phdr_data->p_offset = FILE_OFFSET_TEXT + phdr_data->p_vaddr;
    }

  /* Fix DATA segment size.  */
  phdr_data->p_memsz = real_vaddr_data_end - phdr_data->p_vaddr;
  phdr_data->p_filesz = phdr_data->p_memsz;

  /* Fix TEXT segment size.  */
  phdr_text->p_memsz = phdr_data->p_vaddr - phdr_text->p_vaddr;
  phdr_text->p_filesz = phdr_text->p_memsz;

  /* Fix BSS segment address.  */
  if (phdr_bss->p_memsz == 0)
    {
      phdr_bss->p_vaddr = phdr_data->p_vaddr + phdr_data->p_memsz;
      phdr_bss->p_paddr = phdr_bss->p_vaddr;
    }
  else
    {
      /* To satisfy the alignment of some variables, the linker might try to
	 align the BSS segment after the end of the DATA segment. As this isn't
	 possible with TOS, we extend the front of the BSS segment so it
	 precisely matches the end of the DATA segment. This doesn't matter
	 because the VMA of the .bss section doesn't change.  */
      uint32_t expected_bss_vaddr = phdr_data->p_vaddr + phdr_data->p_memsz;
      if (phdr_bss->p_vaddr > expected_bss_vaddr)
	{
	  uint32_t offset = phdr_bss->p_vaddr - expected_bss_vaddr;
	  phdr_bss->p_vaddr -= offset;
	  phdr_bss->p_paddr -= offset;
	  phdr_bss->p_memsz += offset;
	}
    }
  phdr_bss->p_offset = FILE_OFFSET_TEXT + phdr_bss->p_vaddr;

  /* Fix BSS segment size.  */
  phdr_bss->p_memsz = BFD_ALIGN (phdr_bss->p_memsz, 1 << bed->s->log_file_align);
  BFD_ASSERT (phdr_bss->p_filesz == 0);

  /* Check that DATA segment directly follows TEXT segment.  */
  if (phdr_data->p_vaddr != phdr_text->p_vaddr + phdr_text->p_memsz)
    {
      _bfd_error_handler ("%pB: DATA segment start address " ADR_F " must directly follow TEXT segment at " ADR_F,
			  abfd, (adr_t) phdr_data->p_vaddr,
			  (adr_t) (phdr_text->p_vaddr + phdr_text->p_memsz));
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* Check that BSS segment directly follows DATA segment.  */
  if (phdr_bss->p_vaddr != phdr_data->p_vaddr + phdr_data->p_memsz)
    {
      _bfd_error_handler ("%pB: BSS segment start address " ADR_F " must directly follow DATA segment at " ADR_F,
			  abfd, (adr_t) phdr_bss->p_vaddr,
			  (adr_t) (phdr_data->p_vaddr + phdr_data->p_memsz));
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* Check that VMA of TEXT segment matches its file offset.  */
  if (phdr_text->p_vaddr != phdr_text->p_offset - FILE_OFFSET_TEXT)
    {
      _bfd_error_handler ("%pB: TEXT segment start address " ADR_F " must be " ADR_F " to match its file offset",
			  abfd, (adr_t) phdr_text->p_vaddr,
			  (adr_t) (phdr_text->p_offset - FILE_OFFSET_TEXT));
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* Check that VMA of DATA segment matches its file offset.  */
  if (phdr_data->p_vaddr != phdr_data->p_offset - FILE_OFFSET_TEXT)
    {
      _bfd_error_handler ("%pB: DATA segment start address " ADR_F " must be " ADR_F " to match its file offset",
			  abfd, (adr_t) phdr_data->p_vaddr,
			  (adr_t) (phdr_data->p_offset - FILE_OFFSET_TEXT));
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* Check that VMA of BSS segment matches its file offset.  */
  if (phdr_bss->p_vaddr != phdr_bss->p_offset - FILE_OFFSET_TEXT)
    {
      _bfd_error_handler ("%pB: BSS segment start address " ADR_F " must be " ADR_F " to match its file offset",
			  abfd, (adr_t) phdr_bss->p_vaddr,
			  (adr_t) (phdr_bss->p_offset - FILE_OFFSET_TEXT));
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* As we may have modified the Program Headers, write them again.  */
  if (bfd_seek (abfd, i_ehdrp->e_phoff, SEEK_SET) != 0
      || bed->s->write_out_phdrs (abfd, tdata->phdr, phnum) != 0)
    return false;

  return true;
}

/* Write the PRG extended header before ELF data.  */

static bool
write_prgelf_header (bfd *abfd)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);
  Elf_Internal_Phdr *phdrs = elf_tdata (abfd)->phdr;
  unsigned int phnum = elf_elfheader (abfd)->e_phnum;
  Elf_Internal_Phdr *phdr_text, *phdr_data, *phdr_bss;
  uint32_t prg_text_size, prg_data_size, prg_bss_size, prg_symbols_size;
  PRGELF_HEADER ph_ext;

  TRACE ("write_prgelf_header %s %s\n", abfd->xvec->name, abfd->filename);

  BFD_ASSERT (phnum == PRGELF_PHNUM);
  phdr_text = &phdrs[0];
  phdr_data = &phdrs[1];
  phdr_bss  = &phdrs[2];

  /* The segment sizes have been fixed, so we can trust them.  */
  prg_text_size = phdr_text->p_vaddr + phdr_text->p_filesz;
  prg_data_size = phdr_data->p_filesz;
  prg_bss_size = phdr_bss->p_memsz;

  /* We will write the TPA relocation table right after the ELF data.  */
  myinfo->tparel_pos = elf_next_file_pos (abfd);
  BFD_ASSERT (IS_EVEN (myinfo->tparel_pos));

  /* Compute the size of the PRG symbol table.  */
  prg_symbols_size = myinfo->tparel_pos - myinfo->nonload_pos;

  /* Prepare the PRG/ELF header.  */
  memset (&ph_ext, 0, sizeof ph_ext);

  /* Standard PRG header.  */
  H_PUT_16 (abfd, PRG_MAGIC, &ph_ext.prg_header.magic);
  H_PUT_32 (abfd, prg_text_size, &ph_ext.prg_header.text);
  H_PUT_32 (abfd, prg_data_size, &ph_ext.prg_header.data);
  H_PUT_32 (abfd, prg_bss_size, &ph_ext.prg_header.bss);
  H_PUT_32 (abfd, prg_symbols_size, &ph_ext.prg_header.symbols);
  H_PUT_32 (abfd, PRGELF_RESERVED, &ph_ext.prg_header.reserved);
  H_PUT_32 (abfd, myinfo->prg_flags, &ph_ext.prg_header.flags);

  /* Extended PRG header.  */
  H_PUT_16 (abfd, 0x203a, &ph_ext.trampoline[0]); /* move.l e_entry(pc),d0 */
  H_PUT_16 (abfd, E_ENTRY_PCREL, &ph_ext.trampoline[1]);
  H_PUT_16 (abfd, 0x4efb, &ph_ext.trampoline[2]); /* jmp VMA_TEXT(pc,d0.l) */
  H_PUT_16 (abfd, 0x0800 | ((uint8_t) TEXT_PCREL) , &ph_ext.trampoline[3]);
  H_PUT_32 (abfd, myinfo->stkpos, &ph_ext.g_stkpos); /* stack size address */

  /* Write the PRG/ELF header.  */
  if (bfd_seek (abfd, 0, SEEK_SET) != 0)
    return false;
  if (bfd_bwrite (&ph_ext, sizeof ph_ext, abfd) != sizeof ph_ext)
    return false;

  /* Override the stack size.  */
  if (myinfo->override_stack_size)
    {
      bfd_byte big_endian_stack_size[4];

      if (myinfo->stkpos == 0)
	{
	  _bfd_error_handler ("%pB: unable to determine the _stksize position",
			      abfd);
	  bfd_set_error (bfd_error_invalid_operation);
	  return false;
	}

      bfd_put_32 (abfd, myinfo->stack_size, &big_endian_stack_size);

      if (bfd_seek (abfd, myinfo->stkpos, SEEK_SET) != 0)
	return false;
      if (bfd_bwrite (big_endian_stack_size, 4, abfd) != 4)
	return false;
  }

  return true;
}

/* This is used by qsort() to sort the TPA relocation table.  */

static int
vma_cmp (const void *v1, const void *v2)
{
  return (int) ((*((bfd_vma *) v1)) - (*((bfd_vma *) v2)));
}

/* Alloc and fill the TPA relocation table.  */

static bool
fill_tparel (bfd *abfd)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);
  unsigned int i;
  bfd_size_type bytes;
  bfd_byte *ptr;
  unsigned int val;
  bfd_vma last;

  TRACE ("fill_tparel %s %s\n", abfd->xvec->name, abfd->filename);

  BFD_ASSERT (myinfo->tparel == NULL);
  BFD_ASSERT (myinfo->tparel_size == 0);

  /* Sort the relocation info.  */
  if (myinfo->relocs != NULL)
    qsort (myinfo->relocs, myinfo->relocs_used, sizeof (bfd_vma), vma_cmp);

  /* Now calculate the number of bytes we need.  The relocation info
     is encoded as follows:  The first entry is a 32-bit value
     denoting the first offset to relocate.  All following entries
     are relative to the preceding one.  For relative offsets of
     more than 254 bytes a value of 1 is used.  The OS will then
     add 254 bytes to the current offset.  The list is then terminated
     with the byte 0.  */
  bytes = 4; /* First entry is a long.  */
  for (i = 1; i < myinfo->relocs_used; i++)
    {
      bfd_signed_vma diff = myinfo->relocs[i] - myinfo->relocs[i - 1];
      /* No backward relocation.  */
      if (diff <= 0)
	{
	  _bfd_error_handler ("%pB: duplicate relocation: " ADR_F " <= " ADR_F,
	    abfd,
	    (adr_t)myinfo->relocs[i], (adr_t)myinfo->relocs[i - 1]);
	  bfd_set_error (bfd_error_bad_value);
	  return false;
	}
      BFD_ASSERT (! (diff & 1)); /* No relocation to odd address.  */
      bytes += (diff + 253) / 254;
    }
  /* Last entry is (bfd_byte) 0 if there are some relocations.  */
  if (myinfo->relocs_used > 0)
    bytes++;
  myinfo->tparel_size = bytes;

  /* Allocate the TPA relocation table.  */
  myinfo->tparel = bfd_alloc (abfd, bytes);
  if (myinfo->tparel == NULL)
    return false;

  /* Write the first entry. Always 32-bit.  */
  ptr = myinfo->tparel;
  i = 1;
  last = 0;
  if (myinfo->relocs != NULL)
    {
      last = myinfo->relocs[0];
      while (bfd_read_4byte_int(abfd, last + FILE_OFFSET_TEXT) == 0 && i < myinfo->relocs_used)
	{
	  last = myinfo->relocs[i];
	  i++;
	}
    }
  bfd_put_32 (abfd, last, ptr);
  ptr += 4;

  /* Write next entries.  Always 8-bit.  */
  for (; i < myinfo->relocs_used; i++)
    {
      bfd_signed_vma diff;

      /*
       * Do not add an entry, if the address to be relocated is zero.
       * This can happen with weak symbols.
       */
      val = bfd_read_4byte_int(abfd, myinfo->relocs[i] + FILE_OFFSET_TEXT);
      if (val == 0)
	{
	  continue;
	}
      diff = myinfo->relocs[i] - last;
      last = myinfo->relocs[i];
      while (diff > 254)
	{
	  *ptr++ = 1;
	  diff -= 254;
	}
      *ptr++ = (bfd_byte) diff;
    }
  if (myinfo->relocs_used > 0)
    *ptr = 0;

  return true;
}

/* Write out the TPA relocation table.  */

static bool
write_tparel (bfd *abfd)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);

  TRACE ("write_tparel %s %s\n", abfd->xvec->name, abfd->filename);

  BFD_ASSERT (myinfo->tparel != NULL);
  BFD_ASSERT (myinfo->tparel_size > 0);

  if (bfd_seek (abfd, myinfo->tparel_pos, SEEK_SET) != 0)
    return false;

  if (bfd_bwrite (myinfo->tparel, myinfo->tparel_size, abfd) != myinfo->tparel_size)
    return false;

  return true;
}

/* Write out the section headers and the ELF File Header.  */

static bool
m68k_elf32_atariprg_write_shdrs_and_ehdr (bfd *abfd)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);

  TRACE ("m68k_elf32_atariprg_write_shdrs_and_ehdr %s %s\n", abfd->xvec->name, abfd->filename);

  /* Now we have all the required information to fix the Program Headers.
     We can't do that in modify_headers because we need to know the offset of
     the Section Headers, and it isn't known yet at that time.  */
  if (! fix_phdrs(abfd))
    return false;

  /* Write out the PRG/ELF extended header.  */
  if (! write_prgelf_header (abfd))
    return false;

  /* Write out the Section Headers and the ELF File Header.  */
  if (! xvec_get_elf_backend_data (&m68k_elf32_vec)->s->write_shdrs_and_ehdr (abfd))
    return false;

  /* The TPA relocation table already exists if it has been read from an input
     file with objcopy/strip.  */
  if (myinfo->tparel_size == 0)
    {
      /* Generate the PRG relocation table.  */
      if (! fill_tparel (abfd))
	return false;
    }

  /* Write out the PRG relocation table.  */
  if (! write_tparel (abfd))
    return false;

  return true;
}

/* Called when the BFD is being closed to do any necessary cleanup.  */

static bool
m68k_elf32_atariprg_close_and_cleanup (bfd *abfd)
{
  struct mint_internal_info *myinfo = get_mint_internal_info_maybe_null (abfd);

  TRACE ("m68k_elf32_atariprg_close_and_cleanup %s %s\n", abfd->xvec->name, abfd->filename);

  if (myinfo != NULL && myinfo->relocs != NULL)
    {
      free (myinfo->relocs);
      myinfo->relocs = NULL;
    }

  /* myinfo itself has been allocated by bfd_zalloc()
     so will be automatically freed along with the BFD.
     Same for myinfo->tparel.  */

  return m68k_elf32_vec._close_and_cleanup (abfd);
}

/* objcopy/strip support.
   Sections are loaded from the input file, then written individually to the
   output file. During the process, some of them might be dropped. Headers are
   recreated from scratch. We must copy all extra data manually in order to
   generate the output file identically.  */

/* Copy private header information.  */

static bool
m68k_elf32_atariprg_copy_private_header_data (bfd *ibfd, bfd *obfd)
{
  struct mint_internal_info *myinfo_in;
  struct mint_internal_info *myinfo_out;

  TRACE ("m68k_elf32_atariprg_copy_private_header_data %s %s %s %s\n", ibfd->xvec->name, ibfd->filename, obfd->xvec->name, obfd->filename);

  /* First, call the base implementation.  */
  if (! m68k_elf32_vec._bfd_copy_private_header_data (ibfd, obfd))
    return false;

  /* obfd uses our file format, ibfd may be foreign.  */
  if (ibfd->xvec != &m68k_elf32_atariprg_vec)
    return true;

  myinfo_in = get_mint_internal_info (ibfd);
  myinfo_out = get_mint_internal_info (obfd);

  /* Copy extra header data.  */
  myinfo_out->prg_flags = myinfo_in->prg_flags;
  myinfo_out->stkpos = myinfo_in->stkpos;

  return true;
}

/* Read the TPA relocation table.  */

static bool read_tparel (bfd *abfd)
{
  struct mint_internal_info *myinfo = get_mint_internal_info (abfd);
#define TPAREL_CHUNK_SIZE 4096
  bfd_size_type alloc_size;
  bfd_size_type already_read;
  bfd_size_type amt;

  TRACE ("read_tparel %s %s\n", abfd->xvec->name, abfd->filename);

  BFD_ASSERT (myinfo->tparel == NULL);

  /* The TPA relocation position was determined when reading the PRG header.  */
  BFD_ASSERT (myinfo->tparel_pos > 0);
  if (bfd_seek (abfd, myinfo->tparel_pos, SEEK_SET) != 0)
    return false;

  /* We don't know the size of the TPA relocation table in advance. So just
     read chunks up to the end of file.  */

  alloc_size = TPAREL_CHUNK_SIZE;
  myinfo->tparel = bfd_malloc (alloc_size);
  if (myinfo->tparel == NULL)
    return false;

  already_read = 0;

  for (;;)
    {
      amt = bfd_bread (myinfo->tparel + already_read, TPAREL_CHUNK_SIZE, abfd);
      if (amt == (bfd_size_type) -1)
	return false;

      already_read += amt;

      if (amt < TPAREL_CHUNK_SIZE)
	{
	  myinfo->tparel_size = already_read;
	  return true;
	}

      alloc_size += TPAREL_CHUNK_SIZE;
      myinfo->tparel = bfd_realloc (myinfo->tparel, alloc_size);
      if (myinfo->tparel == NULL)
	return false;
    }
}

/* Copy the program header and other data from one object module to
   another.  */

static bool
m68k_elf32_atariprg_copy_private_bfd_data (bfd *ibfd, bfd *obfd)
{
  struct mint_internal_info *myinfo_in;
  struct mint_internal_info *myinfo_out;

  TRACE ("m68k_elf32_atariprg_copy_private_bfd_data %s %s %s %s\n", ibfd->xvec->name, ibfd->filename, obfd->xvec->name, obfd->filename);

  /* First, call the base implementation.  */
  if (! m68k_elf32_vec._bfd_copy_private_bfd_data (ibfd, obfd))
      return false;

  /* obfd uses our file format, ibfd may be foreign.  */
  if (ibfd->xvec != &m68k_elf32_atariprg_vec)
    return true;

  myinfo_in = get_mint_internal_info (ibfd);
  myinfo_out = get_mint_internal_info (obfd);

  /* Read the input TPA relocation table.  */
  if (! read_tparel (ibfd))
    return false;

  /* Allocate the output relocation table.  */
  myinfo_out->tparel_size = myinfo_in->tparel_size;
  myinfo_out->tparel = bfd_alloc (obfd, myinfo_out->tparel_size);
  if (myinfo_out->tparel == NULL)
    return false;

  /* Copy the TPA relocation table.  */
  memcpy (myinfo_out->tparel, myinfo_in->tparel, myinfo_out->tparel_size);

  return true;
}

/* Initialize our target.
   Called by bfd_init ().  */

void
bfd_elf32_atariprg_init (void)
{
  TRACE0 ("bfd_elf32_atariprg_init\n");

  /* Our target is basically the same as elf32-m68k...  */
  m68k_elf32_atariprg_vec = m68k_elf32_vec;
  m68k_elf32_atariprg_vec.name = "elf32-atariprg";

  /* ... but with a few overrides.  */
  m68k_elf32_atariprg_vec._bfd_check_format[bfd_object] = m68k_elf32_atariprg_object_p;
  m68k_elf32_atariprg_vec._bfd_set_format[bfd_object] = m68k_elf32_atariprg_make_object;
  m68k_elf32_atariprg_vec._bfd_sizeof_headers = m68k_elf32_atariprg_sizeof_headers;
  m68k_elf32_atariprg_vec._close_and_cleanup = m68k_elf32_atariprg_close_and_cleanup;
  m68k_elf32_atariprg_vec._bfd_copy_private_header_data = m68k_elf32_atariprg_copy_private_header_data;
  m68k_elf32_atariprg_vec._bfd_copy_private_bfd_data = m68k_elf32_atariprg_copy_private_bfd_data;
  m68k_elf32_atariprg_vec._bfd_final_link = m68k_elf32_atariprg_final_link;

  /* ELF backend data.  */
  m68k_elf32_atariprg_bed = *xvec_get_elf_backend_data(&m68k_elf32_atariprg_vec);
  m68k_elf32_atariprg_vec.backend_data = &m68k_elf32_atariprg_bed;
  m68k_elf32_atariprg_bed.maxpagesize = 2; /* Align segments on this.  */
  m68k_elf32_atariprg_bed.elf_backend_relocate_section = m68k_elf32_atariprg_relocate_section;

  /* ELF size info.  */
  m68k_elf32_atariprg_size_info = *m68k_elf32_atariprg_bed.s;
  m68k_elf32_atariprg_bed.s = &m68k_elf32_atariprg_size_info;
  m68k_elf32_atariprg_size_info.log_file_align = 1; /* 2**1 = 2 */
  m68k_elf32_atariprg_size_info.write_shdrs_and_ehdr = m68k_elf32_atariprg_write_shdrs_and_ehdr;
}
