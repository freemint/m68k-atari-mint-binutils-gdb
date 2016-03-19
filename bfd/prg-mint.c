/* BFD backend for traditional MiNT executables.
   Copyright 1998, 2007, 2008, 2009 Free Software Foundation, Inc.
   Originally written by Guido Flohr (guido@freemint.de).
   Modified by Vincent Riviere (vincent.riviere@freesbee.fr).

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

/* The format of executables on Atari is actually not a.out,  it is
   only chosen as an approach which comes close enough.  The layout of a
   program image on disk looked like this:

   +-----------------+
   | 28 Bytes Header |
   +-----------------+
   | Text segment    |
   +-----------------+
   | Data segment    |
   +-----------------+
   | BSS	     |
   +-----------------+
   | Symbol table    |
   +-----------------+
   | TPA relocation  |
   +-----------------+

   The 28 byte exec header used to look like this:

   struct old_exec_header
   {
     bfd_byte a_magic[2];
     bfd_byte a_text[4];
     bfd_byte a_data[4];
     bfd_byte a_bss[4];
     bfd_byte a_syms[4];
     bfd_byte a_resvd[4];
     bfd_byte a_abs[2];
   };

   The first two bytes (A_MAGIC) contained an assembler branch
   instruction to the beginning of the text segment.  Because the
   exec header had a fixed size and the text entry point was constant
   this assembler instruction also had a constant value (0x601a).
   In fact the operating system never really executed the branch
   instruction but used this value (0x601a) as a magic value.

   TEXT, DATA and BSS were as one would expect them.  The symbol
   table wasn't.  Several different formats were in use, none of them
   very efficient, none of them powerful enough to support source
   level debugging.  I've changed that and the GNU symbol table will
   now be used instead (unless the --traditional-format option was
   given to the linker).

   If the last member A_ABS of the exec header is zero the program
   image contains an additional table with relocation information
   at the end of the image.  The kernel can load program images at
   virtually any address in the address space.  In fact it will load
   it at the start of the biggest block of free memory.  This block
   is then called the Transient Program Area TPA and the image has
   to be relocated against the TPA at runtime.  The relocation info
   itself is in a simply way compressed:  It starts with a four-byte
   value, the first address within the image to be relocated.  Now
   following are one-byte offsets to the last address.  The special
   value of 1 (which is impossible as an offset) signifies that 254
   has to be added to the next offset.  The table is finished with
   a zero-byte.

   I now simply extended the header from its old 28 bytes to 256
   bytes.  The first 28 bytes give home to a standard Atari header,
   the rest is for extensions.  The extension header starts with
   a ``real'' assembler instruction, a far jump to the text entry
   point.  The extension header gives home to a standard a.out
   exec header (currently NMAGIC) plus some extra
   more or less useful fields plus space to future extensions.
   For the OS the extension header will already belong to the text
   segment, for BFD backends the text segment is 228 (or 0xe4)
   bytes smaller than for the OS.  This explains for example the
   funny TEXT_START_ADDR 0xe4.

   The TARGET_PAGE_SIZE is 2 which is only fake.  There is currently
   no such thing as memory paging on the Atari (and this is why
   ZMAGICs are disabled for now to allow for future enhancements).

   If you think that this whole file looks quite like a big hack
   you're probably right.  But the results (mainly the output of
   the linker) seem to work and they allow to use up-to-date
   binutils on the Atari until a better executable format (maybe
   ELF) has been established for this machine.  */

#include "sysdep.h"
#include "bfd.h"

#define N_HEADER_IN_TEXT(x) 0
#define BYTES_IN_WORD 4
#define ENTRY_CAN_BE_ZERO
#define N_SHARED_LIB(x) 0
#define TEXT_START_ADDR 0xe4
#define TARGET_PAGE_SIZE 2
#define TARGET_IS_BIG_ENDIAN_P
#define DEFAULT_ARCH bfd_arch_m68k
#define N_TXTADDR(x) TEXT_START_ADDR

/* Do not "beautify" the CONCAT* macro args.  Traditional C will not
   remove whitespace added here, and thus will fail to concatenate
   the tokens.  */
#define MY(OP) CONCAT2 (m68k_aout_mintprg_,OP)
#define TARGETNAME "a.out-mintprg"
#define NAME(x,y) CONCAT3 (mintprg,_32_,y)

/* We have to do quite a lot of magic to make the Atari format
   for GEMDOS executables fit into the standard a.out format.
   We start with the original header.  */
#define external_exec mint_external_exec
struct mint_external_exec
{
  bfd_byte g_branch[2]; 	     /* 0x601a.  */
  bfd_byte g_text[4];		     /* Length of text section.  */
  bfd_byte g_data[4];		     /* Length of data section.  */
  bfd_byte g_bss[4];		     /* Length of bss section.  */
  bfd_byte g_syms[4];		     /* Length of symbol table.  */
  bfd_byte g_extmagic[4];	     /* Always 0x4d694e54
					(in ASCII: ``MiNT'').  */
  bfd_byte g_flags[4];		     /* Atari special flags.  */
  bfd_byte g_abs[2];		     /* Non-zero if absolute (no relocation
					info.  */

  /* We extend this header now to provide the information that the
     binutils want to see.  Everything following will actually be part
     of the text segment (from MiNT's point of view).  As a
     consequence the text section has 228 bytes of redundancy.

     The following eight bytes should be treated as opaque.
     If the word ``opaque'' always attracts your curiosity in
     typedefs and structs, here's the explanation:  These eight bytes
     are really two assembler instructions.  The first one moves
     the contents of e_entry into register d0, the second one
     jumps (pc-relative) to the entry point.  See swap_exec_header_out
     for details.  */
  bfd_byte g_jump_entry[8];

  /* Now following a standard a.out header.  Note that the values
     may differ from the one given on top.  The traditional header
     contains the values that the OS wants to see, the values below
     are the values that make the binutils work.  */
  bfd_byte e_info[4];		     /* Magic number and stuff.  */
  bfd_byte e_text[4];		     /* Length of text section in bytes.  */
  bfd_byte e_data[4];		     /* Length of data section.  */
  bfd_byte e_bss[4];		     /* Length of standard symbol
					table.  */
  bfd_byte e_syms[4];		     /* Length of symbol table.  */
  bfd_byte e_entry[4];		     /* Start address.  */
  bfd_byte e_trsize[4]; 	     /* Length of text relocation
					info.  */
  bfd_byte e_drsize[4]; 	     /* Length of data relocation
					info.  */

  bfd_byte g_tparel_pos[4];	     /* File position of TPA relative
					relocation info.  */
  bfd_byte g_tparel_size[4];	     /* Length of TPA relative relocation
					info.  */

  /* This is for extensions.  */
  bfd_byte g_stkpos[4]; 	     /* If stacksize is hardcoded into
					the executable you will find it
					at file offset g_stkpos.  If
					not this is NULL.  */

  bfd_byte g_symbol_format[4];	     /* Format of the symbol table.  See
					definitions for _MINT_SYMBOL_FORMAT*
					above.  */

  /* Pad with zeros.  */
  bfd_byte g_pad0[172];
};
#define EXEC_BYTES_SIZE 256
#define GEMDOS_HEADER_SIZE 28

/* The following defines are required by aoutx.h.
   They are not automatically defined in aout/aout64.h
   if external_exec is defined.  */

#define OMAGIC 0407	/* Object file or impure executable.  */
#define NMAGIC 0410	/* Code indicating pure executable.  */
#define ZMAGIC 0413	/* Code indicating demand-paged executable.  */
#define BMAGIC 0415	/* Used by a b.out object.  */
#define QMAGIC 0314	/* Like ZMAGIC but with N_HEADER_IN_TEXT true.  */

/* Files using the following magic flags will not be loaded.  */
#define N_BADMAG(x)	(N_MAGIC(x) != NMAGIC)

/* For DRI symbol table format.  */
struct dri_symbol
{
  bfd_byte a_name[8];	  /* Symbol name */
  bfd_byte a_type[2];	  /* Type flag, i.e. A_TEXT etc; see below.  */
  bfd_byte a_value[4];	  /* value of this symbol (or sdb offset).  */
};
#define DRI_SYMBOL_SIZE 14

/* Simple values for a_type.  */
#define A_UNDF	0
#define A_BSS	0x0100
#define A_TEXT	0x0200
#define A_DATA	0x0400
#define A_EXT	0x0800	      /* External.  */
#define A_EQREG 0x1000	      /* Equated register.  */
#define A_GLOBL 0x2000	      /* Global.  */
#define A_EQU	0x4000	      /* Equated.  */
#define A_DEF	0x8000	      /* Defined.  */
#define A_LNAM	0x0048	      /* GST compatible long name.  */
			      /* File symbols ala aln.  */
#define A_TFILE 0x0280	      /* Text file corresponding to object module.  */
#define A_TFARC 0x02C0	      /* Text file archive.  Unfortunately this
				 conflicts with the bits in A_LNAM.  */

/* The following include contains the definitions for internal a.out structures
   as well as the prototypes for the NAME(...) functions defined in aoutx.h.  */

#include "libaout.h"

/* The following function is similar to _bfd_final_link_relocate, except it
   adds the reloc structure as an additional parameter.
   It will be used int aoutx.h.  */

static bfd_reloc_status_type
MY (final_link_relocate_rel) (reloc_howto_type *howto,
			      bfd *input_bfd,
			      asection *input_section,
			      bfd_byte *contents,
			      bfd_vma address,
			      bfd_vma value,
			      bfd_vma addend,
			      struct reloc_std_external *rel);

#define MY_final_link_relocate_rel MY (final_link_relocate_rel)

/* The following include contains the definitions for the NAME(...) functions.  */

#include "aoutx.h"

/* Data structure that holds some private information for us.  */
struct mint_internal_info
{
  struct bfd_link_info *linkinfo;    /* Remembered from final_link.  */
  bfd_boolean	traditional_format;  /* Saved from link info.  */
  int		symbol_format;	     /* Format of the symbol table.  */
  void		*tparel;	     /* Data for TPA relative relocation
					information.  */
  file_ptr	tparel_pos;	     /* File position of TPA relative
					relocation information.  */
  bfd_size_type tparel_size;	     /* Size of TPA relative relocation
					information.  */
  bfd_size_type dri_symtab_size;     /* Size of traditional symbol table.  */

#define MINT_RELOC_CHUNKSIZE 0x1000
  bfd_vma	*relocs;	     /* Array of address relocations.  */
  unsigned long relocs_used;	     /* Number of relocation entries
					already used up.  */
  unsigned long relocs_allocated;    /* Number of relocation entries
					allocated.  */

  bfd_vma	stkpos; 	     /* File offset to value of _stksize.  */

  flagword	prg_flags;	     /* Standard GEMDOS flags.  */

  bfd_boolean 	override_stack_size; /* TRUE if the executable stack size
					must be overriden with stack_size.  */
  bfd_signed_vma stack_size;

  bfd_boolean	reloc_error;	     /* TRUE if an unhandled error during
					relocation occured.  */
};

/* If --traditional-format was given to the linker an old-style DRI
   symbol table is written into the executable.  This is with respect
   to many old debugging tools or disassemblers which expect this format.
   Although created by the linker, these symbols will be ignored from
   input files.  */
#define _MINT_SYMBOL_FORMAT_GNU  0
#define _MINT_SYMBOL_FORMAT_DRI  1

/* Declarations for the variables and functions
   defined later in aout-target.h.  */

static const bfd_target *
MY (callback) (bfd *abfd);

static void
MY_final_link_callback (bfd *abfd,
			file_ptr *ptreloff,
			file_ptr *pdreloff,
			file_ptr *psymoff);

extern const bfd_target MY (vec);

/* Initialize a new BFD using our file format.  */

#define MY_mkobject MY (mkobject)

static bfd_boolean
MY (mkobject) (bfd *abfd)
{
  struct mint_internal_info *myinfo;

  if (!NAME (aout, mkobject (abfd)))
    return FALSE;

  /* Allocate our private BFD data.  */
  myinfo = bfd_zalloc (abfd, sizeof (*myinfo));
  if (myinfo == NULL)
    return FALSE;
  obj_aout_ext (abfd) = myinfo;

  return TRUE;
}

/* Finish up the reading of an a.out file header.  */

#define MY_object_p MY (object_p)

static const bfd_target *
MY (object_p) (bfd *abfd)
{
  struct external_exec exec_bytes;	/* Raw exec header from file.  */
  struct internal_exec exec;		/* Cleaned-up exec header.  */
  const bfd_target *target;
  bfd_size_type amt = EXEC_BYTES_SIZE;
  struct mint_internal_info *myinfo;

  /* Read the exec bytesd from the file.  */
  if (bfd_bread (&exec_bytes, amt, abfd) != amt)
    {
      if (bfd_get_error () != bfd_error_system_call)
	bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }

  /* Instead of byte-swapping we compare bytes.  */
  if (exec_bytes.g_branch[0] != 0x60
      || exec_bytes.g_branch[1] != 0x1a
      || exec_bytes.g_extmagic[0] != 'M'
      || exec_bytes.g_extmagic[1] != 'i'
      || exec_bytes.g_extmagic[2] != 'N'
      || exec_bytes.g_extmagic[3] != 'T')
    {
      bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }

  /* Swap the standard a.out fields.  */
  NAME (aout, swap_exec_header_in) (abfd, &exec_bytes, &exec);

  /* Check a.out magic value.  */
  if (N_BADMAG (exec))
    {
      bfd_set_error (bfd_error_wrong_format);
      return NULL;
    }

  /* Initialize this BFD with the exec values.  */
  target = NAME (aout, some_aout_object_p) (abfd, &exec, MY (callback));

  /* Allocate our private BFD data.  */
  myinfo = bfd_zalloc (abfd, sizeof (*myinfo));
  if (myinfo == NULL)
    return NULL;
  obj_aout_ext (abfd) = myinfo;

  /* Now get the missing information.  */
  myinfo->prg_flags = bfd_h_get_32 (abfd, exec_bytes.g_flags);
  myinfo->stkpos = bfd_h_get_32 (abfd, exec_bytes.g_stkpos);
  myinfo->symbol_format = bfd_h_get_32 (abfd, exec_bytes.g_symbol_format);

  /* TPA relocation information.  */
  myinfo->tparel_pos = bfd_h_get_32 (abfd, exec_bytes.g_tparel_pos);
  myinfo->tparel_size = bfd_h_get_32 (abfd, exec_bytes.g_tparel_size);

  /* FIXME:  Currently we always read the TPA relative relocation
     information.  This is suboptimal because often times there
     is no need for it.  Read it only if need be!  Maybe this should
     also depend on abfd->cacheable?  */
  if (myinfo->tparel_size == 0)
    myinfo->tparel = bfd_zalloc (abfd, 4);
  else
    myinfo->tparel = bfd_alloc (abfd, myinfo->tparel_size);

  if (myinfo->tparel == NULL)
    return NULL;

  if (myinfo->tparel_size == 0)
    {
      myinfo->tparel_size = 4;
    }
  else
    {
      /* Read the information from the bfd.  */
      if (bfd_seek (abfd, myinfo->tparel_pos, SEEK_SET) != 0
	  || (bfd_bread (myinfo->tparel, myinfo->tparel_size, abfd)
	      != myinfo->tparel_size))
	return NULL;
    }

  return target;
}

/* Free all information we have cached for this BFD.  We can always
   read it again later if we need it.  */

#define MY_bfd_free_cached_info MY (bfd_free_cached_info)

static bfd_boolean
MY (bfd_free_cached_info) (bfd *abfd)
{
  struct mint_internal_info *myinfo = obj_aout_ext (abfd);

  if (myinfo != NULL && myinfo->relocs != NULL)
    {
      free (myinfo->relocs);
      myinfo->relocs = NULL;
    }

  /* myinfo itself has been allocated by bfd_zalloc()
     so will be automatically freed along with the BFD.
     Same for myinfo->tparel.  */

  return NAME (aout, bfd_free_cached_info) (abfd);
}

/* Write a DRI symbol with TYPE and VALUE.  If the NAME of the
   symbol exceeds 8 characters write a long symbol.  If it
   exceeds 22 characters truncate the name.  */

static int
write_dri_symbol (bfd *abfd, const char *name, int type, bfd_vma value)
{
  int written_bytes = 0;
  struct dri_symbol sym;
  int is_long_name = strlen (name) > sizeof (sym.a_name);

  if (is_long_name)
    type |= A_LNAM;

  strncpy ((char*)sym.a_name, name, sizeof (sym.a_name));
  bfd_put_16 (abfd, type, sym.a_type);
  bfd_put_32 (abfd, value, sym.a_value);

  if (bfd_bwrite (&sym, DRI_SYMBOL_SIZE, abfd) != DRI_SYMBOL_SIZE)
    return -1;
  written_bytes += DRI_SYMBOL_SIZE;

  if (is_long_name)
    {
      char more_name[DRI_SYMBOL_SIZE];

      strncpy (more_name, name + sizeof (sym.a_name), DRI_SYMBOL_SIZE);

      if (bfd_bwrite (more_name, DRI_SYMBOL_SIZE, abfd) != DRI_SYMBOL_SIZE)
	return -1;
      written_bytes += DRI_SYMBOL_SIZE;
    }

  return written_bytes;
}

/* Emit a traditional DRI symbol table while linking.
   Most of this code comes from aout_link_write_symbols() in aoutx.h.  */

static bfd_boolean
link_write_traditional_syms (bfd *abfd, struct bfd_link_info *info)
{
  bfd			     *input_bfd;
  enum bfd_link_strip	     strip = info->strip;
  enum bfd_link_discard      discard = info->discard;
  struct mint_internal_info  *myinfo = obj_aout_ext (abfd);
  bfd			     *last_archive = NULL;

  /* Position file pointer.  */
  if (bfd_seek (abfd, obj_sym_filepos (abfd), SEEK_SET) != 0)
    return FALSE;

  myinfo->dri_symtab_size = 0;

  for (input_bfd = info->input_bfds; input_bfd != NULL; input_bfd = input_bfd->link.next)
    {
      bfd_size_type sym_count = obj_aout_external_sym_count (input_bfd);
      char *strings = obj_aout_external_strings (input_bfd);
      struct external_nlist *sym = obj_aout_external_syms (input_bfd);
      struct external_nlist *sym_end = sym + sym_count;
      struct aout_link_hash_entry **sym_hash = obj_aout_sym_hashes (input_bfd);
      bfd_boolean pass = FALSE;
      bfd_boolean skip = FALSE;
      bfd_boolean skip_next = FALSE;
      int written_bytes;
      int a_type;
      bfd_boolean write_archive_name = FALSE;
      bfd_vma val = 0;

      /* First write out a symbol for the archive if we do not
	 strip these symbols and if it differs from the last
	 one.  */
      if (input_bfd->my_archive != last_archive
	  && input_bfd->my_archive != NULL)
	{
	  write_archive_name = TRUE;
	  last_archive = input_bfd->my_archive;
	}

      if (write_archive_name
	  && strip != strip_all
	  && (strip != strip_some
	      || bfd_hash_lookup (info->keep_hash,
				  input_bfd->my_archive->filename,
				  FALSE, FALSE) != NULL)
	  && discard != discard_all)
	{
	  val = bfd_get_section_vma (abfd,
				     obj_textsec (input_bfd)->output_section)
	    + obj_textsec (input_bfd)->output_offset;

	  written_bytes = write_dri_symbol (abfd,
					    input_bfd->my_archive->filename,
					    A_TFILE, val);

	  if (written_bytes < 0)
	    return FALSE;
	  else
	    myinfo->dri_symtab_size += written_bytes;
	}

      /* Now write out a symbol for the object file if we do not
	 strip these symbols.  */
      if (strip != strip_all
	  && (strip != strip_some
	      || bfd_hash_lookup (info->keep_hash, input_bfd->filename,
				  FALSE, FALSE) != NULL)
	  && discard != discard_all)
	{
	  val = bfd_get_section_vma (abfd,
				     obj_textsec (input_bfd)->output_section)
	    + obj_textsec (input_bfd)->output_offset;

	  written_bytes = write_dri_symbol (abfd, input_bfd->filename,
					    A_TFILE, val);
	  if (written_bytes < 0)
	    return FALSE;
	  else
	    myinfo->dri_symtab_size += written_bytes;
	}

      /* Now we have a problem.  All symbols that we see have already
	 been marked written (because we write them a second time
	 here.  If we would do it the clean way we would have
	 to traverse the entire symbol map and reset the written
	 flag.  We hack here instead...  */
#define mark_written(h) (* (int *) &h->written = (int) TRUE + 1)
#define is_written(h) ((int) h->written == (int) TRUE + 1)
      for (; sym < sym_end; sym++, sym_hash++)
	{
	  const char *name;
	  int type;
	  struct aout_link_hash_entry *h;
	  asection *symsec;
	  val = 0;

	  type = H_GET_8 (input_bfd, sym->e_type);
	  name = strings + GET_WORD (input_bfd, sym->e_strx);

	  h = NULL;

	  if (pass)
	    {
	      /* Pass this symbol through.  It is the target of an
	      indirect or warning symbol.  */
	      val = GET_WORD (input_bfd, sym->e_value);
	      pass = FALSE;
	    }
	  else if (skip_next)
	    {
	      /* Skip this symbol, which is the target of an indirect
		 symbol that we have changed to no longer be an indirect
		 symbol.  */
	      skip_next = FALSE;
	      continue;
	    }
	  else
	    {
	      struct aout_link_hash_entry *hresolve = *sym_hash;

	      /* We have saved the hash table entry for this symbol, if
		 there is one.  Note that we could just look it up again
		 in the hash table, provided we first check that it is an
		 external symbol. */
	      h = *sym_hash;

	      /* Use the name from the hash table, in case the symbol was
		 wrapped.  */
	    if (h != NULL
		&& h->root.type != bfd_link_hash_warning)
		name = h->root.root.string;

	      /* If this is an indirect or warning symbol, then change
		 hresolve to the base symbol.  */
	      hresolve = h;
	      if (h != (struct aout_link_hash_entry *) NULL
		  && (h->root.type == bfd_link_hash_indirect
		      || h->root.type == bfd_link_hash_warning))
		{
		  hresolve = (struct aout_link_hash_entry*) h->root.u.i.link;
		  while (hresolve->root.type == bfd_link_hash_indirect
			 || hresolve->root.type == bfd_link_hash_warning)
		    hresolve = ((struct aout_link_hash_entry*)
				hresolve->root.u.i.link);
		}

	      /* If the symbol has already been written out skip it.  */
	      if (h != NULL
		  && is_written (h))
		{
		  if ((type & N_TYPE) == N_INDR
		      || type == N_WARNING)
		    skip_next = TRUE;
		  continue;
		}

	      /* See if we are stripping this symbol.  */
	      skip = FALSE;

	      /* Skip all debugger symbols.  No way to output them in
		 DRI format.  This will also reduce a lot of headaches.  */
	      if ((type & N_STAB) != 0)
		skip = TRUE;

	      switch (strip)
		{
		case strip_none:
		case strip_debugger:
		  break;
		case strip_some:
		  if (bfd_hash_lookup (info->keep_hash, name, FALSE, FALSE)
		      == NULL)
		    skip = TRUE;
		  break;
		case strip_all:
		  skip = TRUE;
		  break;
		}

	      if (skip)
		{
		  if (h != NULL)
		    mark_written (h);
		  continue;
		}

	      /* Get the value of the symbol.  */
	      if ((type & N_TYPE) == N_TEXT
		  || type == N_WEAKT)
		symsec = obj_textsec (input_bfd);
	      else if ((type & N_TYPE) == N_DATA
		       || type == N_WEAKD)
		symsec = obj_datasec (input_bfd);
	      else if ((type & N_TYPE) == N_BSS
		       || type == N_WEAKB)
		symsec = obj_bsssec (input_bfd);
	      else if ((type & N_TYPE) == N_ABS
		       || type == N_WEAKA)
		symsec = bfd_abs_section_ptr;
	      else if (((type & N_TYPE) == N_INDR
			&& (hresolve == NULL
			    || (hresolve->root.type != bfd_link_hash_defined
				&& hresolve->root.type != bfd_link_hash_defweak
				&& hresolve->root.type != bfd_link_hash_common)))
		       || type == N_WARNING)
		{
		  /* Pass the next symbol through unchanged.  The
		     condition above for indirect symbols is so that if
		     the indirect symbol was defined, we output it with
		     the correct definition so the debugger will
		     understand it.  */
		  pass = TRUE;
		  val = GET_WORD (input_bfd, sym->e_value);
		  symsec = NULL;
		}
	      else
		{
		  /* If we get here with an indirect symbol, it means that
		     we are outputting it with a real definition.  In such
		     a case we do not want to output the next symbol,
		     which is the target of the indirection.  */
		  if ((type & N_TYPE) == N_INDR)
		    skip_next = TRUE;

		  symsec = NULL;

		  /* We need to get the value from the hash table.  We use
		     hresolve so that if we have defined an indirect
		     symbol we output the final definition.  */
		  if (h == NULL)
		    {
		      switch (type & N_TYPE)
			{
			case N_SETT:
			  symsec = obj_textsec (input_bfd);
			  break;
			case N_SETD:
			  symsec = obj_datasec (input_bfd);
			  break;
			case N_SETB:
			  symsec = obj_bsssec (input_bfd);
			  break;
			case N_SETA:
			  symsec = bfd_abs_section_ptr;
			  break;
			default:
			  val = 0;
			  break;
			}
		    }
		  else if (hresolve->root.type == bfd_link_hash_defined
			   || hresolve->root.type == bfd_link_hash_defweak)
		    {
		      asection *input_section;
		      asection *output_section;

		      /* This case usually means a common symbol which was
			 turned into a defined symbol.  */
		      input_section = hresolve->root.u.def.section;
		      output_section = input_section->output_section;
		      BFD_ASSERT (bfd_is_abs_section (output_section)
				  || output_section->owner == abfd);

		      /* The following reference to the output section VMA
			 is commented out because DRI symbols are relative
			 to the beginning of the section.  */
		      val = (hresolve->root.u.def.value
			     /*+ bfd_get_section_vma (abfd, output_section)*/
			     + input_section->output_offset);

		      /* TEXT symbols values must be adjusted
			 by adding the size of the extended header.  */
		      if (output_section == obj_textsec (abfd))
			val += TEXT_START_ADDR;

		      /* Get the correct type based on the section.  If
			 this is a constructed set, force it to be
			 globally visible.  */
		      if (type == N_SETT
			  || type == N_SETD
			  || type == N_SETB
			  || type == N_SETA)
			type |= N_EXT;

		      type &=~ N_TYPE;

		      if (output_section == obj_textsec (abfd))
			type |= N_TEXT;
		      else if (output_section == obj_datasec (abfd))
			type |= N_DATA;
		      else if (output_section == obj_bsssec (abfd))
			type |= N_BSS;
		      else
			type |= N_ABS;
		    }
		  else if (hresolve->root.type == bfd_link_hash_common)
		    val = hresolve->root.u.c.size;
		  else if (hresolve->root.type == bfd_link_hash_undefweak)
		    {
		      val = 0;
		      type = N_UNDF;
		    }
		  else
		    val = 0;
		}
	      if (symsec != NULL)
		{
		  /* The following reference to the output section VMA
		     is commented out because DRI symbols are relative
		     to the beginning of the section.  */
		  val = (/*symsec->output_section->vma
			 +*/ symsec->output_offset
			 + (GET_WORD (input_bfd, sym->e_value)
			 - symsec->vma));

		  /* TEXT symbols values must be adjusted
		     by adding the size of the extended header.  */
		  if (symsec == obj_textsec (input_bfd))
		    val += TEXT_START_ADDR;
		}

	      /* If this is a global symbol set the written flag, and if
		 it is a local symbol see if we should discard it.  */
	      if (h != NULL)
		{
		  mark_written (h);
		}
	      else if ((type & N_TYPE) != N_SETT
		       && (type & N_TYPE) != N_SETD
		       && (type & N_TYPE) != N_SETB
		       && (type & N_TYPE) != N_SETA)
		{
		  switch (discard)
		    {
		    case discard_none:
		    case discard_sec_merge:
		      break;
		    case discard_l:
		      if (bfd_is_local_label_name (input_bfd, name))
			skip = TRUE;
		      break;
		    default:
		    case discard_all:
		      skip = TRUE;
		      break;
		    }
		  if (skip)
		    {
		      pass = FALSE;
		      continue;
		    }
		}
	    }

	  /* Now find the nearest type in DRI format.  */
	  switch (type)
	    {
	    case N_ABS:
	    case N_ABS | N_EXT:
	    case N_SETA:
	    case N_SETA | N_EXT:
	    case N_WEAKA:
	      a_type = A_EQU | A_DEF | A_GLOBL;
	      break;
	    case N_TEXT:
	    case N_TEXT | N_EXT:
	    case N_SETT:
	    case N_SETT | N_EXT:
	    case N_WEAKT:
	      a_type = A_TEXT | A_DEF | A_GLOBL;
	      break;
	    case N_DATA:
	    case N_DATA | N_EXT:
	    case N_SETD:
	    case N_SETD | N_EXT:
	    case N_WEAKD:
	      a_type = A_DATA | A_DEF | A_GLOBL;
	      break;
	    case N_BSS:
	    case N_BSS | N_EXT:
	    case N_SETB:
	    case N_SETB | N_EXT:
	    case N_WEAKB:
	      a_type = A_BSS | A_DEF | A_GLOBL;
	      break;
	    default:
	      continue;
	    }

	  written_bytes = write_dri_symbol (abfd, name, a_type, val);
	  if (written_bytes < 0)
	    return FALSE;

	  myinfo->dri_symtab_size += written_bytes;
	}
    }

  obj_aout_external_string_size (abfd) = 0;
  return TRUE;
}

/* This is used for qsort() to sort addresses
   for the TPA relocation table.  */

static int
vma_cmp (const void *v1, const void *v2)
{
  return (int) ((*((bfd_vma *) v1)) - (*((bfd_vma *) v2)));
}

/* Alloc and fill the TPA relocation table.  */

static bfd_boolean
fill_tparel (bfd *abfd)
{
  struct mint_internal_info *myinfo = obj_aout_ext (abfd);
  unsigned long i;
  bfd_size_type bytes;
  unsigned char *ptr;

  /* Sort the relocation info.  */
  if (myinfo->relocs != NULL)
    qsort (myinfo->relocs, myinfo->relocs_used, sizeof (bfd_vma),
	   vma_cmp);

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
      unsigned long diff = myinfo->relocs[i] - myinfo->relocs[i - 1];
      BFD_ASSERT(diff > 0);
      bytes += (diff + 253) / 254;
    }
  /* Last entry is (bfd_byte) 0 if there are some relocations.  */
  if (myinfo->relocs_used > 0)
    bytes++;

  myinfo->tparel_size = bytes;
  myinfo->tparel = bfd_alloc (abfd, bytes);
  if (myinfo->tparel == NULL)
    return FALSE;

  /* Now fill the array.  */
  ptr = (bfd_byte*) myinfo->tparel;
  if (myinfo->relocs != NULL)
    bfd_put_32 (abfd, myinfo->relocs[0], ptr);
  else
    bfd_put_32 (abfd, 0, ptr);
  ptr += 4;

  for (i = 1; i < myinfo->relocs_used; i++)
    {
      unsigned long diff = myinfo->relocs[i] - myinfo->relocs[i - 1];
      while (diff > 254)
	{
	  *ptr++ = 1;
	  diff -= 254;
	}
      *ptr++ = (bfd_byte) diff;
    }

  if (myinfo->relocs_used > 0)
    *ptr = 0;

  return TRUE;
}

/* Final link routine.  We need to use a call back to get the correct
   offsets in the output file.  And we need to malloc some internal
   buffers.  */

#define MY_bfd_final_link MY (bfd_final_link)

static bfd_boolean
MY (bfd_final_link) (bfd *abfd, struct bfd_link_info *info)
{
  struct mint_internal_info *myinfo = obj_aout_ext (abfd);
  struct bfd_link_hash_table *hash = info->hash;
  enum bfd_link_strip original_strip = info->strip;

  if (bfd_link_relocatable (info))
    {
      _bfd_error_handler ("%B: relocatable output is not supported by format %s",
	abfd, bfd_get_target (abfd));
      bfd_set_error (bfd_error_invalid_operation);
      return FALSE;
    }

  myinfo->linkinfo = info;

  /* Make sure that for now we never write zmagics.  */
  abfd->flags &= ~D_PAGED;

  /* Find the __stksize symbol.  This symbol is used for a MiNT
     special kludge.  The libc defines this symbol in an object file
     initialized to a default value to make sure it is defined in
     every output file.  The start-up code in crtinit() then simply
     sets the stacksize accordingly.  In your programs (if they need
     an unusual stacksize) you can then simply code:

	   long _stksize = 0x2000;

     This will create a program stack of 2k.  Since MiNT cannot detect
     a stack overflow this is the only way to prevent program crashes
     caused by a stack that is too small.

     The ancient linker ignored this feature, the ancient strip
     program paid heed to it.  By default, strip never stripped this
     special symbol from the binary.

     Another program called ``printstk'' and its colleague ``fixstk''
     could be used to either print the current value of the stacksize
     or to modify it without recompiling and rebuilding.  These
     programs traversed the symbol table and then took the appropriate
     measures if the symbol was found.

     Here we do a different approach.  Since we already expanded the
     standard executable header we now hardcode the address (as a file
     offset) that the __stksize symbol points to into the header.  We
     can now let strip safely remove the entry from the symbol table
     and we're not dependent on a special format of the symbol table.
     Because the address is kept in the header we will always be able
     to manipulate the stacksize value later.  */
  if (hash != NULL)
    {
      struct aout_link_hash_entry *h =
	aout_link_hash_lookup (aout_hash_table (info), "__stksize",
			       FALSE, FALSE, FALSE);
      asection *sec;

      if (h != NULL)
	{
	  switch (h->root.type)
	    {
	    case bfd_link_hash_defined:
	    case bfd_link_hash_defweak:
	      sec = h->root.u.def.section->output_section;
	      BFD_ASSERT (sec->owner == abfd);

	      myinfo->stkpos = (h->root.u.def.value + sec->vma
				+ h->root.u.def.section->output_offset
				+ GEMDOS_HEADER_SIZE);
	      break;
	    default:  /* Ignore other types.  */
	      break;
	    }
	}
    }

  if ((abfd->flags & BFD_TRADITIONAL_FORMAT) != 0)
    {
      myinfo->traditional_format = TRUE;
      myinfo->symbol_format = _MINT_SYMBOL_FORMAT_DRI;
    }

  /* Unconditionally unset the traditional flag.  The only effect in
     the a.out code is to disable string hashing (with respect to
     SunOS gdx).  This is not necessary for us.  */

  abfd->flags &= ~BFD_TRADITIONAL_FORMAT;

  /* Do not write GNU symbols in traditional format.  */
  if (myinfo->traditional_format)
    info->strip = strip_all;

  if (NAME(aout,final_link) (abfd, info, MY_final_link_callback)
      != TRUE)
    return FALSE;

  if (myinfo->reloc_error)
    return FALSE;

  /* Restore the strip status for the traditional symbols.  */
  info->strip = original_strip;

  if (myinfo->traditional_format
      && link_write_traditional_syms (abfd, info) != TRUE)
    return FALSE;

  if (fill_tparel (abfd) != TRUE)
    return FALSE;

  return TRUE;
}

/* Copy private BFD header information from the input BFD.  */

#define MY_bfd_copy_private_header_data MY (bfd_copy_private_header_data)

static bfd_boolean
MY (bfd_copy_private_header_data) (bfd *ibfd, bfd *obfd)
{
  (void)obfd; /* Unused.  */

  /* We can only copy BFD files using our own file format.  */
  if (ibfd->xvec != &MY (vec))
    {
      _bfd_error_handler ("%B: cannot convert from format %s to format %s",
	ibfd, bfd_get_target (ibfd), bfd_get_target (obfd));
      bfd_set_error (bfd_error_invalid_operation);
      return FALSE;
    }

  return TRUE;
}

/* Copy backend specific data from one object module to another.
   This function is used by objcopy and strip.  */

#define MY_bfd_copy_private_bfd_data MY (bfd_copy_private_bfd_data)

static bfd_boolean
MY (bfd_copy_private_bfd_data) (bfd *ibfd, bfd *obfd)
{
  struct mint_internal_info *myinfo_in;
  struct mint_internal_info *myinfo_out;

  /* obfd uses our file format, ibfd may be foreign.  */
  if (ibfd->xvec != &MY (vec))
    return TRUE;

  myinfo_in = obj_aout_ext (ibfd);
  BFD_ASSERT (myinfo_in != NULL);

  myinfo_out = obj_aout_ext (obfd);
  BFD_ASSERT (myinfo_out != NULL);

  /* Copy myinfo.  */
  memcpy (myinfo_out, myinfo_in, sizeof (*myinfo_out));

  /* Copy tparel.  */
  myinfo_out->tparel = bfd_alloc (obfd, myinfo_out->tparel_size);
  if (myinfo_out->tparel == NULL)
    return FALSE;
  memcpy (myinfo_out->tparel, myinfo_in->tparel, myinfo_out->tparel_size);

  /* Normalize the type of empty symbols.  */
  if (bfd_get_symcount (obfd) == 0)
    myinfo_out->symbol_format = _MINT_SYMBOL_FORMAT_GNU;

  return TRUE; /* _bfd_generic_bfd_copy_private_bfd_data (ibfd, obfd); */
}

/* Merge private BFD information from an input BFD to the output BFD when linking.  */

#define MY_bfd_merge_private_bfd_data MY (merge_private_bfd_data)

static bfd_boolean
MY (merge_private_bfd_data) (bfd *ibfd, bfd *obfd)
{
  (void)obfd; /* Unused.  */

  /* Our file format cannot be used as linker input.  */
  if (ibfd->xvec == &MY (vec))
    {
      _bfd_error_handler ("%B: file format %s cannot be used as linker input",
	ibfd, bfd_get_target (ibfd));
      bfd_set_error (bfd_error_invalid_operation);
      return FALSE;
    }

  return TRUE; /* _bfd_generic_bfd_merge_private_bfd_data (ibfd, obfd); */
}

/* Find out the symbol name.  */

static const char *
find_symbol_name (reloc_howto_type *howto, bfd *input_bfd,
		  bfd_byte *location, struct reloc_std_external *rel)
{
  struct external_nlist *syms = obj_aout_external_syms (input_bfd);
  char *strings = obj_aout_external_strings (input_bfd);
  struct aout_link_hash_entry **sym_hashes
    = obj_aout_sym_hashes (input_bfd);
  struct aout_link_hash_entry *h = NULL;
  const char *name;
  bfd_size_type r_index;
  int r_extern;

  if (bfd_get_reloc_size (howto) != 4)
    return "(not a symbol)";

  /* The input bfd is always big-endian.  There is no need to
     call bfd_header_big_endian (input_bfd).  */
  r_index  = ((rel->r_index[0] << 16)
	      | (rel->r_index[1] << 8)
	      | (rel->r_index[2]));
  r_extern = (0 != (rel->r_type[0] & RELOC_STD_BITS_EXTERN_BIG));

  if (sym_hashes != NULL)
    h = sym_hashes[r_index];

  if (!r_extern)
    {
      bfd_size_type i;
      bfd_vma wanted_value = bfd_get_32 (input_bfd, location);

      name = NULL;
      for (i = 0; i < obj_aout_external_sym_count (input_bfd); i++)
	{
	  bfd_vma this_value = bfd_get_32 (input_bfd, syms[i].e_value);

	  if (this_value == wanted_value)
	    {
	      bfd_byte symtype = bfd_get_8 (input_bfd, syms[i].e_type);

	      /* Skip debug symbols and the like.  */
	      if ((symtype & N_STAB) != 0)
		continue;

	      /* This is dirty but preferable to a plethoria of
		 single comparisons.  */
	      if (symtype <= (N_BSS | N_EXT)
		  || (symtype >= N_WEAKU && symtype <= N_COMM))
		{
		  name = strings + GET_WORD (input_bfd, syms[i].e_strx);
		  break;
		}
	    }
	}

      /* FIXME:  If the relocation is against a section there is
	 probably a symbol for that section floating around somewhere
	 in the bfd jungle.  */
      if (name == NULL)
	{
	  switch ((r_index & N_TYPE) & ~N_EXT)
	    {
	    case N_TEXT:
	      name = "text section";
	      break;
	    case N_DATA:
	      name = "data section";
	      break;
	    case N_BSS:
	      name = "bss section";
	      break;
	    case N_ABS:
	      name = "absolute section";
	      break;
	    default:
	      name = "unknown section";
	      break;
	    }
	}
    }
  else if (h != NULL)
    name = h->root.root.string;
  else if (r_index >= obj_aout_external_sym_count (input_bfd))
    name = "(unknown symbol)";	/* Shouldn't happen.  */
  else
    name = strings + GET_WORD (input_bfd, syms[r_index].e_strx);

  return name;
}

/* This relocation routine is used by some of the backend linkers.
   They do not construct asymbol or arelent structures, so there is no
   reason for them to use bfd_perform_relocation.  Also,
   bfd_perform_relocation is so hacked up it is easier to write a new
   function than to try to deal with it.

   This routine does a final relocation.  Whether it is useful for a
   relocatable link depends upon how the object format defines
   relocations.

   FIXME: This routine ignores any special_function in the HOWTO,
   since the existing special_function values have been written for
   bfd_perform_relocation.

   HOWTO is the reloc howto information.
   INPUT_BFD is the BFD which the reloc applies to.
   INPUT_SECTION is the section which the reloc applies to.
   CONTENTS is the contents of the section.
   ADDRESS is the address of the reloc within INPUT_SECTION.
   VALUE is the value of the symbol the reloc refers to.
   ADDEND is the addend of the reloc.  */

/* The additional parameter REL is specific to this backend.
   This function is derived from _bfd_final_link_relocate()
   found in reloc.c. It adds additional checking for dangerous
   relocations in MiNT sharable text sections, then it records
   the relocated offset in myinfo->relocs[] for further processing.  */

static bfd_reloc_status_type
MY (final_link_relocate_rel) (reloc_howto_type *howto,
			      bfd *input_bfd,
			      asection *input_section,
			      bfd_byte *contents,
			      bfd_vma address,
			      bfd_vma value,
			      bfd_vma addend,
			      struct reloc_std_external *rel)
{
  bfd_vma relocation;
  bfd *output_bfd = input_section->output_section->owner;
  struct mint_internal_info *myinfo = obj_aout_ext (output_bfd);
  bfd_reloc_status_type retval;
  int r_index;
  int r_extern;
  bfd_boolean need_tpa_relocation;

  /* The input bfd is always big-endian.  There is no need to
     call bfd_header_big_endian (input_bfd).  */
  r_index  = ((rel->r_index[0] << 16)
	      | (rel->r_index[1] << 8)
	      | (rel->r_index[2]));
  r_extern = (0 != (rel->r_type[0] & RELOC_STD_BITS_EXTERN_BIG));

#define _MINT_F_SHTEXT 0x800

  /* Sanity check the address.  */
  if (address > bfd_get_section_limit (input_bfd, input_section))
    return bfd_reloc_outofrange;

  /* This function assumes that we are dealing with a basic relocation
     against a symbol.  We want to compute the value of the symbol to
     relocate to.  This is just VALUE, the value of the symbol, plus
     ADDEND, any addend associated with the reloc.  */
  relocation = value + addend;

  /* Check for dangerous relocations in images with a sharable
     text section.  */
  if ((myinfo->prg_flags & _MINT_F_SHTEXT) != 0
      && bfd_get_reloc_size (howto) == 4)
    {
      bfd_boolean error_found = FALSE;
      const char *name = NULL;

      if (input_section == obj_textsec (input_bfd))
	{
	  if (!r_extern)
	    {
	      /* This is a relocation against another section.  Only
		 relocations against the text section are allowed.  */
	      if (r_index != N_TEXT && r_index != (N_TEXT | N_EXT))
		error_found = TRUE;
	    }
	  else if (relocation > (input_section->output_section->vma
			    + input_section->output_section->size))
	    {
	      error_found = TRUE;
	    }
	  else if (relocation == (input_section->output_section->vma
				  + input_section->output_section->size))
	    {
	      name = find_symbol_name (howto, input_bfd,
						    contents + address,
						    rel);
	      if (strcmp (name, "_etext") == 0)
		error_found = FALSE;
	    }
	}

      if (error_found)
	{
	  const struct bfd_link_callbacks *callbacks
	    = myinfo->linkinfo->callbacks;

	  myinfo->reloc_error = TRUE;

	  if (callbacks->reloc_dangerous != NULL)
	    {
	      if (name == NULL)
		name = find_symbol_name (howto, input_bfd,
						      contents + address,
						      rel);

	      callbacks->reloc_dangerous (myinfo->linkinfo, name,
					  input_bfd,
					  input_section, address);
	    }
	}
    }

  /* If the relocation is PC relative, we want to set RELOCATION to
     the distance between the symbol (currently in RELOCATION) and the
     location we are relocating.  Some targets (e.g., i386-aout)
     arrange for the contents of the section to be the negative of the
     offset of the location within the section; for such targets
     pcrel_offset is FALSE.  Other targets (e.g., m88kbcs or ELF)
     simply leave the contents of the section as zero; for such
     targets pcrel_offset is TRUE.  If pcrel_offset is FALSE we do not
     need to subtract out the offset of the location within the
     section (which is just ADDRESS).  */
  if (howto->pc_relative)
    {
      relocation -= (input_section->output_section->vma
		     + input_section->output_offset);
      if (howto->pcrel_offset)
	relocation -= address;
    }

  retval = _bfd_relocate_contents (howto, input_bfd, relocation,
				   contents + address);

  /* The symbol has to be relocated again iff the length of the relocation
     is 2 words and it is not pc relative.  */
  need_tpa_relocation = FALSE;
  if (!howto->pc_relative && bfd_get_reloc_size (howto) == 4)
    {
      if (r_extern)
	{
	  struct aout_link_hash_entry **sym_hashes = obj_aout_sym_hashes (input_bfd);
	  struct aout_link_hash_entry *h = sym_hashes[r_index];
	  asection *output_section = h->root.u.def.section->output_section;

	  /* Do not relocate absolute symbols.  */
	  if (output_section == obj_textsec (output_bfd)
	      || output_section == obj_datasec (output_bfd)
	      || output_section == obj_bsssec (output_bfd))
	    {
	      need_tpa_relocation = TRUE;
	    }
	}
      else
	{
	  need_tpa_relocation = TRUE;
	}
    }

  /* Here we add the TPA relocation entries for the address references
     located inside the input sections. Note that if some references
     to addresses are generated using data statements in the linker
     script, they will not be relocated here because they do not
     belong to any input section.  */
  if (need_tpa_relocation)
    {
      bfd_vma tpa_address = input_section->output_section->vma
	+ input_section->output_offset + address;

      if (!bfd_m68kmint_add_tpa_relocation_entry(output_bfd, tpa_address))
	return bfd_reloc_other;
    }

  return retval;
}

/* Write out the TPA relocation table.  */

static bfd_boolean
write_tparel (bfd *abfd, struct internal_exec *execp)
{
  struct mint_internal_info* myinfo = obj_aout_ext (abfd);

  if (myinfo->dri_symtab_size == 0)
    myinfo->tparel_pos = N_STROFF (*execp)
      + obj_aout_external_string_size (abfd);
  else
    myinfo->tparel_pos = N_SYMOFF (*execp)
      + myinfo->dri_symtab_size;

  if (bfd_seek (abfd, myinfo->tparel_pos, SEEK_SET) != 0)
    return FALSE;

  if (bfd_bwrite (myinfo->tparel, myinfo->tparel_size, abfd)
      != myinfo->tparel_size)
    return FALSE;

  return TRUE;
}

/* Write the full exec header.
   This function must be called last to ensure that we have all the
   information needed to fill the MiNT-specific header fields.  */

static bfd_boolean
write_exec_header (bfd *abfd, struct internal_exec *execp, struct external_exec *exec_bytes)
{
  struct mint_internal_info *myinfo = obj_aout_ext (abfd);
  bfd_size_type symtab_size;

  bfd_h_put_16 (abfd, 0x601a, exec_bytes->g_branch);

  /* The OS will load our extension header fields into the text segment.  */
  bfd_h_put_32 (abfd, execp->a_text + (EXEC_BYTES_SIZE - GEMDOS_HEADER_SIZE),
		exec_bytes->g_text);
  bfd_h_put_32 (abfd, execp->a_data, exec_bytes->g_data);
  bfd_h_put_32 (abfd, execp->a_bss, exec_bytes->g_bss);

  /* The OS' notion of the size of the symbol table is another than
     the bfd library's.  We have to fill in the size of the table
     itself plus the size of the string table but only if we have not written
     a traditional symbol table.  If we have written a traditional symbol
     table we know the size.  */
  if (myinfo->dri_symtab_size != 0)
    symtab_size = myinfo->dri_symtab_size;
  else
    symtab_size = myinfo->tparel_pos - N_SYMOFF (*execp);

  bfd_h_put_32 (abfd, symtab_size, exec_bytes->g_syms);

  bfd_h_put_32 (abfd, 0x4d694e54, exec_bytes->g_extmagic);
  bfd_h_put_32 (abfd, myinfo->prg_flags, exec_bytes->g_flags);
  bfd_h_put_16 (abfd, 0, exec_bytes->g_abs);

  /* Generate the jump instruction to the entry point.  In m68k
     assembler mnemnonics it looks more or less like this:

       move.l  exec_bytes->e_entry(pc),d0
       jmp     -6(pc,d0.l)

     Sorry for the wrong syntax.  As a real assembler addict I
     never actually use an assembler.  I edit my binaries manually
     with a hex editor, looks much cooler and it strengthens your
     abstraction abilities.  */

  exec_bytes->g_jump_entry[0] = 0x20;
  exec_bytes->g_jump_entry[1] = 0x3a;
  exec_bytes->g_jump_entry[2] = 0x00;
  exec_bytes->g_jump_entry[3] = 0x1a;
  exec_bytes->g_jump_entry[4] = 0x4e;
  exec_bytes->g_jump_entry[5] = 0xfb;
  exec_bytes->g_jump_entry[6] = 0x08;
  exec_bytes->g_jump_entry[7] = 0xfa;

  bfd_h_put_32 (abfd, myinfo->tparel_pos, exec_bytes->g_tparel_pos);
  bfd_h_put_32 (abfd, myinfo->tparel_size, exec_bytes->g_tparel_size);
  bfd_h_put_32 (abfd, myinfo->stkpos, exec_bytes->g_stkpos);

  /* If there are no symbols, pretend they are in GNU format.  */
  if (symtab_size == 0)
    myinfo->symbol_format = _MINT_SYMBOL_FORMAT_GNU;

  bfd_h_put_32 (abfd, myinfo->symbol_format, exec_bytes->g_symbol_format);

  memset (&exec_bytes->g_pad0, 0, sizeof (exec_bytes->g_pad0));

  /* The standard stuff.  */
  NAME(aout, swap_exec_header_out) (abfd, execp, exec_bytes);
  if (myinfo->symbol_format != _MINT_SYMBOL_FORMAT_GNU)
    PUT_WORD (abfd, 0, exec_bytes->e_syms);

  if (bfd_seek (abfd, (file_ptr) 0, SEEK_SET) != 0)
    return FALSE;

  if (bfd_bwrite (exec_bytes, (bfd_size_type) EXEC_BYTES_SIZE, abfd)
      != EXEC_BYTES_SIZE)
    return FALSE;

  /* Override the stack size.  */
  if (myinfo->override_stack_size && myinfo->stkpos)
  {
    bfd_byte big_endian_stack_size[4];

    bfd_put_32 (abfd, myinfo->stack_size, &big_endian_stack_size);

    if (bfd_seek (abfd, (file_ptr) myinfo->stkpos, SEEK_SET) != 0)
      return FALSE;

    if (bfd_bwrite (big_endian_stack_size, 4, abfd) != 4)
      return FALSE;
  }

  return TRUE;
}

/* Write an object file.
   Section contents have already been written.  We write the
   file header, symbols, and relocation.  */

#define MY_write_object_contents MY (write_object_contents)

static bfd_boolean
MY (write_object_contents) (bfd *abfd)
{
  struct external_exec exec_bytes;
  struct internal_exec *execp = exec_hdr (abfd);
  bfd_size_type text_size;
  file_ptr text_end;

  BFD_ASSERT (obj_aout_ext (abfd) != NULL);

  obj_reloc_entry_size (abfd) = RELOC_STD_SIZE;

  /* Most of the following code come from the WRITE_HEADERS macro
     found in libaout.h.  */

  if (adata(abfd).magic == undecided_magic)
    NAME (aout, adjust_sizes_and_vmas) (abfd, & text_size, & text_end);

  execp->a_syms = bfd_get_symcount (abfd) * EXTERNAL_NLIST_SIZE;
  execp->a_entry = bfd_get_start_address (abfd);

  execp->a_trsize = ((obj_textsec (abfd)->reloc_count) *
		     obj_reloc_entry_size (abfd));
  execp->a_drsize = ((obj_datasec (abfd)->reloc_count) *
		     obj_reloc_entry_size (abfd));

  /* Now write out reloc info, followed by syms and strings.  */

  if (bfd_get_outsymbols (abfd) != NULL
      && bfd_get_symcount (abfd) != 0)
    {
      if (bfd_seek (abfd, (file_ptr) (N_SYMOFF(*execp)), SEEK_SET) != 0)
	return FALSE;

      if (! NAME (aout, write_syms) (abfd))
	return FALSE;
    }

  if (bfd_seek (abfd, (file_ptr) (N_TRELOFF (*execp)), SEEK_SET) != 0)
    return FALSE;
  if (!NAME (aout, squirt_out_relocs) (abfd, obj_textsec (abfd)))
    return FALSE;

  if (bfd_seek (abfd, (file_ptr) (N_DRELOFF (*execp)), SEEK_SET) != 0)
    return FALSE;
  if (!NAME (aout, squirt_out_relocs) (abfd, obj_datasec (abfd)))
    return FALSE;

  if (write_tparel (abfd, execp) != TRUE)
    return FALSE;

  if (write_exec_header (abfd, execp, &exec_bytes) != TRUE)
    return FALSE;

  return TRUE;
}

/* Print private BFD data. Used by objdump -p.  */

#define MY_bfd_print_private_bfd_data MY (print_private_bfd_data)

static bfd_boolean
MY (print_private_bfd_data) (bfd *abfd, void *ptr)
{
  FILE *file = (FILE *) ptr;
  struct mint_internal_info *myinfo = obj_aout_ext (abfd);
  const char* symbol_format;
  long stksize = 0;

  fprintf (file, "\n");

  fprintf (file, " GEMDOS flags: 0x%08lx\n", (unsigned long) myinfo->prg_flags);
  fprintf (file, "Start address: 0x%08lx\n", bfd_get_start_address (abfd));

  /* Stack size.  */
  if (myinfo->stkpos != 0)
    {
      if (bfd_seek (abfd, myinfo->stkpos, SEEK_SET) != 0
	  || (bfd_bread (&stksize, sizeof(long), abfd) != sizeof(long)))
	return FALSE;

      stksize = bfd_get_signed_32 (abfd, &stksize);
    }
  fprintf (file, "   Stack size: %ld\n", stksize);

  /* Symbol format.  */
  switch (myinfo->symbol_format)
    {
      case _MINT_SYMBOL_FORMAT_GNU: symbol_format = "stabs"; break;
      case _MINT_SYMBOL_FORMAT_DRI: symbol_format = "DRI";   break;
      default:			    symbol_format = "?";     break;
    }
  fprintf (file, "Symbol format: %s\n", symbol_format);

  return TRUE;
}

/* Special case for NAME (aout, get_section_contents)
   It is not declared in libaout.h, neither implemented in aoutx.h.
   Instead, a macro named aout_32_get_section_contents is defined in libaout.h.
   So the default value of MY_get_section_contents provided by aout-target.h
   is not correct, it has to be defined here with the right value.  */

#define MY_get_section_contents aout_32_get_section_contents

/* The following include will define MY (vec)
   and a default implementation for all the MY_ functions
   not overriden here.  */

#include "aout-target.h"

/* Set the GEMDOS executable flags.
   It is called by the linker emulation script.  */

bfd_boolean
bfd_m68kmint_set_extended_flags (bfd *abfd, flagword prg_flags)
{
  struct mint_internal_info *myinfo;

  BFD_ASSERT(abfd->xvec == &MY (vec));
  myinfo = obj_aout_ext (abfd);
  BFD_ASSERT(myinfo != NULL);

  myinfo->prg_flags = prg_flags;

  return TRUE;
}

/* Override the stack size.
   It is called by the linker emulation script.  */

bfd_boolean
bfd_m68kmint_set_stack_size (bfd *abfd, bfd_signed_vma stack_size)
{
  struct mint_internal_info *myinfo;

  BFD_ASSERT(abfd->xvec == &MY (vec));
  myinfo = obj_aout_ext (abfd);
  BFD_ASSERT(myinfo != NULL);

  myinfo->stack_size = stack_size;
  myinfo->override_stack_size = TRUE;

  return TRUE;
}

/* Add a TPA relocation entry.
   It is called by BFD when linking the input sections, and by the
   linker when it generates a reference to an address (in particular,
   when building the constructors list).  */

bfd_boolean
bfd_m68kmint_add_tpa_relocation_entry (bfd *abfd, bfd_vma address)
{
  struct mint_internal_info *myinfo;

  BFD_ASSERT(abfd->xvec == &MY (vec));
  myinfo = obj_aout_ext (abfd);
  BFD_ASSERT(myinfo != NULL);

  /* Enlarge the buffer if necessary.  */
  if (myinfo->relocs_used * sizeof (bfd_vma) >= myinfo->relocs_allocated)
    {
      bfd_vma *newbuf;
      myinfo->relocs_allocated += MINT_RELOC_CHUNKSIZE;
      newbuf = bfd_realloc (myinfo->relocs, myinfo->relocs_allocated);
      if (newbuf == NULL)
	return FALSE;

      myinfo->relocs = newbuf;
    }

  /* The TPA relative relocation actually just adds the address of
     the text segment (i. e. beginning of the executable in memory)
     to the addresses at the specified locations.  This allows an
     executable to be loaded everywhere in the address space without
     memory management.  */
  myinfo->relocs[myinfo->relocs_used++] = address;

  return TRUE;
}
