/* BFD backend for traditional MiNT executables.
   Copyright (C) 1998 Free Software Foundation, Inc.
   Written by Guido Flohr (gufl0000@stud.uni-sb.de).

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

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
   | BSS             |
   +-----------------+
   | Symbol table    |
   +-----------------+
   | TPA relocation  |
   +-----------------+

   The 28 byte exec header used to look like this:

   struct old_exec_header {
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
   a ``real'' assembler instsruction, a far jump to the text entry
   point.  The extension header gives home to a standard a.out
   exec header (currently NMAGIC or OMAGIC only) plus some extra
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

#define N_HEADER_IN_TEXT(x) 0
#define BYTES_IN_WORD 4
#define ENTRY_CAN_BE_ZERO
#define N_SHARED_LIB(x) 0 /* Avoids warning.  */
#define TEXT_START_ADDR 0xe4  
#define TARGET_PAGE_SIZE 2
#define SEGMENT_SIZE TARGET_PAGE_SIZE
#define TARGET_IS_BIG_ENDIAN_P
#define DEFAULT_ARCH bfd_arch_m68k

#define MY(OP) CAT(m68kmint_prg_,OP)
#define TARGETNAME "a.out-mintprg"

#define NAME(x,y) CAT3(mintprg,_32_,y)

/* The only location that the macro ZMAGIC_DISK_BLOCK_SIZE seems to
   be needed is in aout-target.h where the file position of the text
   segment is determined.  Our target page size of 2 is actually just a
   joke (no paging on Atari, we simply short-word-align sections) so
   we have to explicitely tell aout-target here.  Nonetheless, this
   doesn't seem clean to me.  Does aoutx.h (adjust_n_magic) has to
   be modified?  */
#define ZMAGIC_DISK_BLOCK_SIZE EXEC_BYTES_SIZE

/* If --traditional-format was given to the linker an old-style DRI
   symbol table is written into the executable.  This is with respect 
   to many old debugging tools or disassemblers which expect this format.  
   Although created by the linker, these executables will not be recognized
   as a valid bfd input file.  It is too much effort to evaluate the
   symbols from such files.  */
#define _MINT_SYMBOL_FORMAT_GNU  0
#define _MINT_SYMBOL_FORMAT_DRI  1

#include "bfd.h"
#include "sysdep.h"

     /* Forward declarations.  */
struct internal_exec;
struct external_exec;
struct aout_final_link_info;
struct bfd_link_info;
struct reloc_std_external;

/* Data structure that holds some private information for us.  */
struct mint_internal_info {
  struct bfd_link_info* linkinfo;    /* Remembered from final_link.  */
  boolean       traditional_format;  /* Saved from link info.  */
  int           symbol_format;       /* Format of the symbol table.  */
  PTR           tparel;              /* Data for TPA relative relocation
                                        information.  */
  file_ptr      tparel_pos;          /* File position of TPA relative
					relocation information.  */
  bfd_size_type tparel_size;         /* Size of TPA relative relocation
					information.  */
  bfd_size_type symtab_size;         /* Size of traditional symbol table.  */

#define MINT_RELOC_CHUNKSIZE 0x1000
  bfd_vma*      relocs;              /* Array of address relocations.  */
  unsigned long relocs_used;         /* Number of relocation entries 
                                        already used up.  */
  unsigned long relocs_allocated;    /* Number of relocation entries
                                        allocated.  */

  bfd_vma       stkpos;              /* File offset to value of _stksize.  */

  flagword      prg_flags;           /* Standard GEMDOS flags.  */

  boolean       reloc_error;         /* True if an unhandled error during
					relocation occured.  */
};

/* We have to do quite a lot of magic to make the Atari format
   for GEMDOS executables fit into the standard a.out format.
   We start with the original header.  */
#define external_exec external_exec
struct external_exec {
  bfd_byte g_branch[2];              /* 0x601a (or 0xdead for relocatable
				        linker output).  */
  bfd_byte g_text[4];                /* Length of text section.  */
  bfd_byte g_data[4];                /* Length of data section.  */
  bfd_byte g_bss[4];                 /* Length of bss section.  */
  bfd_byte g_syms[4];                /* Length of symbol table.  */
  bfd_byte g_extmagic[4];            /* Always 0x4d694e54
					(in ASCII: ``MiNT'').  */
  bfd_byte g_flags[4];               /* Atari special flags.  */
  bfd_byte g_abs[2];                 /* Non-zero if absolute (no relocation
                                        info.  */
  
  /* We extend this header now to provide the information that the
     binutils want to see.  Everything following will actually be part
     of the text segment (from MiNT's point of view).  As a
     consequence the text section has 228 bytes of redundancy.

     The following eight bytes should be treated as opaque.
     If the word ``opaque'' always attracts your curiosity in
     typedefs and structs, here's the explanation:  These eight bytes
     are really two assembler instructions.  The first one moves
     the contents of e_entry into register d4, the second one
     jumps (pc-relative) to the entry point.  See swap_exec_header_out
     for details.  */
  bfd_byte g_jump_entry[8];

  /* Now following a standard a.out header.  Note that the values
     may differ from the one given on top.  The traditional header
     contains the values that the OS wants to see, the values below
     are the values that make the binutils work.  */
  bfd_byte e_info[4];                /* Magic number and stuff.  */
  bfd_byte e_text[4];                /* Length of text section in bytes.  */
  bfd_byte e_data[4];                /* Length of data section.  */
  bfd_byte e_bss[4];                 /* Length of standard symbol 
					table.  */
  bfd_byte e_syms[4];                /* Length of symbol table.  */
  bfd_byte e_entry[4];               /* Start address.  */
  bfd_byte e_trsize[4];              /* Length of text relocation
                                        info.  */
  bfd_byte e_drsize[4];              /* Length of data relocation
					info.  */

  bfd_byte g_tparel_pos[4];          /* File position of TPA relative
					relocation info.  */
  bfd_byte g_tparel_size[4];         /* Length of TPA relative relocation
					info.  */

  /* This is for extensions.  */
  bfd_byte g_stkpos[4];              /* If stacksize is hardcoded into
                                        the executable you will find it
					at file offset g_stkpos.  If
					not this is NULL.  */

  bfd_byte g_symbol_format[4];       /* Format of the symbol table.  See
                                        definitions for _MINT_SYMBOL_FORMAT*
                                        above.  */
                                     
  /* Pad with zeros.  */
  bfd_byte g_pad0[172];
};

#define EXEC_BYTES_SIZE 256

/* Code indicating object file or impure executable.  */
#define OMAGIC 0407
/* Code indicating pure executable.  */
#define NMAGIC 0410
/* Code indicating demand-paged executable.  */
#define ZMAGIC 0413

#ifndef N_BADMAG
# define N_BADMAG(e) (N_MAGIC (e) != OMAGIC && \
		      N_MAGIC (e) != NMAGIC && \
		      N_MAGIC (e) != ZMAGIC)
#endif

     /* For DRI symbol table format.  */
struct dri_symbol {
  bfd_byte a_name[8];     /* Symbol name */
  bfd_byte a_type[2];     /* Type flag, i.e. A_TEXT etc; see below.  */
  bfd_byte a_value[4];    /* value of this symbol (or sdb offset).  */
};
#define DRI_SYMBOL_SIZE 14

/* Simple values for a_type.  */
#define A_UNDF  0             
#define A_BSS   0x0100      
#define A_TEXT  0x0200       
#define A_DATA  0x0400        
#define A_EXT   0x0800        /* External.  */
#define A_EQREG 0x1000        /* Equated register.  */
#define A_GLOBL 0x2000        /* Global.  */
#define A_EQU   0x4000        /* Equated.  */
#define A_DEF   0x8000        /* Defined.  */
#define A_LNAM  0x0048        /* GST compatible long name.  */
                              /* File symbols ala aln.  */
#define A_TFILE 0x0280        /* Text file corresponding to object module.  */
#define A_TFARC 0x02C0        /* Text file archive.  Unfortunately this
			         conflicts with the bits in A_LNAM.  */


#define MY_object_p MY_object_p
#define MY_BFD_TARGET MY_bfd_target
#define MY_get_section_contents _bfd_generic_get_section_contents
#define MY_bfd_final_link MY_bfd_final_link
#define MY_bfd_free_cached_info MY_bfd_free_cached_info
#define MY_close_and_cleanup MY_bfd_free_cached_info
#define MY_bfd_copy_private_bfd_data MY_bfd_copy_private_bfd_data

static CONST bfd_target* MY_object_p PARAMS ((bfd*));

/* This is a hack.  We have to retrieve the symbol name.  But
   to do achieve this with reasonable effort we need an extra
   parameter.   */
#define MY_final_link_relocate(howto, ibfd, isec, contents, \
			       address, value, addend) \
m68kmint_prg_final_link_relocate (howto, ibfd, isec, contents, \
				  address, value, addend, \
				  (struct reloc_std_external*) rel);

bfd_reloc_status_type m68kmint_prg_final_link_relocate
     PARAMS ((reloc_howto_type*, bfd*, asection*, bfd_byte*, bfd_vma,
	      bfd_vma, bfd_vma, struct reloc_std_external*));

static boolean MY_bfd_final_link PARAMS ((bfd*, struct bfd_link_info*));
static boolean MY_bfd_free_cached_info PARAMS ((bfd*));
static boolean MY_bfd_copy_private_bfd_data PARAMS ((bfd*, bfd*));
static CONST char* m68kmint_prg_find_symbol_name
  PARAMS ((reloc_howto_type*, bfd*, asection*, bfd_vma, bfd_byte*,
	   struct reloc_std_external* rel));

static int cmp_vma PARAMS ((bfd_vma*, bfd_vma*));
static int squirt_out_tparel PARAMS ((bfd*, struct internal_exec*,
				      struct external_exec*));

static boolean link_write_traditional_syms
     PARAMS ((bfd*, struct bfd_link_info*));
static int write_dri_symbol PARAMS ((bfd*, CONST char*, bfd_vma, bfd_vma));

extern CONST bfd_target MY(vec);

/* aoutx.h requires definitions for BMAGIC and QMAGIC.  Other 
   implementations have either chosen OMAGIC or zero for BMAGIC if
   not available.  We try it with 0777 which is hopefully impossible. */
#define BMAGIC 0777
#define QMAGIC 0314

#define WRITE_HEADERS(abfd, execp)					      \
      {									      \
	bfd_size_type text_size; /* dummy vars */			      \
	file_ptr text_end;						      \
        struct mint_internal_info* myinfo = obj_aout_ext (abfd);              \
                                                                              \
	if (adata(abfd).magic == undecided_magic)			      \
	  NAME(aout,adjust_sizes_and_vmas) (abfd, &text_size, &text_end);     \
    									      \
	execp->a_syms = bfd_get_symcount (abfd) * EXTERNAL_NLIST_SIZE;	      \
	execp->a_entry = bfd_get_start_address (abfd);			      \
    									      \
	execp->a_trsize = ((obj_textsec (abfd)->reloc_count) *		      \
			   obj_reloc_entry_size (abfd));		      \
	execp->a_drsize = ((obj_datasec (abfd)->reloc_count) *		      \
			   obj_reloc_entry_size (abfd));		      \
	/* We don't have to call swap_exec_header_out here because the        \
           contents will be overwritten in a second pass.  */                 \
									      \
	if (bfd_seek (abfd, (file_ptr) 0, SEEK_SET) != 0) return false;	      \
	if (bfd_write ((PTR) &exec_bytes, 1, EXEC_BYTES_SIZE, abfd)	      \
	    != EXEC_BYTES_SIZE)						      \
	  return false;							      \
	/* Now write out reloc info, followed by syms and strings.  */	      \
  									      \
        if (bfd_seek (abfd, (file_ptr)(N_SYMOFF(*execp)), SEEK_SET) != 0)     \
          return false;                                                       \
	if (bfd_get_outsymbols (abfd) != (asymbol**) NULL		      \
	    && bfd_get_symcount (abfd) != 0) 				      \
	  {								      \
            if (! NAME(aout,write_syms)(abfd))                                \
              return false;		                                      \
	  }								      \
									      \
	/* This will also rewrite  the exec header.  */                       \
        if (squirt_out_tparel (abfd, execp, &exec_bytes) != 0)                \
          return false;                                                       \
	if (bfd_seek (abfd, (file_ptr)(N_TRELOFF(*execp)), SEEK_SET) != 0)    \
	  return false;						      	      \
	if (!NAME(aout,squirt_out_relocs) (abfd, obj_textsec (abfd)))         \
	  return false;						      	      \
									      \
	if (bfd_seek (abfd, (file_ptr)(N_DRELOFF(*execp)), SEEK_SET) != 0)    \
	  return false;						      	      \
	if (!NAME(aout,squirt_out_relocs)(abfd, obj_datasec (abfd)))          \
	  return false;						      	      \
      }									      

#include "aoutx.h"

#include "libaout.h"

#include "aout-target.h"

boolean
bfd_m68kmint_set_extended_flags (abfd, prg_flags)
     bfd* abfd;
     flagword prg_flags;
{
  struct mint_internal_info* myinfo = obj_aout_ext (abfd);

  if (myinfo == NULL)
    myinfo = bfd_zalloc (abfd, sizeof (struct bfd_link_info));
  
  if (myinfo == NULL) 
    {
      /* The internal function bfd_zalloc has already set the error
	 state to "out of memory".  */
      return false;
    }

  obj_aout_ext (abfd) = myinfo;

  if (abfd->xvec != &(MY (vec)) || myinfo == NULL)
    return false;
  
  myinfo->prg_flags = prg_flags;
  return true;
}

static CONST bfd_target*
MY_object_p (abfd)
     bfd* abfd;
{
  struct external_exec exec_bytes;	/* Raw exec header from file */
  struct internal_exec exec;		/* Cleaned-up exec header */
  CONST bfd_target* target;
  struct mint_internal_info* myinfo;
  boolean is_executable = true;

  if (bfd_read ((PTR) &exec_bytes, 1, EXEC_BYTES_SIZE, abfd)
      != EXEC_BYTES_SIZE) {
    if (bfd_get_error () != bfd_error_system_call)
      bfd_set_error (bfd_error_wrong_format);
    return 0;
  }

  /* Instead of byte-swapping we compare bytes.  */
  if (exec_bytes.g_branch[0] == 0xde
      && exec_bytes.g_branch[1] == 0xad)
    {
      /* This is the result of an invalid objcopy operation.  */
      is_executable = false;
    }
  else if (exec_bytes.g_branch[0] != 0x60
	   || exec_bytes.g_branch[1] != 0x1a)
    {
      bfd_set_error (bfd_error_wrong_format);
      return 0;
    } 

  if (exec_bytes.g_branch[0] != 0x60
      || exec_bytes.g_branch[1] != 0x1a
      || exec_bytes.g_extmagic[0] != 'M' 
      || exec_bytes.g_extmagic[1] != 'i' 
      || exec_bytes.g_extmagic[2] != 'N' 
      || exec_bytes.g_extmagic[3] != 'T' 
      || exec_bytes.g_symbol_format[0] != 0 
      || exec_bytes.g_symbol_format[1] != 0 
      || exec_bytes.g_symbol_format[2] != 0 
      || exec_bytes.g_symbol_format[3] != 0)
    {
      bfd_set_error (bfd_error_wrong_format);
      return 0;
    }

#ifdef SWAP_MAGIC
  exec.a_info = SWAP_MAGIC (exec_bytes.e_info);
#else
  exec.a_info = bfd_h_get_32 (abfd, exec_bytes.e_info);
#endif /* SWAP_MAGIC */

  if (N_BADMAG (exec)) return 0;
#ifdef MACHTYPE_OK
  if (!(MACHTYPE_OK (N_MACHTYPE (exec)))) return 0;
#endif

  NAME(aout,swap_exec_header_in)(abfd, &exec_bytes, &exec);

#ifdef SWAP_MAGIC
  /* swap_exec_header_in read in a_info with the wrong byte order */
  exec.a_info = SWAP_MAGIC (exec_bytes.e_info);
#endif /* SWAP_MAGIC */

  target = NAME(aout,some_aout_object_p) (abfd, &exec, MY(callback));

  myinfo = bfd_zalloc (abfd, sizeof (struct bfd_link_info));
  
  if (myinfo == NULL)
    {
      /* Error is already set to "out of memory".  */
      return 0;
    }

  obj_aout_ext (abfd) = myinfo;

  /* Now get the missing information.  */
  myinfo->tparel_pos = GET_WORD (abfd, exec_bytes.g_tparel_pos);
  myinfo->tparel_size = GET_WORD (abfd, exec_bytes.g_tparel_size);

  /* FIXME:  Currently we always read the TPA relative relocation
     information.  This is suboptimal because often times there
     is no need for it.  Read it only if need be!  Maybe this should
     also depend on abfd->cacheable?  */
  if (myinfo->tparel_size == 0)
    myinfo->tparel = bfd_zalloc (abfd, 4);
  else
    myinfo->tparel = bfd_alloc (abfd, myinfo->tparel_size);
  
  if (myinfo->tparel == NULL)
    return 0;

  if (myinfo->tparel_size == 0)
    {
      myinfo->tparel_size = 4;
    }
  else 
    {
    /* Read the information from the bfd.  */
    if (bfd_seek (abfd, myinfo->tparel_pos, SEEK_SET) != 0
	|| (bfd_read (myinfo->tparel, 1, myinfo->tparel_size, abfd)
	    != myinfo->tparel_size))
      {
	return 0;
      }
    }

  myinfo->stkpos = GET_WORD (abfd, exec_bytes.g_stkpos);
  myinfo->prg_flags = GET_WORD (abfd, exec_bytes.g_flags);

  /* We don't support other formats for the symbol table actively.  */
  myinfo->symbol_format = _MINT_SYMBOL_FORMAT_GNU;
  return target;
}

static boolean 
MY_bfd_free_cached_info (abfd)
     bfd* abfd;
{
  if (obj_aout_ext (abfd) != NULL) 
    {
      struct mint_internal_info* myinfo = obj_aout_ext (abfd);
      if (myinfo != NULL) 
	{
	  if (myinfo->relocs != NULL) 
	    {
	      free (myinfo->relocs);
	      myinfo->relocs = NULL;
	    }
	}
    }

  return NAME (aout, bfd_free_cached_info) (abfd);
}

/* This is used for qsort to sort addresses for the TPA relocation table.  */
static int vma_cmp (v1, v2)
     bfd_vma* v1;
     bfd_vma* v2;
{
  return (int) ((*v1) - (*v2));
}

/* Final link routine.  We need to use a call back to get the correct
   offsets in the output file.  And we need to malloc some internal
   buffers.  */

static boolean
MY_bfd_final_link (abfd, info)
     bfd* abfd;
     struct bfd_link_info* info;
{
  struct mint_internal_info* myinfo = obj_aout_ext (abfd);
  boolean retval;
  unsigned long i;
  bfd_size_type bytes;
  unsigned char* ptr;
  struct bfd_link_hash_table* hash = info->hash;

  if (myinfo == NULL)
    myinfo = bfd_zalloc (abfd, sizeof (struct bfd_link_info));
  
  if (myinfo == NULL) 
    {
      /* The internal function bfd_zalloc has already set the error
	 state to "out of memory".  */
      return false;
    }

  obj_aout_ext (abfd) = myinfo;
  
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
      struct aout_link_hash_entry* h =
	aout_link_hash_lookup (aout_hash_table (info), "__stksize",
			       false, false, false);
      asection* sec;

      if (h != NULL) 
	{
	  switch (h->root.type) 
	    {
	    case bfd_link_hash_defined:
	    case bfd_link_hash_defweak:
	      sec = h->root.u.def.section->output_section;
	      BFD_ASSERT (bfd_is_abs_section (sec)
			  || sec->owner == abfd);
	      
	      myinfo->stkpos = (h->root.u.def.value + sec->vma 
				+ h->root.u.def.section->output_offset
				+ 0x1c);
	      break;
	    case bfd_link_hash_common:
	      myinfo->stkpos = h->root.u.c.size + 0x1c;
	      break;
	    }
	}
    }
  
  if ((abfd->flags & BFD_TRADITIONAL_FORMAT) != 0) 
    {
      if (info->relocateable) 
	{
	  _bfd_error_handler ("\
warning: traditional format not supported for relocatable output");
	} 
      else 
	{
	  _bfd_error_handler ("\
warning: traditional format is obsolete");
	  _bfd_error_handler ("\
(Executables in traditional format will not be recognized");
	  _bfd_error_handler ("by other bfd front ends.)");
	  myinfo->traditional_format = true;
	  myinfo->symbol_format = _MINT_SYMBOL_FORMAT_DRI;
	}
    }
  
  /* Unconditionally unset the traditional flag.  The only effect in
     the a.out code is to disable string hashing (with respect to
     SunOS gdx).  This is not necessary for us.

     Just in case you are interested in orthography:  In the above
     paragraph there seems to be an apostrophe missing after SunOS.
     But this breaks emacs (sigh, again) C mode.  */

  abfd->flags &= ~BFD_TRADITIONAL_FORMAT;
  
  if (NAME(aout,final_link) (abfd, info, MY_final_link_callback)
      != true)
    return false;

  if (myinfo->reloc_error)
    return false;

  if (myinfo->traditional_format
      && link_write_traditional_syms (abfd, info) != true)
    return false;

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
     with 0L.  */
  bytes = 4 + 4;    /* First entry is a long, last is (long) 0.  */
  for (i = 1; i < myinfo->relocs_used; i++) {
    unsigned long diff = myinfo->relocs[i] - myinfo->relocs[i - 1];
    bytes += 1 + diff / 254;
  }
  
  myinfo->tparel_size = bytes;  
  myinfo->tparel = bfd_alloc (abfd, bytes);
  if (myinfo->tparel == NULL)
    return false;
  
  /* Now fill the array.  */
  ptr = (bfd_byte*) myinfo->tparel;
  if (myinfo->relocs != NULL)
    bfd_put_32 (abfd, myinfo->relocs[0], ptr);

  ptr += 4;
  for (i = 1; i < myinfo->relocs_used; i++)
    {
      unsigned long addr = myinfo->relocs[i] - myinfo->relocs[i - 1];
      while (addr > 254) {
	*ptr = 1;
	addr -= 254;
	ptr++;
      } 
      *ptr = (bfd_byte) addr;
      ptr++;
    }
  bfd_put_32 (abfd, 0, ptr);
  
  return retval;
}

static boolean 
MY_bfd_copy_private_bfd_data (ibfd, obfd)
     bfd* ibfd;
     bfd* obfd;
{
  struct mint_internal_info* myinfo_in = obj_aout_ext (ibfd);
  struct mint_internal_info* myinfo_out = obj_aout_ext (obfd);

  /* Our routine only makes sense if both the input and the output
     bfd are MiNT program files.  FIXME:  It is not absolutely clear
     to me if this function is a method of the input or the output
     bfd.  One part of the following AND relation is not redundant,
     but which one?  */
  if (ibfd->xvec == &(MY (vec)) && obfd->xvec == &(MY (vec))) 
    {
      BFD_ASSERT (myinfo_in != NULL);
    
      if (myinfo_out == NULL) {
	myinfo_out = bfd_zalloc (obfd, sizeof (struct mint_internal_info));
	
	if (myinfo_out == NULL) {
	  /* The internal function bfd_zalloc has already set the error
	     state to "out of memory".  */
	  return false;
	}
	
	memcpy (myinfo_out, myinfo_in, sizeof (struct mint_internal_info));
	myinfo_out->tparel = NULL;
	obj_aout_ext (obfd) = myinfo_out;
      }
      
      if (myinfo_out->tparel != NULL)
	free (myinfo_out->tparel);
    
      if (myinfo_in->tparel != NULL) 
      {
	if (bfd_seek (ibfd, myinfo_in->tparel_pos, SEEK_SET) != 0)
	  return false;

	if (bfd_read (myinfo_in->tparel, 1, myinfo_in->tparel_size, ibfd) 
	    != myinfo_in->tparel_size)
	  return false;
      }
      myinfo_out->tparel = bfd_alloc (obfd, myinfo_out->tparel_size);
      if (myinfo_out->tparel == NULL)
	return false;
      
      memcpy (myinfo_out->tparel, myinfo_in->tparel,
	      myinfo_out->tparel_size);
    } 
  else if (ibfd->xvec != &(MY (vec))) 
    {
      /* Can this ever happen?  FIXME!  */
      _bfd_error_handler ("\
error: the input file ``%s'' contains no", ibfd->filename);
      _bfd_error_handler ("TPA-relative relocation info.");
      
      /* We will invalidate the output file so that no attempt is
	 made to actually run the image.  Maybe we should return
	 false instead but it is possible that some curious soul has
	 tried to objcopy onto our format for research reasons.  */
      _bfd_error_handler ("Will mark output file ``%s''");
      _bfd_error_handler ("as non-executable.");
      obfd->flags &= (~EXEC_P);
    }
  
  return _bfd_generic_bfd_copy_private_bfd_data (ibfd, obfd);
}

static
CONST char* m68kmint_prg_find_symbol_name (howto, input_bfd,
					   input_section, value,
					   location, rel)
     reloc_howto_type* howto;
     bfd* input_bfd;
     asection* input_section;
     bfd_vma value;
     bfd_byte* location;
     struct reloc_std_external* rel;
{
  /* Find out the symbol name.  */
  struct external_nlist *syms = obj_aout_external_syms (input_bfd);
  char* strings = obj_aout_external_strings (input_bfd);
  struct aout_link_hash_entry** sym_hashes
    = obj_aout_sym_hashes (input_bfd);
  struct aout_link_hash_entry* h = NULL;
  CONST char* name;
  int r_index;
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
    name = "(unknown symbol)";  /* Shouldn't happen.  */
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
   relocateable link depends upon how the object format defines
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

bfd_reloc_status_type
m68kmint_prg_final_link_relocate (howto, input_bfd, input_section,
				  contents, address, value, addend, rel)
     reloc_howto_type* howto;
     bfd* input_bfd;
     asection* input_section;
     bfd_byte* contents;
     bfd_vma address;
     bfd_vma value;
     bfd_vma addend;
     struct reloc_std_external* rel;
{
  bfd_vma relocation;
  bfd* output_bfd = input_section->output_section->owner;
  struct mint_internal_info* myinfo = obj_aout_ext (output_bfd);
  bfd_reloc_status_type retval;

#define _MINT_F_SHTEXT 0x800

  /* Sanity check the address.  */
  if (address > input_section->_raw_size)
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
      boolean error_found = false;
      const char* name = NULL;
      /* The input bfd is always big-endian.  There is no need to
	 call bfd_header_big_endian (input_bfd).  */
      int r_index  = ((rel->r_index[0] << 16)
		      | (rel->r_index[1] << 8)
		      | (rel->r_index[2]));
      int r_extern = (0 != (rel->r_type[0] & RELOC_STD_BITS_EXTERN_BIG));
	  

      if (input_section == obj_textsec (input_bfd))
	{
	  if (!r_extern)
	    {
	      /* This is a relocation against another section.  Only
		 relocations against the text section are allowed.  */
	      if (r_index != N_TEXT && r_index != (N_TEXT | N_EXT))
		error_found = true;
	    }
	  else if (relocation > (input_section->output_section->vma
			    + input_section->output_section->_raw_size))
	    {
	      error_found = true;
	    }
	  else if (relocation == (input_section->output_section->vma
				  + input_section->output_section->_raw_size))
	    {
	      name = m68kmint_prg_find_symbol_name (howto, input_bfd,
						    input_section,
						    value,
						    contents + address,
						    rel);
	      if (strcmp (name, "_etext") == 0)
		error_found = false;
	    }
	}

      if (error_found)
	{
	  const struct bfd_link_callbacks* callbacks
	    = myinfo->linkinfo->callbacks;

          myinfo->reloc_error = true;

	  if (callbacks->reloc_dangerous != NULL)
	    {
	      if (name == NULL)
		name = m68kmint_prg_find_symbol_name (howto, input_bfd,
						      input_section,
						      value,
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
     pcrel_offset is false.  Other targets (e.g., m88kbcs or ELF)
     simply leave the contents of the section as zero; for such
     targets pcrel_offset is true.  If pcrel_offset is false we do not
     need to subtract out the offset of the location within the
     section (which is just ADDRESS).  */
  if (howto->pc_relative)
    {
      relocation -= (input_section->output_section->vma
		     + input_section->output_offset);
      if (howto->pcrel_offset)
	relocation -= address;
    }
  else if (bfd_get_reloc_size (howto) < 4)
    {
      /* Umh, don't know why... */
      relocation += (-32768 - (obj_textsec (output_bfd))->_raw_size
		     -228);
    }
						      
  retval = _bfd_relocate_contents (howto, input_bfd, relocation,
				   contents + address);
  
  /* The symbol has to be relocated again iff the length of the relocation
     is 2 words and it is not pc relative.  */

  if (!howto->pc_relative && bfd_get_reloc_size (howto) == 4) {

    /* Enlarge the buffer if necessary.  */
    if (myinfo->relocs_used <= myinfo->relocs_allocated) {
      bfd_vma* newbuf;
      myinfo->relocs_allocated += MINT_RELOC_CHUNKSIZE;
      newbuf = bfd_realloc (myinfo->relocs, myinfo->relocs_allocated);
      if (newbuf == NULL) {
        bfd_set_error (bfd_error_no_memory);
        bfd_perror ("fatal error");
        xexit (1);
      }
      myinfo->relocs = newbuf;
    }

    /* The TPA relative relocation actually just adds the address of
       the text segment (i. e. beginning of the executable in memory)
       to the addresses at the specified locations.  This allows an
       executable to be loaded everywhere in the address space without
       memory management.  */

    myinfo->relocs[myinfo->relocs_used++] =
      input_section->output_section->vma
      + input_section->output_offset + address;
  }

  return retval;
}

static int
squirt_out_tparel (abfd, execp, exec_bytes)
     bfd* abfd;
     struct internal_exec* execp;
     struct external_exec* exec_bytes;
{
  struct mint_internal_info* myinfo = obj_aout_ext (abfd);
  
  if (myinfo->symtab_size == 0)
    myinfo->tparel_pos = N_STROFF (*execp) 
      + obj_aout_external_string_size (abfd);
  else
    myinfo->tparel_pos = N_SYMOFF (*execp) 
      + myinfo->symtab_size;

  if (bfd_seek (abfd, myinfo->tparel_pos, SEEK_SET) != 0)
    return 1;

  if (bfd_write (myinfo->tparel, 1, myinfo->tparel_size, abfd) 
      != myinfo->tparel_size)
    return 1;

  /* Now that we have all the information we need we can fill in the
     MiNT-specific header fields.  Unfortunately this is already the
     second time that we write out the header.  */

  if ((abfd->flags & EXEC_P) == 0)
    bfd_h_put_16 (abfd, 0xdead, exec_bytes->g_branch);
  else
    bfd_h_put_16 (abfd, 0x601a, exec_bytes->g_branch);
  
  /* The OS will load our extension header fields into the text segment.  */
  PUT_WORD (abfd, execp->a_text + EXEC_BYTES_SIZE - 28,
	    exec_bytes->g_text);
  PUT_WORD (abfd, execp->a_data, exec_bytes->g_data);
  PUT_WORD (abfd, execp->a_bss, exec_bytes->g_bss);
  
  /* The OS' notion of the size of the symbol table is another than
     the bfd library's.  We have to fill in the size of the table
     itself plus the size of the string table but only if we have not written
     a traditional symbol table.  If we have written a traditional symbol
     table we know the size.  */
  if (myinfo->symtab_size != 0)
    PUT_WORD (abfd, myinfo->symtab_size, exec_bytes->g_syms);
  else
    PUT_WORD (abfd, myinfo->tparel_pos - N_SYMOFF (*execp),
	      exec_bytes->g_syms);
  bfd_h_put_32 (abfd, 0x4d694e54, exec_bytes->g_extmagic);
  bfd_h_put_32 (abfd, myinfo->prg_flags, exec_bytes->g_flags);
  bfd_h_put_16 (abfd, 0, exec_bytes->g_abs);
  
  /* Generate the jump instruction to the entry point.  In m68k
     assembler mnemnonics it looks more or less like this:

       move.l  exec_bytes->e_entry(pc), d4
       jmp     0(pc,d4.l)

     Sorry for the wrong syntax.  As a real assembler addict I
     never actually use an assembler.  I edit my binaries manually
     with a hex editor, looks much cooler and it strengthens your
     abstraction abilities.  */

  exec_bytes->g_jump_entry[0] = 0x28;
  exec_bytes->g_jump_entry[1] = 0x3a;
  exec_bytes->g_jump_entry[2] = 0x00;
  exec_bytes->g_jump_entry[3] = 0x1a;
  exec_bytes->g_jump_entry[4] = 0x4e;
  exec_bytes->g_jump_entry[5] = 0xfb;
  exec_bytes->g_jump_entry[6] = 0x48;
  exec_bytes->g_jump_entry[7] = 0xfa;

  bfd_h_put_32 (abfd, myinfo->tparel_pos, exec_bytes->g_tparel_pos);
  bfd_h_put_32 (abfd, myinfo->tparel_size, exec_bytes->g_tparel_size);
  
  PUT_WORD (abfd, myinfo->stkpos, exec_bytes->g_stkpos);
  PUT_WORD (abfd, myinfo->symbol_format, exec_bytes->g_symbol_format);
  
  memset (&exec_bytes->g_pad0, 0, sizeof (exec_bytes->g_pad0));  
  
  /* The standard stuff.  */
  NAME(aout,swap_exec_header_out) (abfd, execp, exec_bytes);
  if (myinfo->symbol_format != _MINT_SYMBOL_FORMAT_GNU)
    PUT_WORD (abfd, 0, exec_bytes->e_syms);
  
  if (bfd_seek (abfd, (file_ptr) 0, SEEK_SET) != 0) return false;
  
  if (bfd_write ((PTR) exec_bytes, 1, EXEC_BYTES_SIZE, abfd)
      != EXEC_BYTES_SIZE)						      
    return 1;							      
  
  return 0;
} 

/* Emit a traditional DRI symbol table while linking.  */
static boolean
link_write_traditional_syms (abfd, info)
     bfd* abfd;
     struct bfd_link_info* info;
{
  bfd*                       ibfd;
  enum bfd_link_strip        strip = info->strip;
  enum bfd_link_discard      discard = info->discard;
  bfd_size_type              symcount = 0;
  struct mint_internal_info* myinfo = obj_aout_ext (abfd);
  bfd*                       last_archive = NULL;

  /* Position file pointer.  */
  if (bfd_seek (abfd, obj_sym_filepos (abfd), SEEK_SET) != 0)
    return false;

  myinfo->symtab_size = 0;

  for (ibfd = info->input_bfds; ibfd != NULL; ibfd = ibfd->link_next)
    {
      bfd_size_type sym_count = obj_aout_external_sym_count (ibfd);
      char* strings = obj_aout_external_strings (ibfd);
      bfd_size_type strtab_index;
      register struct external_nlist* sym = obj_aout_external_syms (ibfd);
      struct external_nlist* sym_end = sym + sym_count;
      struct aout_link_hash_entry** sym_hash = obj_aout_sym_hashes (ibfd);
      boolean pass = false;
      boolean skip = false;
      boolean skip_next = false;
      int written_bytes;
      int a_type;
      boolean write_archive_name = false;
      bfd_vma val = 0;
      
      /* First write out a symbol for the archive if we do not
	 strip these symbols and if it differs from the last
	 one.  */
      if (ibfd->my_archive != last_archive
	  && ibfd->my_archive != NULL) 
	{
	  write_archive_name = true;
	  last_archive = ibfd->my_archive;
	}

      if (write_archive_name
	  && strip != strip_all
	  && (strip != strip_some
	      || bfd_hash_lookup (info->keep_hash,
				  ibfd->my_archive->filename,
				  false, false) != NULL)
	  && discard != discard_all) 
	{
	  val = bfd_get_section_vma (abfd,
				     obj_textsec (ibfd)->output_section)
	    + obj_textsec (ibfd)->output_offset;
	  
	  written_bytes = write_dri_symbol (abfd,
					    ibfd->my_archive->filename,
					    A_TFILE, val);

	  if (written_bytes <= 0)
	    return false;
	  else
	    myinfo->symtab_size += written_bytes;
	}

      /* Now write out a symbol for the object file if we do not
	 strip these symbols.  */
      if (strip != strip_all
	  && (strip != strip_some
	      || bfd_hash_lookup (info->keep_hash, ibfd->filename,
				  false, false) != NULL)
	  && discard != discard_all) 
	{
	  val = bfd_get_section_vma (abfd,
				     obj_textsec (ibfd)->output_section)
	    + obj_textsec (ibfd)->output_offset;
				     
	  written_bytes = write_dri_symbol (abfd, ibfd->filename,
					    A_TFILE, val);
	  if (written_bytes <= 0)
	    return false;
	  else
	    myinfo->symtab_size += written_bytes;
	}

      /* Now we have a problem.  All symbols that we see have already
	 been marked written (because we write them a second time
	 here.  If we would do it the clean way we would have
	 to traverse the entire symbol map and reset the written
	 flag.  We hack here instead...  */
#define mark_written(h) ((int) h->written = (int) true + 1) 
#define is_written(h) ((int) h->written == (int) true + 1)
      for (; sym < sym_end; sym++, sym_hash++) {
	CONST char* name = strings + GET_WORD (ibfd, sym->e_strx);
	struct aout_link_hash_entry* h = NULL;
	int type;
	
	val = 0;
	type = bfd_h_get_8 (ibfd, sym->e_type);
	name = strings + GET_WORD (ibfd, sym->e_strx);
	
	if (pass) {
	  /* Pass this symbol through.  It is the target of an
	     indirect or warning symbol.  */
	  val = GET_WORD (ibfd, sym->e_value);
	  pass = false;
	} else if (skip_next) {
	  /* Skip this symbol, which is the target of an indirect
	     symbol that we have changed to no longer be an indirect
	     symbol.  */
	  skip_next = false;
	  continue;
	} else {
	  struct aout_link_hash_entry* hresolve = *sym_hash;
	  asection* symsec;
	  
	  /* We have saved the hash table entry for this symbol, if
	     there is one.  Note that we could just look it up again
	     in the hash table, provided we first check that it is an
	     external symbol. */
	  h = *sym_hash;
	  
	  /* Use the name from the hash table, in case the symbol was
	     wrapped.  */
	  if (h != NULL)
	    name = h->root.root.string;	
	  
	  /* If this is an indirect or warning symbol, then change
	     hresolve to the base symbol.  */
	  hresolve = h;
	  if (h != (struct aout_link_hash_entry*) NULL
	      && (h->root.type == bfd_link_hash_indirect
		  || h->root.type == bfd_link_hash_warning)) {
	    hresolve = (struct aout_link_hash_entry*) h->root.u.i.link;
	    while (hresolve->root.type == bfd_link_hash_indirect
		   || hresolve->root.type == bfd_link_hash_warning)
	      hresolve = ((struct aout_link_hash_entry*)
			  hresolve->root.u.i.link);
	  }

	  /* If the symbol has already been written out skip it.  */
	  if (h != (struct aout_link_hash_entry*) NULL
	      && h->root.type != bfd_link_hash_warning
	      && is_written (h)) {
	    if ((type & N_TYPE) == N_INDR
		|| type == N_WARNING)
	      skip_next = true;
	    continue;
	  }
	  
	  /* See if we are stripping this symbol.  */
	  skip = false;
        
	  /* Skip all debugger symbols.  No way to output them in
	     DRI format.  This will also reduce a lot of headaches.  */
	  if ((type & N_STAB) != 0)
	    skip = true;
	
	  switch (strip)
	    {
	    case strip_none:
	    case strip_debugger:
	      break;
	    case strip_some:
	      if (bfd_hash_lookup (info->keep_hash, name, false, false)
		  == NULL)
		skip = true;
	      break;
	    case strip_all:
	      skip = true;
	      break;
	    }
	  
	  if (skip) 
	    {
	      if (h != (struct aout_link_hash_entry*) NULL)
		mark_written (h);
	      continue;
	    }

	  /* Get the value of the symbol.  */
	  if ((type & N_TYPE) == N_TEXT
	      || type == N_WEAKT)
	    symsec = obj_textsec (ibfd);
	  else if ((type & N_TYPE) == N_DATA
		   || type == N_WEAKD)
	    symsec = obj_datasec (ibfd);
	  else if ((type & N_TYPE) == N_BSS
		   || type == N_WEAKB)
	    symsec = obj_bsssec (ibfd);
	  else if ((type & N_TYPE) == N_ABS
		   || type == N_WEAKA)
	    symsec = bfd_abs_section_ptr;
	  else if (((type & N_TYPE) == N_INDR
		    && (hresolve == (struct aout_link_hash_entry*) NULL
			|| (hresolve->root.type != bfd_link_hash_defined
			    && hresolve->root.type != bfd_link_hash_defweak
			    && hresolve->root.type != bfd_link_hash_common)))
		   || type == N_WARNING) {
	    /* Pass the next symbol through unchanged.  The
	       condition above for indirect symbols is so that if
	       the indirect symbol was defined, we output it with
	       the correct definition so the debugger will
	       understand it.  */
	    pass = true;
	    val = GET_WORD (ibfd, sym->e_value);
	    symsec = NULL;
	  } else {
	    /* If we get here with an indirect symbol, it means that
	       we are outputting it with a real definition.  In such
	       a case we do not want to output the next symbol,
	       which is the target of the indirection.  */
	    if ((type & N_TYPE) == N_INDR)
	      skip_next = true;
	    
	    symsec = NULL;
	  
	    /* We need to get the value from the hash table.  We use
	       hresolve so that if we have defined an indirect
	       symbol we output the final definition.  */
	    if (h == (struct aout_link_hash_entry*) NULL) 
	      {
		switch (type & N_TYPE)
		  {
		  case N_SETT:
		    symsec = obj_textsec (ibfd);
		    break;
		  case N_SETD:
		    symsec = obj_datasec (ibfd);
		    break;
		  case N_SETB:
		    symsec = obj_bsssec (ibfd);
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
		asection* input_section;
		asection* output_section;

		/* This case usually means a common symbol which was
		   turned into a defined symbol.  */
		input_section = hresolve->root.u.def.section;
		output_section = input_section->output_section;
		BFD_ASSERT (bfd_is_abs_section (output_section)
			    || output_section->owner == abfd);
		val = (hresolve->root.u.def.value
		       + bfd_get_section_vma (abfd, output_section)
		       + input_section->output_offset);
	    
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
	    else if (hresolve->root.type == bfd_link_hash_undefweak) {
	      val = 0;
	      type = N_UNDF;
	    } 
	    else
	      val = 0;
	  }
	  if (symsec != (asection*) NULL)
	  val = (symsec->output_section->vma
		 + symsec->output_offset
		 + (GET_WORD (ibfd, sym->e_value)
		    - symsec->vma));

	  /* If this is a global symbol set the written flag, and if
	     it is a local symbol see if we should discard it.  */
	  if (h != (struct aout_link_hash_entry*) NULL) {
	    mark_written (h);
	  } else if ((type & N_TYPE) != N_SETT
		     && (type & N_TYPE) != N_SETD
		     && (type & N_TYPE) != N_SETB
		     && (type & N_TYPE) != N_SETA) {
	    switch (discard)
	      {
	      case discard_none:
		break;
	      case discard_l:
		if (bfd_is_local_label_name (ibfd, name))
		  skip = true;
		break;
	      case discard_all:
		skip = true;
		break;
	      }
	    if (skip)
	      {
		pass = false;
		continue;
	      }
	  }
	}

	/* Now find the nearest type in DRI format.  We actually
	   don't have to care about weak symbols because we have
	   ignored the weak character of a symbol above.  Better
	   is better yet.  */
	switch (type & ~N_EXT)
	  {
	  case N_UNDF:
	  case N_WEAKU:
	    a_type = A_UNDF;
	    break;
	  case N_ABS:
	  case N_WEAKA:
	  case N_SETA:
	    a_type = A_EQU;
	    break;
	  case N_TEXT:
	  case N_WEAKT:
	  case N_SETT:
	    a_type = A_TEXT;
	    break;
	  case N_DATA:
	  case N_WEAKD:
	  case N_SETD:
	    a_type = A_DATA;
	    break;
	  case N_BSS:
	  case N_WEAKB:
	  case N_SETB:
	    a_type = A_BSS;
	    break;
	  default:
	    a_type = A_UNDF;
	    break;
	  }
	
	if (type & N_EXT)
	  a_type |= A_GLOBL;
	
	written_bytes = write_dri_symbol (abfd, name, a_type, val);
	if (written_bytes <= 0)
	  return false;
	myinfo->symtab_size += written_bytes;
      }
    }
  
  obj_aout_external_string_size (abfd) = 0;
  return true;
}

/* Write a DRI symbol with TYPE and VALUE.  If the NAME of the
   symbol exceeds 8 characters write a long symbol.  If it 
   exceeds 22 characters truncate the name.  */
static int
write_dri_symbol (abfd, name, type, value)
     bfd* abfd;
     CONST char* name;
     bfd_vma type;
     bfd_vma value;
{
  struct dri_symbol sym;
  char* ptr = &sym.a_name[0];
  CONST char* str = name;
  char more_name[DRI_SYMBOL_SIZE];
  int i = sizeof (sym.a_name);
  int written_bytes = 0;
  
  bfd_put_16 (abfd, type, sym.a_type);
  bfd_put_32 (abfd, value, sym.a_value);
  
  while (--i >= 0 && ('\0' != (*ptr++ = *str)))
    str++;
  
  /* If i >= 0 then *str == '\0' and if i == 0 there is nothing to fill.  */
  if (i > 0) 
    {   /* We are done - fill it with 0.  */
      do
	*ptr++ = '\0';
      while (--i > 0);
    } 
  else if (*str) 
    {	/* If more to write.  */
      type |= A_LNAM;
      bfd_put_16 (abfd, type, sym.a_type);
      i = sizeof sym;
    }
  
  if (bfd_write ((PTR) &sym, 1, DRI_SYMBOL_SIZE, abfd) != DRI_SYMBOL_SIZE)
    return -1;
  written_bytes += DRI_SYMBOL_SIZE;
  
  if (i > 0) 
    {
      ptr = more_name;
      i = sizeof more_name;
      while (--i >= 0 && ('\0' != (*ptr++ = *str)))
	str++;
      
      if (bfd_write ((PTR) more_name, 1, sizeof more_name, abfd)
	  != sizeof more_name)
	return -1;
      written_bytes += sizeof more_name;
    }
  
  return written_bytes;
}

CONST bfd_target MY(vec) = 
{
  TARGETNAME,           /* name */
  bfd_target_aout_flavour,
  BFD_ENDIAN_BIG,               /* target byte order (big) */
  BFD_ENDIAN_BIG,               /* target headers byte order (big) */
  (HAS_RELOC | EXEC_P |         /* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | WP_TEXT),
  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC | SEC_CODE | SEC_DATA),
  MY_symbol_leading_char,
  AR_PAD_CHAR,                  /* ar_pad_char */
  15,                           /* ar_max_namelen */
  bfd_getb64, bfd_getb_signed_64, bfd_putb64,
     bfd_getb32, bfd_getb_signed_32, bfd_putb32,
  bfd_getb16, bfd_getb_signed_16, bfd_putb16, /* data */
  bfd_getb64, bfd_getb_signed_64, bfd_putb64,
     bfd_getb32, bfd_getb_signed_32, bfd_putb32,
  bfd_getb16, bfd_getb_signed_16, bfd_putb16, /* hdrs */
  {_bfd_dummy_target, MY_object_p, /* bfd_check_format */
   bfd_generic_archive_p, MY_core_file_p},
  {bfd_false, MY_mkobject,    /* bfd_set_format */
   _bfd_generic_mkarchive, bfd_false},
  {bfd_false, MY_write_object_contents, /* bfd_write_contents */
   _bfd_write_archive_contents, bfd_false},

     BFD_JUMP_TABLE_GENERIC (MY),
     BFD_JUMP_TABLE_COPY (MY),
     BFD_JUMP_TABLE_CORE (MY),
     BFD_JUMP_TABLE_ARCHIVE (MY),
     BFD_JUMP_TABLE_SYMBOLS (MY),
     BFD_JUMP_TABLE_RELOCS (MY),
     BFD_JUMP_TABLE_WRITE (MY),
     BFD_JUMP_TABLE_LINK (MY),
     BFD_JUMP_TABLE_DYNAMIC (MY),

  (PTR) MY_backend_data,
};















