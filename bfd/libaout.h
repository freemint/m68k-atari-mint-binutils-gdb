/* BFD back-end data structures for a.out (and similar) files.
   Copyright 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001
   Free Software Foundation, Inc.
   Written by Cygnus Support.

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

#ifndef LIBAOUT_H
#define LIBAOUT_H

/* We try to encapsulate the differences in the various a.out file
   variants in a few routines, and otherwise share large masses of code.
   This means we only have to fix bugs in one place, most of the time.  */

#include "bfdlink.h"

/* Parameterize the a.out code based on whether it is being built
   for a 32-bit architecture or a 64-bit architecture.  */
#if ARCH_SIZE==64
#define GET_WORD bfd_h_get_64
#define GET_SWORD bfd_h_get_signed_64
#define GET_MAGIC bfd_h_get_32
#define PUT_WORD bfd_h_put_64
#define PUT_MAGIC bfd_h_put_32
#ifndef NAME
#define NAME(x,y) CAT3(x,_64_,y)
#endif
#define JNAME(x) CAT(x,_64)
#define BYTES_IN_WORD 8
#else
#if ARCH_SIZE==16
#define GET_WORD bfd_h_get_16
#define GET_SWORD bfd_h_get_signed_16
#define GET_MAGIC bfd_h_get_16
#define PUT_WORD bfd_h_put_16
#define PUT_MAGIC bfd_h_put_16
#ifndef NAME
#define NAME(x,y) CAT3(x,_16_,y)
#endif
#define JNAME(x) CAT(x,_16)
#define BYTES_IN_WORD 2
#else /* ARCH_SIZE == 32 */
#define GET_WORD bfd_h_get_32
#define GET_SWORD bfd_h_get_signed_32
#define GET_MAGIC bfd_h_get_32
#define PUT_WORD bfd_h_put_32
#define PUT_MAGIC bfd_h_put_32
#ifndef NAME
#define NAME(x,y) CAT3(x,_32_,y)
#endif
#define JNAME(x) CAT(x,_32)
#define BYTES_IN_WORD 4
#endif /* ARCH_SIZE==32 */
#endif /* ARCH_SIZE==64 */

/* Declare at file level, since used in parameter lists, which have
   weird scope.  */
struct external_exec;
struct external_nlist;
struct reloc_ext_external;
struct reloc_std_external;

/* a.out backend linker hash table entries.  */

struct aout_link_hash_entry
{
  struct bfd_link_hash_entry root;
  /* Whether this symbol has been written out.  */
  boolean written;
  /* Symbol index in output file.  */
  int indx;
};

/* a.out backend linker hash table.  */

struct aout_link_hash_table
{
  struct bfd_link_hash_table root;
};

/* Look up an entry in an a.out link hash table.  */

#define aout_link_hash_lookup(table, string, create, copy, follow) \
  ((struct aout_link_hash_entry *) \
   bfd_link_hash_lookup (&(table)->root, (string), (create), (copy), (follow)))

/* Traverse an a.out link hash table.  */

#define aout_link_hash_traverse(table, func, info)			\
  (bfd_link_hash_traverse						\
   (&(table)->root,							\
    (boolean (*) PARAMS ((struct bfd_link_hash_entry *, PTR))) (func),	\
    (info)))

/* Get the a.out link hash table from the info structure.  This is
   just a cast.  */

#define aout_hash_table(p) ((struct aout_link_hash_table *) ((p)->hash))

/* Back-end information for various a.out targets.  */
struct aout_backend_data
{
  /* Are ZMAGIC files mapped contiguously?  If so, the text section may
     need more padding, if the segment size (granularity for memory access
     control) is larger than the page size.  */
  unsigned char zmagic_mapped_contiguous;
  /* If this flag is set, ZMAGIC/NMAGIC file headers get mapped in with the
     text section, which starts immediately after the file header.
     If not, the text section starts on the next page.  */
  unsigned char text_includes_header;

  /* If this flag is set, then if the entry address is not in the
     first SEGMENT_SIZE bytes of the text section, it is taken to be
     the address of the start of the text section.  This can be useful
     for kernels.  */
  unsigned char entry_is_text_address;

  /* The value to pass to N_SET_FLAGS.  */
  unsigned char exec_hdr_flags;

  /* If the text section VMA isn't specified, and we need an absolute
     address, use this as the default.  If we're producing a relocatable
     file, zero is always used.  */
  /* ?? Perhaps a callback would be a better choice?  Will this do anything
     reasonable for a format that handles multiple CPUs with different
     load addresses for each?  */
  bfd_vma default_text_vma;

  /* Callback for setting the page and segment sizes, if they can't be
     trivially determined from the architecture.  */
  boolean (*set_sizes) PARAMS ((bfd *));

  /* zmagic files only. For go32, the length of the exec header contributes
     to the size of the text section in the file for alignment purposes but
     does *not* get counted in the length of the text section. */
  unsigned char exec_header_not_counted;

  /* Callback from the add symbols phase of the linker code to handle
     a dynamic object.  */
  boolean (*add_dynamic_symbols) PARAMS ((bfd *, struct bfd_link_info *,
					  struct external_nlist **,
					  bfd_size_type *, char **));

  /* Callback from the add symbols phase of the linker code to handle
     adding a single symbol to the global linker hash table.  */
  boolean (*add_one_symbol) PARAMS ((struct bfd_link_info *, bfd *,
				     const char *, flagword, asection *,
				     bfd_vma, const char *, boolean,
				     boolean,
				     struct bfd_link_hash_entry **));

  /* Called to handle linking a dynamic object.  */
  boolean (*link_dynamic_object) PARAMS ((struct bfd_link_info *, bfd *));

  /* Called for each global symbol being written out by the linker.
     This should write out the dynamic symbol information.  */
  boolean (*write_dynamic_symbol) PARAMS ((bfd *, struct bfd_link_info *,
					   struct aout_link_hash_entry *));

  /* If this callback is not NULL, the linker calls it for each reloc.
     RELOC is a pointer to the unswapped reloc.  If *SKIP is set to
     true, the reloc will be skipped.  *RELOCATION may be changed to
     change the effects of the relocation.  */
  boolean (*check_dynamic_reloc) PARAMS ((struct bfd_link_info *info,
					  bfd *input_bfd,
					  asection *input_section,
					  struct aout_link_hash_entry *h,
					  PTR reloc, bfd_byte *contents,
					  boolean *skip,
					  bfd_vma *relocation));

  /* Called at the end of a link to finish up any dynamic linking
     information.  */
  boolean (*finish_dynamic_link) PARAMS ((bfd *, struct bfd_link_info *));
};
#define aout_backend_info(abfd) \
	((CONST struct aout_backend_data *)((abfd)->xvec->backend_data))

/* This is the layout in memory of a "struct exec" while we process it.
   All 'lengths' are given as a number of bytes.
   All 'alignments' are for relinkable files only;  an alignment of
	'n' indicates the corresponding segment must begin at an
	address that is a multiple of (2**n).  */

struct internal_exec
{
    long a_info;		/* Magic number and flags, packed */
    bfd_vma a_text;		/* length of text, in bytes  */
    bfd_vma a_data;		/* length of data, in bytes  */
    bfd_vma a_bss;		/* length of uninitialized data area in mem */
    bfd_vma a_syms;		/* length of symbol table data in file */
    bfd_vma a_entry;		/* start address */
    bfd_vma a_trsize;		/* length of text's relocation info, in bytes */
    bfd_vma a_drsize;		/* length of data's relocation info, in bytes */
    /* Added for i960 */
    bfd_vma a_tload;		/* Text runtime load address */
    bfd_vma a_dload;		/* Data runtime load address */
    unsigned char a_talign;	/* Alignment of text segment */
    unsigned char a_dalign;	/* Alignment of data segment */
    unsigned char a_balign;	/* Alignment of bss segment */
    char a_relaxable;           /* Enough info for linker relax */
};

/* Magic number is written 
< MSB          >
3130292827262524232221201918171615141312111009080706050403020100
< FLAGS        >< MACHINE TYPE ><  MAGIC NUMBER                >
*/
/* Magic number for NetBSD is
<MSB           >
3130292827262524232221201918171615141312111009080706050403020100
< FLAGS    >< MACHINE TYPE     ><  MAGIC NUMBER                >
*/

enum machine_type {
  M_UNKNOWN = 0,
  M_68010 = 1,
  M_68020 = 2,
  M_SPARC = 3,
  /* skip a bunch so we don't run into any of suns numbers */
  /* make these up for the ns32k*/
  M_NS32032 = (64),		/* ns32032 running ? */
  M_NS32532 = (64 + 5),		/* ns32532 running mach */

  M_386 = 100,
  M_29K = 101,          /* AMD 29000 */
  M_386_DYNIX = 102,	/* Sequent running dynix */
  M_ARM = 103,		/* Advanced Risc Machines ARM */
  M_SPARCLET = 131,	/* SPARClet = M_SPARC + 128 */
  M_386_NETBSD = 134,	/* NetBSD/i386 binary */
  M_68K_NETBSD = 135,	/* NetBSD/m68k binary */
  M_68K4K_NETBSD = 136,	/* NetBSD/m68k4k binary */
  M_532_NETBSD = 137,	/* NetBSD/ns32k binary */
  M_SPARC_NETBSD = 138,	/* NetBSD/sparc binary */
  M_PMAX_NETBSD = 139,	/* NetBSD/pmax (MIPS little-endian) binary */
  M_VAX_NETBSD = 140,	/* NetBSD/vax binary */
  M_ALPHA_NETBSD = 141,	/* NetBSD/alpha binary */
  M_ARM6_NETBSD = 143,	/* NetBSD/arm32 binary */
  M_SPARCLET_1 = 147,	/* 0x93, reserved */
  M_MIPS1 = 151,        /* MIPS R2000/R3000 binary */
  M_MIPS2 = 152,        /* MIPS R4000/R6000 binary */
  M_SPARCLET_2 = 163,	/* 0xa3, reserved */
  M_SPARCLET_3 = 179,	/* 0xb3, reserved */
  M_SPARCLET_4 = 195,	/* 0xc3, reserved */
  M_HP200 = 200,	/* HP 200 (68010) BSD binary */
  M_HP300 = (300 % 256), /* HP 300 (68020+68881) BSD binary */
  M_HPUX = (0x20c % 256), /* HP 200/300 HPUX binary */
  M_SPARCLET_5 = 211,	/* 0xd3, reserved */
  M_SPARCLET_6 = 227,	/* 0xe3, reserved */
  /*  M_SPARCLET_7 = 243	/ * 0xf3, reserved */
  M_SPARCLITE_LE = 243,
  M_CRIS = 255		/* Axis CRIS binary.  */
};

#define N_DYNAMIC(exec) ((exec).a_info & 0x80000000)

#ifndef N_MAGIC
# define N_MAGIC(exec) ((exec).a_info & 0xffff)
#endif

#ifndef N_MACHTYPE
# define N_MACHTYPE(exec) ((enum machine_type)(((exec).a_info >> 16) & 0xff))
#endif

#ifndef N_FLAGS
# define N_FLAGS(exec) (((exec).a_info >> 24) & 0xff)
#endif

#ifndef N_SET_INFO
# define N_SET_INFO(exec, magic, type, flags) \
((exec).a_info = ((magic) & 0xffff) \
 | (((int)(type) & 0xff) << 16) \
 | (((flags) & 0xff) << 24))
#endif

#ifndef N_SET_DYNAMIC
# define N_SET_DYNAMIC(exec, dynamic) \
((exec).a_info = (dynamic) ? ((exec).a_info | 0x80000000) : \
((exec).a_info & 0x7fffffff))
#endif

#ifndef N_SET_MAGIC
# define N_SET_MAGIC(exec, magic) \
((exec).a_info = (((exec).a_info & 0xffff0000) | ((magic) & 0xffff)))
#endif

#ifndef N_SET_MACHTYPE
# define N_SET_MACHTYPE(exec, machtype) \
((exec).a_info = \
 ((exec).a_info&0xff00ffff) | ((((int)(machtype))&0xff) << 16))
#endif

#ifndef N_SET_FLAGS
# define N_SET_FLAGS(exec, flags) \
((exec).a_info = \
 ((exec).a_info&0x00ffffff) | (((flags) & 0xff) << 24))
#endif

typedef struct aout_symbol {
  asymbol symbol;
  short desc;
  char other;
  unsigned char type;
} aout_symbol_type;

/* The `tdata' struct for all a.out-like object file formats.
   Various things depend on this struct being around any time an a.out
   file is being handled.  An example is dbxread.c in GDB.  */

struct aoutdata {
  struct internal_exec *hdr;		/* exec file header */
  aout_symbol_type *symbols;		/* symtab for input bfd */
  
  /* For ease, we do this */
  asection *textsec;
  asection *datasec;
  asection *bsssec;

  /* We remember these offsets so that after check_file_format, we have
     no dependencies on the particular format of the exec_hdr.  */
  file_ptr sym_filepos;
  file_ptr str_filepos;

  /* Size of a relocation entry in external form */
  unsigned reloc_entry_size;

  /* Size of a symbol table entry in external form */
  unsigned symbol_entry_size;

  /* Page size - needed for alignment of demand paged files. */
  unsigned long page_size;

  /* Segment size - needed for alignment of demand paged files. */
  unsigned long segment_size;

  /* Zmagic disk block size - need to align the start of the text
     section in ZMAGIC binaries.  Normally the same as page_size.  */
  unsigned long zmagic_disk_block_size;

  unsigned exec_bytes_size;
  unsigned vma_adjusted : 1;

  /* used when a bfd supports several highly similar formats */
  enum
    {
      default_format = 0,
      /* Used on HP 9000/300 running HP/UX.  See hp300hpux.c.  */
      gnu_encap_format,
      /* Used on Linux, 386BSD, etc.  See include/aout/aout64.h.  */
      q_magic_format
    } subformat;

  enum
    {
      undecided_magic = 0,
      z_magic,
      o_magic,
      n_magic
    } magic;

  /* A buffer for find_nearest_line.  */
  char *line_buf;

  /* The external symbol information.  */
  struct external_nlist *external_syms;
  bfd_size_type external_sym_count;
  bfd_window sym_window;
  char *external_strings;
  bfd_size_type external_string_size;
  bfd_window string_window;
  struct aout_link_hash_entry **sym_hashes;

  /* A pointer for shared library information.  */
  PTR dynamic_info;

  /* A mapping from local symbols to offsets into the global offset
     table, used when linking on SunOS.  This is indexed by the symbol
     index.  */
  bfd_vma *local_got_offsets;

  /* A pointer for data used by aout extensions.  (Currently only used
     by MiNT executables (see prg-mint.c).  */
  PTR ext;
};

struct  aout_data_struct {
    struct aoutdata a;
    struct internal_exec e;
};

#define	adata(bfd)		((bfd)->tdata.aout_data->a)
#define	exec_hdr(bfd)		(adata(bfd).hdr)
#define	obj_aout_symbols(bfd)	(adata(bfd).symbols)
#define	obj_textsec(bfd)	(adata(bfd).textsec)
#define	obj_datasec(bfd)	(adata(bfd).datasec)
#define	obj_bsssec(bfd)		(adata(bfd).bsssec)
#define	obj_sym_filepos(bfd)	(adata(bfd).sym_filepos)
#define	obj_str_filepos(bfd)	(adata(bfd).str_filepos)
#define	obj_reloc_entry_size(bfd) (adata(bfd).reloc_entry_size)
#define	obj_symbol_entry_size(bfd) (adata(bfd).symbol_entry_size)
#define obj_aout_subformat(bfd)	(adata(bfd).subformat)
#define obj_aout_external_syms(bfd) (adata(bfd).external_syms)
#define obj_aout_external_sym_count(bfd) (adata(bfd).external_sym_count)
#define obj_aout_sym_window(bfd) (adata(bfd).sym_window)
#define obj_aout_external_strings(bfd) (adata(bfd).external_strings)
#define obj_aout_external_string_size(bfd) (adata(bfd).external_string_size)
#define obj_aout_string_window(bfd) (adata(bfd).string_window)
#define obj_aout_sym_hashes(bfd) (adata(bfd).sym_hashes)
#define obj_aout_dynamic_info(bfd) (adata(bfd).dynamic_info)
#define obj_aout_ext(bfd)	(adata(bfd).ext)

/* We take the address of the first element of an asymbol to ensure that the
   macro is only ever applied to an asymbol */
#define aout_symbol(asymbol) ((aout_symbol_type *)(&(asymbol)->the_bfd))

/* Information we keep for each a.out section.  This is currently only
   used by the a.out backend linker.  */

struct aout_section_data_struct
{
  /* The unswapped relocation entries for this section.  */
  PTR relocs;
};

#define aout_section_data(s) \
  ((struct aout_section_data_struct *) (s)->used_by_bfd)

#define set_aout_section_data(s,v) \
  ((s)->used_by_bfd = (PTR)&(v)->relocs)

/* Prototype declarations for functions defined in aoutx.h  */

boolean
NAME(aout,squirt_out_relocs) PARAMS ((bfd *abfd, asection *section));

boolean
NAME(aout,make_sections) PARAMS ((bfd *));

const bfd_target *
NAME(aout,some_aout_object_p) PARAMS ((bfd *abfd,
				       struct internal_exec *execp,
				       const bfd_target *(*callback)(bfd *)));

boolean
NAME(aout,mkobject) PARAMS ((bfd *abfd));

enum machine_type
NAME(aout,machine_type) PARAMS ((enum bfd_architecture arch,
				 unsigned long machine,
				 boolean *unknown));

boolean
NAME(aout,set_arch_mach) PARAMS ((bfd *abfd, enum bfd_architecture arch,
		 		  unsigned long machine));

boolean
NAME(aout,new_section_hook) PARAMS ((bfd *abfd, asection *newsect));

boolean
NAME(aout,set_section_contents) PARAMS ((bfd *abfd, sec_ptr section,
			 PTR location, file_ptr offset, bfd_size_type count));

asymbol *
NAME(aout,make_empty_symbol) PARAMS ((bfd *abfd));

boolean
NAME(aout,translate_symbol_table) PARAMS ((bfd *, aout_symbol_type *,
					   struct external_nlist *,
					   bfd_size_type, char *,
					   bfd_size_type,
					   boolean dynamic));

boolean
NAME(aout,slurp_symbol_table) PARAMS ((bfd *abfd));

boolean
NAME(aout,write_syms) PARAMS ((bfd *abfd));

void
NAME(aout,reclaim_symbol_table) PARAMS ((bfd *abfd));

long
NAME(aout,get_symtab_upper_bound) PARAMS ((bfd *abfd));

long
NAME(aout,get_symtab) PARAMS ((bfd *abfd, asymbol **location));

void
NAME(aout,swap_ext_reloc_in) PARAMS ((bfd *, struct reloc_ext_external *,
				      arelent *, asymbol **, bfd_size_type));
void
NAME(aout,swap_std_reloc_in) PARAMS ((bfd *, struct reloc_std_external *,
				      arelent *, asymbol **, bfd_size_type));

reloc_howto_type *
NAME(aout,reloc_type_lookup) PARAMS ((bfd *abfd,
				      bfd_reloc_code_real_type code));

boolean
NAME(aout,slurp_reloc_table) PARAMS ((bfd *abfd, sec_ptr asect,
				      asymbol **symbols));

long
NAME(aout,canonicalize_reloc) PARAMS ((bfd *abfd, sec_ptr section,
				       arelent **relptr, asymbol **symbols));

long
NAME(aout,get_reloc_upper_bound) PARAMS ((bfd *abfd, sec_ptr asect));

void
NAME(aout,reclaim_reloc) PARAMS ((bfd *ignore_abfd, sec_ptr ignore));

alent *
NAME(aout,get_lineno) PARAMS ((bfd *ignore_abfd, asymbol *ignore_symbol));

void
NAME(aout,print_symbol) PARAMS ((bfd *ignore_abfd, PTR file,
			    asymbol *symbol, bfd_print_symbol_type how));

void
NAME(aout,get_symbol_info) PARAMS ((bfd *ignore_abfd,
                           asymbol *symbol, symbol_info *ret));

boolean
NAME(aout,find_nearest_line) PARAMS ((bfd *abfd, asection *section,
      asymbol **symbols, bfd_vma offset, CONST char **filename_ptr,
      CONST char **functionname_ptr, unsigned int *line_ptr));

long
NAME(aout,read_minisymbols) PARAMS ((bfd *, boolean, PTR *, unsigned int *));

asymbol *
NAME(aout,minisymbol_to_symbol) PARAMS ((bfd *, boolean, const PTR,
					 asymbol *));

int
NAME(aout,sizeof_headers) PARAMS ((bfd *abfd, boolean exec));

boolean
NAME(aout,adjust_sizes_and_vmas) PARAMS ((bfd *abfd,
       bfd_size_type *text_size, file_ptr *text_end));

void
NAME(aout,swap_exec_header_in) PARAMS ((bfd *abfd,
       struct external_exec *raw_bytes, struct internal_exec *execp));

void
NAME(aout,swap_exec_header_out) PARAMS ((bfd *abfd,
       struct internal_exec *execp, struct external_exec *raw_bytes));

struct bfd_hash_entry *
NAME(aout,link_hash_newfunc)
  PARAMS ((struct bfd_hash_entry *, struct bfd_hash_table *, const char *));

boolean
NAME(aout,link_hash_table_init)
     PARAMS ((struct aout_link_hash_table *, bfd *,
	      struct bfd_hash_entry *(*) (struct bfd_hash_entry *,
					  struct bfd_hash_table *,
					  const char *)));

struct bfd_link_hash_table *
NAME(aout,link_hash_table_create) PARAMS ((bfd *));

boolean
NAME(aout,link_add_symbols) PARAMS ((bfd *, struct bfd_link_info *));

boolean
NAME(aout,final_link) PARAMS ((bfd *, struct bfd_link_info *,
			       void (*) (bfd *, file_ptr *, file_ptr *,
					 file_ptr *)));

boolean
NAME(aout,bfd_free_cached_info) PARAMS ((bfd *));

/* A.out uses the generic versions of these routines... */

#define	aout_16_get_section_contents	_bfd_generic_get_section_contents

#define	aout_32_get_section_contents	_bfd_generic_get_section_contents

#define	aout_64_get_section_contents	_bfd_generic_get_section_contents
#ifndef NO_WRITE_HEADER_KLUDGE
#define NO_WRITE_HEADER_KLUDGE 0
#endif

#ifndef aout_32_bfd_is_local_label_name
#define aout_32_bfd_is_local_label_name bfd_generic_is_local_label_name
#endif

#ifndef WRITE_HEADERS
#define WRITE_HEADERS(abfd, execp)					      \
      {									      \
	bfd_size_type text_size; /* dummy vars */			      \
	file_ptr text_end;						      \
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
	NAME(aout,swap_exec_header_out) (abfd, execp, &exec_bytes);	      \
									      \
	if (bfd_seek (abfd, (file_ptr) 0, SEEK_SET) != 0) return false;	      \
	if (bfd_write ((PTR) &exec_bytes, 1, EXEC_BYTES_SIZE, abfd)	      \
	    != EXEC_BYTES_SIZE)						      \
	  return false;							      \
	/* Now write out reloc info, followed by syms and strings */	      \
  									      \
	if (bfd_get_outsymbols (abfd) != (asymbol **) NULL		      \
	    && bfd_get_symcount (abfd) != 0) 				      \
	  {								      \
	    if (bfd_seek (abfd, (file_ptr)(N_SYMOFF(*execp)), SEEK_SET) != 0) \
	      return false;						      \
									      \
	    if (! NAME(aout,write_syms)(abfd)) return false;		      \
	  }								      \
									      \
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
#endif

#endif /* ! defined (LIBAOUT_H) */
