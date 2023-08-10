/* Support for Atari TOS PRG/ELF binaries.
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

/* Standard GEMDOS program flags.  */
#define _MINT_F_FASTLOAD      0x01    /* Don't clear heap.  */
#define _MINT_F_ALTLOAD       0x02    /* OK to load in alternate RAM.  */
#define _MINT_F_ALTALLOC      0x04    /* OK to malloc from alt. RAM.  */
#define _MINT_F_BESTFIT       0x08    /* Load with optimal heap size.  */
/* The memory flags are mutually exclusive.  */
#define _MINT_F_MEMPROTECTION 0xf0    /* Masks out protection bits.  */
#define _MINT_F_MEMPRIVATE    0x00    /* Memory is private.  */
#define _MINT_F_MEMGLOBAL     0x10    /* Read/write access to mem allowed.  */
#define _MINT_F_MEMSUPER      0x20    /* Only supervisor access allowed.  */
#define _MINT_F_MEMREADABLE   0x30    /* Any read access OK.  */
#define _MINT_F_SHTEXT        0x800   /* Program's text may be shared */

extern bfd_target m68k_elf32_atariprg_vec;

/* Called by bfd_init ().  */
extern void bfd_elf32_atariprg_init
  (void);

/* Called by elf.c.  */
extern void bfd_elf32_atariprg_get_extra_header_info
  (bfd *, file_ptr *, bfd_size_type *);
extern void bfd_elf32_atariprg_set_nonload_pos
  (bfd *, file_ptr);

/* Called by the linker.  */
extern bool bfd_elf32_atariprg_set_extended_flags
  (bfd *, uint32_t);
extern bool bfd_elf32_atariprg_set_stack_size
  (bfd *, int32_t);
