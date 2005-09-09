/* BFD backend for MiNT flavored aout binaries.
   Copyright (C) 1998 Free Software Foundation, Inc.
   Written by Guido Flohr (gufl0000 (somewhere at) stud.uni-sb.de).

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

#define N_HEADER_IN_TEXT(x) 0
#define BYTES_IN_WORD 4
#define ENTRY_CAN_BE_ZERO
#define N_SHARED_LIB(x) 0 /* Avoids warning */
#define TEXT_START_ADDR 0
#define TARGET_PAGE_SIZE 2
#define SEGMENT_SIZE TARGET_PAGE_SIZE
#define TARGET_IS_BIG_ENDIAN_P
#define DEFAULT_ARCH bfd_arch_m68k

#define MY(OP) CONCAT2(m68kmint_aout_,OP)
#define TARGETNAME "a.out-mint"

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "libaout.h"

#define MY_BFD_TARGET MY(vec)

#include "aout-target.h"

CONST bfd_target MY(vec) = {
  TARGETNAME,           /* name */
  bfd_target_aout_flavour,
  BFD_ENDIAN_BIG,               /* target byte order (big) */
  BFD_ENDIAN_BIG,               /* target headers byte order (big) */
  (HAS_RELOC |                  /* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | WP_TEXT ),
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
  
  NULL,
  (PTR) MY_backend_data,
};
