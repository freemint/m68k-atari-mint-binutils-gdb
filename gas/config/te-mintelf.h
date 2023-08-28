/* Copyright (C) 2023 Free Software Foundation, Inc.

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3,
   or (at your option) any later version.

   GAS is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
   the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#define TE_MINTELF
#define LOCAL_LABELS_FB 1
#define REGISTER_PREFIX_OPTIONAL 1

/* The .lcomm directive mustn't try to align more than possible.  */
#define TC_IMPLICIT_LCOMM_ALIGNMENT(SIZE, P2VAR)		\
  do								\
    {								\
      if ((SIZE) >= 4)						\
	(P2VAR) = 2;						\
      else if ((SIZE) >= 2)					\
	(P2VAR) = 1;						\
      else							\
	(P2VAR) = 0;						\
    }								\
  while (0)

#include "obj-format.h"

/* No shared lib support, so we don't need to ensure externally
   visible symbols can be overridden.  */
#undef  EXTERN_FORCE_RELOC
#define EXTERN_FORCE_RELOC 0
