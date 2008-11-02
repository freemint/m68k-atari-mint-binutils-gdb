/* Parameters for execution on a m68k-atari running FreeMiNT, for GDB.
   Copyright 2000 Free Software Foundation, Inc.

   This file is part of GDB.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef XM_MINT_H
#define XM_MINT_H

/* Include this to get things like NGROUPS which <limits.h> doesn't
   define on some systems. */
#include <sys/param.h>

/* We have to include these files now, so that GDB will not make
   competing definitions in defs.h.  */
#include <limits.h>


#include "m68k/xm-m68k.h"


#endif /* XM_MINT_H */
