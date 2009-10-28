/* Native-dependent definitions for m68k-atari running FreeMiNT, for GDB.
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

#define PTRACE_ARG3_TYPE void*

#define FETCH_INFERIOR_REGISTERS
#define ATTACH_DETACH

#define CHILD_POST_STARTUP_INFERIOR
#define CHILD_POST_ATTACH

struct target_ops;
extern void mint_post_unpush_target (struct target_ops *t);
#define TARGET_POST_UNPUSH_TARGET_HOOK(t) mint_post_unpush_target (t)
