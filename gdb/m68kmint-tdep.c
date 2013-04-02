/* Target-dependent code for Motorola m68k's running FreeMiNT, for GDB.

   Copyright (C) 2004, 2005, 2007, 2008, 2009 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include "defs.h"
#include "arch-utils.h"
#include "frame.h"
#include "osabi.h"
#include "regcache.h"
#include "regset.h"
#include "trad-frame.h"
#include "tramp-frame.h"
#include "gdbtypes.h"

#include "gdb_assert.h"
#include "gdb_string.h"

#include "m68k-tdep.h"

static const gdb_byte *
m68kmint_local_breakpoint_from_pc (struct gdbarch *gdbarch,
			       CORE_ADDR *pcptr, int *lenptr)
{
  static gdb_byte break_insn[] = {0x4a, 0xfc}; // ILLEGAL
  *lenptr = sizeof (break_insn);
  return break_insn;
}

static void
m68kmint_init_abi (struct gdbarch_info info, struct gdbarch *gdbarch)
{
  struct gdbarch_tdep *tdep = gdbarch_tdep (gdbarch);

  set_gdbarch_breakpoint_from_pc (gdbarch, m68kmint_local_breakpoint_from_pc);
  set_gdbarch_decr_pc_after_break (gdbarch, 2);
/*
  tdep->jb_pc = 5;
  tdep->jb_elt_size = 4;

  tdep->struct_return = reg_struct_return;
*/
}

static enum gdb_osabi
m68kmint_osabi_sniffer (bfd *abfd)
{
  if (strcmp (bfd_get_target (abfd), "a.out-mintprg") == 0)
    return GDB_OSABI_MINT;

  return GDB_OSABI_UNKNOWN;
}

/* Provide a prototype to silence -Wmissing-prototypes.  */
void _initialize_m68kmint_tdep (void);

void
_initialize_m68kmint_tdep (void)
{
  gdbarch_register_osabi_sniffer (bfd_arch_m68k, bfd_target_aout_flavour,
				  m68kmint_osabi_sniffer);

  gdbarch_register_osabi (bfd_arch_m68k, 0, GDB_OSABI_MINT,
			  m68kmint_init_abi);
}
