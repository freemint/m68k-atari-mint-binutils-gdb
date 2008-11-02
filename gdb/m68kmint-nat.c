/* Native-dependent code for Motorola m68k's running FreeMiNT, for GDB.
   Copyright 1988, 1989, 1991, 1992, 1994, 1996, 2000, 2001
   Free Software Foundation, Inc.

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

#include "defs.h"
#include <sys/types.h>
#include <sys/ptrace.h>
#include <sys/wait.h>

struct reg
{
	long	regs[16];	/* D0-D7/A0-A7 */
	long	sr;
	long	pc;
};

struct fpreg
{
	long	regs[3*8];	/* FP0-FP7 */
	long	fpcr;
	long	fpsr;
	long	fpiar;
};

#include "inferior.h"
#include "gdbcore.h"
#include "regcache.h"

void
fetch_inferior_registers (int regno)
{
  struct reg inferior_registers;
  struct fpreg inferior_fp_registers;

  ptrace (PT_GETREGS, PIDGET (inferior_ptid),
	  (PTRACE_ARG3_TYPE) & inferior_registers, 0);
  memcpy (&registers[REGISTER_BYTE (0)], &inferior_registers,
	  sizeof (inferior_registers));

  ptrace (PT_GETFPREGS, PIDGET (inferior_ptid),
	  (PTRACE_ARG3_TYPE) & inferior_fp_registers, 0);
  memcpy (&registers[REGISTER_BYTE (FP0_REGNUM)], &inferior_fp_registers,
	  sizeof (inferior_fp_registers));

  registers_fetched ();
}

void
store_inferior_registers (int regno)
{
  struct reg inferior_registers;
  struct fpreg inferior_fp_registers;

  memcpy (&inferior_registers, &registers[REGISTER_BYTE (0)],
	  sizeof (inferior_registers));
  ptrace (PT_SETREGS, PIDGET (inferior_ptid),
	  (PTRACE_ARG3_TYPE) & inferior_registers, 0);

  memcpy (&inferior_fp_registers, &registers[REGISTER_BYTE (FP0_REGNUM)],
	  sizeof (inferior_fp_registers));
  ptrace (PT_SETFPREGS, PIDGET (inferior_ptid),
	  (PTRACE_ARG3_TYPE) & inferior_fp_registers, 0);
}

# include "gdb-stabs.h"
# include "symfile.h"
# include "objfiles.h"

static void
child_initialize (int pid)
{
  CORE_ADDR basepage;
  struct objfile *objfile;

  if (ptrace (999, pid, 0, (long) &basepage))
    error ("cannot get basepage address for pid %d", pid);

  basepage += 0x100;

  ALL_OBJFILES(objfile)
  {
    struct section_offsets *new_offsets;
    int i;

    new_offsets = (struct section_offsets *) alloca (SIZEOF_SECTION_OFFSETS);

    for (i = 0; i < objfile->num_sections; ++i)
      new_offsets->offsets[i] = objfile->section_offsets->offsets[i];

    new_offsets->offsets[SECT_OFF_TEXT (objfile)] = basepage;
    new_offsets->offsets[SECT_OFF_DATA (objfile)] = basepage;
    new_offsets->offsets[SECT_OFF_BSS (objfile)] = basepage;
    /* new_offsets->offsets[SECT_OFF_RODATA (objfile)] = basepage; */

    objfile_relocate (objfile, new_offsets);
  }
}

void
child_post_startup_inferior (ptid_t ptid)
{
  child_initialize (PIDGET (ptid));
}

void
child_post_attach (int pid)
{
  child_initialize (pid);
}

void
mint_post_unpush_target (struct target_ops *t)
{
  extern struct target_ops child_ops; /* inftarg.c */
  struct objfile *objfile;

  if (t != &child_ops)
    return;

  ALL_OBJFILES(objfile)
  {
    struct section_offsets *new_offsets;
    int i;

    new_offsets = (struct section_offsets *) alloca (SIZEOF_SECTION_OFFSETS);

    for (i = 0; i < objfile->num_sections; ++i)
      new_offsets->offsets[i] = objfile->section_offsets->offsets[i];

    new_offsets->offsets[SECT_OFF_TEXT (objfile)] = 0;
    new_offsets->offsets[SECT_OFF_DATA (objfile)] = 0;
    new_offsets->offsets[SECT_OFF_BSS (objfile)] = 0;
    /* new_offsets->offsets[SECT_OFF_RODATA (objfile)] = 0; */

    objfile_relocate (objfile, new_offsets);
  }
}
