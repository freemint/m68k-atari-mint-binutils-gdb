/* Native-dependent code for Motorola m68k's running FreeMiNT, for GDB.
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

#include "defs.h"
#include "inferior.h"
#include "gdbcore.h"

void
fetch_inferior_registers (regno)
     int regno;
{
	struct reg inferior_registers;
	struct fpreg inferior_fp_registers;
	
	ptrace (PT_GETREGS, inferior_pid,
		(PTRACE_ARG3_TYPE) & inferior_registers, 0);
	memcpy (&registers[REGISTER_BYTE (0)], &inferior_registers,
		sizeof (inferior_registers));
	
	ptrace (PT_GETFPREGS, inferior_pid,
		(PTRACE_ARG3_TYPE) & inferior_fp_registers, 0);
	memcpy (&registers[REGISTER_BYTE (FP0_REGNUM)], &inferior_fp_registers,
		sizeof (inferior_fp_registers));
	
	registers_fetched ();
}

void
store_inferior_registers (regno)
     int regno;
{
	struct reg inferior_registers;
	struct fpreg inferior_fp_registers;
	
	memcpy (&inferior_registers, &registers[REGISTER_BYTE (0)],
		sizeof (inferior_registers));
	ptrace (PT_SETREGS, inferior_pid,
		(PTRACE_ARG3_TYPE) & inferior_registers, 0);
	
	memcpy (&inferior_fp_registers, &registers[REGISTER_BYTE (FP0_REGNUM)],
		sizeof (inferior_fp_registers));
	ptrace (PT_SETFPREGS, inferior_pid,
		(PTRACE_ARG3_TYPE) & inferior_fp_registers, 0);
}

# include "gdb-stabs.h"
# include "symfile.h"
# include "objfiles.h"

void
child_post_startup_inferior (pid)
     int pid;
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
			ANOFFSET (new_offsets, i) = ANOFFSET (objfile->section_offsets, i);
		
		ANOFFSET (new_offsets, SECT_OFF_TEXT) = basepage;
		ANOFFSET (new_offsets, SECT_OFF_DATA) = basepage;
		ANOFFSET (new_offsets, SECT_OFF_BSS) = basepage;
		/* ANOFFSET (new_offsets, SECT_OFF_RODATA) = basepage; */
		
		objfile_relocate (objfile, new_offsets);
	}
}

void
child_post_attach (pid)
     int pid;
{
	child_post_startup_inferior (pid);
}

void
mint_post_unpush_target (t)
	struct target_ops *t;
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
			ANOFFSET (new_offsets, i) = ANOFFSET (objfile->section_offsets, i);
		
		ANOFFSET (new_offsets, SECT_OFF_TEXT) = 0;
		ANOFFSET (new_offsets, SECT_OFF_DATA) = 0;
		ANOFFSET (new_offsets, SECT_OFF_BSS) = 0;
		/* ANOFFSET (new_offsets, SECT_OFF_RODATA) = 0; */
		
		objfile_relocate (objfile, new_offsets);
	}
}
