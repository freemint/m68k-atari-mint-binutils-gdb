/* Native-dependent code for Motorola m68k's running FreeMiNT, for GDB.

   Copyright (C) 1988, 1989, 1991, 1992, 1994, 1996, 2000, 2001, 2009
   Free Software Foundation, Inc.

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
#include "gdbcore.h"
#include "inferior.h"
#include "regcache.h"

#include "gdb_assert.h"
#include <sys/types.h>
#include <sys/ptrace.h>

#include "m68k-tdep.h"
#include "inf-ptrace.h"

#include "gdb-stabs.h"
#include "symfile.h"
#include "objfiles.h"

/* The following structures are used by the FreeMiNT kernel
   when calling ptrace() with PT_GETREGS and PT_GETFPREGS.
   They must match the FreeMiNT functions process_getregs()
   and process_getfpregs() in the source sys/arch/process_reg.c
   Since they are supplied to the regcache wih a loop,
   the register order must also match the enum m68k_regnum
   from gdb/m68k-tdep.h  */

struct reg
{
  long regs[16]; /* D0-D7/A0-A7 */
  long sr;
  long pc;
};

struct fpreg
{
  long regs[3*8]; /* FP0-FP7 */ /* FIXME: Wrong size on ColdFire */
  long fpcr;
  long fpsr;
  long fpiar;
};

static int
m68kmint_fpreg_offset (struct gdbarch *gdbarch, int regnum)
{
  int fp_len = TYPE_LENGTH (gdbarch_register_type (gdbarch, M68K_FP0_REGNUM));
  
  if (regnum >= M68K_FPC_REGNUM)
    return 8 * fp_len + (regnum - M68K_FPC_REGNUM) * 4;

  return (regnum - M68K_FP0_REGNUM) * fp_len;
}

static int
m68kmint_gregset_supplies_p (int regnum)
{
  return (regnum >= M68K_D0_REGNUM && regnum <= M68K_PC_REGNUM);
}

static int
m68kmint_fpregset_supplies_p (int regnum)
{
  return (regnum >= M68K_FP0_REGNUM && regnum <= M68K_FPI_REGNUM);
}

/* Supply the general-purpose registers stored in GREGS to REGCACHE.  */

static void
m68kmint_supply_gregset (struct regcache *regcache, const void *gregs)
{
  const char *regs = gregs;
  int regnum;

  for (regnum = M68K_D0_REGNUM; regnum <= M68K_PC_REGNUM; regnum++)
    regcache_raw_supply (regcache, regnum, regs + regnum * 4);
}

/* Supply the floating-point registers stored in FPREGS to REGCACHE.  */

static void
m68kmint_supply_fpregset (struct regcache *regcache, const void *fpregs)
{
  struct gdbarch *gdbarch = get_regcache_arch (regcache);
  const char *regs = fpregs;
  int regnum;

  for (regnum = M68K_FP0_REGNUM; regnum <= M68K_FPI_REGNUM; regnum++)
    regcache_raw_supply (regcache, regnum,
			 regs + m68kmint_fpreg_offset (gdbarch, regnum));
}

/* Collect the general-purpose registers from REGCACHE and store them
   in GREGS.  */

static void
m68kmint_collect_gregset (const struct regcache *regcache,
			  void *gregs, int regnum)
{
  char *regs = gregs;
  int i;

  for (i = M68K_D0_REGNUM; i <= M68K_PC_REGNUM; i++)
    {
      if (regnum == -1 || regnum == i)
	regcache_raw_collect (regcache, i, regs + i * 4);
    }
}

/* Collect the floating-point registers from REGCACHE and store them
   in FPREGS.  */

static void
m68kmint_collect_fpregset (struct regcache *regcache,
			   void *fpregs, int regnum)
{
  struct gdbarch *gdbarch = get_regcache_arch (regcache);
  char *regs = fpregs;
  int i;

  for (i = M68K_FP0_REGNUM; i <= M68K_FPI_REGNUM; i++)
    {
      if (regnum == -1 || regnum == i)
	regcache_raw_collect (regcache, i,
			      regs + m68kmint_fpreg_offset (gdbarch, i));
    }
}

/* Fetch register REGNUM from the inferior.  If REGNUM is -1, do this
   for all registers (including the floating-point registers).  */

static void
m68kmint_fetch_inferior_registers (struct target_ops *ops,
				  struct regcache *regcache, int regnum)
{
  if (regnum == -1 || m68kmint_gregset_supplies_p (regnum))
    {
      struct reg regs;

      if (ptrace (PT_GETREGS, PIDGET (inferior_ptid),
		  (PTRACE_TYPE_ARG3) &regs, 0) == -1)
	perror_with_name (_("Couldn't get registers"));

      m68kmint_supply_gregset (regcache, &regs);
    }

  if (regnum == -1 || m68kmint_fpregset_supplies_p (regnum))
    {
      struct fpreg fpregs;

      if (ptrace (PT_GETFPREGS, PIDGET (inferior_ptid),
		  (PTRACE_TYPE_ARG3) &fpregs, 0) == -1)
	perror_with_name (_("Couldn't get floating point status"));

      m68kmint_supply_fpregset (regcache, &fpregs);
    }
}

/* Store register REGNUM back into the inferior.  If REGNUM is -1, do
   this for all registers (including the floating-point registers).  */

static void
m68kmint_store_inferior_registers (struct target_ops *ops,
				  struct regcache *regcache, int regnum)
{
  if (regnum == -1 || m68kmint_gregset_supplies_p (regnum))
    {
      struct reg regs;

      if (ptrace (PT_GETREGS, PIDGET (inferior_ptid),
                  (PTRACE_TYPE_ARG3) &regs, 0) == -1)
        perror_with_name (_("Couldn't get registers"));

      m68kmint_collect_gregset (regcache, &regs, regnum);

      if (ptrace (PT_SETREGS, PIDGET (inferior_ptid),
	          (PTRACE_TYPE_ARG3) &regs, 0) == -1)
        perror_with_name (_("Couldn't write registers"));
    }

  if (regnum == -1 || m68kmint_fpregset_supplies_p (regnum))
    {
      struct fpreg fpregs;

      if (ptrace (PT_GETFPREGS, PIDGET (inferior_ptid),
		  (PTRACE_TYPE_ARG3) &fpregs, 0) == -1)
	perror_with_name (_("Couldn't get floating point status"));

      m68kmint_collect_fpregset (regcache, &fpregs, regnum);

      if (ptrace (PT_SETFPREGS, PIDGET (inferior_ptid),
		  (PTRACE_TYPE_ARG3) &fpregs, 0) == -1)
	perror_with_name (_("Couldn't write floating point status"));
    }
}

static void
child_initialize (int pid)
{
  CORE_ADDR basepage_adr;
  CORE_ADDR text_adr;
  struct objfile *objfile;

  if (ptrace (PT_BASEPAGE, pid, 0, (int) &basepage_adr))
    error ("cannot get basepage address for pid %d", pid);

  /* The TEXT segment is just after the basepage.  */
  text_adr = basepage_adr + 0x100;

  ALL_OBJFILES(objfile)
  {
    struct section_offsets *new_offsets;
    int i;

    new_offsets = (struct section_offsets *) alloca (SIZEOF_N_SECTION_OFFSETS (objfile->num_sections));

    /* Copy the original sections offsets.  */
    for (i = 0; i < objfile->num_sections; ++i)
      new_offsets->offsets[i] = objfile->section_offsets->offsets[i];

    /* Each section will be offset by the address of the TEXT segment.  */
    new_offsets->offsets[SECT_OFF_TEXT (objfile)] = text_adr;
    new_offsets->offsets[SECT_OFF_DATA (objfile)] = text_adr;
    new_offsets->offsets[SECT_OFF_BSS (objfile)] = text_adr;

    objfile_relocate (objfile, new_offsets);
  }
}

static void
m68kmint_child_post_startup_inferior (ptid_t ptid)
{
  child_initialize (PIDGET (ptid));
}

/* Provide a prototype to silence -Wmissing-prototypes.  */
void _initialize_m68kmint_nat (void);

void
_initialize_m68kmint_nat (void)
{
  struct target_ops *t;

  t = inf_ptrace_target ();
  t->to_fetch_registers = m68kmint_fetch_inferior_registers;
  t->to_store_registers = m68kmint_store_inferior_registers;
  t->to_post_startup_inferior = m68kmint_child_post_startup_inferior;
  add_target (t);
}
