/* Common target-dependent code for MiNT ELF systems.

   Copyright (C) 2002-2023 Free Software Foundation, Inc.

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
#include "m68k-tdep.h"

/* This enum is derived from MiNTLib's <bits/signum.h>.  */

enum
  {
   MINT_SIGHUP = 1,
   MINT_SIGINT = 2,
   MINT_SIGQUIT = 3,
   MINT_SIGILL = 4,
   MINT_SIGTRAP = 5,
   MINT_SIGABRT = 6,
   MINT_SIGEMT = 7,
   MINT_SIGFPE = 8,
   MINT_SIGKILL = 9,
   MINT_SIGBUS = 10,
   MINT_SIGSEGV = 11,
   MINT_SIGSYS = 12,
   MINT_SIGPIPE = 13,
   MINT_SIGALRM = 14,
   MINT_SIGTERM = 15,
   MINT_SIGURG = 16,
   MINT_SIGSTOP = 17,
   MINT_SIGTSTP = 18,
   MINT_SIGCONT = 19,
   MINT_SIGCHLD = 20,
   MINT_SIGTTIN = 21,
   MINT_SIGTTOU = 22,
   MINT_SIGIO = 23,
   MINT_SIGXCPU = 24,
   MINT_SIGXFSZ = 25,
   MINT_SIGVTALRM = 26,
   MINT_SIGPROF = 27,
   MINT_SIGWINCH = 28,
   MINT_SIGUSR1 = 29,
   MINT_SIGUSR2 = 30,
   MINT_SIGPWR = 31,
  };

/* Implement the "gdb_signal_from_target" gdbarch method.  */

static enum gdb_signal
mintelf_gdb_signal_from_target (struct gdbarch *gdbarch, int signal)
{
  switch (signal)
    {
    case 0:
      return GDB_SIGNAL_0;

    case MINT_SIGHUP:
      return GDB_SIGNAL_HUP;

    case MINT_SIGINT:
      return GDB_SIGNAL_INT;

    case MINT_SIGQUIT:
      return GDB_SIGNAL_QUIT;

    case MINT_SIGILL:
      return GDB_SIGNAL_ILL;

    case MINT_SIGTRAP:
      return GDB_SIGNAL_TRAP;

    case MINT_SIGABRT:
      return GDB_SIGNAL_ABRT;

    case MINT_SIGEMT:
      return GDB_SIGNAL_EMT;

    case MINT_SIGFPE:
      return GDB_SIGNAL_FPE;

    case MINT_SIGKILL:
      return GDB_SIGNAL_KILL;

    case MINT_SIGBUS:
      return GDB_SIGNAL_BUS;

    case MINT_SIGSEGV:
      return GDB_SIGNAL_SEGV;

    case MINT_SIGSYS:
      return GDB_SIGNAL_SYS;

    case MINT_SIGPIPE:
      return GDB_SIGNAL_PIPE;

    case MINT_SIGALRM:
      return GDB_SIGNAL_ALRM;

    case MINT_SIGTERM:
      return GDB_SIGNAL_TERM;

    case MINT_SIGURG:
      return GDB_SIGNAL_URG;

    case MINT_SIGSTOP:
      return GDB_SIGNAL_STOP;

    case MINT_SIGTSTP:
      return GDB_SIGNAL_TSTP;

    case MINT_SIGCONT:
      return GDB_SIGNAL_CONT;

    case MINT_SIGCHLD:
      return GDB_SIGNAL_CHLD;

    case MINT_SIGTTIN:
      return GDB_SIGNAL_TTIN;

    case MINT_SIGTTOU:
      return GDB_SIGNAL_TTOU;

    case MINT_SIGIO:
      return GDB_SIGNAL_IO;

    case MINT_SIGXCPU:
      return GDB_SIGNAL_XCPU;

    case MINT_SIGXFSZ:
      return GDB_SIGNAL_XFSZ;

    case MINT_SIGVTALRM:
      return GDB_SIGNAL_VTALRM;

    case MINT_SIGPROF:
      return GDB_SIGNAL_PROF;

    case MINT_SIGWINCH:
      return GDB_SIGNAL_WINCH;

    case MINT_SIGUSR1:
      return GDB_SIGNAL_USR1;

    case MINT_SIGUSR2:
      return GDB_SIGNAL_USR2;

    case MINT_SIGPWR:
      return GDB_SIGNAL_PWR;
    }

  return GDB_SIGNAL_UNKNOWN;
}

/* Implement the "gdb_signal_to_target" gdbarch method.  */

static int
mintelf_gdb_signal_to_target (struct gdbarch *gdbarch,
		enum gdb_signal signal)
{
  switch (signal)
    {
    case GDB_SIGNAL_0:
      return 0;

    case GDB_SIGNAL_HUP:
      return MINT_SIGHUP;

    case GDB_SIGNAL_INT:
      return MINT_SIGINT;

    case GDB_SIGNAL_QUIT:
      return MINT_SIGQUIT;

    case GDB_SIGNAL_ILL:
      return MINT_SIGILL;

    case GDB_SIGNAL_TRAP:
      return MINT_SIGTRAP;

    case GDB_SIGNAL_ABRT:
      return MINT_SIGABRT;

    case GDB_SIGNAL_EMT:
      return MINT_SIGEMT;

    case GDB_SIGNAL_FPE:
      return MINT_SIGFPE;

    case GDB_SIGNAL_KILL:
      return MINT_SIGKILL;

    case GDB_SIGNAL_BUS:
      return MINT_SIGBUS;

    case GDB_SIGNAL_SEGV:
      return MINT_SIGSEGV;

    case GDB_SIGNAL_SYS:
      return MINT_SIGSYS;

    case GDB_SIGNAL_PIPE:
      return MINT_SIGPIPE;

    case GDB_SIGNAL_ALRM:
      return MINT_SIGALRM;

    case GDB_SIGNAL_TERM:
      return MINT_SIGTERM;

    case GDB_SIGNAL_URG:
      return MINT_SIGSTOP;

    case GDB_SIGNAL_TSTP:
      return MINT_SIGTSTP;

    case GDB_SIGNAL_CONT:
      return MINT_SIGCONT;

    case GDB_SIGNAL_CHLD:
      return MINT_SIGCHLD;

    case GDB_SIGNAL_TTIN:
      return MINT_SIGTTIN;

    case GDB_SIGNAL_TTOU:
      return MINT_SIGTTOU;

    case GDB_SIGNAL_IO:
      return MINT_SIGIO;

    case GDB_SIGNAL_XCPU:
      return MINT_SIGXCPU;

    case GDB_SIGNAL_XFSZ:
      return MINT_SIGXFSZ;

    case GDB_SIGNAL_VTALRM:
      return MINT_SIGVTALRM;

    case GDB_SIGNAL_PROF:
      return MINT_SIGPROF;

    case GDB_SIGNAL_WINCH:
      return MINT_SIGWINCH;

    case GDB_SIGNAL_USR1:
      return MINT_SIGUSR1;

    case GDB_SIGNAL_USR2:
      return MINT_SIGUSR2;

    case GDB_SIGNAL_PWR:
      return MINT_SIGPWR;
    }

  return -1;
}

static void
mintelf_init_abi (struct gdbarch_info info, struct gdbarch *gdbarch)
{
  m68k_gdbarch_tdep *tdep = gdbarch_tdep<m68k_gdbarch_tdep> (gdbarch);

  set_gdbarch_gdb_signal_from_target (gdbarch, mintelf_gdb_signal_from_target);
  set_gdbarch_gdb_signal_to_target (gdbarch, mintelf_gdb_signal_to_target);

  /* MiNT ELF uses the SVR4 ABI.  */
  m68k_svr4_init_abi (info, gdbarch);
  tdep->struct_return = pcc_struct_return;

}

void _initialize_mintelf_tdep ();
void
_initialize_mintelf_tdep ()
{
  gdbarch_register_osabi (bfd_arch_m68k, 0, GDB_OSABI_MINTELF, mintelf_init_abi);
}
