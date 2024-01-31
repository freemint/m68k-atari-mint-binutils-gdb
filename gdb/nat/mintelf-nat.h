/* Internal interfaces for the MiNT ELF code.

   Copyright (C) 2006-2023 Free Software Foundation, Inc.

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

#ifndef NAT_MINTELF_NAT_H
#define NAT_MINTELF_NAT_H

#include "gdbsupport/function-view.h"

#include <unistd.h>

namespace mintelf_nat
{

/* Write gdb's LEN bytes from WRITEBUF and copy it to OFFSET in inferior
   process' address space. The inferior is specified by PID.
   Returns 0 on success or errno on failure and the number of bytes
   on a successful transfer in XFERED_LEN.

   This function assumes internally that the queried process is stopped and
   traced.  */

extern int write_memory (pid_t pid, unsigned const char *writebuf,
			 CORE_ADDR offset, size_t len, size_t *xfered_len);

/* Read inferior process's LEN bytes from OFFSET and copy it to WRITEBUF in
   gdb's address space.
   Returns 0 on success or errno on failure and the number of bytes
   on a successful transfer in XFERED_LEN.

   This function assumes internally that the queried process is stopped and
   traced.  */

extern int read_memory (pid_t pid, unsigned char *readbuf, CORE_ADDR offset,
			size_t len, size_t *xfered_len);
}

#endif
