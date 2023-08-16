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

#include "gdbsupport/common-defs.h"
#include "nat/mintelf-nat.h"

#include <sys/ptrace.h>

#include <cstring>

namespace mintelf_nat
{

/* See mintelf-nat.h.  */

int
write_memory (pid_t pid, unsigned const char *writebuf, CORE_ADDR offset,
	      size_t len, size_t *xfered_len)
{
  int i;
  /* Round starting address down to longword boundary.  */
  CORE_ADDR addr = offset & -(CORE_ADDR) sizeof (int);
  /* Round ending address up; get number of longwords that makes.  */
  int count = (((offset + len) - addr) + sizeof (int) - 1) / sizeof (int);
  /* Allocate buffer of that many longwords.  */
  int *buffer = (int *) alloca (count * sizeof (int));

  /* Fill start and end extra bytes of buffer with existing memory data.  */

  buffer[0] = ptrace (PT_READ_D, pid, (caddr_t) addr, 0);

  if (count > 1)
    {
      buffer[count - 1]
	= ptrace (PT_READ_D, pid,
		  (caddr_t) addr + (count - 1) * sizeof (int), 0);
    }

  /* Copy data to be written over corresponding part of buffer */

  memcpy ((char *) buffer + (offset & (sizeof (int) - 1)), writebuf, len);

  /* Write the entire buffer.  */

  for (i = 0; i < count; i++, addr += sizeof (int))
    {
      errno = 0;
      ptrace (PT_WRITE_D, pid, (caddr_t) addr, buffer[i]);
      if (errno)
	return errno;
    }

  return 0;
}

/* See mintelf-nat.h.  */

int
read_memory (pid_t pid, unsigned char *readbuf, CORE_ADDR offset,
	      size_t len, size_t *xfered_len)
{
  int i;
  /* Round starting address down to longword boundary.  */
  CORE_ADDR addr = offset & -(CORE_ADDR) sizeof (int);
  /* Round ending address up; get number of longwords that makes.  */
  int count = (((offset + len) - addr) + sizeof (int) - 1) / sizeof (int);
  /* Allocate buffer of that many longwords.  */
  int *buffer = (int *) alloca (count * sizeof (int));

  /* Read all the longwords */
  for (i = 0; i < count; i++, addr += sizeof (int))
    {
      buffer[i] = ptrace (PT_READ_D, pid, (caddr_t) addr, 0);
    }

  /* Copy appropriate bytes out of the buffer.  */
  memcpy (readbuf, (char *) buffer + (offset & (sizeof (int) - 1)), len);

  return 0;
}

}
