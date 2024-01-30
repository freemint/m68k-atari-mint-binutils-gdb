# This shell script emits a C file. -*- C -*-
#   Copyright (C) 1998-2023 Free Software Foundation, Inc.
#
# This file is part of the GNU Binutils.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
# MA 02110-1301, USA.
#

# This script is sourced from elf.em, and defines extra MiNT-specific routines.
# It produces a C source file named em68kmintelf.c.

# This is essentially an m68kelf emulation, with a few overrides.
source_em "${srcdir}/emultempl/m68kelf.em"

# We don't know if m68kelf.em or elf.em already provide an implementation for
# the emulation methods. The situation could change in future updates.
# To stay safe, we dynamically guess the name of the super-implementation.
# **CRITICAL** The values below must stay the same as in emulation.em.
SUPER_LDEMUL_BEFORE_PARSE=${LDEMUL_BEFORE_PARSE-gld${EMULATION_NAME}_before_parse}
SUPER_LDEMUL_FINISH=${LDEMUL_FINISH-finish_default}

fragment <<EOF

#include "../bfd/elf32-atariprg.h"

/* Option flags.  */
static uint32_t prg_flags = (_MINT_F_FASTLOAD | _MINT_F_ALTLOAD
			     | _MINT_F_ALTALLOC | _MINT_F_MEMPRIVATE);

/* If override_stack_size is true, then the executable stack size
 * must be overridden with the value of stack_size.  */
static bool override_stack_size = false;
static int32_t stack_size;

/* This method is called before parsing the command line and script file.  */

static void
m68kmintelf_before_parse (void)
{
  /* First, call the base implementation.  */
  ${SUPER_LDEMUL_BEFORE_PARSE} ();

  /* Then add our own linker initialization here.  */

  /* Standard default entry point is the "start" symbol. But the MiNTLib entry
     point is named "_start". We could change the default here, but that would
     be very non-standard. Instead, I prefer to add a custom ENTRY() in the
     linker script.  */
  /*lang_default_entry ("_start");*/
}

/* This method is called after assigning values from the script.  */

static void
m68kmintelf_finish (void)
{
  /* First, call the base implementation.  */
  ${SUPER_LDEMUL_FINISH} ();

  /* Do nothing if we are not generating a MiNT executable (ex: binary).  */
  if (strcmp (bfd_get_target (link_info.output_bfd), "${OUTPUT_FORMAT}") != 0)
    return;

  /* Set the GEMDOS executable header flags.  */
  if (! bfd_elf32_atariprg_set_extended_flags (link_info.output_bfd, prg_flags))
    einfo (_("%F%P: %pB: unable to set the prgflags\n"), link_info.output_bfd);

  /* Override the stack size.  */
  if (override_stack_size)
    if (! bfd_elf32_atariprg_set_stack_size (link_info.output_bfd, stack_size))
      einfo (_("%F%P: %pB: unable to set the stack size\n"), link_info.output_bfd);
}

EOF

# Define some shell vars to insert bits of code into the standard elf
# parse_args and list_options functions.
#
PARSE_AND_LIST_PROLOGUE=$PARSE_AND_LIST_PROLOGUE'
/* Used for setting flags in the MiNT header.  */
enum mintelf_options
{
  OPTION_FASTLOAD = 500,
  OPTION_NO_FASTLOAD,
  OPTION_FASTRAM,
  OPTION_NO_FASTRAM,
  OPTION_FASTALLOC,
  OPTION_NO_FASTALLOC,
  OPTION_BESTFIT,
  OPTION_NO_BESTFIT,
  OPTION_BASEREL,
  OPTION_NO_BASEREL,
  OPTION_MEM_PRIVATE,
  OPTION_MEM_GLOBAL,
  OPTION_MEM_SUPER,
  OPTION_MEM_READONLY,
  OPTION_PRG_FLAGS,
  OPTION_STACK
};
'

PARSE_AND_LIST_LONGOPTS=$PARSE_AND_LIST_LONGOPTS'
  {"mfastload", no_argument, NULL, OPTION_FASTLOAD},
  {"mno-fastload", no_argument, NULL, OPTION_NO_FASTLOAD},
  {"mfastram", no_argument, NULL, OPTION_FASTRAM},
  {"mno-fastram", no_argument, NULL, OPTION_NO_FASTRAM},
  {"maltram", no_argument, NULL, OPTION_FASTRAM},
  {"mno-altram", no_argument, NULL, OPTION_NO_FASTRAM},
  {"mfastalloc", no_argument, NULL, OPTION_FASTALLOC},
  {"mno-fastalloc", no_argument, NULL, OPTION_NO_FASTALLOC},
  {"maltalloc", no_argument, NULL, OPTION_FASTALLOC},
  {"mno-altalloc", no_argument, NULL, OPTION_NO_FASTALLOC},
  {"mbest-fit", no_argument, NULL, OPTION_BESTFIT},
  {"mno-best-fit", no_argument, NULL, OPTION_NO_BESTFIT},
  {"mbaserel", no_argument, NULL, OPTION_BASEREL},
  {"mno-baserel", no_argument, NULL, OPTION_NO_BASEREL},
  {"mshared-text", no_argument, NULL, OPTION_BASEREL},
  {"mno-shared-text", no_argument, NULL, OPTION_NO_BASEREL},
  {"msharable-text", no_argument, NULL, OPTION_BASEREL},
  {"mno-sharable-text", no_argument, NULL, OPTION_NO_BASEREL},
  /* Memory protection bits.  */
  {"mprivate-memory", no_argument, NULL, OPTION_MEM_PRIVATE },
  {"mglobal-memory", no_argument, NULL, OPTION_MEM_GLOBAL},
  {"msuper-memory", no_argument, NULL, OPTION_MEM_SUPER},
  {"mreadable-memory", no_argument, NULL, OPTION_MEM_READONLY},
  {"mreadonly-memory", no_argument, NULL, OPTION_MEM_READONLY},
  {"mprg-flags", required_argument, NULL, OPTION_PRG_FLAGS},
  {"stack", required_argument, NULL, OPTION_STACK},
'

PARSE_AND_LIST_OPTIONS=$PARSE_AND_LIST_OPTIONS'
  fprintf (file, _("  --m[no-]fastload            Enable/Disable not cleaning the heap on startup\n"));
  fprintf (file, _("  --m[no-]altram, --m[no-]fastram\n"));
  fprintf (file, _("                              Enable/Disable loading into alternate RAM\n"));
  fprintf (file, _("  --m[no-]altalloc, --m[no-]fastalloc\n"));
  fprintf (file, _("                              Enable/Disable malloc from alternate RAM\n"));
  fprintf (file, _("  --m[no-]best-fit            Enable/Disable loading with optimal heap size\n"));
  fprintf (file, _("  --m[no-]sharable-text, --m[no-]shared-text, --m[no-]baserel\n"));
  fprintf (file, _("                              Enable/Disable sharing the text segment\n"));
  fprintf (file, "\n");
  fprintf (file, _("The following memory options are mutually exclusive:\n"));
  fprintf (file, _("  --mprivate-memory           Process memory is not accessible\n"));
  fprintf (file, _("  --mglobal-memory            Process memory is readable and writable\n"));
  fprintf (file, _("  --msuper-memory             Process memory is accessible in supervisor mode\n"));
  fprintf (file, _("  --mreadonly-memory, --mreadable-memory\n"));
  fprintf (file, _("                              Process memory is readable but not writable\n"));
  fprintf (file, "\n");
  fprintf (file, _("  --mprg-flags <value>        Set all the flags with an integer raw value\n"));
  fprintf (file, _("  --stack <size>              Override the stack size (suffix k or M allowed)\n"));
'

PARSE_AND_LIST_ARGS_CASES=$PARSE_AND_LIST_ARGS_CASES'
    case OPTION_FASTLOAD:
      prg_flags |= _MINT_F_FASTLOAD;
      break;

    case OPTION_NO_FASTLOAD:
      prg_flags &= ~_MINT_F_FASTLOAD;
      break;

    case OPTION_FASTRAM:
      prg_flags |= _MINT_F_ALTLOAD;
      break;

    case OPTION_NO_FASTRAM:
      prg_flags &= ~_MINT_F_ALTLOAD;
      break;

    case OPTION_FASTALLOC:
      prg_flags |= _MINT_F_ALTALLOC;
      break;

    case OPTION_NO_FASTALLOC:
      prg_flags &= ~_MINT_F_ALTALLOC;
      break;

    case OPTION_BESTFIT:
      prg_flags |= _MINT_F_BESTFIT;
      break;

    case OPTION_NO_BESTFIT:
      prg_flags &= ~_MINT_F_BESTFIT;
      break;

    case OPTION_BASEREL:
      prg_flags |= _MINT_F_SHTEXT;
      break;

    case OPTION_NO_BASEREL:
      prg_flags &= ~_MINT_F_SHTEXT;
      break;

    case OPTION_MEM_PRIVATE:
      prg_flags &= ~_MINT_F_MEMPROTECTION;
      break;

    case OPTION_MEM_GLOBAL:
      prg_flags &= ~_MINT_F_MEMPROTECTION;
      prg_flags |= _MINT_F_MEMPRIVATE;
      break;

    case OPTION_MEM_SUPER:
      prg_flags &= ~_MINT_F_MEMPROTECTION;
      prg_flags |= _MINT_F_MEMSUPER;
      break;

    case OPTION_MEM_READONLY:
      prg_flags &= ~_MINT_F_MEMPROTECTION;
      prg_flags |= _MINT_F_MEMREADABLE;
      break;

    case OPTION_PRG_FLAGS:
      {
	char* tail;
	unsigned long flag_value = strtoul (optarg, &tail, 0);

	if (*tail != '\''\0'\'')
	  einfo (_("%P: warning: ignoring invalid prgflags %s\n"), optarg);
	else
	  prg_flags = flag_value;

	break;
      }
    case OPTION_STACK:
      {
	char* tail;
	long size = strtol (optarg, &tail, 0);

	if (*tail == '\''K'\'' || *tail == '\''k'\'')
	  {
	    size *= 1024;
	    ++tail;
	  }
	else if (*tail == '\''M'\'' || *tail == '\''m'\'')
	  {
	    size *= 1024*1024;
	    ++tail;
	  }

	if (*tail != '\''\0'\'')
	  einfo (_("%P: warning: ignoring invalid stack size %s\n"), optarg);
	else
	  {
	    stack_size = size;
	    override_stack_size = true;
	  }

	break;
      }
'

# Override emulation methods
LDEMUL_BEFORE_PARSE=m68kmintelf_before_parse
LDEMUL_FINISH=m68kmintelf_finish
