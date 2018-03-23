# This shell script emits a C file. -*- C -*-
#   Copyright (C) 2006-2017 Free Software Foundation, Inc.
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

# This file is sourced from generic.em
#
fragment <<EOF

#include "getopt.h"
#include "ldgram.h"

/* Standard GEMDOS program flags.  */
#define _MINT_F_FASTLOAD      0x01    /* Don't clear heap.  */
#define _MINT_F_ALTLOAD       0x02    /* OK to load in alternate RAM.  */
#define _MINT_F_ALTALLOC      0x04    /* OK to malloc from alt. RAM.  */
#define _MINT_F_BESTFIT       0x08    /* Load with optimal heap size.  */
/* The memory flags are mutually exclusive.  */
#define _MINT_F_MEMPROTECTION 0xf0    /* Masks out protection bits.  */
#define _MINT_F_MEMPRIVATE    0x00    /* Memory is private.  */
#define _MINT_F_MEMGLOBAL     0x10    /* Read/write access to mem allowed.  */
#define _MINT_F_MEMSUPER      0x20    /* Only supervisor access allowed.  */
#define _MINT_F_MEMREADABLE   0x30    /* Any read access OK.  */
#define _MINT_F_SHTEXT        0x800   /* Program's text may be shared */

/* Option flags.  */
static flagword prg_flags = (_MINT_F_FASTLOAD | _MINT_F_ALTLOAD
			     | _MINT_F_ALTALLOC | _MINT_F_MEMPRIVATE);

/* If override_stack_size is TRUE, then the executable stack size
 * must be overriden with the value of stack_size.  */
static bfd_boolean override_stack_size = FALSE;
static bfd_signed_vma stack_size;

/* MiNT format extra command line options.  */

/* Used for setting flags in the MiNT header.  */
#define OPTION_FASTLOAD (300)
#define OPTION_NO_FASTLOAD (OPTION_FASTLOAD + 1)
#define OPTION_FASTRAM (OPTION_NO_FASTLOAD + 1)
#define OPTION_NO_FASTRAM (OPTION_FASTRAM + 1)
#define OPTION_FASTALLOC (OPTION_NO_FASTRAM + 1)
#define OPTION_NO_FASTALLOC (OPTION_FASTALLOC + 1)
#define OPTION_BESTFIT (OPTION_NO_FASTALLOC + 1)
#define OPTION_NO_BESTFIT (OPTION_BESTFIT + 1)
#define OPTION_BASEREL (OPTION_NO_BESTFIT + 1)
#define OPTION_NO_BASEREL (OPTION_BASEREL + 1)
#define OPTION_MEM_PRIVATE (OPTION_NO_BASEREL + 1)
#define OPTION_MEM_GLOBAL (OPTION_MEM_PRIVATE + 1)
#define OPTION_MEM_SUPER (OPTION_MEM_GLOBAL + 1)
#define OPTION_MEM_READONLY (OPTION_MEM_SUPER + 1)
#define OPTION_PRG_FLAGS (OPTION_MEM_READONLY + 1)
#define OPTION_STACK (OPTION_PRG_FLAGS + 1)

static void
gld${EMULATION_NAME}_add_options
  (int ns ATTRIBUTE_UNUSED, char **shortopts ATTRIBUTE_UNUSED, int nl,
    struct option **longopts, int nrl ATTRIBUTE_UNUSED,
    struct option **really_longopts ATTRIBUTE_UNUSED)
{
  static const struct option xtra_long[] = {
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
    {NULL, no_argument, NULL, 0}
  };

  *longopts = (struct option *)
    xrealloc (*longopts, nl * sizeof (struct option) + sizeof (xtra_long));
  memcpy (*longopts + nl, &xtra_long, sizeof (xtra_long));
}

static bfd_boolean
gld${EMULATION_NAME}_handle_option (int optc)
{
  switch (optc)
    {
    default:
      return FALSE;

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

	if (*tail != '\0')
	  einfo (_("%P: warning: ignoring invalid program flags %s\n"), optarg);
	else
	  prg_flags = flag_value;

	break;
      }
    case OPTION_STACK:
      {
	char* tail;
	long size = strtol (optarg, &tail, 0);

	if (*tail == 'K' || *tail == 'k')
	  {
	    size *= 1024;
	    ++tail;
	  }
	else if (*tail == 'M' || *tail == 'm')
	  {
	    size *= 1024*1024;
	    ++tail;
	  }

	if (*tail != '\0')
	  einfo (_("%P: warning: ignoring invalid stack size %s\n"), optarg);
	else
	{
	  stack_size = (bfd_signed_vma) size;
	  override_stack_size = TRUE;
	}

	break;
      }
    }
  return TRUE;
}

/* This callback is called when ld is invoked
   with the --help and --target-help options.  */

static void
gld_${EMULATION_NAME}_list_options (FILE *file)
{
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
}

/* This callback is called by lang_for_each_statement. It checks that the
   output sections speficied in the linker script are compatible with the MiNT
   executable format.  */

static void
gld${EMULATION_NAME}_check_output_sections (lang_statement_union_type *s)
{
  if (s->header.type == lang_output_section_statement_enum)
    {
      lang_output_section_statement_type *oss = &s->output_section_statement;

      if (strcmp(oss->name, ".text") == 0 && oss->bfd_section->vma != ${TEXT_START_ADDR})
	einfo (_("%F%P: the VMA of section %A must be 0x%V, but actual value is 0x%V\n"),
	  oss->bfd_section, ${TEXT_START_ADDR}, oss->bfd_section->vma);
      else if (strcmp(oss->name, ".data") == 0 && oss->addr_tree != NULL)
	einfo (_("%F%P: the VMA of section %A must not be specified\n"),
	  oss->bfd_section);
      else if (strcmp(oss->name, ".bss") == 0 && oss->addr_tree != NULL)
	einfo (_("%F%P: the VMA of section %A must not be specified\n"),
	  oss->bfd_section);
    }
}

/* This callback is called by lang_for_each_statement. It looks for the data
   statements of type REL generated by the linker, and adds a TPA relocation
   entry for them. This is used by the CONSTRUCTORS list.  */

static void
gld${EMULATION_NAME}_add_tpa_relocs (lang_statement_union_type *s)
{
  if (s->header.type == lang_data_statement_enum)
    {
      lang_data_statement_type *ds = &s->data_statement;

      if (ds->exp->type.node_code == REL)
	{
	  if (ds->type == LONG)
	    {
	      bfd_vma tpa_address = ds->output_section->vma + ds->output_offset;
	      if (!bfd_m68kmint_add_tpa_relocation_entry(link_info.output_bfd, tpa_address))
		einfo (_("%F%P:%B: unable to add a relocation entry\n"), link_info.output_bfd);
	    }
	    else
	    {
	      einfo (_("%F%P:%B: invalid size for TPA relocation entry in section %A, offset 0x%V\n"),
		link_info.output_bfd, ds->output_section, ds->output_offset);
	    }
	}
    }
}

/* Final emulation specific call.  */

static void
gld${EMULATION_NAME}_finish (void)
{
  /* Do nothing if we are not generating a MiNT executable (ex: binary).  */
  if (strcmp (bfd_get_target (link_info.output_bfd), "${OUTPUT_FORMAT}") != 0)
    return;

  /* Check the output sections.  */
  lang_for_each_statement (gld${EMULATION_NAME}_check_output_sections);

  /* Set the GEMDOS executable header flags.  */
  if (!bfd_m68kmint_set_extended_flags (link_info.output_bfd, prg_flags))
    einfo (_("%F%P:%B: unable to set the header flags\n"), link_info.output_bfd);

  /* Override the stack size.  */
  if (override_stack_size)
    if (!bfd_m68kmint_set_stack_size (link_info.output_bfd, stack_size))
      einfo (_("%F%P:%B: unable to set the stack size\n"), link_info.output_bfd);

  /* Generate TPA relocation entries for the data statements.  */
  lang_for_each_statement (gld${EMULATION_NAME}_add_tpa_relocs);
}

EOF

# Put these extra routines in ld_${EMULATION_NAME}_emulation
#
LDEMUL_ADD_OPTIONS=gld${EMULATION_NAME}_add_options
LDEMUL_HANDLE_OPTION=gld${EMULATION_NAME}_handle_option
LDEMUL_LIST_OPTIONS=gld_${EMULATION_NAME}_list_options
LDEMUL_FINISH=gld${EMULATION_NAME}_finish
