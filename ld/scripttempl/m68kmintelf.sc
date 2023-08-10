cat <<EOF
/* Linker script for Atari ST PRG/ELF executables.
   Written by Vincent Riviere, 2023.
   Based on elf.sc and the generated m68kelf.x.  */

/* Copyright (C) 2014-2023 Free Software Foundation, Inc.

   Copying and distribution of this script, with or without modification,
   are permitted in any medium without royalty provided the copyright
   notice and this notice are preserved.  */

OUTPUT_FORMAT("${OUTPUT_FORMAT}")
ENTRY(__start) /* The MiNTLib uses this entry symbol, so we do.  */

/* ELF Program Headers are mapped to GEMDOS process segments.  */
PHDRS
{
  /* TEXT segment starts with PRG extended header + ELF headers.  */
  TEXT PT_LOAD FILEHDR PHDRS FLAGS (5); /* PF_X */
  DATA PT_LOAD FLAGS (6); /* PF_R | PF_W */
  BSS  PT_LOAD FLAGS (6); /* PF_R | PF_W */
}

/* Sections are assigned to segments managed by the operating system.
   There are 4 possible assignments:
     :TEXT for program code and read-only data
     :DATA for read-write data with initial value
     :BSS  for read-write data initialized to 0, not stored in the executable
     :NONE for extra sections not loaded by the operating system
   The extra sections a stored in the PRG symbols table. They can be used to
   store additional data inside executables, such as debugging information.
   Extra sections take space in executable files, but aren't automatically
   loaded into the process memory.
   If an output section is not explicitly assigned to a segment, it will be
   assigned to the segment used by the previous section.  */
SECTIONS
{
  /*** TEXT segment: program code and read-only data **************************/

  /* Skip PRG extra header, ELF File Header, ELF Program Headers.  */
  . = SIZEOF_HEADERS;

  /* Program code.  */
  .text :
  {
    PROVIDE(__start = .); /* Default entry point if __start isn't defined.  */
    *crt0.o(.text .text.*)
    *(.text.unlikely .text.*_unlikely .text.unlikely.*)
    *(.text.exit .text.exit.*)
    *(.text.startup .text.startup.*)
    *(.text.hot .text.hot.*)
    *(SORT(.text.sorted.*))
    *(.text .stub .text.* .gnu.linkonce.t.*)
    /* .gnu.warning sections are handled specially by elf.em.  */
    *(.gnu.warning)
  } :TEXT =0x4afc /* Pad with ILLEGAL instruction */

  /* End of .text section.  */
  __etext = .;
  PROVIDE(_etext = .);

  /* Global Constructors.  */
  .ctors (READONLY) : ALIGN(2)
  {
    ___CTOR_LIST__ = .;
    LONG((___CTOR_END__ - ___CTOR_LIST__) / 4 - 2)
    KEEP (*(.ctors))
    KEEP (*(SORT(.ctors.*)))
    LONG(0)
    ___CTOR_END__ = .;
  }

  /* Global Destructors.  */
  .dtors (READONLY) : ALIGN(2)
  {
    ___DTOR_LIST__ = .;
    LONG((___DTOR_END__ - ___DTOR_LIST__) / 4 - 2)
    KEEP (*(.dtors))
    KEEP (*(SORT(.dtors.*)))
    LONG(0)
    ___DTOR_END__ = .;
  }

  /* Read-only data.  */
  .rodata :
  {
    *(.rodata .rodata.* .gnu.linkonce.r.*)
  }

  /* Exception handling.  */
  .eh_frame_hdr   : { *(.eh_frame_hdr) *(.eh_frame_entry .eh_frame_entry.*) }
  .eh_frame       : ONLY_IF_RO { KEEP (*(.eh_frame)) *(.eh_frame.*) }
  .sframe         : ONLY_IF_RO { *(.sframe) *(.sframe.*) }
  .gcc_except_table   : ONLY_IF_RO { *(.gcc_except_table .gcc_except_table.*) }
  .gnu_extab   : ONLY_IF_RO { *(.gnu_extab*) }

  /*** DATA segment: read-write data with initial value ***********************/

  /* Standard data.  */
  .data : ALIGN(2)
  {
    *(.data .data.* .gnu.linkonce.d.*)
  } :DATA

  /* End of .data section. */
  __edata = .;
  PROVIDE(_edata = .);

  /* Exception handling. */
  .eh_frame       : ONLY_IF_RW { KEEP (*(.eh_frame)) *(.eh_frame.*) }
  .sframe         : ONLY_IF_RW { *(.sframe) *(.sframe.*) }
  .gnu_extab      : ONLY_IF_RW { *(.gnu_extab) }
  .gcc_except_table   : ONLY_IF_RW { *(.gcc_except_table .gcc_except_table.*) }
  .exception_ranges   : ONLY_IF_RW { *(.exception_ranges*) }

  /*** BSS segment: read-write data initialized to 0 **************************/

  /* Standard BSS.  */
  .bss : ALIGN(2)
  {
   *(.bss .bss.* .gnu.linkonce.b.*)
   *(COMMON)
  } :BSS

  /* End of .bss section */
  __end = .;
  PROVIDE(_end = .);

  /*** Extra sections not loaded by the operating system **********************/

  .comment       0 : { *(.comment) } :NONE
  .gnu.build.attributes : { *(.gnu.build.attributes .gnu.build.attributes.*) }
  .note.gnu.build-id  : { *(.note.gnu.build-id) }

   /* ELF relocation information.  */
  .rela.init      : { *(.rela.init) }
  .rela.text      : { *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*) }
  .rela.fini      : { *(.rela.fini) }
  .rela.rodata    : { *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*) }
  .rela.data.rel.ro   : { *(.rela.data.rel.ro .rela.data.rel.ro.* .rela.gnu.linkonce.d.rel.ro.*) }
  .rela.data      : { *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*) }
  .rela.tdata	  : { *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*) }
  .rela.tbss	  : { *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*) }
  .rela.ctors     : { *(.rela.ctors) }
  .rela.dtors     : { *(.rela.dtors) }
  .rela.got       : { *(.rela.got) }
  .rela.bss       : { *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*) }
EOF

# DWARF debug sections
source_sh ${srcdir}/scripttempl/DWARF.sc

cat <<EOF

  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }

  /* Input sections below won't be merged into the PRG.  */
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
}
EOF
