cat <<EOF
OUTPUT_FORMAT("${OUTPUT_FORMAT}")
${RELOCATING+${LIB_SEARCH_DIRS}}
SECTIONS
{
  /* The VMA of the .text segment is ${TEXT_START_ADDR} instead of 0
     because the extended MiNT header is just before.  */
  .text ${TEXT_START_ADDR} :
  {
    CREATE_OBJECT_SYMBOLS
    *(.text)
    ${CONSTRUCTING+CONSTRUCTORS}
    ${RELOCATING+_etext = .;}
    ${RELOCATING+__etext = .;}
  }
  .data :
  {
    *(.data)
    ${RELOCATING+_edata = .;}
    ${RELOCATING+__edata = .;}
  }
  .bss :
  {
    ${RELOCATING+__bss_start = .;}
    *(.bss)
    *(COMMON)
    ${RELOCATING+_end = .;}
    ${RELOCATING+__end = .;}
  }

  /* Unfortunately, stabs are not mappable from ELF to a.out.
     It can probably be fixed with some amount of work.  */
  /DISCARD/ :
  { *(.stab) *(.stab*) *(.debug) *(.debug*) *(.comment) *(.gnu.warning.*) }

}
EOF
