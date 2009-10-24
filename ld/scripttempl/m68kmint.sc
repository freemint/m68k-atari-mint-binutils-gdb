cat <<EOF
${RELOCATING+OUTPUT_FORMAT(${OUTPUT_FORMAT})}
${RELOCATING-OUTPUT_FORMAT(${RELOCATEABLE_OUTPUT_FORMAT})}
${RELOCATING+${LIB_SEARCH_DIRS}}
SECTIONS
{
  ${RELOCATING+/* The VMA of the .text section is ${TEXT_START_ADDR} instead of 0
     because the extended MiNT header is just before,
     at the beginning of the TEXT segment.  */}
  .text ${RELOCATING+${TEXT_START_ADDR}}:
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
}
EOF
