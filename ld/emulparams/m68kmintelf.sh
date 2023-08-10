# Customizer script for m68kmintelf emulation.
# It is sourced by genscripts.sh to customize the templates.

# This is essentially an m68kelf emulation, with a few overrides.
source_sh ${srcdir}/emulparams/m68kelf.sh

# The linker will produce PRG/ELF executables, not plain ELF.
OUTPUT_FORMAT="elf32-atariprg"

# Emulation template. Suffix ".em" will be appended.
TEMPLATE_NAME=elf
EXTRA_EM_FILE=m68kmintelf

# Linker script template. Suffix ".sc" will be appended.
SCRIPT_NAME=m68kmintelf

# Additional parameters for above templates.
GENERATE_SHLIB_SCRIPT=
GENERATE_PIE_SCRIPT=

# Use external linker script files.
COMPILE_IN=no
