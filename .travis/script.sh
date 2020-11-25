#!/bin/bash -eux
# -e: Exit immediately if a command exits with a non-zero status.
# -u: Treat unset variables as an error when substituting.
# -x: Display expanded script commands

./configure --target=m68k-atari-mint --prefix=/usr/m68k-atari-mint --disable-nls --disable-werror --disable-gdb --disable-libdecnumber --disable-readline --disable-sim
make
make install-strip DESTDIR="${INSTALL_DIR}"
