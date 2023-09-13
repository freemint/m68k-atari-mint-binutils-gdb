#!/bin/bash -eux
# -e: Exit immediately if a command exits with a non-zero status.
# -u: Treat unset variables as an error when substituting.
# -x: Display expanded script commands

unset CDPATH
unset LANG LANGUAGE LC_ALL LC_CTYPE LC_TIME LC_NUMERIC LC_COLLATE LC_MONETARY LC_MESSAGES

VERSIONPATCH=-20230911
REVISION="GNU Binutils for MiNT ${VERSIONPATCH#-}"

TARGET=${CROSS_TOOL:-m68k-atari-mintelf}
if test "$TARGET" = m68k-atari-mintelf; then
REVISION="GNU Binutils for MiNT ELF ${VERSIONPATCH#-}"
fi
PREFIX=/usr

TAR=${TAR-tar}
TAR_OPTS=${TAR_OPTS---owner=0 --group=0}
LN_S=ln
BUILD_EXEEXT=
MAKE=make

here=`pwd`
srcdir=`pwd`

host=linux64
if test -d /usr/lib64 -a $host = linux64; then
	BUILD_LIBDIR=${PREFIX}/lib64
else
	BUILD_LIBDIR=${PREFIX}/lib
fi

enable_plugins=--disable-plugins
enable_lto=--disable-lto

case "${TARGET}" in
    *-*-*elf* | *-*-linux* | *-*-darwin*)
    	enable_lto=--enable-lto
		enable_plugins=--enable-plugins
		;;
esac

MINT_BUILD_DIR=$here/build

rm -rf "$MINT_BUILD_DIR"
mkdir -p "$MINT_BUILD_DIR"

cd "$MINT_BUILD_DIR"

# Note: gdb is explictly disabled here, because it is not needed for the cross-compiler
../configure \
	--target="${TARGET}" \
	--prefix="${PREFIX}" \
	--libdir="$BUILD_LIBDIR" \
	--bindir="${PREFIX}/bin" \
	--libexecdir='${libdir}' \
	--with-pkgversion="$REVISION" \
	--with-bugurl='https://github.com/freemint/m68k-atari-mint-binutils-gdb/issues' \
	--with-gcc --with-gnu-as --with-gnu-ld \
	--disable-gdb --disable-gdbserver --disable-sim \
	--disable-werror \
	--disable-threads \
	--enable-new-dtags \
	--enable-relro \
	--enable-default-hash-style=both \
	$enable_lto \
	$enable_plugins \
	--disable-nls \
	--with-system-zlib \
	--with-system-readline \
	--disable-bracketed-paste-default \
	--with-sysroot="${PREFIX}/${TARGET}/sys-root"

${MAKE} || exit 1

# INSTALL_DIR is set in setup_env.sh
make DESTDIR="$INSTALL_DIR" install-strip >/dev/null || exit 1

mkdir -p "${INSTALL_DIR}/${PREFIX}/${TARGET}/bin"

cd "${INSTALL_DIR}/${PREFIX}/${TARGET}/bin"

for i in addr2line ar as nm ld ld.bfd objcopy objdump ranlib strip readelf dlltool dllwrap size strings; do
	if test -x ../../bin/${TARGET}-$i; then
		rm -f ${i} ${i}${BUILD_EXEEXT}
		$LN_S ../../bin/${TARGET}-$i${BUILD_EXEEXT} $i
	fi
done

cd "${INSTALL_DIR}/${PREFIX}/bin"

rm -f ${TARGET}-ld ${TARGET}-ld${BUILD_EXEEXT}
$LN_S ${TARGET}-ld.bfd${BUILD_EXEEXT} ${TARGET}-ld${BUILD_EXEEXT}
cd "${INSTALL_DIR}" || exit 1

rm -f ${BUILD_LIBDIR#/}/libiberty.a

# do not overwrite the system files
rm -rf ${PREFIX#/}/share/info
rm -rf ${PREFIX#/}/share/man
rm -f ${BUILD_LIBDIR#/}/bfd-plugins/libdep.so
rm -f ${BUILD_LIBDIR#/}/bfd-plugins/*dep.dll
rmdir ${BUILD_LIBDIR#/}/bfd-plugins 2>/dev/null || :
rmdir ${BUILD_LIBDIR#/} 2>/dev/null || :
rm -f ${PREFIX#/}/${TARGET}/lib/ldscripts/m68kmintelf.{xbn,xe,xn,xr,xu}

# remove gdb if it was build; we don't need it for the cross compiler
rm -f ${PREFIX#/}/bin/${TARGET}-gdb*
rm -rf "${PREFIX#/}/share/gdb"
rmdir "${PREFIX#/}/share" || :
rm -rf "${PREFIX#/}/include/gdb"
rm -rf "${PREFIX#/}/include/sim"
rmdir "${PREFIX#/}/include" || :

# archive is created in deploy.sh
# toolsuffix=${TARGET##*-}
# TARNAME=${PROJECT_NAME}-${PROJECT_VERSION}-${toolsuffix}

# ${TAR} ${TAR_OPTS} -jcf ${DEPLOY_DIR}/${TARNAME}-ubuntu-20.04.tar.bz2 ${PREFIX#/}
