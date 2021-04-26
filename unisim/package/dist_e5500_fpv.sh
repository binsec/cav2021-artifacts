#!/bin/bash
SIMPKG=e5500fpv
SIMPKG_SRCDIR=cxx/e5500fpv
SIMPKG_DSTDIR=e5500fpv
source "$(dirname $0)/dist_common.sh"

import_genisslib || exit

import unisim/kernel/logger/console || exit
import unisim/component/cxx/processor/powerpc
import unisim/component/cxx/memory/sparse
import unisim/util/reg/core
import unisim/util/random
import unisim/util/os/linux_os
import unisim/service/interfaces/memory
import unisim/service/interfaces/memory_access_reporting
import unisim/service/interfaces/memory_injection
import unisim/service/interfaces/registers
import unisim/service/interfaces/linux_os
import unisim/service/interfaces/debug_yielding
import unisim/service/interfaces/disassembly
import unisim/service/interfaces/trap_reporting
import unisim/service/interfaces/registers
import unisim/service/interfaces/blob
import unisim/service/debug/debugger
import unisim/service/debug/inline_debugger
import unisim/service/debug/gdb_server

import unisim/component/cxx/processor/powerpc/isa/book_i/branch
import unisim/component/cxx/processor/powerpc/isa/book_i/cond_reg
import unisim/component/cxx/processor/powerpc/isa/book_i/fixed_point
import unisim/component/cxx/processor/powerpc/isa/book_i/floating_point
import unisim/component/cxx/processor/powerpc/isa/book_i/legacy_int_mac
import unisim/component/cxx/processor/powerpc/isa/book_i/legacy_move_assist
import unisim/component/cxx/processor/powerpc/isa/book_i/syscall
import unisim/component/cxx/processor/powerpc/isa/book_ii
import unisim/component/cxx/processor/powerpc/isa/book_iii_e
import unisim/component/cxx/processor/powerpc/isa/book_e

import libc/inttypes
import std/iostream
import std/sstream
import std/string
import std/vector
import std/map
import std/stdexcept

import m4/ax_cflags_warn_all || exit

copy source isa header template data
copy m4 && has_to_build_simulator_configure=yes # Some imported files (m4 macros) impact configure generation

UNISIM_LIB_SIMULATOR_SOURCE_FILES="$(files source)"

UNISIM_LIB_SIMULATOR_ISA_FILES="$(files isa)"

UNISIM_LIB_SIMULATOR_HEADER_FILES="${UNISIM_LIB_SIMULATOR_ISA_FILES} $(files header) $(files template)"

UNISIM_LIB_SIMULATOR_M4_FILES="$(files m4)"

UNISIM_LIB_SIMULATOR_DATA_FILES="$(files data)"

UNISIM_SIMULATOR_ISA_FILES="\
top_ppc64.isa \
"

UNISIM_SIMULATOR_SOURCE_FILES="\
main.cc \
linuxsystem.cc \
debugger.cc \
core.cc \
arch.cc \
simfloat.cc \
"

UNISIM_SIMULATOR_HEADER_FILES="\
${UNISIM_SIMULATOR_ISA_FILES} \
linuxsystem.hh \
debugger.hh \
core.hh \
arch.hh \
simfloat.hh \
types.hh \
"

UNISIM_SIMULATOR_PKG_DATA_FILES="\
COPYING \
NEWS \
ChangeLog \
"

UNISIM_SIMULATOR_DATA_FILES="\
COPYING \
INSTALL \
NEWS \
README \
AUTHORS \
ChangeLog \
"

UNISIM_SIMULATOR_FILES=" \
${UNISIM_SIMULATOR_SOURCE_FILES} \
${UNISIM_SIMULATOR_HEADER_FILES} \
${UNISIM_SIMULATOR_DATA_FILES} \
"

for file in ${UNISIM_SIMULATOR_FILES}; do
	dist_copy "${UNISIM_SIMULATOR_DIR}/${file}" "${DEST_DIR}/${SIMPKG_DSTDIR}/${file}"
done

for file in ${UNISIM_SIMULATOR_PKG_DATA_FILES}; do
	dist_copy "${UNISIM_SIMULATOR_DIR}/${file}" "${DEST_DIR}/${file}"
done

# Top level

cat << EOF > "${DEST_DIR}/AUTHORS"
Yves Lhuillier <yves.lhuillier@cea.fr>
EOF

cat << EOF > "${DEST_DIR}/README"
This package contains:
  - e5500_fpv: a QorIQ e5500 (PowerPC64) simulator with floating point instrumentation
  - UNISIM GenISSLib (will not be installed): an instruction set simulator generator

See INSTALL for installation instructions.
EOF

cat << EOF > "${DEST_DIR}/INSTALL"
INSTALLATION
------------

Requirements:
  - GNU C++ compiler
  - GNU C++ standard library
  - GNU bash
  - GNU make
  - GNU autoconf
  - GNU automake
  - GNU flex
  - GNU bison
  - libxml2 (http://xmlsoft.org/libxml2) development package (libxml2-devel for Redhat/Mandriva, libxml2-dev for Debian/Ubuntu)


Building instructions:
  $ ./configure
  $ make

Installing (optional):
  $ make install
EOF

output_top_configure_ac <(cat << EOF
AC_INIT([UNISIM FPV e5500 Floating-Point verification package], [${SIMULATOR_VERSION}], [Yves Lhuillier <yves.lhuillier@cea.fr>], [unisim-${SIMPKG}])
AC_CONFIG_AUX_DIR(config)
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([subdir-objects tar-pax])
AC_PATH_PROGS(SH, sh)
AC_PROG_INSTALL
AC_PROG_LN_S
AC_CONFIG_SUBDIRS([genisslib]) 
AC_CONFIG_SUBDIRS([${SIMPKG_DSTDIR}]) 
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
EOF
)

output_top_makefile_am <(cat << EOF
SUBDIRS=genisslib ${SIMPKG_DSTDIR}
EXTRA_DIST = configure.cross
EOF
)

build_top_configure
build_top_configure_cross

# Simulator

output_simulator_configure_ac <(cat << EOF
AC_INIT([UNISIM FPV e5500 Floating-Point verification], [${SIMULATOR_VERSION}], [Yves Lhuillier <yves.lhuillier@cea.fr>], [unisim-${SIMPKG}-core])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADERS([config.h])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([subdir-objects tar-pax])
AC_PATH_PROGS(SH, sh)
AC_PROG_CXX
AC_PROG_INSTALL
LT_INIT
AC_SUBST(LIBTOOL_DEPS)
AC_PROG_LN_S
AC_LANG([C++])
AM_PROG_CC_C_O
case "\${host}" in
	*mingw*)
		CPPFLAGS="-U__STRICT_ANSI__ \${CPPFLAGS}"
		;;
	*)
		;;
esac
$(lines ac)
GENISSLIB_PATH=\$(pwd)/../genisslib/genisslib
AC_SUBST(GENISSLIB_PATH)
AC_DEFINE([BIN_TO_SHARED_DATA_PATH], ["../share/unisim-${SIMPKG}-${SIMULATOR_VERSION}"], [path of shared data relative to bin directory])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
EOF
)

output_simulator_makefile_am <(cat << EOF
ACLOCAL_AMFLAGS=-I m4
AM_CPPFLAGS=-I\$(top_srcdir) -I\$(top_builddir)
LIBTOOL_DEPS = @LIBTOOL_DEPS@
libtool: \$(LIBTOOL_DEPS)
	\$(SHELL) ./config.status libtool

# Program
bin_PROGRAMS = unisim-${SIMPKG}-${SIMULATOR_VERSION}
unisim_${AM_SIMPKG}_${AM_SIMULATOR_VERSION}_SOURCES = ${UNISIM_SIMULATOR_SOURCE_FILES}
unisim_${AM_SIMPKG}_${AM_SIMULATOR_VERSION}_LDFLAGS = -static-libtool-libs
unisim_${AM_SIMPKG}_${AM_SIMULATOR_VERSION}_LDADD = libunisim-${SIMPKG}-${SIMULATOR_VERSION}.la

# Static Library
noinst_LTLIBRARIES = libunisim-${SIMPKG}-${SIMULATOR_VERSION}.la
libunisim_${AM_SIMPKG}_${AM_SIMULATOR_VERSION}_la_SOURCES = ${UNISIM_LIB_SIMULATOR_SOURCE_FILES}
libunisim_${AM_SIMPKG}_${AM_SIMULATOR_VERSION}_la_LDFLAGS = -static
nodist_libunisim_${AM_SIMPKG}_${AM_SIMULATOR_VERSION}_la_SOURCES = top_ppc64.cc

noinst_HEADERS = ${UNISIM_LIB_SIMULATOR_HEADER_FILES} ${UNISIM_SIMULATOR_HEADER_FILES}
EXTRA_DIST = ${UNISIM_LIB_SIMULATOR_M4_FILES}
sharedir = \$(prefix)/share/unisim-${SIMPKG}-${SIMULATOR_VERSION}
dist_share_DATA = ${UNISIM_SIMULATOR_DATA_FILES}
nobase_dist_share_DATA = ${UNISIM_LIB_SIMULATOR_DATA_FILES}

BUILT_SOURCES=\
	\$(top_builddir)/top_ppc64.hh\
	\$(top_builddir)/top_ppc64.cc\

CLEANFILES=\
	\$(top_builddir)/top_ppc64.hh\
	\$(top_builddir)/top_ppc64.cc\

\$(top_builddir)/top_ppc64.cc: \$(top_builddir)/top_ppc64.hh
\$(top_builddir)/top_ppc64.hh: ${UNISIM_SIMULATOR_ISA_FILES} ${UNISIM_LIB_SIMULATOR_ISA_FILES}
	\$(GENISSLIB_PATH) -o \$(top_builddir)/top_ppc64 -w 8 -I \$(top_srcdir) \$(top_srcdir)/top_ppc64.isa

EOF
)

build_simulator_configure

echo "Distribution is up-to-date"

