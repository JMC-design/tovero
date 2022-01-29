# Makefile.am
#
# Copyright 2018 Kavalogic, Inc.
#
# This file is part of Tovero Library.
#
# Tovero Library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1 of
# the License, or (at your option) any later version.
#
# Tovero Library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.  You should have
# received a copy of the GNU Lesser General Public License along with
# Tovero Library. If not, see <http://www.gnu.org/licenses/>.

include Makefile.inc

AUTOMAKE_OPTIONS = subdir-objects

#
# Tovero to libfive adapter library
#

TOVERO_LIBRARY_VERSION = '0:0:0'
tovero_lib_LTLIBRARIES = libTovero.la

# library source and header files
#   source
TOVERO_LIBRARY_SOURCE_FILES = \
tovero.cpp                    \
${NULL}
libTovero_la_SOURCES = ${TOVERO_LIBRARY_SOURCE_FILES}
#   header
TOVERO_LIBRARY_HEADER_FILES = \
tovero.h                      \
${NULL}

# additional libraries
libTovero_la_LIBADD = \
@CFG_LIBFIVE_LIBS@    \
${NULL}

# compile flags
libTovero_la_CPPFLAGS =                  \
  ${AM_CPPFLAGS}                         \
  -I ${TOVERO_LIB_SOURCE_DIR}            \
${NULL}

libTovero_la_CXXFLAGS = \
${AM_CXXFLAGS} -fPIC    \
@CFG_LIBFIVE_CFLAGS@    \
${NULL}

libTovero_la_LDFLAGS =                     \
  ${AM_LDFLAGS}                            \
  --version-info ${TOVERO_LIBRARY_VERSION} \
${NULL}

# pkg-config
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libTovero-0.pc

EXTRA_DIST =     \
COPYING.LGPLV2_1 \
${NULL}

MAINTAINERCLEANFILES = \
Makefile.in            \
${NULL}
