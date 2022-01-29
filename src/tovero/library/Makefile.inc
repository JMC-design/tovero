# -*- makefile-automake -*-
#
# Makefile.inc
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

#
# Debug options
#
if CFG_COND_DEBUG
#  debug
FLAG_OPTIMIZE = -g
FLAG_DEF_NDEBUG =
FLAG_STRIP_SYMBOLS =
else
#  non-debug
FLAG_OPTIMIZE = -O2
FLAG_DEF_NDEBUG = -DNDEBUG
FLAG_STRIP_SYMBOLS = -s
endif

#
# Setup the automake compiler flags
#
#   Preprocessor flags
AM_CPPFLAGS = ${FLAG_DEF_NDEBUG}
#   C++ flags
AM_CXXFLAGS = -fmessage-length=0    \
              -pthread              \
              -fno-nonansi-builtins \
              ${FLAG_OPTIMIZE}      \
              -Wall                 \
              -std=c++11

#   Linker flags
AM_LDFLAGS = ${FLAG_STRIP_SYMBOLS}

LIBS = @LIBS@

#
# Miscellaneous setup
#
TOVERO_LIB_SOURCE_DIR=$(top_srcdir)/src/tovero/library

#
# Distribution directories
#
TOVERO_LIB_BASE_DIST_INCLUDE_DIR = $(includedir)@CFG_TOVERO_LIBRARY_PARALLEL@
tovero_libdir = $(libdir)@CFG_TOVERO_LIBRARY_PARALLEL@
