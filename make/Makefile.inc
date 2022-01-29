# -*- makefile-automake -*-
#
# Makefile.inc
#
# Copyright 2018 Kavalogic, Inc.
#
# This file is part of Tovero.
#
# Tovero is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# Tovero is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.  You should have received a
# copy of the GNU General Public License along with Tovero. If not,
# see <http://www.gnu.org/licenses/>.

#
# Miscellaneous setup
#
TOVERO_LIB_SOURCE_DIR=$(top_srcdir)/src/tovero/library

# Lisp install dir
TOVERO_LISP_INSTALL_DIR=@CFG_LISP_INSTALL_DIR@/tovero
