# Makefile.am
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

# specify not GNU
AUTOMAKE_OPTIONS = foreign

SUBDIRS =         \
scripts           \
src/tovero        \
src/tovero-viewer \
${NULL}

# Note: can use DIST_SUBDIRS to make sure optionally made directories are
# built

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST =  \
bootstrap     \
COPYING       \
COPYING.GPLV2 \
CREDITS       \
README.md     \
${NULL}

MAINTAINERCLEANFILES = \
Makefile.in            \
${NULL}
