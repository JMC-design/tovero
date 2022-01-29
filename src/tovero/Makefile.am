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

include $(top_srcdir)/make/Makefile.inc

AUTOMAKE_OPTIONS = subdir-objects

SUBDIRS = library

#
# Tovero Lisp installation
#

TOVERO_LISP_FILES =   \
addons.lisp           \
tovero.lisp           \
csg.lisp              \
extra-shapes.lisp     \
extra-transforms.lisp \
extra-vec.lisp        \
generic.lisp          \
kernel.lisp           \
shapes.lisp           \
transforms.lisp       \
utilities.lisp        \
vec.lisp              \
${NULL}

install-exec-local: install-tovero-package

install-tovero-package:
	mkdir -p ${TOVERO_LISP_INSTALL_DIR}
	cp tovero.asd ${TOVERO_LISP_INSTALL_DIR}
	cp ${TOVERO_LISP_FILES} ${TOVERO_LISP_INSTALL_DIR}

uninstall-local : uninstall-tovero-package

uninstall-tovero-package :
	for i in ${TOVERO_LISP_FILES} ; do        \
	  rm -f ${TOVERO_LISP_INSTALL_DIR}/${i} ; \
        done
	rm -f ${TOVERO_LISP_INSTALL_DIR}/tovero.lisp
	rm -f ${TOVERO_LISP_INSTALL_DIR}/tovero.asd

EXTRA_DIST =         \
${TOVERO_LISP_FILES} \
tovero.asd           \
${NULL}

MAINTAINERCLEANFILES = \
Makefile.in            \
${NULL}
