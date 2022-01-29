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

TOVERO_VIEWER_LISP_INSTALL_DIR=${TOVERO_LISP_INSTALL_DIR}/tovero-viewer

install-exec-local : install-tovero-viewer-package

install-tovero-viewer-package :
	mkdir -p ${TOVERO_LISP_INSTALL_DIR}
	cp tovero-viewer.asd ${TOVERO_LISP_INSTALL_DIR}
	mkdir -p ${TOVERO_VIEWER_LISP_INSTALL_DIR}
	cp camera.lisp ${TOVERO_VIEWER_LISP_INSTALL_DIR}
	cp shader.lisp ${TOVERO_VIEWER_LISP_INSTALL_DIR}
	cp viewer.lisp ${TOVERO_VIEWER_LISP_INSTALL_DIR}
	cp tovero-viewer.lisp ${TOVERO_VIEWER_LISP_INSTALL_DIR}

uninstall-local : uninstall-tovero-viewer-package

uninstall-tovero-viewer-package :
	rm -f ${TOVERO_VIEWER_LISP_INSTALL_DIR}/camera.lisp
	rm -f ${TOVERO_VIEWER_LISP_INSTALL_DIR}/shader.lisp
	rm -f ${TOVERO_VIEWER_LISP_INSTALL_DIR}/viewer.lisp
	rm -f ${TOVERO_VIEWER_LISP_INSTALL_DIR}/tovero-viewer.lisp
	rm -f ${TOVERO_LISP_INSTALL_DIR}/tovero-viewer.asd
EXTRA_DIST =       \
camera.lisp        \
shader.lisp        \
tovero-viewer.asd  \
tovero-viewer.lisp \
viewer.lisp \
${NULL}

MAINTAINERCLEANFILES = \
Makefile.in            \
${NULL}
