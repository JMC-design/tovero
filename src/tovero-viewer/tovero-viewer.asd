#| tovero-viewer.asd

Copyright 2018 Kavalogic, Inc.

This file is part of Tovero.

Tovero is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Tovero is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
for more details.  You should have received a copy of the GNU General
Public License along with Tovero. If not, see
<http://www.gnu.org/licenses/>.

|#

(defsystem "tovero-viewer"
    :author "Kavalogic, Inc."
    :version "0.0.0"
    :license "GPL2+"
    :description
    "Tovero Viewer -- lightweight Tovero shape viewer"
    :long-description
    "Tovero Viewer ('tovero-viewer') is a simple Tovero shape viewer with one window."
    :depends-on (:cl-glu :cl-glut :tovero)
    :components ((:file "tovero-viewer/tovero-viewer")
                 (:file "tovero-viewer/camera")
                 (:file "tovero-viewer/shader")
                 (:file "tovero-viewer/viewer")))
