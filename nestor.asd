;;; nestor.asd --- Nestor ASDF system setup

;;; Copyright (C) 2012 Joao Tavora

;;; Author: Joao Tavora <joaotavora@gmail.com>
;;; Keywords:

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(in-package :cl-user)

(defvar *nestor-version* "0.1"
  "A string denoting the current version of Nestor.  Used
for diagnostic output.")

(asdf:defsystem :nestor
  :serial t
  :version #.*nestor-version*
  :description "Nestor is a file-based blogging engine modelled after
  the excellent ruby Nesta"
  :depends-on (:hunchentoot
               :cl-who
               :cl-markdown
               :cl-fad
               :metabang-bind
               :cl-ppcre)
  :components ((:file "package")
               (:file "nestor")
               (:file "styles")))
