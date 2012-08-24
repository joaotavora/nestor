;;; package.lisp --- Nestor package setup

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

(defpackage :nestor-layout (:use :cl :cl-who)
  (:export #:*layout*
           #:find-layout
           #:*javascripts*
           #:*stylesheets*))

(defpackage :nestor (:use :cl
                          #:nestor-layout)
            (:export #:start))
