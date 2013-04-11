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

(defpackage :nestor-view (:use :cl)
  (:export #:*theme*
           #:*render-output*
           #:*javascripts*
           #:*stylesheets*
           #:deftemplate
           #:defstyle
           #:find-view
           #:find-template
           #:css-dispatcher

           ;; cl-who helpers (move to :nestor-templates ???)
           #:display-menu
           #:format-date))

(defpackage :nestor
  (:use :cl #:nestor-view)
  (:export #:start
           ;; configuration
           ;;
           #:heading
           #:subtitle
           ;; bound during the render
           ;;
           #:*page*
           ;; page class and subclasses
           ;;
           #:page
           ;; page accessors
           ;;
           #:date
           #:summary
           #:description
           #:read-more
           #:categories
           #:file
           #:path
           #:title
           #:to-html
           ;; category accessors
           ;;
           #:pages-about
           #:articles-heading

           ;; menu model
           #:menu-items
           ))

(defpackage :nestor-default-theme
  (:use :cl
        #:nestor
        #:nestor-view  :cl-who)
  (:documentation "Holds mostly functions created with DEFTEMPLATE and DEFSTYLE")
  (:export
   :sidebar-categories
   :sidebar))


;;;; package renames
;;;; ---------------

(rename-package :cl-ppcre :cl-ppcre '(:re))
(rename-package :cl-who :cl-who '(:who))
