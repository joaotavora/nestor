;;; nestor.lisp --- File-based blogging, cloning the excellent github.com/gma/nesta

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


;; Should really turn this into an .asd
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (progn
    (require 'hunchentoot)
    (require 'cl-who)
    (require 'cl-markdown)
    (require 'metabang-bind)
    (require 'cl-ppcre))
  #+allegro
  (mapc #'(lambda (system)
            #+allegro
            (asdf:oos 'asdf:load-op system)
            #+sbcl
            (require system))
        '(hunchentoot
          cl-who
          cl-markdown
          metabang-bind
          cl-ppcre)))


;;;; Setup
;;;;
(defpackage :nestor (:use :cl)
            (:export #:start))
(in-package :nestor)



;;;; Server start/stop-control
;;;;
(defvar *nestor-acceptor* (make-instance 'hunchentoot:easy-acceptor
                                         :port 9494))

(defun start ()
  (pushnew 'page-dispatcher hunchentoot:*dispatch-table*)
  (setq *nestor-acceptor* (make-instance 'hunchentoot:easy-acceptor
                                         :port 9494))
  (setf (cl-who:html-mode) :sgml)

  (hunchentoot:start *nestor-acceptor*))

(defun stop ()
  (hunchentoot:stop *nestor-acceptor*))


;;;; Pathname/config management
;;;;
(defvar *nestor-lisp-file* (load-time-value
                            (or #.*compile-file-pathname* *load-pathname*)))

(defparameter *content-root* (merge-pathnames (pathname "content-demo/")
                                                     (make-pathname :directory
                                                                    (pathname-directory *nestor-lisp-file*))))

(defun pages-directory ()
  (merge-pathnames (pathname "pages/")
                   *content-root*))


;;;; Handling of page requests
;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *metadata-ids* (make-hash-table :test #'equal)
    "A hashtable of strings to metadata initarg keywords"))

(defmacro defclass* (name direct-superclasses direct-slots &rest options)
  "Also consider :ID options in slots, for adding to *METADATA-IDS*"
  `(defclass ,name ,direct-superclasses
     (,@(mapcar #'(lambda (slot)
                    (setf (gethash (getf (cdr slot) :id) *metadata-ids*)
                          (getf (cdr slot) :initarg))
                    (remf (cdr slot) :id)
                    slot)
                direct-slots))
     ,@options))

(defclass* page ()
  ((categories  :initarg :categories  :accessor categories  :id "Categories")
   (summary     :initarg :summary     :accessor summary     :id "Summary")
   (description :initarg :description :accessor description :id "Description")
   (read-more   :initarg :read-more   :accessor read-more   :id "Read more")
   (date        :initarg :date        :accessor date        :id "Date")
   (atom-id     :initarg :atom-id     :accessor atom-id     :id "Atom ID")
   (flags       :initarg :flags       :accessor flags       :id "Flags")
   (keywords    :initarg :keywords    :accessor keywords    :id "Keywords")
   (layout      :initarg :layout      :accessor layout      :id "Layout")
   (template    :initarg :template    :accessor template    :id "Template")
   ;; and finally
   ;;
   (content     :initarg :content     :accessor content))
  (:documentation "A page, in all it's fieldy glory"))

(defclass* category (page)
  ((name :initarg :name :accessor :category-name)
   (articles-heading :initarg :articles-heading :accessor :category-articles-heading))
  (:documentation "TODO: does this make any sense??? categories are pages??? almost, right? or completely?"))

(defun page-from-string (string)
  (destructuring-bind (content &optional metadata-hunk)
      (reverse (cl-ppcre:split "\\n\\n"
                               string :limit 2))
    (let ((metadata (cl-ppcre:split "\\n" metadata-hunk)))
      (apply #'make-instance 'page :content content
             (mapcan #'(lambda (line)
                         (destructuring-bind (key value)
                             (cl-ppcre:split "[ \t]*:[ \t]*" line :limit 2)
                           (let ((initarg (gethash key *metadata-ids*)))
                             (if initarg
                                 (list initarg value)
                                 (warn "Metadata key ~a has no initarg" key)))))
                     metadata)))))

(defun page-from-file (file)
  (page-from-string (with-open-file (stream file)
                      (let ((data (make-string (file-length stream))))
                        (read-sequence data stream)
                        data))))

(defgeneric render-page (page type)
  (:documentation "Return a string rendering PAGE of TYPE")
  (:method (page (type (eql :mdown)))
    (cl-markdown:render-to-stream (cl-markdown:markdown (content page)) :html nil))
  (:method (page (type (eql :who)))
    (let ((forms (with-input-from-string (stream (content page))
                   (loop for element = (read stream nil #1='#:eof)
                         until (eq element #1#)
                         collect element))))
      (with-output-to-string (bla)
        (eval (cl-who::tree-to-commands forms bla))))))

(defun page-dispatcher (request)
  (let (file)
    (loop for type in '(:mdown :who)
          when (setq file
                     (probe-file
                      (metatilities:relative-pathname (pages-directory)
                                         (pathname
                                          (format nil "~a.~(~a~)"
                                                  (hunchentoot:request-uri request)
                                                  (string type))))))
            return (lambda ()
                     (funcall #'render-page (page-from-file file) type)))))
