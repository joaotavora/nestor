;;; -*-mode: lisp; coding: utf-8;-*-
;;; nestor.lisp --- File-based blogging, a clone of the excellent ruby Nesta

;;; Copyright (C) 2012 João Tavora

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

(in-package :nestor)


;;;; Server start/stop-control
;;;;
(defvar *nestor-acceptor* (make-instance 'hunchentoot:easy-acceptor
                                         :port 9494))

(defun start ()
  (setf (cl-who:html-mode) :sgml)
  (setq hunchentoot:*dispatch-table* (list 'nestor-view:css-dispatcher
                                           'page-dispatcher))
  (hunchentoot:start *nestor-acceptor*))

(defun stop ()
  (hunchentoot:stop *nestor-acceptor*))


;;;; Pathname/config management
;;;;
(defparameter *content-root* (cl-fad:pathname-as-directory (asdf:system-relative-pathname :nestor "content-demo/")))

(defun pages-directory () (cl-fad:pathname-as-directory (merge-pathnames "pages" *content-root*)))

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
  ((title            :initarg :title                          :reader   title)
   (categories       :initarg :categories       :initform nil :reader   categories       :id "Categories")
   (summary          :initarg :summary          :initform nil :accessor summary          :id "Summary")
   (description      :initarg :description      :initform nil :accessor description      :id "Description")
   (read-more        :initarg :read-more        :initform nil :accessor read-more        :id "Read more")
   (date             :initarg :date             :initform nil :accessor date             :id "Date")
   (atom-id          :initarg :atom-id          :initform nil :accessor atom-id          :id "Atom ID")
   (flags            :initarg :flags            :initform nil :accessor flags            :id "Flags")
   (keywords         :initarg :keywords         :initform nil :accessor keywords         :id "Keywords")
   (layout           :initarg :template                       :accessor layout           :id "Layout")
   (template         :initarg :template                       :accessor template         :id "Template")
   (articles-heading :initarg :articles-heading :initform nil :accessor articles-heading :id "Articles heading")
   ;; and finally
   ;;
   (content          :initarg :content                        :accessor content)
   ;; also
   ;;
   (file             :initarg :file                           :accessor file))
  (:documentation "A page, in all it's fieldy glory"))

(defclass who-page (page) ())
(defclass markdown-page (page) ())

(defun pages-about (page)
  (declare (ignore page))
  "Find pages that include PAGE in their CATEGORIES"
  nil)

(defun articles-about (page)
  ;; Articles are pages with dates
  "Delegates to PAGES"
  (pages-about page))

(defmethod initialize-instance :after ((instance page) &rest initargs)
  (setf (slot-value instance 'categories) nil
        (slot-value instance 'layout) (or (and (getf initargs :layout)
                                               (find-template (getf initargs :layout)))
                                          'nestor-default-theme::master)
        (slot-value instance 'template) (or (and (getf initargs :template)
                                                 (find-template (getf initargs :template)))
                                            'nestor-default-theme::page)))

(defmethod page-from-file (file (type (eql :mdown)))
  (let ((string (with-open-file (stream file)
                  (let ((data (make-string (file-length stream))))
                    (read-sequence data stream)
                    data))))
    (destructuring-bind (content &optional metadata-hunk)
        (reverse (cl-ppcre:split "\\n\\n"
                                 string :limit 2))
      (let ((metadata (cl-ppcre:split "\\n" metadata-hunk)))
        (apply #'make-instance 'markdown-page
               :content content
               :file file
               (mapcan #'(lambda (line)
                           (destructuring-bind (key value)
                               (cl-ppcre:split "[ \t]*:[ \t]*" line :limit 2)
                             (let ((initarg (gethash key *metadata-ids*)))
                               (if initarg
                                   (list initarg value)
                                   (warn "Metadata key ~a has no initarg" key)))))
                       metadata))))))


(defvar *page*)
(defgeneric to-html (page)
  (:documentation "Return a string rendering PAGE's content")
  (:method ((page markdown-page))
    (multiple-value-bind (document string)
        (cl-markdown:markdown (content page) :stream nil)
      (declare (ignore document))
      string))
  (:method ((page who-page))
    (let ((forms (with-input-from-string (stream (content page))
                   (loop for element = (read stream nil #1='#:eof)
                         until (eq element #1#)
                         collect element))))
      (with-output-to-string (bla)
        (eval (cl-who::tree-to-commands forms bla))))))

(defun calculate-page-pathname (uri type)
  (merge-pathnames (make-pathname :name (pathname-name uri)
                                  :directory (cons :relative (cdr (pathname-directory uri)))
                                  :type (string-downcase (symbol-name type)))
                   (pages-directory)))

(defvar *last-page* nil)
(defun page-dispatcher (request)
  (loop for type in '(:mdown :who)
        for file = (probe-file (calculate-page-pathname (hunchentoot:request-uri request) type))
        when file
          return (lambda ()
                   (let ((*page* (page-from-file file type)))
                     (setq *last-page* *page*)
                     (with-output-to-string (nestor-view::*render-output*)
                       (funcall (layout *page*)))))))


;;;; Reading configuration and the menu and such
;;;;
(defparameter *heading* "Pega na lancheira")
(defun heading () *heading*)

(defparameter *subtitle* "Vai levar o almoco ao pai")
(defun subtitle () *subtitle*)

(defun menu-items ()
  '("foo" "bar" '("quux" "current" "quax") "baz"))


;;;; Layout engine
;;;;
(in-package :nestor-view)

(defvar *theme* :nestor-default-theme
  "A package name specifying the theme")

(defvar *render-output*)

(defun css-dispatcher (request)
  (cl-ppcre:register-groups-bind (name) ("/css/([^ ]+)\\.css" (hunchentoot:request-uri request))
    (let ((style (find-style name)))
      (when style
        (lambda ()
          (funcall #'render-css style))))))

(defun render-css (style)
  ;; should later:
  ;;
  ;;  check the mtime of /public/css/STYLE-NAME.css
  ;;  check the mtime of STYLE's fdefinition
  ;;  check the mtime of /views/STYLE-NAME.clss
  ;;  check the mtime of /views/theme/STYLE-NAME.clss
  ;;  check the mtime of (getf (gethash STYLE *css-render-cache*) :mtime)
  ;;
  ;; do something incredibly smart with these
  (setf (hunchentoot:content-type*) "text/css")
  (funcall style))

(defmacro defstyle (name-or-name-and-options &optional doc &body forms)
  (let* ((sym (first (alexandria::ensure-list name-or-name-and-options))))
    `(progn
       (lambda ,sym ()
         ;; style-functions take no arguments
         ,(if (stringp doc)
              doc
              (progn (push doc forms)
                     nil))
         ,(css-rules-to-string forms)))))

(defmacro deftemplate (name-or-name-and-options arglist &body body)
  (let* ((sym (first (alexandria::ensure-list name-or-name-and-options)))
         (doc (and (stringp (first body))
                   (first body)))
         (body (or (and doc
                        (rest body))
                   body)))
    `(progn
       (setf (get ',sym :template) t)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',sym))
       (defun ,sym ,arglist ,doc
         (cl-who:with-html-output (nestor-view::*render-output* nil :indent t)
           ,@body)))))

(defun find-view (name &key (test #'identity) (package *theme*))
  (with-package-iterator (next (find-package package) :external :internal)
    (loop (multiple-value-bind (morep sym) (next)
            (cond ((not morep) (return))
                  ((and (funcall test sym)
                        (string= name
                                 (format nil "~(~a~)" sym)))
                   (return sym)))))))

(defun find-template (name)
  (find-view name :test #'(lambda (sym)
                            (get sym :template))))
(defun find-style (name)
  (find-view name :test #'(lambda (sym)
                            (getf sym :style))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun css-rules-to-string (rules)
    "Return a CSS string for RULES.

Each R in RULES looks like:

\(SELECTOR KEY VAL...)"
    (reduce
     (alexandria:curry #'concatenate 'string)
     (mapcar
      #'(lambda (form)
          (format nil "~a {~%~a  }~%"
                  (first form)
                  (reduce
                   #'(lambda (s1 s2)
                       (format nil "~a~%~a" s1 s2))
                   (loop for (key value) on (rest form) by #'cddr
                         collect (format nil "~a~(~a~): ~a;"
                                         (make-string 8 :initial-element #\Space)
                                         (string key)
                                         value)))))
      rules))))

(defun display-menu (menu-items &key class (levels 2))
  (if (> levels 0)
      (cl-who:with-html-output-to-string (s nil :indent t)
        (:ul :class class
             (loop for item in menu-items
                   do (if (consp item)
                          (cl-who:htm
                           (:li
                            (cl-who:str (display-menu item :class class
                                                           :levels (1- levels)))))
                          (cl-who:htm
                           (:li :class (if (current-item-p item)
                                           "current")
                                (:a :href "/TODO/"
                                    (cl-who:str item))))))))))

(defun format-date (date)
  ;;; TODO
  (declare (ignore date))
  "30 Feb 2666")


(defun current-item-p (item)
  ;;; TODO
  (string= item "current"))


;;;; debug hacks
;; (in-package :nestor)
;; (defvar *all-requests* nil)
;; (defvar *last-error-request* nil)
;; (defmethod hunchentoot:process-request :before (request)
;;   (push request *all-requests*))

;; (defmethod hunchentoot:handle-request :around (acceptor request)
;;   (multiple-value-bind (body error backtrace)
;;       (catch 'handler-done
;;         (call-next-method))
;;     (when error
;;       (break "what")
;;       (setq *last-error-request* request))
;;     (throw 'handler-done (values body error backtrace))))
