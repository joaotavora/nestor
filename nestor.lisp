(require 'hunchentoot)
(require 'cl-who)
(require 'cl-markdown)

(in-package :hunchentoot)
(defvar *last-request* nil)
(export (intern "*LAST-REQUEST*" :hunchentoot) :hunchentoot)


(defpackage :nestor (:use :cl))
(in-package :nestor)

(defvar *nestor-acceptor*)

(defun start ()
  (setq *nestor-acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :port 4242))
  (hunchentoot:start *nestor-acceptor*))

(defun stop ()
  (hunchentoot:stop *nestor-acceptor*))

(defvar *nestor-content-root* *default-pathname-defaults*)

(defvar *nestor-lisp-file* (load-time-value
                            (or #.*compile-file-pathname* *load-pathname*)))

(defvar *page-to-serve* nil)
(defun find-page-to-serve (request)
  (let ((found (some #'(lambda (desc)
                         (let ((probe (probe-file (make-pathname :directory (pathname-directory *nestor-content-root*)
                                                                 :name (hunchentoot:request-uri request)
                                                                 :type (car desc)))))1
                           (format t "~&proble is ~A~%" probe)
                           (when probe
                             (list probe (cdr desc)))))
                     `(("mdown" . ,#'(lambda (file)
                                       (multiple-value-bind (doc string)
                                           (cl-markdown:markdown file
                                                                 :stream nil)
                                         (declare (ignore doc))
                                         string)))))))
    (setq *page-to-serve* found)))

(hunchentoot:define-easy-handler (say-yo :uri #'find-page-to-serve)
    ((name)
     (shit))
  (declare (ignore name) (ignore shit))
  (funcall (second *page-to-serve*)
           (first *page-to-serve*)))

(when nil
  (setq hunchentoot:*dispatch-table* '(hunchentoot:dispatch-easy-handlers)))
