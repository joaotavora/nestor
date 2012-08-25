(defun string-numberp (s)
  (condition-case oops
      (destructuring-bind (read . idx)
          (read-from-string "0")
        (and (numberp read)
             (eql idx (length s))))
    (error nil)))




(defun nestor-to-css-lite ()
  (interactive)
  (goto-char (point-min))
  (flet ((trim (s)
               (let ((s1 (replace-regexp-in-string "[ \t\n]*$" "" s)))
                 (replace-regexp-in-string "^[ \t\n]*" "" s1))))
    (let (retval
          next-sexp-start)
      (while (setq next-sexp-start (save-excursion
                                     (and (search-forward-regexp "{" nil t)
                                          (goto-char (1- (point))))))
        (push (cons (buffer-substring-no-properties (point)
                                                    (goto-char next-sexp-start))
                    (buffer-substring-no-properties (1+ (point))
                                                    (progn
                                                      (forward-sexp 1)
                                                      (1- (point)))))
              retval))
      (with-current-buffer (generate-new-buffer "*css-lite*")
        (dolist (name-and-sexp (reverse retval))
          (insert "(")
          (insert (pp-to-string (trim (car name-and-sexp))))
          (insert "\n")
          (dolist (prop-and-value (with-temp-buffer
                                    (let (retval)
                                      (insert (cdr name-and-sexp))
                                      (goto-char (point-min))
                                      (while (search-forward-regexp "\\([^:]+\\):\\([^;]+\\);"
                                                                    nil
                                                                    t)
                                        (push (cons (trim (match-string 1))
                                                    (trim (match-string 2)))
                                              retval))
                                      retval)))
            (insert (format ":%s %s\n"
                            (car prop-and-value)
                            (or (and (string-numberp (cdr prop-and-value))
                                     (cdr prop-and-value))
                                (format "\"%s\"" (cdr prop-and-value))))))
          (delete-char -1)
          (insert (format ")\n")))
        (lisp-mode)
        (indent-region (point-min) (point-max))
        (pop-to-buffer (current-buffer))))))
