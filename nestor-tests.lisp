(stefil:define-test-package :nestor-tests)
(in-package :nestor-tests)

(defun test-simple-page ()
  (multiple-value-bind
        (body status headers reply-uri stream should-be-closed-p reason)
      (drakma:http-request "http://localhost:9494/start-here")
    (is (eq status 200))))
