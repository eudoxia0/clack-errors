(in-package :cl-user)
(defpackage clack-errors-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :clack-errors-test)

(def-suite tests
  :description "clack-errors tests")
(in-suite tests)

(defun request-code (url)
  (nth-value 1
             (drakma:http-request (concatenate 'string
                                               "http://localhost:8000"
                                               url))))

(test start-app
  (finishes
   (clack-errors-test.app:start)))

(test routes
  (is
   (equal (request-code "/no-error")
          200))
  (is
   (equal (request-code "/error")
          500)))

(test stop-app
  (finishes
   (clack-errors-test.app:stop)))

(defun run-tests ()
  (run! 'tests))
