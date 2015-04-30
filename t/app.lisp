(in-package :cl-user)
(defpackage clack-errors-test.app
  (:use :cl)
  (:export :start
           :stop))
(in-package :clack-errors-test.app)

(defparameter *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/no-error")
      "Nothing to see here.")

(setf (ningle:route *app* "/error")
      (lambda (params)
        (declare (ignore params))
        (error "test")
        "This will never actually appear."))

(defparameter *handler* nil)

(defun start ()
  (setf *handler*
        (clack:clackup
         (funcall clack-errors:*clack-error-middleware*
                  *app*
                  :debug t)
         :port 8000)))

(defun stop ()
  (clack:stop *handler*))
