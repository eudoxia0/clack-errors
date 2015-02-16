(in-package :cl-user)
(defpackage clack-errors-test.app
  (:use :cl :ningle)
  (:export :start
           :stop))
(in-package :clack-errors-test.app)

(defvar *app* (make-instance '<app>))

(setf (route *app* "/no-error")
      "Nothing to see here.")

(defun fun-b ()
  (error "test"))

(defun fun-a (params)
  (fun-b))

(setf (route *app* "/error")
      (lambda (params)
        (fun-a params)
        "This will never actually appear."))

(defvar *handler* nil)

(defun start ()
  (setf *handler*
        (clack:clackup
         (clack.builder:builder
          (clack-errors:<clack-error-middleware>
           :debug t)
          *app*)
         :port 8000)))

(defun stop ()
  (clack:stop *handler*))
