(defpackage clack-errors-demo
  (:use :cl :clack.builder :ningle)
  (:import-from :cl-markup
                :html5)
  (:export :stop))
(in-package :clack-errors-demo)
(defvar *app* (make-instance '<app>))

(setf (route *app* "/")
      (html5
       (:a :href "/no-error" "Error-free page") (:br)
       (:a :href "/error" "Error page")))

(setf (route *app* "/no-error")
      (html5 (:h1 "Nothing to see here.")))

(defun fun-b ()
  (error "test"))

(defun fun-a (params)
  (fun-b))

(setf (route *app* "/error")
      (lambda (params)
        (fun-a params)
        (html5 (:h1 "This will never actually appear."))))

(defparameter *handler*
  (clack:clackup
   (builder
    clack-errors:<clack-error-middleware>
    *app*)
   :port 8000))

(defun stop () (clack:stop *handler*))
