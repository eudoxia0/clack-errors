(defpackage clack-errors-demo
  (:use :cl)
  (:export :start
           :stop))
(in-package :clack-errors-demo)

(defmacro html-response (&rest body)
  `(list 200
         (list :content-type "text/html")
         (list (markup:html5 ,@body))))

(defparameter *app*
  (lambda (env)
    (let ((url (getf env :path-info)))
      (cond
        ((string= url "/")
         (html-response
          (:a :href "/no-error" "Error-free page") (:br)
          (:a :href "/error" "Error page")))
        ((string= url "/no-error")
         (html-response
          (:h1 "Nothing to see here.")))
        ((string= url "/error")
         (error "test"))))))

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
