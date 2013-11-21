(in-package :cl-user)
(defpackage clack-errors-asd
  (:use :cl :asdf))
(in-package :clack-errors-asd)

(defsystem clack-errors
  :version "0.2"
  :author "Fernando Borretti"
  :license "LLGPL"
  :description "Error page middleware for Clack."
  :defsystem-depends-on (#:closure-template)
  :depends-on (:clack
               :mop-utils
               :trivial-backtrace
               :closure-template
               :cl-ppcre)
  :serial t
  :components ((:module "static"
                :components
                ((:static-file "style.css")))
               (:module "src"
                :serial t
                :components
                ((:closure-template "error-page")
                 (:closure-template "prod-page")
                 (:file "clack-errors"))))
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op clack-errors-test))))
