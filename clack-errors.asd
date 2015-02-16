(defsystem clack-errors
  :version "0.2"
  :author "Fernando Borretti"
  :license "LLGPL"
  :description "Error page middleware for Clack."
  :homepage "https://github.com/eudoxia0/clack-errors"
  :defsystem-depends-on (:closure-template)
  :depends-on (:clack
               :closer-mop
               :local-time
               :trivial-backtrace
               :closure-template
               :cl-ppcre)
  :serial t
  :components ((:module "static"
                :components
                ((:static-file "style-dev.css")
                 (:static-file "style-prod.css")))
               (:module "src"
                :serial t
                :components
                ((:closure-template "dev-page")
                 (:closure-template "prod-page")
                 (:file "clack-errors"))))
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (load-op clack-errors-test))))
