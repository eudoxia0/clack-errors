(defsystem clack-errors
  :version "0.2"
  :author "Fernando Borretti"
  :license "LLGPL"
  :description "Error page middleware for Clack."
  :homepage "https://github.com/eudoxia0/clack-errors"
  :depends-on (:clack
               :closer-mop
               :local-time
               :trivial-backtrace
               :djula
               :cl-ppcre)
  :components ((:module "src"
                :serial t
                :components
                ((:file "clack-errors"))))
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (load-op clack-errors-test))))
