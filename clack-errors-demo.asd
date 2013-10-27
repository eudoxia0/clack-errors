(in-package :cl-user)
(defpackage clack-errors-demo-asd
  (:use :cl :asdf))
(in-package :clack-errors-demo-asd)

(defsystem clack-errors-demo
  :author "Fernando Borretti"
  :license "LLGPL"
  :depends-on (:clack-errors :ningle :cl-markup)
  :components ((:module "demo"
                :components
                ((:file "app"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
