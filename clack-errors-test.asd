(defsystem clack-errors-test
  :author "Fernando Borretti"
  :license "LLGPL"
  :description "clack-errors tests"
  :depends-on (:clack-errors
               :fiveam
               :drakma)
  :components ((:module "t"
                :serial t
                :components
                ((:file "app")
                 (:file "clack-errors")))))
