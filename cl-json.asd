(defsystem #:cl-json
  :description "A JSON reader and writer for Common Lisp."
  :version "0.1.0"
  :license "Unlicense"
  :depends-on ("cl-sdk/meta-definitions" "closer-mop")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "reader")
               (:file "writer")
               (:file "derive"))
  :in-order-to ((test-op (test-op #:cl-json.tests))))
