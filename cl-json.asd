(defsystem #:cl-json
  :description "A JSON reader and writer for Common Lisp."
  :version "0.1.0"
  :license "Unlicense"
  :depends-on (#:meta-definitions #:closer-mop #:flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "reader")
               (:file "writer")
               (:file "derive"))
  :in-order-to ((test-op (test-op #:cl-json.tests))))
