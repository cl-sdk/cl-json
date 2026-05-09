(defsystem #:io.github.cl-sdk.json
  :description "A JSON reader and writer for Common Lisp."
  :long-description "io.github.cl-sdk.json provides a JSON parser (parse) and serializer
(stringify) for Common Lisp, with automatic encoding/decoding derivations for
CLOS classes via the derive-json macro and the cl-sdk/meta-definitions library.
Binary streams are transparently handled with UTF-8 encoding via flexi-streams."
  :version "0.1.0"
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :maintainer "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "Unlicense"
  :homepage "https://github.com/cl-sdk/cl-json"
  :source-control (:git "https://github.com/cl-sdk/cl-json.git")
  :bug-tracker "https://github.com/cl-sdk/cl-json/issues"
  :depends-on (#:meta-definitions #:closer-mop #:flexi-streams)
  :in-order-to ((test-op (test-op #:io.github.cl-sdk.json.test)))
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "reader")
               (:file "writer")
               (:file "derive")))
