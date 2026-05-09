(defsystem #:io.github.cl-sdk.json.test
  :description "Tests for io.github.cl-sdk.json."
  :version "0.1.0"
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :maintainer "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "Unlicense"
  :homepage "https://github.com/cl-sdk/cl-json"
  :source-control (:git "https://github.com/cl-sdk/cl-json.git")
  :bug-tracker "https://github.com/cl-sdk/cl-json/issues"
  :depends-on (#:io.github.cl-sdk.json #:parachute)
  :perform (test-op (o c) (symbol-call :parachute :test :io.github.cl-sdk.json.test))
  :pathname "t"
  :serial t
  :components ((:file "tests")))
