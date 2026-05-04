(defsystem #:cl-json.test
  :description "Tests for cl-json."
  :depends-on (#:cl-json #:parachute)
  :pathname "t"
  :serial t
  :components ((:file "tests")))
