(defsystem #:cl-json.tests
  :description "Tests for cl-json."
  :depends-on (#:cl-json #:parachute)
  :pathname "t"
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o c)
             (uiop:symbol-call :parachute :test :cl-json.tests)))
