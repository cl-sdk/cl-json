(push *default-pathname-defaults* ql:*local-project-directories*)

(asdf:oos 'asdf:load-op :cl-json.test :force t)

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore c h))
        (uiop:quit -1))
      5am:*on-error* nil)

(unless (5am:run-all-tests)
  (uiop:quit -1))
