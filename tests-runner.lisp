(push *default-pathname-defaults* ql:*local-project-directories*)

(setf asdf/source-registry::*source-registry-file* #P"./.qlot/")

(asdf:initialize-source-registry)

(ql:quickload :cl-json.test)

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore c h))
        (uiop:quit -1))
      5am:*on-error* nil)

(unless (5am:run-all-tests)
  (uiop:quit -1))
