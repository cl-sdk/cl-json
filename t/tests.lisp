(defpackage #:cl-json.tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:json #:cl-json)
                    (#:flexi #:flexi-streams)))

(in-package #:cl-json.tests)

;;; ── helpers ─────────────────────────────────────────────────────────────────

(defun ht (&rest kvs)
  "Build a hash-table from alternating key/value pairs."
  (let ((table (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k table) v))
    table))

(defun ht= (a b)
  "Return T when hash-tables A and B have equal keys and values."
  (and (= (hash-table-count a) (hash-table-count b))
       (block nil
         (maphash (lambda (k v)
                    (multiple-value-bind (bv present) (gethash k b)
                      (unless (and present (equalp v bv))
                        (return nil))))
                  a)
         t)))

;;; ── reader tests ────────────────────────────────────────────────────────────

(define-test "parse literals"
  (is eq   t     (json:parse "true"))
  (is eq   nil   (json:parse "false"))
  (is eq   :null (json:parse "null")))

(define-test "parse integers"
  (is =    0     (json:parse "0"))
  (is =    42    (json:parse "42"))
  (is =   -7     (json:parse "-7"))
  (is =    1000000 (json:parse "1000000")))

(define-test "parse floats"
  (is =    1.5d0  (json:parse "1.5"))
  (is =   -0.5d0  (json:parse "-0.5"))
  (is =    1.0d2  (json:parse "1e2"))
  (is =    1.5d3  (json:parse "1.5e3"))
  (is =    2.0d-1 (json:parse "2e-1")))

(define-test "parse strings"
  (is string=  ""          (json:parse "\"\""))
  (is string=  "hello"     (json:parse "\"hello\""))
  (is string=  "say \"hi\"" (json:parse "\"say \\\"hi\\\"\""))
  (is string=  "a/b"       (json:parse "\"a\\/b\""))
  (is string=  (string #\Newline) (json:parse "\"\\n\""))
  (is string=  (string #\Tab)     (json:parse "\"\\t\"")))

(define-test "parse unicode escapes"
  (is string= (string (code-char #x41)) (json:parse "\"\\u0041\""))
  (is string= (string (code-char #x00e9)) (json:parse "\"\\u00E9\"")))

(define-test "parse arrays"
  (is equalp #()              (json:parse "[]"))
  (is equalp #(1 2 3)         (json:parse "[1,2,3]"))
  (is equalp #("a" t nil)     (json:parse "[\"a\",true,false]"))
  (is equalp #(#(1 2) #(3))   (json:parse "[[1,2],[3]]")))

(define-test "array roundtrip"
  ;; Empty and non-empty arrays parse to vectors and round-trip correctly.
  (is string= "[]"      (json:stringify (json:parse "[]")))
  (is string= "[1,2,3]" (json:stringify (json:parse "[1,2,3]")))
  (is equalp #(1 2) (json:parse (json:stringify #(1 2)))))

(define-test "parse objects"
  (true (ht= (ht "a" 1) (json:parse "{\"a\":1}")))
  (true (ht= (ht "x" "y" "n" :null)
             (json:parse "{\"x\":\"y\",\"n\":null}"))))

(define-test "parse whitespace"
  (is =  1 (json:parse "  1  "))
  (is equalp #(1 2)
     (json:parse (format nil "[ 1 ,~%  2 ]"))))

(define-test "parse errors"
  (fail (json:parse "") 'json:json-parse-error)
  (fail (json:parse "tru")  'json:json-parse-error)
  (fail (json:parse "{")    'json:json-parse-error)
  (fail (json:parse "[1,]") 'json:json-parse-error)
  (fail (json:parse "1 2")  'json:json-parse-error))

;;; ── writer tests ─────────────────────────────────────────────────────────────

(define-test "stringify literals"
  (is string= "null"  (json:stringify :null))
  (is string= "true"  (json:stringify t))
  (is string= "false" (json:stringify nil)))

(define-test "stringify numbers"
  (is string= "0"   (json:stringify 0))
  (is string= "42"  (json:stringify 42))
  (is string= "-7"  (json:stringify -7)))

(define-test "stringify strings"
  (is string= "\"\""       (json:stringify ""))
  (is string= "\"hello\""  (json:stringify "hello"))
  (is string= "\"a\\\"b\"" (json:stringify "a\"b"))
  (is string= "\"a\\nb\""  (json:stringify (format nil "a~%b"))))

(define-test "stringify symbols"
  (is string= "\"foo\""  (json:stringify 'foo))
  (is string= "\"bar\""  (json:stringify :bar)))

(define-test "stringify arrays"
  (is string= "[]"        (json:stringify #()))
  (is string= "[1,2,3]"   (json:stringify '(1 2 3)))
  (is string= "[1,2,3]"   (json:stringify #(1 2 3)))
  (is string= "[[1],[2]]" (json:stringify '((1) (2)))))

(define-test "stringify objects"
  (is string= "{\"a\":1}"
      (json:stringify (ht "a" 1)))
  ;; Keys should be sorted.
  (is string= "{\"a\":1,\"b\":2}"
      (json:stringify (ht "b" 2 "a" 1))))

(define-test "stringify pretty"
  (let ((result (json:stringify '(1 2) :pretty t)))
    (true (search (string #\Newline) result))))

(define-test "roundtrip"
  ;; Values that survive a parse → stringify → parse cycle unchanged.
  (dolist (v '(:null t nil 0 "hello"))
    (is equalp v (json:parse (json:stringify v))))
  ;; Array roundtrip (parse always produces vectors)
  (is equalp #(1 "two" nil t :null)
      (json:parse (json:stringify #(1 "two" nil t :null)))))

;;; ── stream tests ─────────────────────────────────────────────────────────────

(define-test "parse from character stream"
  (let ((stream (make-string-input-stream "[1,2,3]")))
    (is equalp #(1 2 3) (json:parse stream))))

(define-test "parse from binary stream"
  (let* ((bytes (flexi:string-to-octets "{\"a\":1}" :external-format :utf-8))
         (stream (flexi:make-in-memory-input-stream bytes)))
    (true (json:parse stream))))

(define-test "stringify to character stream"
  (let ((stream (make-string-output-stream)))
    (json:stringify #(1 2 3) :stream stream)
    (is string= "[1,2,3]" (get-output-stream-string stream))))

(define-test "stringify to binary stream"
  (let ((out (flexi:make-in-memory-output-stream)))
    (json:stringify "hello" :stream out)
    (let* ((bytes (flexi:get-output-stream-sequence out))
           (result (flexi:octets-to-string bytes :external-format :utf-8)))
      (is string= "\"hello\"" result))))
