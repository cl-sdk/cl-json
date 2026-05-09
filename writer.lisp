(in-package #:io.github.cl-sdk.json)

;;; Lisp → JSON type mapping
;;;
;;;   :null        → null
;;;   t            → true
;;;   nil          → false  (nil always encodes as false, never as an empty array;
;;;                             the nil/list disambiguation is only available when
;;;                             decoding with derive-json, which has slot-type context)
;;;   string       → string
;;;   integer      → integer number
;;;   float        → floating-point number
;;;   ratio        → floating-point number (coerced to double-float)
;;;   keyword      → string  (symbol-name, downcased)
;;;   symbol       → string  (symbol-name, downcased)
;;;   list         → array   (non-nil list)
;;;   vector       → array
;;;   hash-table   → object  (keys converted via key→string)
;;;   CLOS object  → object  (via encode-json generic function)

(defgeneric encode-json (value stream)
  (:documentation
   "Write VALUE as JSON to STREAM.
Specialise this generic function to add support for custom types."))

;; ── helpers ─────────────────────────────────────────────────────────────────

(defun %key->string (key)
  "Convert a hash-table key to a JSON object key string."
  (typecase key
    (string  key)
    (symbol  (string-downcase (symbol-name key)))
    (integer (write-to-string key))
    (t       (error 'json-encode-error
                    :message (format nil "Cannot use ~S as a JSON object key" key)))))

(defun %write-string-json (s stream)
  "Write the string S as a quoted, escaped JSON string to STREAM."
  (write-char #\" stream)
  (loop for c across s do
    (case c
      (#\"        (write-string "\\\"" stream))
      (#\\        (write-string "\\\\" stream))
      (#\Newline  (write-string "\\n"  stream))
      (#\Return   (write-string "\\r"  stream))
      (#\Tab      (write-string "\\t"  stream))
      (#\Backspace(write-string "\\b"  stream))
      (#\Page     (write-string "\\f"  stream))
      (otherwise
       (if (char< c #\Space)
           (format stream "\\u~4,'0X" (char-code c))
           (write-char c stream)))))
  (write-char #\" stream))

(defun %write-indent (stream depth indent-string)
  (dotimes (_ depth)
    (write-string indent-string stream)))

(defun %write-float (d stream)
  "Write the finite double-float D as a JSON number to STREAM.
Uses fixed-point notation for numbers in a readable range, and scientific
notation (with lowercase 'e') for very large or very small values."
  (let ((ad (abs d)))
    (cond
      ;; Zero and numbers in a compact fixed-point range.
      ((or (zerop d) (and (<= 1.0d-6 ad) (< ad 1.0d15)))
       ;; ~F always produces a decimal point; trim trailing zeros but keep
       ;; at least one fractional digit so the output is valid JSON
       ;; (the spec requires fraction = "." 1*DIGIT).
       (let* ((s    (format nil "~F" d))
              (dot  (position #\. s))
              (end  (if dot
                        (loop with e = (1- (length s))
                              while (and (> e (1+ dot)) (char= (char s e) #\0))
                              do (decf e)
                              finally (return (1+ e)))
                        (length s))))
         (write-string s stream :end end)))
      ;; Scientific notation: fix CL's 'd'/'D' exponent marker → 'e'.
      (t
       (let ((s (format nil "~E" d)))
         (loop for c across s
               do (write-char (case c ((#\d #\D) #\e) (otherwise c))
                              stream)))))))

(defun %encode (value stream &key (pretty nil) (depth 0) (indent "  "))
  (cond
    ((eq value :null)
     (write-string "null" stream))

    ((eq value t)
     (write-string "true" stream))

    ((null value)
     (write-string "false" stream))

    ((stringp value)
     (%write-string-json value stream))

    ((integerp value)
     (write-string (write-to-string value) stream))

    ((floatp value)
     ;; Guard against NaN (NaN /= NaN) and infinity (> most-positive-double-float).
     (let ((d (float value 1.0d0)))
       (when (or (/= d d)
                 (> (abs d) most-positive-double-float))
         (error 'json-encode-error
                :message (format nil "Cannot encode non-finite float ~S as JSON" value)))
       (%write-float d stream)))

    ((typep value 'ratio)
     (%encode (float value 1.0d0) stream :pretty pretty :depth depth :indent indent))

    ((symbolp value)
     (%write-string-json (string-downcase (symbol-name value)) stream))

    ((or (listp value) (vectorp value))
     (%encode-array value stream :pretty pretty :depth depth :indent indent))

    ((hash-table-p value)
     (%encode-object value stream :pretty pretty :depth depth :indent indent))

    (t
     ;; Fallback: try user-defined encode-json method.
     (encode-json value stream))))

(defun %encode-array (seq stream &key pretty depth indent)
  (write-char #\[ stream)
  (let ((items (if (vectorp seq) (coerce seq 'list) seq)))
    (when items
      (if pretty
          (progn
            (terpri stream)
            (%write-indent stream (1+ depth) indent)
            (%encode (first items) stream :pretty t :depth (1+ depth) :indent indent)
            (dolist (item (rest items))
              (write-char #\, stream)
              (terpri stream)
              (%write-indent stream (1+ depth) indent)
              (%encode item stream :pretty t :depth (1+ depth) :indent indent))
            (terpri stream)
            (%write-indent stream depth indent))
          (progn
            (%encode (first items) stream :pretty nil :depth depth :indent indent)
            (dolist (item (rest items))
              (write-char #\, stream)
              (%encode item stream :pretty nil :depth depth :indent indent))))))
  (write-char #\] stream))

(defun %encode-object (table stream &key pretty depth indent)
  (write-char #\{ stream)
  (let ((pairs nil))
    (maphash (lambda (k v) (push (cons k v) pairs)) table)
    (setf pairs (sort pairs #'string< :key (lambda (p) (%key->string (car p)))))
    (when pairs
      (flet ((write-pair (pair)
               (%write-string-json (%key->string (car pair)) stream)
               (write-char #\: stream)
               (when pretty (write-char #\Space stream))
               (%encode (cdr pair) stream :pretty pretty :depth (1+ depth) :indent indent)))
        (if pretty
            (progn
              (terpri stream)
              (%write-indent stream (1+ depth) indent)
              (write-pair (first pairs))
              (dolist (pair (rest pairs))
                (write-char #\, stream)
                (terpri stream)
                (%write-indent stream (1+ depth) indent)
                (write-pair pair))
              (terpri stream)
              (%write-indent stream depth indent))
            (progn
              (write-pair (first pairs))
              (dolist (pair (rest pairs))
                (write-char #\, stream)
                (write-pair pair)))))))
  (write-char #\} stream))

;; ── public API ───────────────────────────────────────────────────────────────

(defun stringify (value &key (pretty nil) (indent "  ") (stream nil))
  "Encode VALUE as JSON.

If STREAM is provided, write JSON to STREAM and return nil.
Otherwise, return a JSON string.

Keyword arguments:
  PRETTY  — when non-nil, emit indented, human-readable JSON.
  INDENT  — the string used as one indentation level (default: two spaces).
  STREAM  — when non-nil, write JSON to this character or binary stream.
             Binary streams are wrapped with flexi-streams using UTF-8 encoding.

Type mapping:
  :null        → null
  t            → true
  nil          → false  (nil always encodes as false; use derive-json for
                            list/false disambiguation when decoding)
  string       → JSON string
  integer      → integer number
  float        → floating-point number
  ratio        → floating-point number
  keyword      → JSON string (downcased symbol name)
  symbol       → JSON string (downcased symbol name)
  list         → JSON array  (non-nil list)
  vector       → JSON array
  hash-table   → JSON object (keys sorted, converted to strings)
  CLOS object  → via ENCODE-JSON generic function"
  (flet ((encode-to (out)
           (%encode value out :pretty pretty :indent indent)))
    (cond
      ((null stream)
       (with-output-to-string (out)
         (encode-to out)))
      ((and (streamp stream)
            (subtypep (stream-element-type stream) 'character))
       (encode-to stream))
      ((streamp stream)
       ;; Binary / octet stream: wrap with flexi-streams for UTF-8 encoding.
       (let ((flexi (flexi-streams:make-flexi-stream stream :external-format :utf-8)))
         (encode-to flexi)))
      (t
       (error 'json-encode-error
              :message (format nil "Cannot write to ~S: expected nil or stream" stream))))))

;; Default encode-json method signals a helpful error.
(defmethod encode-json (value stream)
  (declare (ignore stream))
  (error 'json-encode-error
         :message (format nil "Don't know how to encode ~S as JSON" value)))
