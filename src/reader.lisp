(in-package #:cl-json)

;;; JSON → Lisp type mapping (for the default tree-building handler)
;;;
;;;   null    → :null
;;;   true    → t
;;;   false   → nil
;;;   string  → string
;;;   number  → integer or double-float
;;;   array   → list
;;;   object  → hash-table (test: equal, keys: strings)

;;; ─── SAX-style handler protocol ─────────────────────────────────────────────
;;;
;;; Subclass JSON-SAX-HANDLER and specialise the SAX-* generics to react to
;;; parse events without building an intermediate Lisp tree.
;;;
;;; Event ordering for an object {"a":1}:
;;;   sax-begin-object
;;;   sax-object-key  "a"
;;;   sax-value       1
;;;   sax-end-object
;;;
;;; Event ordering for an array [1,2]:
;;;   sax-begin-array
;;;   sax-value  1
;;;   sax-value  2
;;;   sax-end-array

(defclass json-sax-handler ()
  ()
  (:documentation
   "Base class for SAX-style JSON event handlers.
Subclass this and specialise the SAX-* generics to react to parse events."))

(defgeneric sax-value (handler value)
  (:documentation
   "Called with a primitive JSON value.
VALUE is one of: :null, t (true), nil (false), a string, an integer,
or a double-float."))

(defgeneric sax-begin-object (handler)
  (:documentation "Called when a JSON object '{' is opened."))

(defgeneric sax-object-key (handler key)
  (:documentation "Called with the string KEY of the next object field."))

(defgeneric sax-end-object (handler)
  (:documentation "Called when a JSON object '}' is closed."))

(defgeneric sax-begin-array (handler)
  (:documentation "Called when a JSON array '[' is opened."))

(defgeneric sax-end-array (handler)
  (:documentation "Called when a JSON array ']' is closed."))

(defgeneric sax-result (handler)
  (:documentation
   "Return the result value after PARSE-SAX has finished."))

;;; Default no-op methods — subclasses need only specialise what they care about.
(defmethod sax-value      ((h json-sax-handler) v) (declare (ignore v)))
(defmethod sax-begin-object ((h json-sax-handler)))
(defmethod sax-object-key   ((h json-sax-handler) k) (declare (ignore k)))
(defmethod sax-end-object   ((h json-sax-handler)))
(defmethod sax-begin-array  ((h json-sax-handler)))
(defmethod sax-end-array    ((h json-sax-handler)))
(defmethod sax-result       ((h json-sax-handler)) nil)

;;; ─── Tree-building handler ───────────────────────────────────────────────────
;;;
;;; Reconstructs the standard Lisp tree from SAX events:
;;;   object  → hash-table (string keys, EQUAL test)
;;;   array   → list
;;;   null    → :null, true → t, false → nil
;;;   string/number → as-is
;;;
;;; The handler keeps a stack of accumulator frames:
;;;   (:root   value)           — top-level result slot
;;;   (:array  reversed-items)  — array under construction
;;;   (:object hash-table key)  — object under construction; KEY is the
;;;                               pending key string (nil between entries)

(defclass json-tree-handler (json-sax-handler)
  ((stack :initform (list (list :root nil)) :accessor %handler-stack))
  (:documentation
   "A SAX handler that builds the standard Lisp representation of JSON."))

(defun %tree-accept (h v)
  "Insert value V into the innermost accumulator frame of handler H."
  (let ((frame (first (%handler-stack h))))
    (ecase (first frame)
      (:root
       (setf (second frame) v))
      (:array
       (push v (second frame)))
      (:object
       (setf (gethash (third frame) (second frame)) v)
       (setf (third frame) nil)))))

(defmethod sax-value ((h json-tree-handler) v)
  (%tree-accept h v))

(defmethod sax-begin-object ((h json-tree-handler))
  (push (list :object (make-hash-table :test 'equal) nil)
        (%handler-stack h)))

(defmethod sax-object-key ((h json-tree-handler) k)
  (setf (third (first (%handler-stack h))) k))

(defmethod sax-end-object ((h json-tree-handler))
  (let ((ht (second (pop (%handler-stack h)))))
    (%tree-accept h ht)))

(defmethod sax-begin-array ((h json-tree-handler))
  (push (list :array nil) (%handler-stack h)))

(defmethod sax-end-array ((h json-tree-handler))
  (let ((items (nreverse (second (pop (%handler-stack h))))))
    (%tree-accept h items)))

(defmethod sax-result ((h json-tree-handler))
  (second (first (%handler-stack h))))

;;; ─── Core SAX parser ─────────────────────────────────────────────────────────

(defun parse-sax (string handler)
  "Parse STRING as JSON, firing SAX events on HANDLER.
Returns (SAX-RESULT HANDLER) after the parse completes."
  (let ((pos 0)
        (len (length string)))
    (labels
        ((%error (msg)
           (error 'json-parse-error :message msg :position pos))

         (peek ()
           (when (< pos len)
             (char string pos)))

         (advance ()
           (when (>= pos len)
             (%error "Unexpected end of input"))
           (prog1 (char string pos)
             (incf pos)))

         (expect (expected)
           (let ((c (advance)))
             (unless (char= c expected)
               (%error (format nil "Expected '~C' but got '~C'" expected c)))))

         (skip-whitespace ()
           (loop while (and (< pos len)
                            (member (char string pos)
                                    '(#\Space #\Tab #\Newline #\Return)))
                 do (incf pos)))

         (parse-value ()
           (skip-whitespace)
           (let ((c (peek)))
             (cond
               ((null c)        (%error "Unexpected end of input"))
               ((char= c #\")  (sax-value handler (parse-raw-string)))
               ((char= c #\{)  (parse-object))
               ((char= c #\[)  (parse-array))
               ((char= c #\t)  (parse-true))
               ((char= c #\f)  (parse-false))
               ((char= c #\n)  (parse-null))
               ((or (char= c #\-) (digit-char-p c)) (parse-number))
               (t (%error (format nil "Unexpected character '~C'" c))))))

         ;; Parse a JSON string token and return the Lisp string.
         ;; Used for both string values and object keys.
         (parse-raw-string ()
           (expect #\")
           (let ((buf (make-array 64 :element-type 'character
                                      :adjustable t :fill-pointer 0)))
             (loop
               (when (>= pos len)
                 (%error "Unterminated string"))
               (let ((c (advance)))
                 (cond
                   ((char= c #\")
                    (return (coerce buf 'string)))
                   ((char= c #\\)
                    (when (>= pos len)
                      (%error "Unterminated escape sequence"))
                    (vector-push-extend (parse-escape) buf))
                   ((char< c #\Space)
                    (%error (format nil "Unescaped control character in string")))
                   (t
                    (vector-push-extend c buf)))))))

         (parse-escape ()
           (let ((c (advance)))
             (case c
               (#\"  #\")
               (#\\  #\\)
               (#\/  #\/)
               (#\b  #\Backspace)
               (#\f  #\Page)
               (#\n  #\Newline)
               (#\r  #\Return)
               (#\t  #\Tab)
               (#\u  (parse-unicode-escape))
               (otherwise
                (%error (format nil "Invalid escape character '~C'" c))))))

         (parse-unicode-escape ()
           (let ((code 0))
             (dotimes (i 4)
               (when (>= pos len)
                 (%error "Incomplete unicode escape"))
               (let* ((c     (advance))
                      (digit (digit-char-p c 16)))
                 (unless digit
                   (%error (format nil "Invalid hex digit '~C' in \\uXXXX escape" c)))
                 (setf code (+ (* code 16) digit))))
             ;; Handle surrogate pairs (U+D800–U+DFFF)
             (if (and (>= code #xD800) (<= code #xDBFF))
                 (parse-surrogate-pair code)
                 (code-char code))))

         (parse-surrogate-pair (high)
           ;; Expect a low surrogate \uDC00–\uDFFF
           (unless (and (< (+ pos 1) len)
                        (char= (char string pos)      #\\)
                        (char= (char string (+ pos 1)) #\u))
             (%error "Expected low surrogate after high surrogate"))
           (incf pos 2) ; consume \u
           (let ((low 0))
             (dotimes (i 4)
               (when (>= pos len)
                 (%error "Incomplete low surrogate escape"))
               (let* ((c     (advance))
                      (digit (digit-char-p c 16)))
                 (unless digit
                   (%error (format nil "Invalid hex digit '~C' in surrogate escape" c)))
                 (setf low (+ (* low 16) digit))))
             (unless (and (>= low #xDC00) (<= low #xDFFF))
               (%error "Invalid low surrogate value"))
             (code-char (+ #x10000 (* (- high #xD800) #x400) (- low #xDC00)))))

         (parse-object ()
           (expect #\{)
           (sax-begin-object handler)
           (skip-whitespace)
           (cond
             ((and (peek) (char= (peek) #\}))
              (advance)
              (sax-end-object handler))
             (t
              (loop
                (skip-whitespace)
                (unless (and (peek) (char= (peek) #\"))
                  (%error "Expected string key in object"))
                (sax-object-key handler (parse-raw-string))
                (skip-whitespace)
                (expect #\:)
                (parse-value)
                (skip-whitespace)
                (let ((next (peek)))
                  (cond
                    ((null next)      (%error "Unterminated object"))
                    ((char= next #\}) (advance) (sax-end-object handler) (return))
                    ((char= next #\,) (advance))
                    (t (%error (format nil "Expected ',' or '}' in object, got '~C'" next)))))))))

         (parse-array ()
           (expect #\[)
           (sax-begin-array handler)
           (skip-whitespace)
           (cond
             ((and (peek) (char= (peek) #\]))
              (advance)
              (sax-end-array handler))
             (t
              (loop
                (parse-value)
                (skip-whitespace)
                (let ((next (peek)))
                  (cond
                    ((null next)      (%error "Unterminated array"))
                    ((char= next #\]) (advance) (sax-end-array handler) (return))
                    ((char= next #\,) (advance))
                    (t (%error (format nil "Expected ',' or ']' in array, got '~C'" next)))))))))

         (parse-literal (chars value)
           (dolist (c chars)
             (let ((got (advance)))
               (unless (char= got c)
                 (%error (format nil "Invalid literal; expected '~C' got '~C'" c got)))))
           (sax-value handler value))

         (parse-true  () (parse-literal '(#\t #\r #\u #\e)        t))
         (parse-false () (parse-literal '(#\f #\a #\l #\s #\e)    nil))
         (parse-null  () (parse-literal '(#\n #\u #\l #\l)        :null))

         (parse-number ()
           (let ((start pos))
             ;; Optional leading minus
             (when (and (peek) (char= (peek) #\-))
               (advance))
             ;; Integer part
             (cond
               ((and (peek) (char= (peek) #\0))
                (advance))
               ((and (peek) (digit-char-p (peek)))
                (loop while (and (peek) (digit-char-p (peek))) do (advance)))
               (t (%error "Invalid number")))
             ;; Optional fractional part
             (let ((is-float nil))
               (when (and (peek) (char= (peek) #\.))
                 (setf is-float t)
                 (advance)
                 (unless (and (peek) (digit-char-p (peek)))
                   (%error "Expected digit after decimal point"))
                 (loop while (and (peek) (digit-char-p (peek))) do (advance)))
               ;; Optional exponent
               (when (and (peek) (member (peek) '(#\e #\E)))
                 (setf is-float t)
                 (advance)
                 (when (and (peek) (member (peek) '(#\+ #\-)))
                   (advance))
                 (unless (and (peek) (digit-char-p (peek)))
                   (%error "Expected digit in exponent"))
                 (loop while (and (peek) (digit-char-p (peek))) do (advance)))
               (sax-value handler
                          (let ((num-str (subseq string start pos)))
                            (if is-float
                                (let ((*read-default-float-format* 'double-float))
                                  (read-from-string num-str))
                                (parse-integer num-str))))))))

      (parse-value)
      (skip-whitespace)
      (when (< pos len)
        (%error (format nil "Unexpected trailing content '~C'" (char string pos))))
      (sax-result handler))))

;;; ─── Public API ──────────────────────────────────────────────────────────────

(defun parse (string)
  "Parse a JSON string and return the corresponding Lisp value.

Type mapping:
  JSON null   → :null
  JSON true   → t
  JSON false  → nil
  JSON string → string
  JSON number → integer or double-float
  JSON array  → list  (note: the empty array '[]' parses to NIL, which is
                        indistinguishable from JSON false on the write side;
                        use a vector #() when a round-trippable empty array
                        is required)
  JSON object → hash-table with string keys (test: equal)"
  (parse-sax string (make-instance 'json-tree-handler)))
