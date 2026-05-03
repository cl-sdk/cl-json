(in-package #:cl-json)

;;; JSON → Lisp type mapping (for the default tree-building handler)
;;;
;;;   null    → :null
;;;   true    → t
;;;   false   → nil
;;;   string  → string
;;;   number  → integer or double-float
;;;   array   → vector
;;;   object  → hash-table (test: equal, keys: strings)

;;; ─── Event handler protocol ──────────────────────────────────────────────────
;;;
;;; Subclass JSON-HANDLER and specialise the generics to react to
;;; parse events without building an intermediate Lisp tree.
;;;
;;; Event ordering for an object {"a":1}:
;;;   begin-object
;;;   object-key  "a"
;;;   on-value    1
;;;   end-object
;;;
;;; Event ordering for an array [1,2]:
;;;   begin-array
;;;   on-value  1
;;;   on-value  2
;;;   end-array

(defclass json-handler ()
  ()
  (:documentation
   "Base class for JSON event handlers.
Subclass this and specialise the generics to react to parse events."))

(defgeneric on-value (handler value)
  (:documentation
   "Called with a primitive JSON value.
VALUE is one of: :null, t (true), nil (false), a string, an integer,
or a double-float."))

(defgeneric begin-object (handler)
  (:documentation "Called when a JSON object '{' is opened."))

(defgeneric object-key (handler key)
  (:documentation "Called with the string KEY of the next object field."))

(defgeneric end-object (handler)
  (:documentation "Called when a JSON object '}' is closed."))

(defgeneric begin-array (handler)
  (:documentation "Called when a JSON array '[' is opened."))

(defgeneric end-array (handler)
  (:documentation "Called when a JSON array ']' is closed."))

(defgeneric handler-result (handler)
  (:documentation "Return the result value after parsing has finished."))

;;; Default no-op methods — subclasses need only specialise what they care about.
(defmethod on-value      ((h json-handler) v) (declare (ignore v)))
(defmethod begin-object  ((h json-handler)))
(defmethod object-key    ((h json-handler) k) (declare (ignore k)))
(defmethod end-object    ((h json-handler)))
(defmethod begin-array   ((h json-handler)))
(defmethod end-array     ((h json-handler)))
(defmethod handler-result ((h json-handler)) nil)

;;; ─── Tree-building handler ───────────────────────────────────────────────────
;;;
;;; Reconstructs the standard Lisp tree from parse events:
;;;   object  → hash-table (string keys, EQUAL test)
;;;   array   → vector
;;;   null    → :null, true → t, false → nil
;;;   string/number → as-is
;;;
;;; The handler keeps a stack of accumulator frames:
;;;   (:root   value)           — top-level result slot
;;;   (:array  reversed-items)  — array under construction
;;;   (:object hash-table key)  — object under construction; KEY is the
;;;                               pending key string (nil between entries)

(defclass json-tree-handler (json-handler)
  ((stack :initform (list (list :root nil)) :accessor %handler-stack))
  (:documentation
   "A handler that builds the standard Lisp representation of JSON."))

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

(defmethod on-value ((h json-tree-handler) v)
  (%tree-accept h v))

(defmethod begin-object ((h json-tree-handler))
  (push (list :object (make-hash-table :test 'equal) nil)
        (%handler-stack h)))

(defmethod object-key ((h json-tree-handler) k)
  (setf (third (first (%handler-stack h))) k))

(defmethod end-object ((h json-tree-handler))
  (let ((ht (second (pop (%handler-stack h)))))
    (%tree-accept h ht)))

(defmethod begin-array ((h json-tree-handler))
  (push (list :array nil) (%handler-stack h)))

(defmethod end-array ((h json-tree-handler))
  (let ((items (nreverse (second (pop (%handler-stack h))))))
    (%tree-accept h (coerce items 'vector))))

(defmethod handler-result ((h json-tree-handler))
  (second (first (%handler-stack h))))

;;; ─── Core parser ────────────────────────────────────────────────────────────────────────────

(defun %parse (stream handler)
  "Internal: parse CHARACTER-STREAM as JSON, firing events on HANDLER.
Returns (HANDLER-RESULT HANDLER) after the parse completes."
  (let ((pos 0))
    (labels
        ((%error (msg)
           (error 'json-parse-error :message msg :position pos))

         (peek ()
           (peek-char nil stream nil nil))

         (advance ()
           (let ((c (read-char stream nil nil)))
             (unless c
               (%error "Unexpected end of input"))
             (incf pos)
             c))

         (expect (expected)
           (let ((c (advance)))
             (unless (char= c expected)
               (%error (format nil "Expected '~C' but got '~C'" expected c)))))

         (skip-whitespace ()
           (loop for c = (peek-char nil stream nil nil)
                 while (and c (member c '(#\Space #\Tab #\Newline #\Return)))
                 do (read-char stream nil nil)
                    (incf pos)))

         (parse-value ()
           (skip-whitespace)
           (let ((c (peek)))
             (cond
               ((null c)        (%error "Unexpected end of input"))
               ((char= c #\")  (on-value handler (parse-raw-string)))
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
               (let ((c (read-char stream nil nil)))
                 (unless c
                   (%error "Unterminated string"))
                 (incf pos)
                 (cond
                   ((char= c #\")
                    (return (coerce buf 'string)))
                   ((char= c #\\)
                    (vector-push-extend (parse-escape) buf))
                   ((char< c #\Space)
                    (%error "Unescaped control character in string"))
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
               (let* ((c     (advance))
                      (digit (digit-char-p c 16)))
                 (unless digit
                   (%error (format nil "Invalid hex digit '~C' in \\uXXXX escape" c)))
                 (setf code (+ (* code 16) digit))))
             ;; Handle surrogate pairs (U+D800-U+DFFF)
             (if (and (>= code #xD800) (<= code #xDBFF))
                 (parse-surrogate-pair code)
                 (code-char code))))

         (parse-surrogate-pair (high)
           ;; Expect a low surrogate \uDC00-\uDFFF
           (let ((next1 (advance))
                 (next2 (advance)))
             (unless (and (char= next1 #\\) (char= next2 #\u))
               (%error "Expected low surrogate after high surrogate"))
             (let ((low 0))
               (dotimes (i 4)
                 (let* ((c     (advance))
                        (digit (digit-char-p c 16)))
                   (unless digit
                     (%error (format nil "Invalid hex digit '~C' in surrogate escape" c)))
                   (setf low (+ (* low 16) digit))))
               (unless (and (>= low #xDC00) (<= low #xDFFF))
                 (%error "Invalid low surrogate value"))
               (code-char (+ #x10000 (* (- high #xD800) #x400) (- low #xDC00))))))

         (parse-object ()
           (expect #\{)
           (begin-object handler)
           (skip-whitespace)
           (cond
             ((and (peek) (char= (peek) #\}))
              (advance)
              (end-object handler))
             (t
              (loop
                (skip-whitespace)
                (unless (and (peek) (char= (peek) #\"))
                  (%error "Expected string key in object"))
                (object-key handler (parse-raw-string))
                (skip-whitespace)
                (expect #\:)
                (parse-value)
                (skip-whitespace)
                (let ((next (peek)))
                  (cond
                    ((null next)      (%error "Unterminated object"))
                    ((char= next #\}) (advance) (end-object handler) (return))
                    ((char= next #\,) (advance))
                    (t (%error (format nil "Expected ',' or '}' in object, got '~C'" next)))))))))

         (parse-array ()
           (expect #\[)
           (begin-array handler)
           (skip-whitespace)
           (cond
             ((and (peek) (char= (peek) #\]))
              (advance)
              (end-array handler))
             (t
              (loop
                (parse-value)
                (skip-whitespace)
                (let ((next (peek)))
                  (cond
                    ((null next)      (%error "Unterminated array"))
                    ((char= next #\]) (advance) (end-array handler) (return))
                    ((char= next #\,) (advance))
                    (t (%error (format nil "Expected ',' or ']' in array, got '~C'" next)))))))))

         (parse-literal (chars value)
           (dolist (c chars)
             (let ((got (advance)))
               (unless (char= got c)
                 (%error (format nil "Invalid literal; expected '~C' got '~C'" c got)))))
           (on-value handler value))

         (parse-true  () (parse-literal '(#\t #\r #\u #\e)        t))
         (parse-false () (parse-literal '(#\f #\a #\l #\s #\e)    nil))
         (parse-null  () (parse-literal '(#\n #\u #\l #\l)        :null))

         (parse-number ()
           (let ((num-chars (make-array 32 :element-type 'character
                                           :adjustable t :fill-pointer 0)))
             ;; Optional leading minus
             (when (and (peek) (char= (peek) #\-))
               (vector-push-extend (advance) num-chars))
             ;; Integer part
             (cond
               ((and (peek) (char= (peek) #\0))
                (vector-push-extend (advance) num-chars))
               ((and (peek) (digit-char-p (peek)))
                (loop while (and (peek) (digit-char-p (peek)))
                      do (vector-push-extend (advance) num-chars)))
               (t (%error "Invalid number")))
             ;; Optional fractional part
             (let ((is-float nil))
               (when (and (peek) (char= (peek) #\.))
                 (setf is-float t)
                 (vector-push-extend (advance) num-chars)
                 (unless (and (peek) (digit-char-p (peek)))
                   (%error "Expected digit after decimal point"))
                 (loop while (and (peek) (digit-char-p (peek)))
                       do (vector-push-extend (advance) num-chars)))
               ;; Optional exponent
               (when (and (peek) (member (peek) '(#\e #\E)))
                 (setf is-float t)
                 (vector-push-extend (advance) num-chars)
                 (when (and (peek) (member (peek) '(#\+ #\-)))
                   (vector-push-extend (advance) num-chars))
                 (unless (and (peek) (digit-char-p (peek)))
                   (%error "Expected digit in exponent"))
                 (loop while (and (peek) (digit-char-p (peek)))
                       do (vector-push-extend (advance) num-chars)))
               (on-value handler
                          (let ((num-str (coerce num-chars 'string)))
                            (if is-float
                                (let ((*read-default-float-format* 'double-float))
                                  (read-from-string num-str))
                                (parse-integer num-str))))))))

      (parse-value)
      (skip-whitespace)
      (when (peek)
        (%error (format nil "Unexpected trailing content '~C'" (peek))))
      (handler-result handler))))

;;; ─── Public API ────────────────────────────────────────────────────────────────────────────

(defun %input->stream (input)
  "Coerce INPUT to a character stream for the JSON parser.
INPUT may be a string, a character stream, or a binary stream
(binary streams are wrapped with flexi-streams using UTF-8 encoding)."
  (cond
    ((stringp input)
     (make-string-input-stream input))
    ((and (streamp input)
          (subtypep (stream-element-type input) 'character))
     input)
    ((streamp input)
     ;; Binary / octet stream: wrap with flexi-streams for UTF-8 decoding.
     (flexi-streams:make-flexi-stream input :external-format :utf-8))
    (t
     (error 'json-parse-error
            :message (format nil "Cannot parse ~S: expected string or stream" input)
            :position 0))))

(defun parse (input)
  "Parse JSON from INPUT and return the corresponding Lisp value.

INPUT may be a string, a character stream, or a binary (octet) stream.
Binary streams are decoded as UTF-8 via flexi-streams.

Type mapping:
  JSON null   -> :null
  JSON true   -> t
  JSON false  -> nil
  JSON string -> string
  JSON number -> integer or double-float
  JSON array  -> vector
  JSON object -> hash-table with string keys (test: equal)"
  (%parse (%input->stream input) (make-instance 'json-tree-handler)))
