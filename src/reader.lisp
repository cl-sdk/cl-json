(in-package #:cl-json)

;;; JSON → Lisp type mapping
;;;
;;;   null    → :null
;;;   true    → t
;;;   false   → nil
;;;   string  → string
;;;   number  → integer or double-float
;;;   array   → list
;;;   object  → hash-table (test: equal, keys: strings)

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
               ((char= c #\")  (parse-string))
               ((char= c #\{)  (parse-object))
               ((char= c #\[)  (parse-array))
               ((char= c #\t)  (parse-true))
               ((char= c #\f)  (parse-false))
               ((char= c #\n)  (parse-null))
               ((or (char= c #\-) (digit-char-p c)) (parse-number))
               (t (%error (format nil "Unexpected character '~C'" c))))))

         (parse-string ()
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
           (skip-whitespace)
           (let ((table (make-hash-table :test 'equal)))
             (cond
               ((and (peek) (char= (peek) #\}))
                (advance)
                table)
               (t
                (loop
                  (skip-whitespace)
                  (unless (and (peek) (char= (peek) #\"))
                    (%error "Expected string key in object"))
                  (let ((key (parse-string)))
                    (skip-whitespace)
                    (expect #\:)
                    (setf (gethash key table) (parse-value)))
                  (skip-whitespace)
                  (let ((next (peek)))
                    (cond
                      ((null next)          (%error "Unterminated object"))
                      ((char= next #\})     (advance) (return table))
                      ((char= next #\,)     (advance))
                      (t (%error (format nil "Expected ',' or '}' in object, got '~C'" next))))))))))

         (parse-array ()
           (expect #\[)
           (skip-whitespace)
           (cond
             ((and (peek) (char= (peek) #\]))
              (advance)
              nil)
             (t
              (let ((items nil))
                (loop
                  (push (parse-value) items)
                  (skip-whitespace)
                  (let ((next (peek)))
                    (cond
                      ((null next)          (%error "Unterminated array"))
                      ((char= next #\])     (advance) (return (nreverse items)))
                      ((char= next #\,)     (advance))
                      (t (%error (format nil "Expected ',' or ']' in array, got '~C'" next))))))))))

         (parse-literal (chars value)
           (dolist (c chars)
             (let ((got (advance)))
               (unless (char= got c)
                 (%error (format nil "Invalid literal; expected '~C' got '~C'" c got)))))
           value)

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
               (let ((num-str (subseq string start pos)))
                 (if is-float
                     (let ((*read-default-float-format* 'double-float))
                       (read-from-string num-str))
                     (parse-integer num-str)))))))

      (let ((result (parse-value)))
        (skip-whitespace)
        (when (< pos len)
          (%error (format nil "Unexpected trailing content '~C'" (char string pos))))
        result))))
