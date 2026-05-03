(in-package #:cl-json)

;;; derive-json — automatically derive JSON encoding and decoding for a CLOS class.
;;;
;;; The macro integrates with cl-sdk/meta-definitions by registering itself as a
;;; named derivation so that classes can opt-in via the standard derive mechanism.
;;;
;;; Usage:
;;;
;;;   (defclass point ()
;;;     ((x :initarg :x :accessor point-x)
;;;      (y :initarg :y :accessor point-y)))
;;;
;;;   ;; Derive using slot names as JSON keys (downcased):
;;;   (derive-json point)
;;;
;;;   ;; Derive with explicit slot list and optional key overrides:
;;;   (derive-json point
;;;     :slots ((x :key "px")
;;;             (y :key "py")))
;;;
;;; The macro defines:
;;;   • An ENCODE-JSON method        — used by STRINGIFY.
;;;   • A FROM-JSON/<class> function — converts a parsed hash-table to an instance.
;;;   • A DECODE-JSON method         — dispatches on (eql 'class-name).

(defgeneric decode-json (type value)
  (:documentation
   "Decode the parsed JSON VALUE (typically a hash-table) into an object of TYPE.
TYPE is a symbol naming the target class.  Add methods via DERIVE-JSON."))

(defmethod decode-json (type value)
  (declare (ignore value))
  (error 'json-decode-error
         :message (format nil "No JSON decoder registered for type ~S" type)))

;;; ── helpers ─────────────────────────────────────────────────────────────────

(defun %slot-json-key (slot-name options)
  "Return the JSON key string for SLOT-NAME, honouring :KEY in OPTIONS."
  (let ((explicit (getf options :key)))
    (if explicit
        explicit
        (string-downcase (symbol-name slot-name)))))

(defun %all-slot-names (class-name)
  "Return the names of all slots defined on CLASS-NAME using the MOP."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots
           (closer-mop:ensure-finalized (find-class class-name)))))

;;; ── macro ────────────────────────────────────────────────────────────────────

(defmacro derive-json (class-name &key slots)
  "Derive JSON encoding and decoding for CLASS-NAME.

SLOTS is an optional list of slot descriptors, each of the form:

  slot-name
  (slot-name &key key)

where :KEY overrides the JSON key string (default: downcased slot name).
If SLOTS is omitted, all slots reported by the MOP are used.

Generated definitions:
  ENCODE-JSON method on (instance <class-name>)   — called by STRINGIFY
  FROM-JSON/<class-name> (hash-table)             — convenience constructor
  DECODE-JSON method on (eql '<class-name>)       — called by users"
  (let* (;; Normalise to ((slot-name . plist) ...) at macro-expansion time.
         (slot-specs (if slots
                         (mapcar (lambda (s)
                                   (if (consp s)
                                       (cons (first s) (rest s))
                                       (cons s nil)))
                                 slots)
                         ;; Discover slots via MOP now (class must be defined).
                         (mapcar (lambda (sname) (cons sname nil))
                                 (%all-slot-names class-name))))
         (instance-var (gensym "INSTANCE"))
         (stream-var   (gensym "STREAM"))
         (ht-var       (gensym "TABLE"))
         (from-json-fn (intern (format nil "FROM-JSON/~A" class-name)
                               (symbol-package class-name))))
    `(progn
       ;; ── encoder ─────────────────────────────────────────────────────────
       (defmethod encode-json ((,instance-var ,class-name) ,stream-var)
         (let ((,ht-var (make-hash-table :test 'equal)))
           ,@(mapcar (lambda (spec)
                       (let ((sname (car spec))
                             (key   (%slot-json-key (car spec) (cdr spec))))
                         `(when (slot-boundp ,instance-var ',sname)
                            (setf (gethash ,key ,ht-var)
                                  (slot-value ,instance-var ',sname)))))
                     slot-specs)
           (%encode ,ht-var ,stream-var)))

       ;; ── decoder ─────────────────────────────────────────────────────────
       (defun ,from-json-fn (,ht-var)
         ,(format nil "Construct a ~A from a parsed JSON hash-table." class-name)
         (let ((,instance-var (make-instance ',class-name)))
           ,@(mapcar (lambda (spec)
                       (let ((sname (car spec))
                             (key   (%slot-json-key (car spec) (cdr spec))))
                         `(let ((v (gethash ,key ,ht-var)))
                            (when v
                              (setf (slot-value ,instance-var ',sname) v)))))
                     slot-specs)
           ,instance-var))

       (defmethod decode-json ((type (eql ',class-name)) ,ht-var)
         (,from-json-fn ,ht-var))

       ;; ── cl-sdk/meta-definitions integration ─────────────────────────────
       ;; Register the derivation so the cl-sdk ecosystem can discover that
       ;; CLASS-NAME derives :json.  The call is conditional so that loading
       ;; cl-json standalone (without meta-definitions) still works.
       (when (fboundp 'meta-definitions:register-derivation)
         (meta-definitions:register-derivation ',class-name :json))

       ',class-name)))
