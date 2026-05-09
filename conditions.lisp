(in-package #:io.github.cl-sdk.json)

(define-condition json-error (error) ())

(define-condition json-parse-error (json-error)
  ((message  :initarg :message  :reader json-parse-error-message)
   (position :initarg :position :reader json-parse-error-position))
  (:report (lambda (condition stream)
             (format stream "JSON parse error at position ~A: ~A"
                     (json-parse-error-position condition)
                     (json-parse-error-message  condition)))))

(define-condition json-encode-error (json-error)
  ((message :initarg :message :reader json-encode-error-message))
  (:report (lambda (condition stream)
             (format stream "JSON encode error: ~A"
                     (json-encode-error-message condition)))))

(define-condition json-decode-error (json-error)
  ((message :initarg :message :reader json-decode-error-message))
  (:report (lambda (condition stream)
             (format stream "JSON decode error: ~A"
                     (json-decode-error-message condition)))))
