(defpackage #:io.github.cl-sdk.json
  (:nicknames #:json)
  (:use #:cl)
  (:export
   ;; Reader
   #:parse
   ;; Writer
   #:stringify
   ;; Conditions
   #:json-error
   #:json-parse-error
   #:json-parse-error-message
   #:json-parse-error-position
   #:json-encode-error
   #:json-encode-error-message
   #:json-decode-error
   #:json-decode-error-message
   ;; Encoding / decoding protocol
   #:encode-json
   #:decode-json
   ;; Derive macro
   #:derive-json))
