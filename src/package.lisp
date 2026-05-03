(defpackage #:cl-json
  (:nicknames #:json)
  (:use #:cl)
  (:export
   ;; Reader
   #:parse
   ;; SAX-style reader protocol
   #:parse-sax
   #:json-sax-handler
   #:json-tree-handler
   #:sax-value
   #:sax-begin-object
   #:sax-object-key
   #:sax-end-object
   #:sax-begin-array
   #:sax-end-array
   #:sax-result
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
