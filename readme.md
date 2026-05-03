# cl-json

A JSON reader and writer for Common Lisp. It also provides automatic JSON
encoding/decoding derivations for CLOS classes via `cl-sdk/meta-definitions`.

## Installation

Add `cl-json` to your system's `:depends-on` list. The system requires
`meta-definitions`, `closer-mop`, and `flexi-streams`.

```lisp
(ql:quickload :cl-json)
```

The package is also accessible through its nickname `json`.

## Type mapping

### JSON → Lisp (`parse`)

| JSON       | Lisp                                     |
|------------|------------------------------------------|
| `null`     | `:null`                                  |
| `true`     | `t`                                      |
| `false`    | `nil`                                    |
| string     | `string`                                 |
| integer    | `integer`                                |
| float      | `double-float`                           |
| array      | `vector`                                 |
| object     | `hash-table` (string keys, `equal` test) |

### Lisp → JSON (`stringify`)

| Lisp                    | JSON                               |
|-------------------------|------------------------------------|
| `:null`                 | `null`                             |
| `t`                     | `true`                             |
| `nil`                   | `false`                            |
| `string`                | JSON string                        |
| `integer`               | integer number                     |
| `float`                 | floating-point number              |
| `ratio`                 | floating-point number (coerced)    |
| `keyword` / `symbol`    | JSON string (downcased symbol name)|
| list (non-nil)          | JSON array                         |
| `vector`                | JSON array                         |
| `hash-table`            | JSON object (keys sorted)          |
| CLOS object             | JSON object (via `encode-json`)    |

## Public API

### `parse input` → value

Parse JSON from `input` and return the corresponding Lisp value.

`input` may be a string, a character stream, or a binary (octet) stream.
Binary streams are decoded as UTF-8 via `flexi-streams`.

```lisp
(json:parse "42")             ; => 42
(json:parse "[1,2,3]")        ; => #(1 2 3)
(json:parse "{\"a\":1}")      ; => #<hash-table "a"→1>
(json:parse "null")           ; => :null
```

### `stringify value &key pretty indent stream` → string or nil

Encode `value` as JSON.

- If `stream` is `nil` (the default), return a JSON string.
- If `stream` is a character or binary stream, write JSON to it and return `nil`.

Keyword arguments:

| Argument | Default | Description |
|----------|---------|-------------|
| `pretty` | `nil`   | Emit indented, human-readable JSON when non-nil. |
| `indent` | `"  "`  | String used as one indentation level. |
| `stream` | `nil`   | Write to this stream instead of returning a string. |

Binary streams are encoded as UTF-8 via `flexi-streams`.

```lisp
(json:stringify #(1 2 3))              ; => "[1,2,3]"
(json:stringify t)                     ; => "true"
(json:stringify :null)                 ; => "null"
(json:stringify '(1 2 3) :pretty t)   ; => "[\n  1,\n  2,\n  3\n]"

;; Write directly to a stream:
(json:stringify "hello" :stream *standard-output*)
```

### `derive-json class-name &key slots`

Macro that automatically derives JSON encoding and decoding for a CLOS class.

```lisp
(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y)))

;; Use downcased slot names as JSON keys:
(json:derive-json point)

;; Override individual key names:
(json:derive-json point
  :slots ((x :key "px")
          (y :key "py")))
```

`derive-json` generates:

- An `encode-json` method — called by `stringify` when the value is a `point` instance.
- A `from-json/<class-name>` function — constructs an instance from a parsed hash-table.
- A `decode-json` method dispatching on `(eql 'class-name)`.

```lisp
(let ((p (make-instance 'point :x 1 :y 2)))
  (json:stringify p))
; => "{\"x\":1,\"y\":2}"

(json:decode-json 'point (json:parse "{\"x\":3,\"y\":4}"))
; => #<point x=3 y=4>
```

### `encode-json value stream` (generic function)

Called by `stringify` for CLOS objects that don't have a built-in mapping.
Specialise this generic to add JSON support for custom types.

```lisp
(defmethod json:encode-json ((v my-type) stream)
  (json:stringify (my-type->hash-table v) :stream stream))
```

### `decode-json type value` (generic function)

Converts a parsed JSON value (typically a hash-table) into a Lisp object of
the given type. `derive-json` adds methods automatically; you can also add them
manually.

```lisp
(defmethod json:decode-json ((type (eql 'my-type)) value)
  (make-instance 'my-type :field (gethash "field" value)))
```

## Error conditions

All conditions inherit from `json-error`.

| Condition              | Readers                                              | Signalled when                        |
|------------------------|------------------------------------------------------|---------------------------------------|
| `json-parse-error`     | `json-parse-error-message`, `json-parse-error-position` | Input is not valid JSON             |
| `json-encode-error`    | `json-encode-error-message`                          | A value cannot be encoded as JSON     |
| `json-decode-error`    | `json-decode-error-message`                          | No decoder is registered for a type  |

```lisp
(handler-case (json:parse "{bad}")
  (json:json-parse-error (e)
    (format t "Parse error at ~A: ~A"
            (json:json-parse-error-position e)
            (json:json-parse-error-message  e))))
```

## Stream support

`parse` and `stringify` both accept binary (octet) streams. Binary streams are
transparently wrapped using `flexi-streams` with UTF-8 encoding.

```lisp
;; Parse from a binary stream:
(let* ((bytes  (flexi-streams:string-to-octets "[1,2]" :external-format :utf-8))
       (stream (flexi-streams:make-in-memory-input-stream bytes)))
  (json:parse stream))   ; => #(1 2)

;; Stringify to a binary stream:
(let ((out (flexi-streams:make-in-memory-output-stream)))
  (json:stringify "hello" :stream out)
  (flexi-streams:get-output-stream-sequence out))
```

## Event handler protocol

For low-level SAX-style parsing without building an intermediate Lisp tree,
subclass `json-handler` and specialise the event generics:

| Generic          | Called when                                 |
|------------------|---------------------------------------------|
| `begin-object`   | A JSON object `{` is opened.                |
| `object-key`     | A key string inside an object is read.      |
| `end-object`     | A JSON object `}` is closed.                |
| `begin-array`    | A JSON array `[` is opened.                 |
| `end-array`      | A JSON array `]` is closed.                 |
| `on-value`       | A primitive JSON value is read.             |
| `handler-result` | Called after parsing to retrieve the result.|

All generics have no-op default methods on `json-handler`; subclasses need only
specialise the methods they care about.

```lisp
(defclass key-collector (json:json-handler)
  ((keys :initform nil :accessor collected-keys)))

(defmethod json:object-key ((h key-collector) key)
  (push key (collected-keys h)))

(defmethod json:handler-result ((h key-collector))
  (nreverse (collected-keys h)))

;; Drive the parser with a custom handler using internal helpers:
(let ((h (make-instance 'key-collector)))
  (json::%parse (json::%input->stream "{\"a\":1,\"b\":2}") h))
; => ("a" "b")
```
