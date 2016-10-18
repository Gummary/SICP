(load "streamfunc")
(load "intergral")

(define zero-crossings
  (stream-map sign-change-dector sense-data (cons-stream 0 sen-data)))

