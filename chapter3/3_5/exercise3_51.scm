(load "streamfunc")

(define (show x)
  (display x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
