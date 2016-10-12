(load "streamfunc")
(load "exercise3_61")

(define (x stream)
  (stream-cons 1
	       (scale-stream (mul-series (stream-cdr stream)
					 (x stream)))))



