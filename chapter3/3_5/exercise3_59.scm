(load "streamfunc")

(define (integers-series stream)
  (define (iter stream n)
    (cons-stream (* (/ 1 n) 
		    (stream-car stream))
		 (iter (stream-cdr stream)
		       (+ n 1))))
  (iter stream 1))


(define (integers-series coeffs)
  (stream-map / coeffs integers))


(define cosine-series
  (cons-stream 1
	       (integers-series sine-series)))

(define sine-series
  (cons-stream 0
	       (scale-stream (integers-series cosine-series) -1)))

