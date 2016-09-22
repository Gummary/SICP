(define (make-accumulator x)
  (lambda (n)
      (begin (set! x (+ x n))
	     x)))
