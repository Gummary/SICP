(load "exercise3_70")

(define (square-numbers)
  (define (sum x)
    (let ((a (car x))
	  (b (cadr x)))
      (+ (* a a)  (* b b))))
  (define (numbers triples)
    (let ((current (stream-car triples))
	  (next (stream-car (stream-cdr triples)))
	  (third (stream-car (stream-cdr (stream-cdr triples)))))
      (cond ((= (sum current)
		(sum next)
		(sum third))
	     (cons-stream (list (sum current) current next third)
			  (numbers (stream-cdr (stream-cdr (stream-cdr triples))))))
	    (else (numbers (stream-cdr triples))))))
  (numbers (weighted-pairs integers
			     integers
			     sum)))

