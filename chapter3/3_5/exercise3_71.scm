(load "exercise3_70")

(define (ramanujan-numbers)
  (define (sum x)
    (let ((a (car x))
	  (b (cadr x)))
      (+ (* a a a) (* b b b))))
  (define (ramanujan triples)
    (let ((current (stream-car triples))
	  (next (stream-car (stream-cdr triples))))
      (cond ((= (sum current)
		(sum next))
	     (cons-stream (sum current)
			  (ramanujan (stream-cdr (stream-cdr triples)))))
	    (else (ramanujan (stream-cdr triples))))))
  (ramanujan (weighted-pairs integers
			     integers
			     sum)))

