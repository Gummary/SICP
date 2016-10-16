(load "exercise3_67")

(define (triples s t u)
  (cons-stream (list (stream-car s)
		     (stream-car t)
		     (stream-car u))
	       (interleave (stream-map (lambda (x)
					 (list x
					       (pairs t u)))
				       (stream-cdr s))
			   (triples (stream-cdr s)
				    (stream-cdr t)
				    (stream-cdr u)))))

(define 
