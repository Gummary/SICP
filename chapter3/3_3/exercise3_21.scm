(load "queue")

(define (print-queue queue)
  (define (iter q)
    (cond  ((null? q)
	    'done)
	   (else 
	     (display (car q))
	     (iter (cdr q)))))
  (iter (front-ptr queue)))
      
