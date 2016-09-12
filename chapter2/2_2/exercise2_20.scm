(load "funclist")

(define (same-parity first . other)
  (define (iter items  ans)
    (cond ((null? items) ans)
	  ((r (car items)) 
	   (iter (cdr items) (append ans (list (car items))))) 
	  (else (iter (cdr items) ans))))
  (define (r i)
    (= (remainder i 2) (remainder first 2)))
  (iter other (list first)))

