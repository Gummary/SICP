(load "exercise2_18")

(define (same-parity first . other)
	(define (iter items  ans)
		(cond ((null? items) (reserve ans))
			((or (r (car items) 0) (r (car items) 1)) 
				(iter (cdr items) (cons (car items) ans))) 
			(else (iter (cdr items) ans))))
	(define (r i num)
		(and (= (remainder i 2) num) 
			(= (remainder first 2) num)))
	(iter other (list first)))

