(define nil '())

; Map
;(load "map")

; Filter

(define (filter predicate sequence)
	(cond ((null? sequence) '())
	 	((predicate (car sequence))
			 (cons (car sequence) (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))


;Accumulate

(define (accumulate op initial seq)
	(if (null? seq)
	 	initial	
		(op (car seq)
		 	(accumulate op initial (cdr seq)))))
