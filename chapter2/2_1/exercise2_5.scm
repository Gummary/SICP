(load "funclist.scm")

(define (cons x y)
	(* (fast-exp 2 x)
		(fast-exp 3 y)))

(define (car c)
	(define (iter v result)
		(if (= (remainder v 2) 1)
			result
			(iter (/ v 2) (+ result 1))))
	(iter c 0))

(define (cdr c)
	(define (iter v result)
		(if (= (remainder v 3) 1)
			result
			(iter (/ v 3) (+ result 1))))
	(iter c 0))