(load "funclist.scm")


; a
(define (addone a)
	(+ a 1))

(define (product term a next b)
	(if (> a b)
		1
		(* (term a)
			(product term (next a) next b))))

(define (factorial n)
	(product identity 1 addone n))

(define (pi)
	(define (getvalue a)
		(cond 
			((even? a)
				(exact->inexact(/ a (+ a 1))))
			(else 
				(exact->inexact(/ (+ a 1) a)))))
	(exact->inexact(* (product getvalue 2 addone 10000) 4)))
	; (product getvalue 2 addone 100))


; b
(define (product term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (term a)))))
	(iter a 1))