(load "funclist")

(define tolerance 0.0001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try-guess guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try-guess next))))
	(try-guess first-guess))


; calculate (sqrt x) by using fixed-point

(define (sqrt x)
	(fixed-point (lambda (y) (average (/ x y) y))
		5.0))