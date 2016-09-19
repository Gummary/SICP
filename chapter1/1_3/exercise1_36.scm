(load "funclist")

(define tolerance 0.0001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try-guess guess)
		(let ((next (f guess)))
			(display "Guess:")
			(display guess)
			(display "  Next:")
			(display next)
			(newline)
			(if (close-enough? guess next)
				next
				(try-guess next))))
	(try-guess first-guess))
; (fixed-point (lambda (x) (/ (log 1000) (log x)))
; 					2.0)


