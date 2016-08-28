(load "funclist.scm")

(define (search f neg-point pos-point)
	(let ((midpoint (average neg-point pos-point)))
		(if (close-enough? neg-point pos-point)
			midpoint
			(let ((test-value (f midpoint)))
				(cond ((positive? test-value) 
						(search f neg-point midpoint))
					((negative? test-value) 
						(search f midpoint pos-point))
					(else midpoint))))))


(define (half-interval-method f a b)
	(let ((a-value (f a))
		(b-value (f b)))
	(cond 
		((and (positive? a-value) (negative? b-value)) 
			(search f b a))
		((and (positive? b-value) (negative? a-value))
			(search f a b))
		(else 
			(error "Value are not of opposit sign" a b)))))