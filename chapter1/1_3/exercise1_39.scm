(load "exercise1_37")


(define (tan-cf x k)
	(cont-frac (lambda (i) (if (= i 1)
								x
								(- 0 (square x))))
				
				(lambda (i) (- (* 2 i) 1))
				k))