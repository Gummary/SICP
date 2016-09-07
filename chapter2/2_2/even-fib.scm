(load "seq_operations")
(load "fib")
		
(define (enumerate-interval low high)
 	(if (> low high)
	 	nil
		(cons low (enumerate-interval (+ low 1) high))))

(define (even-fibs n)
 	(accumulate cons
	 			nil
				(filter even? 
				 		(map fib
						 	 (enumerate-interval 0 n)))))
