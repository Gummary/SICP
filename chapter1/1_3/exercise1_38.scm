(load "exercise1_37")

(cont-frac (lambda (i) 1) 
	(lambda (i) 
		(cond ((= (remainder i 3) 0) 1)
			((= (remainder i 3) 1) 1)
			((= (remainder i 3) 2) (/ (* 2 (+ i 1)) 3)))) 100.0)