(load "seq_operations")

(define (map proc sequence)
 	(accumulate (lambda (x y)
				 	(cons (proc x) y))
	 			nil
				sequence))


(define (length sequence)
 	(accumulate (lambda (x y)
				 (+ 1 y))
	 			0
	 			sequence))
