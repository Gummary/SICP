(load "seq_operations")

(define (map proc sequence)
 	(accumulate (lambda (x y)
	 	(cons (proc x) y))	
		nil	
		sequence))

(define (my-append list1 list2)
	(accumulate cons
		    list2
		    list1))
	

(define (length sequence)
 	(accumulate (lambda (x y) 
			(+ 1 y))
	 		0
	 		sequence))
