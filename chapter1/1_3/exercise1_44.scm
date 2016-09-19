(load "exercise1_43")

(define (smooth f)
	(lambda (x)
		(/ (+ (f x) 
			(f (+ x dx)) 
			(f (- x dx))) 
		3)))


(define (smooth-n-times f n)
	(let ((smooth-n (repeated smooth n)))
		(smooth-n f)))