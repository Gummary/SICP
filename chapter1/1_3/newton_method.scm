(load "fixedpoint")

(define dx 0.00001)

(define (deriv g)
	(lambda (x)
		(/ (- (g (+ dx x)) (g x)) 
			dx)))

(define (newton-transform g)
	(lambda (x)
		(- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
	(fixed-point (newton-transform g) 
		guess))


(define (sqrt x)
	(newtons-method (lambda (x) (* x x)) 1.0))