(car (cons x y))

(car (lambda (m) (m x y)))

((lambda (m) (m x y)) 
	((lambda (p q)) p))

(x)


(define (cdr z)
	(z (lambda (p q) (q))))