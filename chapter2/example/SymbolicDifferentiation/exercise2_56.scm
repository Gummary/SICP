(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))


(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation u n)
  (cond ((=number? u 1) 1)
    	((=number? n 0) 1)
	((=number? n 1) u)
	(else (list '** u n))))


(cond ((exponentiation? exp)
       (make-product 
	 (make-product (exponent exp)
		       (make-exponentiation (base exp)
					    (if (number? exponent exp))
					    (- (exponent exp) 1)
					    (' (- (exponent exp) 1))))
	 (deriv (base exp) var))))
