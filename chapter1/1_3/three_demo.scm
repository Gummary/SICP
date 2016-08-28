(define (dum-integers a b)
	(if (> a b)
		0
		(+ a (dum-integers (+ a 1) b))))

(define (sum-cubes a b)
	(if (> a b)
		(+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))