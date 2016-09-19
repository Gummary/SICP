(define (cont-frac n d k)
	(define (calculate i)
		(if (= i k)
			(/ (n i) (d i))
			(/ (n i) (+ (d i) (calculate (+ i 1))))))
	 (calculate 1))

(define (cont-frac n d k)
	(define (calculate i result)
		(if (= i 0)
			result
			(calculate (- i 1) (/ (n i) (+ (d i) result)))))
	(calculate k 0))

