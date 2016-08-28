(load "exercise1_42")


(define (repeated f n)
	(if (= n 1)
		f
		(compose f (repeated f (- n 1)))))

(define (repeated f n)
	(define (iter repeated-f index)
		(if (= index 1)
			repeated-f
			(iter (compose f repeated-f) (- index 1))))
	(iter f n))

