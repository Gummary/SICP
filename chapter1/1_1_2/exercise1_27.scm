(define (expmod base exp n)
	(cond ((= exp 0) 1)
		((even? exp) 
			(remainder (square (expmod base (/ exp 2) n)) n ))
		(else 
			(remainder  (* base (expmod base (- exp 1) n)) n))))

(define (carmichael n)
	(define (try-it a)
		(= (expmod a n n ) a))
	(try-it (+ 1 (random (- n 1)))))