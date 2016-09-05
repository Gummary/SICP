(define (reserve items)
	(define (iter l ans)
		(if (null? l)
			ans
			(iter (cdr l) (cons (car l) ans))))
	(iter items '()))
