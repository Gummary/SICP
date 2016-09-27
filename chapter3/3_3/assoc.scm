(define (assoc key records)
  (cond ((null? records) false)
	((equal? (caar records) key)
	 (car records))
	(else (assoc key (cdr records)))))


