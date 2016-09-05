(load "map.scm")

(define (square x)
	(* x x))

(define (square-list items)
	(map items
		(lambda (x) (square x))))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))