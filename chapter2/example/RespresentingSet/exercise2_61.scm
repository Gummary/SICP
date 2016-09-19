(load "ordered")

(define (adjoin-set x set)
  (if (element-of-set x set)
    set
    (cond ((null? set) (list x))
	  ((> (car set) x) (cons x set))
	  ((< (car set) x) (cons (car set) (adjoin-set x (cdr set))))))


