(define (memq? items x)
  (cond ((null? items) false)
	((eq? (car items) x) true)
	(else (memq? (cdr items) x))))

(define (count-pairs items)
  (let ((encountered '()))
    (define (loop x)
      (if (or (not (pair? x)) (memq x encountered))
	0
	(begin
	  (set! encountered (cons x encountered))
	  (+ (loop (car x))
	     (loop (cdr x))
	     1))))
    (loop items)))
