(define (average a b)
	(/ (+ a b) 2))

(define (make-segment start-point end-point)
	(cons start-point end-point))


(define (start-segment seg)
	(car seg))

(define (end-segment seg)
	(cdr seg))

(define (make-point x y)
	(cons x y))

(define (x-point point)
	(car point))

(define (y-point point)
	(cdr point))

(define (mid-point seg)
	(let ((sp (start-segment seg))
		(ep (end-segment seg)))
	(make-point (average 	(x-point sp) 
							(x-point ep))
				(average 	(y-point sp) 
							(y-point ep)))))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

