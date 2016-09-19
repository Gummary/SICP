; (define (iter-improve good-enough? improve)
; 	(lambda (x)
; 		(define (iter guess)
; 			(if (good-enough? guess)
; 				guess
; 				(iter (improve guess))))
; 		(iter x)))


(define (iter good-enough? improve)
	(lambda (x)
		(if (good-enough? x)
			x
			((iter good-enough? improve) (improve x)))))


; (define (sqrt x)
; 	(define (good-enough? guess)
; 		(< (abs (- x (square guess))) 0.0001))
; 	(define (improve guess)
; 		(/ (+ guess x) 2))
; 	((iter-improve good-enough? improve) 1))

(define tolerance 0.00001)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (f guess)
    (average guess (/ x guess)))
  ((iter good-enough? f) 1.0))


(define (fixed-point f first-guess)
	(define (good-enough? guess)
		(< (abs (- (f guess) guess)) tolerance))
	(define (improve guess)
		(f guess))
	((iter good-enough? improve) first-guess))

