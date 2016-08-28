(load "exercise1_43")
(load "exercise1_36")

(define (average-damp f)
	(lambda (x)
		(average x (f x))))

; (define (average-damp-n-times f n)
;     ((repeated average-damp n) f))


(define (damp-n-times n)
	(repeated average-damp n))



; (define (sqrt x)
; 	(fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

; (define (cube-root x)
; 	(fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

; (define (four-root x)
; 	(fixed-point ((damp-n-times 2) (lambda (y) (/ x (* y y y)))) 1.0))

; (define (five-root x)
; 	(fixed-point ((damp-n-times 2) (lambda (y) (/ x (fast-exp y 4)))) 1.0))

; (define (six-root x)
; 	(fixed-point ((damp-n-times 2) (lambda (y) (/ x (fast-exp y 5)))) 1.0))

; (define (seven-root x)
; 	(fixed-point ((damp-n-times 2) (lambda (y) (/ x (fast-exp y 6)))) 1.0))

; (define (seven-root x)
; 	(fixed-point ((damp-n-times 2) (lambda (y) (/ x (fast-exp y 6)))) 1.0))

; (define (nth-root n damp-time)
; 	(fixed-point ((damp-n-times damp-time) (lambda (y) (/ (fast-exp 2 n) (fast-exp y (- n 1))))) 1.0))


(define (n-root n x)
	(fixed-point ((damp-n-times (lg n)) (lambda (y) (/ x (fast-exp y (- n 1))))) 1.0))