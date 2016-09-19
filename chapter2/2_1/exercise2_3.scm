(define (rect-perimeter rect)
	(+ 
		(* 2 (rect-length rect)) 
		(* 2 (rect-width rect))))

(define (rect-area rect)
	(* (rect-length rect)
		(rect-width rect)))

(define (make-rect lt-point rb-point)
	(cons lt-point rb-point))

; (define (rect-width rect)
; 	(- (cdr (car rect)) (cdr (cdr rect))))

(define (rect-width rect)
	(let ((lt-point (car rect))
		(rb-point (cdr rect)))
	(- (cdr lt-point) (cdr (rb-point)))))

(define (rect-length rect)
	(- (car (car rect)) (car (cdr rect))))

(define (make-rect left-seg bottom-seg)
	(cons left-seg bottom-seg))

; (define (rect-width rect)
; 	(let ((seg-start-y (cdr (car (car rect))))
; 		(seg-end-y (cdr (cdr (car rect)))))
; 	(- seg-end-y seg-start-y)))

(define (rect-width rect)
	(- (y-point (start-segment (car rect))) 
		(y-point (start-segment (cdr rect)))))

(define (rect-length rect)
	(let ((seg-start-x (car (car (cdr rect))))
		(seg-end-x (car (cdr (cdr rect)))))
	(- seg-end-x seg-start-x)))