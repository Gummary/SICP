(load "interval_arithmetic")

(define (make-center-percent c p)
	(make-interval (- c (* c (* p 0.01))) (+ c (* c (* p 0.01)))))


(define (percent c)
	(* (width c) 100))
