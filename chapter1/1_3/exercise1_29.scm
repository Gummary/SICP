(load "sum.scm")

; (define (simpson-integral f a b n)
; 	(define h (/ (- b a) n))
; 	(define (getk at)
; 		(/ (- at a) h))
; 	(define (fun at)
; 		(cond 
; 			((or (= (getk at) 0) (= (getk at) n))
; 				(f (+ a (* (getk at) h))))
; 			((even? (getk at))
; 				(* (f (+ a (* (getk at) h))) 2))
; 			(else 
; 				(* (f (+ a (* (getk at) h))) 4)))) 
; 	(define (addh at)
; 		(+ at h))
; 	(* (sum fun a addh b) (exact->inexact(/ h 3))))

; 

(define (simpson-integral f a b n)
	(define h (/ (- b a) n))
	(define (func k)
		(f (+ a (* k h))))
	(define (getyk k)
		(cond ((or (= k 0) (= k n)) (func k))
			((= (even? k) 0) (* 2 (func k)))
			(else (* 4 (func k)))))
	(define (addone k)
		(+ k 1))
	(sum getyk (exact->inexact 0) addone n))