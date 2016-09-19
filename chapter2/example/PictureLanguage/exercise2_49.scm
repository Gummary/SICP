(load "exercise2_46")
(load "exercise2_47")

(define lb (make-vect 0 0))
(define lt (make-vect 0 1))
(define rb (make-vect 1 0))
(define rt (make-vect 11))


;; a)

(segments->painter (list 
		     (make-vect lb lt)
		     (make-vect rb rt)
		     (make-vect lb rb)
		     (make-vect lt rt)))

;; b)

(segments->painter (list
		     (make-vect lt rb)
		     (make-vect lb rt)))

;; c)

 (let ((l (make-vect 0 0.5)) 
       (t (make-vect 0.5 1))
       (r (make-vect 1 0.5)) 
       (b (make-vect 0.5 0))) 
   
(segments->painter (list 
		     (make-segment l t) 
		     (make-segment t r) 
		     (make-segment r b) 
		     (make-segment b l)))) 
