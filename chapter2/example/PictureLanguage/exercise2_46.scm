(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))


(define (add-vect vector1 vector2)
  (make-vect (+ (xcor-vect vector1)
		(xcor-vect vector2))
	     (+ (ycor-vect vector1)
		(ycor-vect vector2))))

(define (sub-vect vector1 vector2)
  (make-vect (- (xcor-vect vector1)
		(xcor-vect vector2))
	     (- (ycor-vect vector1)
		(ycor-vect vector2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))
