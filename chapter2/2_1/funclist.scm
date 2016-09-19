(define (cube x)
	(* x x x))

(define (square x)
	(* x x))

(define (identity x) x)

(define (close-enough? a b)
	(< (abs (- a b)) 0.00001))

(define (average a b)
	(/ (+ a b) 2))

(define inc
	(lambda (x)
		(+ x 1)))

(define dx 0.00001)

(define (fast-exp base exp)
	(cond ((= exp 0) 1)
		((even? exp) (square (fast-exp base (/ exp 2))))
		(else (* base (fast-exp base (- exp 1))))))

(define (lg n)
    (cond ((> (/ n 2) 1)
            (+ 1 (lg (/ n 2))))
          ((< (/ n 2) 1)
            0)
          (else
            1)))
