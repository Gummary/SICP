; (define (make-rat n d)
; 	(let ((g (gcd (abs n) (abs d))))
; 		(cond 
; 			((< (* n d) 0) 
; 				(cons (- 0 (abs (/ n g))) (abs (/ d g))))
; 			(else 
; 				(cons (abs (/ n g)) (abs (/ d g)))))))

(define (make-rat n d)
    (if (< d 0)
        (cons (- n) (- d))
        (cons n d)))