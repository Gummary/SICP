(load "monte-carlo")


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define square
  (lambda (x) (* x x)))


(define (get-area x1 x2 y1 y2)
  (* (- x2 x1)
     (- y2 y1)))

(define (P x y)
  (<= (+ (square x) (square y)) 1))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (* (get-area x1 x2 y1 y2) (monte-carlo trials experiment)))
  

