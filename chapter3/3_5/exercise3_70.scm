(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
		  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      (weight))))

;; a

(define qa
  (weighted-pairs integers 
		  integers
		  (lambda (x) (+ (car x) (cadr x)))))

;; b

(define qb
  (weighted-pairs (stream-filters 
		    (lambda (x) (or (= (remainder x 2) 0)
				    (= (remainder x 3) 0)
				    (= (remainder x 5) 0)))
		    integers)
		  (stream-filters 
		    (lambda (x) (or (= (remainder x 2) 0)
				    (= (remainder x 3) 0)
				    (= (remainder x 5) 0)))
		    integers)
		  (lambda (x) (+ (* 2 j) (* 3 j) (* 5 i j)))))


