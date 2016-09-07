(load "flatmap")
(load "even-fib")

(define (smallest-divisor n)
    (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	          ((divides? test-divisor n) test-divisor)
		          (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
    (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap (lambda (i)
			  (map (lambda (j)
				 (list i j))
			       (enumerate-interval 1 (- i 1))))
			(enumerate-interval 1 n)))))
