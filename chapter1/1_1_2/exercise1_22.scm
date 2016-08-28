(load "smallest_divisor.scm")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time-clock) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))



(define (next-odd n)
	(if (even? n) (+ n 1)
		(+ n 2)))

(define (search-for-primes base count)
	(cond ((= count 0) (display "End"))
		((prime? base) 
			(timed-prime-test base)
			(search-for-primes (next-odd base) (- count 1)))
		(else (search-for-primes (next-odd base) count))))
