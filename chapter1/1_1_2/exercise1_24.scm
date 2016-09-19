(load "fermat.scm")

(define (next-odd n)
	(if (even? n) (+ n 1)
		(+ n 2)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (real-time-clock) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes base count)
	(cond ((= count 0) (display "End"))
		((fast-prime? base 10) 
			(timed-prime-test base) 
			(search-for-primes (next-odd base) (- count 1)))
		(else (search-for-primes (next-odd base) count))))