(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless-predicate clause)
  (cadr clause))

(define (unless-consequent clause)
  (caddr clause))

(define (unless-alternative clause)
  (cadddr clause))

(define (unless->if exp)
  (make-if (unless-predicate exp)
	   (unless-alternative exp)
	   (unless-consequent exp)))


((unless? exp) (analyze (unless->if exp)))

