(define (install-polynomial-package)
  ;; ...
  (define (=zero? poly)
    (define (poly? x)
      (pair? x))
    (cond ((number? x) (= x 0))
	  ((poly? x) false)
	  (else (error "Unknown type"))))
  (put '=zero? '(polynomial) =zero?))



