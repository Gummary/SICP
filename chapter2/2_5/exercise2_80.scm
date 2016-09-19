(define (install-scheme-number-package)
  ;; ...
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  'done)

(define (install-rational-package)
  ;; ...
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  'done)

(define (install-complex-package)
  ;; ...
  (put '=zero? '(complex)
       (lambda (x) (= (real-part x) (imag-part x) 0)))
  'done)

