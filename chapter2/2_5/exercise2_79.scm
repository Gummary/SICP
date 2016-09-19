(define (equ? x y)
  (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  ;; ...
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  'done)

(define (install-rational-package)
  ;; ...
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (number x) (number y))
			  (= (denom x) (denom y)))))
  'done)

(define (install-complex-package)
  ;; ...
  (put 'equ? '(complex complex)
       (lambda (x y)
	 (and (= (real-part x) (real-part y))
	      (= (imag-part x) (imag-part y)))))
  'done)

