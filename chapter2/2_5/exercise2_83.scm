(define (raise x)
  (apply-generic 'raise x))

(define (install-scheme-number-package)
  ;; ...
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  'done)

(define (install-rational-package)
  ;; ...
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer r) (denom r)))))
  'done)

(define (install-real-package)
  ;; ...
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-image x 0)))
  'done)

