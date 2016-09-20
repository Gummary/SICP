(define (drop x)
  (let ((project-x ((get 'project (type-tag x)) x)))
    (cond ((equal? 'scheme-number (type-tag x)) x)
	  ((equ? x project-x)
	   (drop project-x))
	  (else x))))

(define (install-scheme-number-package)
  ;; ...
  (put  'project '(scheme-number)
	(lambda (x) x))
  'done)

(define (install-rational-package)
  ;; ...
  (put 'project '(rational)
       (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))
  'done)

(define (install-real-package)
  ;; ...
  (put 'project '(real)
       (lambda (x) (round x)))
  'done)

(define (install-complex-package)
  ;; ...
  (put 'project '(complex)
       (lambda (x) (real-part x)))
  'done)

