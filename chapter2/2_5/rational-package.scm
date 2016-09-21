(define (install-rational-package)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rational x y))))

  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rational x y))))

  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rational x y))))

  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rational x y))))

  (put 'equ? '(rational rational)
       (lambda (x y) (equal-rat? x y)))

  (put 'make 'rational
       (lambda (x y) (tag (make-rational x y))))
  
  (define (tag item)
    (cons 'rational item))

  (define (add-rational r1 r2)
    (make-rational (+ (* (numer r1) (denom r2))
		      (* (numer r2) (denom r1)))
		   (* (denom r1) (denom r2))))

  (define (sub-rational r1 r2)
    (make-rational (- (* (numer r1) (denom r2))
		      (* (numer r2) (denom r1)))
		   (* (denom r1) (denom r2))))

  (define (mul-rational r1 r2)
    (make-rational (* (numer r1) (numer r2))
		   (* (denom r1) (denom r2))))

  (define (div-rational r1 r2)
    (make-ratioanl (* (numer r1) (denom r2))
		   (* (numer r2) (denom r1))))

  (define (equal-rat? r1 r2)
    (and (= (numer r1) (numer r2))
	 (= (denom r1) (denom r2))))

  (define (numer r)
    (car r))

  (define (denom r)
    (cdr r))

  (define (make-rational n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

