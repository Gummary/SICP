(define (install-complex-package)
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))

  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))

  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))

  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'equ? '(complex complex)
       equal-complex?)

  (define (tag datum)
    (cons 'complex datum))

  (define (add-complex x y)
    (make-from-real-imag (+ (real-part x) (real-part y))
			 (+ (image-part x) (image-part y))))
  (define (sub-complex x y)
    (make-from-real-image (- (real-part x) (real-part y))
			  (- (image-part x) (image-part y))))

  (define (mul-complex x y)
    (make-from-mag-ang (* (magnitude x) (magnitude y))
		       (+ (angle x) (angle y))))

  (define (div-complex x y)
    (make-from-amg-ang (/ (magnitude x) (magnitude y))
		       (- (angle x) (angle y))))
  (define (equal-complex? x y)
    (and (= (real-part x) (real-part y))
	 (= (image-part x) (image-part y))))



