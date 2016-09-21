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
  
  (put 'make 'polar
      (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'make 'rectangular
       (lambda (r i) (tag (make-from-real-image r i))))

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

  (define (real-part z)
    (cond ((equal? (tag-type z) 'polar)
	   (* (magnitude z) (cons (angle z))))
	  ((equal? (tag-type z) 'rectangular)
	   (car z))
	  (else (error "Unkonw Type --- real-part" (list (tag-type z) z)))))

  (define (image-part z)
    (let ((type (tag-type z)))
      (cond ((equal? type 'polar)
	     (* (magnitude z) (sin (angle z))))
	    ((equal? type 'rectangular)
	     (cdr z))
	    (else (error "Unkonw Type --- real-part" (list type z))))))

  (define (magnitude z)
    (let ((type (tag-type z)))
      (cond ((equal? type 'polar)
	     (car z))
	    ((equal? type 'rectangular)
	     (sqrt (+ (square (real-part z))
		      (square (image-part z)))))
	    (else (error "Unkonw Type --- real-part" (list type z))))))

  (define (angle z)
    (let ((type (tag-type z)))
      (cond ((equal? type 'polar)
	     (cdr z))
	    ((equal? type 'rectangular)
	     (atan (imag-part z) (real-part z)))
	    (else (error "Unkonw Type --- real-part" (list type z))))))

  ;; Polar Package
  (define (tag-polar x)
    (attach-tag 'polar x))

  (define (make-from-mag-ang r a)
    (tag-polar (cons r a)))

  ;; Rectangular Package
  (define (tag-rectangular x)
    (attach-tag 'rectangular x))

  (define (make-from-real-image r i)
    (tag-rectangular (cons r i)))

  'done)
 
(define (make-from-real-imag x y)
  ((get 'make 'rectangular) x y))

(define (make-from-mag-ang x y)
  ((get 'make 'polar) x y))

