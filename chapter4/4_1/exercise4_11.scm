(load "eval")

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (if (null? frame)
	(lookup-variable-value var (enclosing-environment env))
	(let ((first-pair (car frame))
	      (rest-pair (cdr frame)))
	  (if (eq? var (car first-pair))
	    (cdr first-pair)
	    (scan rest)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
	(scan frame))))
  (env-loop env))

(define (set-variable-value! var value  env)
  (define (env-loop env)
    (define (scan frame)
      (if (null? frame)
	(set-variable-value! var (enclosing-environment env))
	(let ((first-pair (car frame))
	      (rest-pair (cdr frame)))
	  (if (eq? var (car first-pair))
	    (set-cdr! first-pair value)
	    (scan rest)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
	(scan frame))))
  (env-loop env))  

(define (define-variable! var value  env)
  (define (scan frame)
    (cond ((null? frame)
	   (add-binding-to-frame! var value frame))
	  ((eq? var (caar frame))
	   (set-cdr! (car frame) value))
	  (else (scan (cdr frame)))))
  (scan (first-frame env)))




