(define (scan var vars vals env)
  (cond ((null? vars)
	 false)
	((eq? var (car vars))
	 vals)
	(else (scan var (cdr vars) (cdr vals) env))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (frist-frame env)))
	(let ((result (scan var (frame-variables frame) (frame-values frame)env)))
	  (if result
	    (car result)
	    (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (set-variable-value var value env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (frist-frame env)))
	(let ((result (scan var (frame-variables frame) (frame-values frame)env)))
	  (if result
	    (set-car! result value)
	    (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((result (scan var (frame-variables frame) (frame-values frame) env)))
      (if result
	(set-car! result val)
	(add-binding-to-frame! var val env)))))


	      
