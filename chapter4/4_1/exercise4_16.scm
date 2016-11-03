;; a)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
	     (if (eq? (car vals) '*unassigned*)
	       (error "UNASSIGNED VARS -- LOOK-VARIABLE-VALUE" var)
	       (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env)) 

;; b)

(define (scan-out-defines body)
  (let ((defined-vars (definitions body)))
    (if (null? defined-vars)
      body
      (list 
	(make-let-seq
	  (unassigned-definitions defined-vars)
	  (unassigned-initialisations defined-vars)
	  (scanned-body body))))))

(define (scanned-body body)
  (cond ((null? body) '())
	((definition? body) (scanned-body (cdr body)))
	(else (cons (car body) (scanned-body body)))))


(define (definition body)
  (define (scan-iter body definitions-complete)
    (cond ((null? body) null)
	  ((definition? (car body))
	   (if (definitions-complete)
	     (error "define cannot appear in an expression context - DEFINITIONS" exp)
	     (cons (car body)
		   (scan-iter (cdr body) false))))
	  ((else (scan-iter (cdr body) true)))))
  (scan-iter body false))


(define (unassigned-definitions define-list)
  (map (lambda (def)
	 (list (definition-variable def) '*unassigned*)) 
       define-list))

(define (unassigned-initialisations define-list)
  (map (lambda (def)
	 (list 'set! (definition-variable def) (definition-value def)))
       define-list))



(define (make-let-seq unassigned-vars unassigned-values body)
  (append (list 'let unassigned-vars)
	  unassigned-values
	  body))

;; c)

;; produce 


