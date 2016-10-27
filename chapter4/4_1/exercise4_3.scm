
(define (eval exp env)
  (cond ((self-evaluationg? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((get 'eval (car exp))
	 ((get 'eval (car exp)) exp env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else 
	  (error "Unknown expression type -- EVAL" exp))))

(define install-eval
  (put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set!
       eval-assignment)
  (put 'eval 'define
       evla-definition)
  (put 'eval 'if
       eval-if)
  (put 'eval 'lambda
       (lambda (exp env) (make-procedure (lambda-parameters exp)
					 (lambda-body exp)
					 env)))
  (put 'eval 'begin
       (lambda (exp env) (make-procedure (lambda-parameters exp)
					 (lambda-body exp)
					 env)))
  (put 'eval 'cond
       (lambda (exp env)
	 (list-of-values (operands exp) env))))
