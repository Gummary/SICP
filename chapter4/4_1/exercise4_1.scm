
;; Left to right

(define (list-of-values exps env) 
  (if (no-operands? exp)
    '()
    (let (value (eval (first-operand exps) env))
      (cons value
	    (list-of-values (rest-operands exps) env)))))

;; Right to right

(define (list-of-values exps env)
  (if (no-operands? exp)
    '()
    (let (value (list-of-values (rest-operands exps) env))
      (cons ((eval (first-operand exps) env)
	     value)))))

