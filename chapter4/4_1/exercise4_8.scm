(define (make-combination lambdas exps)
  (list lambdas exps))

(define (let-initials exp) (map cadr (cadr exp)))
(define (let-parameters exp) (map car (cadr exp)))
(define named-let-identifier car)
(define let-body cddr)

(define (let->combination letexp)
  (if (named-let? letexp)
    (named-let->combination letexp)
    (make-combination (make-lambda (let-parameters letexp)
				   (let-body letexp))
		      (let-initials letexp))))

(define (named-let? letexp)
  (and (tagged-list? letexp)
       (symbol? (cadr letexp))))
    
(define (named-let->combination letexp)
  (let ((produce-name (cadr letexp)))
    (make-begin 
      (list (list 'define produce-name 
		  (make-lambda (let-parameters (cddr letexp)
			       (let-body (cddr leexp)))
	    (list produce-name initial-values)))))



