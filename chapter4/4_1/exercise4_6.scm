(define (make-combination lambdas exps)
  (list lambdas exps))

(define (let-combination letexp)
  (let ((vars (map car (cadr letexp)))
	(exps (map cadr (cadr letexp)))
	(body (caddr letexp)))
    (make-combination (make-lambda vars body) exps)))


