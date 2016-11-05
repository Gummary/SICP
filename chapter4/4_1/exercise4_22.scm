((let? exp) (analyze (let->combination exp)))


(define (make-combination lambdas exps)
  (list lambdas exp))

(define (let-vars exp)
  (map car (cadr exp)))

(define (let-vals exp)
  (map cdr (cadr exp)))

(define (let-body exp)
  (caddr exp))

(define (let->combination exp)
  (let ((vars (let-vars exp))
	(vals (map ananlyze (let-vals exp)))
	(body (analyze-sequence (let-body exp))))
    (make-combination (make-lambda vars body) vals)))


  
