;; and

;; (and <value> <value>)

((and? exp)
 (eval-and (and-exps exp) env))

(define (eval-and seqs env)
  (cond ((last-exp? seqs) (true? (eval (first-exp seqs) env)))
	((true? (eval (first-exp seqs) env)) 
	 (eval-and (rest-exp seqs) env))
	(else false)))


(define (and? exp)
  (tagged-list? exp 'and))

(define (and-exps exp) (cdr exp))

(define (first-exp seq) (car seq))

(define (rest-exp seq) (cdr seq))

(define (last-exp? seq) (null? (cdr seq)))

;; or

((or? exp)
 (eval-or (or-exps exp) env))

(define (eval-or seqs env)
  ((last-exp? seqs) (true? (eval (first-exp seqs) env)))
  (not ((true? (eval (first-exp seqs) env)) 
	 (eval-or (rest-exp seqs) env)))
  (else true))

(define or?
  (tagged-list? exp 'or))

(define (or-exps exp) (cdr exp))


;; Another way

;; and

;; (and <exp1> <exp2>) => (if <exp1>
;;			      (if <exp2>
;;				  true
;;				  false)
;;			      false) 

(define (and->if exp)
  (expand-clauses (or-exps exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    true
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (make-if first (expand-clauses rest) false))))
;; or

;; (or <exp1> <exp2>) => (if <exp1>
;;			     true
;;			     (if <exp2>
;;			         true
;;				 false))


(define (expand-clauses clauses)
  (if (null? clauses)
    false
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (make-if first true (expand-clauses rest)))))
