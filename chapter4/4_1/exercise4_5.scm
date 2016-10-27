(define (cond-=>-clause? clause)
  (eq? (car (cond-actions clause)) '=>))

(define (cond-consequent clause predicate)
  (if (cond-=>-clause? clause)
    (list (caddr clause) predicate)
    (sequence->exp (cond-actions clause))))


(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((predicate (cond-predicate first)))
              (make-if predicate
                       (cond-consequent first predicate)
                       (expand-clauses rest)))))))


