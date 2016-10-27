(define (let*-pairs exp)
  (cadr exp))

(define (let*-body exp)
  (caddr exp))


(define (let*->nested-lets exp)
  (let*-help (let*-pairs exp) (let*-body exp)))


(define (let*-help seqs body)
  (if (null? (cdr seqs))
    (append (list 'let (list (car seqs))) body)
    (list 'let (list (car seqs)) (let*-help (cdr seqs) body))))
