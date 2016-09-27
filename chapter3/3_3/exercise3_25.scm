(load "assoc")

(define (lookup table  keylist)
  (if (null? keylist)
    table
    (let ((record (assoc (car key) table)))
      (if record
	(lookup (car table) (cdr keylist))
	false))))

(define (insert! table keylist value)
  (define (createnewsubtable subtable keys)
    (if (= (length keys) 1)
      (cons (car keys) value)
      (set-cdr! subtable
		(cons (createnewsubtable subtable (cdr keys))
		      (cdr subtable)))))

  (define (iter subtable keys)
    (let ((sub (assoc subtable (car keys))))
      (if sub
	(if (= (length keys) 1)
	  (set-cdr! sub value)
	  (iter sub (cdr keys)))
	(createnewsubtable subtable keys))))
  (iter table keylist))
