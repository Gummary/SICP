(load "assoc")

(define (lookup key records)
  (let ((record (assoc key records)))
    (if record
      (car record)
      false)))

(define (insert! key value table)
  (let ((record (assoc key (car table))))
    (if record
      (set-cdr! record value)
      (cons table 
	    (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))
