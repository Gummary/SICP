(load "assoc")

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr table))))
	(if record
	  (cdr record)
	  false))
      false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr (table)))))
	(if record
	  (set-cdr! record value)
	  (set-cdr! table
		    (cons (cons key-2 value)
			  (cdr table)))))
      (set-cdr! table
		(cons (list key-1
			    (cons key-2 value))
		      (cdr table)))))
  'done)

	    
