(define (iscircle? x)
  (let ((head x))
    (define (iter items)
      (cond ((null? items) false)
	    ((memq head items) true)
	    (else (begin
		    (set! head (cons items head))
		    (iter (cdr items))))))
    (iter (cdr x))))
