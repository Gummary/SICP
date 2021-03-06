(define (make-register name)
  (let ((contents '*unassigned*)
	(trace-on false))
    (define (display-set new-value)
      ((newline)
       (display (list 'name '= name 
		      'oldvalue '= contents
		      'newvalue '= value))))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
	       (if (trace-on) (display-set value))
	       (set! contents value)))
	    ((eq? message 'trace-on) (set! trace-on true))
	    ((eq? message 'trace-off) (set! trace-off false))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (register-trace-on register)
  (register 'trace-on))

(define (register-trace-off register)
  (register 'treace-off))
