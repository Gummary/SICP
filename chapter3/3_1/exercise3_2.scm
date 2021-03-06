(define (make-monitored function)
  (let ((count 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?)
	     count)
	    ((eq? m 'reset-count)
	     (set! count 0))
	    (else (begin (set! count (+ count 1))
			 (function m)))))
    mf))
