(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
	     (lambda ()
	       (set! x (rand-update x))
	       x))
	    ((eq? m 'reset)
	     (lambda (new-value)
	       (set! x new-value)))
	    (else (error "No method -- rand" m))))
    dispatch))
