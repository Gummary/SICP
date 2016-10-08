(define (ripple-carrt-adder an bn sn c)
  (define (is-last-bit? n)
    (null? (cdr n)))

  (define (iter a b s c-in c-out)
    (if (null? a)
      'ok
      (full-adder (car a)
		  (car b)
		  c-in
		  (car s)
		  (if (last-bit? as) c c-out))
      (iter (cdr a)
	    (cdr b)
	    (cdr s)
	    c-out
	    (make-wire))))
  (iter an bn sn 0 (make-wire)))
