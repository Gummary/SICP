(define f 
  (let ((init 10))
    (lambda (x)
      (begin (set! init (* init x))
	     init))))

