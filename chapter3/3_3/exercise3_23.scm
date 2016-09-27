(define make-deque (cons '() '()))

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))


(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (caar (front-ptr deque)))


(define (rear-deque deque)
  (caar (rear-ptr deque)))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	    (set-cdr! (car (front-ptr deque)) new-pair)
	    (set-cdr! new-pair (front-ptr deque))
	    (set-front-ptr! deque new-pair)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	    (set-cdr! (rear-ptr deque) new-pair)
	    (set-cdr! (car new-pair) (rear-ptr deque))
	    (set-rear-ptr! deque new-pair)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	  (set-front-ptr! deque (cdr (front-ptr deque)))
	  (set-cdr! (car (front-ptr deque)) '()))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	  (set-rear-ptr! deque (cdr (car (rear-ptr deque))))
	  (set-cdr! (cdr (car (rear-ptr deque))) '()))))


(define (print-deque deque)
  (define (iter items)
    (cond ((null? items)
	   'done)
	  (else 
	    (display (caar items))
	    (iter (cdr items)))))
  (iter (front-ptr deque)))



