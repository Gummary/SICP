(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))

    ;;inner-function
    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (car front-ptr))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((null? front-ptr)
	       (set! front-ptr item)
	       (set! rear-ptr item)
	      (else
		(set-cdr! rear-ptr item)
		(set! rear-ptr item))))))

    (define (delete-queue!)
      (cond ((null? front-ptr)
	     (error "The queue is empty" front-ptr))
	    (else
	      (set! front-ptr (cdr front-ptr))
	      )))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) 
	     empty-queue?)
	    ((eq? m 'front-queue)
	     front-queue)
	    ((eq? m 'insert-queue!)
	     insert-queue!)
	    ((eq? m 'delete-queue!)
	     delete-queue!)
	    (else
	      (error "No Such Operations" m))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty-queue))

(define (front-queue queue)
  (queue 'front-queue))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue!)
  (queue 'delete-queue!))



