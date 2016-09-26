(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue)
  (cons '() '()))

(define (insert-queue! queue value)
  (let ((new-node (cons value '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-node)
	   (set-rear-ptr! queue new-node)
	   queue)
	  (else
	    (set-cdr! (rear-ptr queue) new-node)
	    (set-rear-ptr! queue new-node)
	    queue))))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "The queue is empty" queue)
    (car (front-ptr queue))))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "The queue is empty" queue))
	(else
	  (set-front-ptr! queue (cdr (front-ptr queue)))
	  queue)))




