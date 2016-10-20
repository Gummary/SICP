(define (rand commands)
  (define (rand-iter command)
    (if (stream-null? command)
      'done
      (let ((current (stream-car command)))
	(cond ((eq 'generate current)
	       (stream-cons (
