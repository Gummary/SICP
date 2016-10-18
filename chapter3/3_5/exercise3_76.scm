(load "streamfunc")

(define (smooth stream)
  (define (iter stream last-value)
    (let ((avpt (/ (+ (stream-car stream)
		      last-value)
		   2)))
      (cons-stream avpt
		   (iter (stream-cdr stream) (stream-car stream)))))
  (iter (stream-cdr stream) (stream-car stream)))

(define (make-zero-crossings input-stream)
  (let ((smoothed-values (smooth input-stream)))
    (stream-map sign-change-detector
                smoothed-values
                (cons-stream 0 smoothed-values))))
  
