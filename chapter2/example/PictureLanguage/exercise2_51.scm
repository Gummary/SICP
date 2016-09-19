(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    ((let ((paint-up
	     (transform-painter painter2
				split-point
				(make-vect 0.0 1.0)
				(make-vect 1.0 0.5)))
	   (paint-bottom
	     (transform-painter painter1
				(make-vect 0.0 0.0)
				split-point
				(make-vect 1.0 0.0))))
       (lambda (frame
		 (paint-up frame)
		 (paint-down frame)))))))

(define (below painter1 painter2)
  (rotate270 (beside (rorate90 painter2) (rotate90 painter1))))
