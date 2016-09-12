(define (flip-horiz painter)
  (transfor-painter painte
		    (make-rect 1 0)
		    (make-rect 1 1)
		    (make-rect 0 0)))

;; 180

(define (flip-180 painter)
  (transfor-painter painter
		    (make-rect 1 1)
		    (make-rect 0 1)
		    (make-rect 1 0)))

;; 270

(define (flip-270 painter)
  (transfor-painter painter
		    (make-rect 0 1)
		    (make-rect 0 0)
		    (make-rect 1 1)))

