(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter painter2)))
