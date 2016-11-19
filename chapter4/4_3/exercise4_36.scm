(define (a-pythagorean-triple-from low)
  (let (k (an-integer-starting-from low))
    (let (i (an-integer-between low k))
      (let (j (an-integer-between i k))
	(require (= (* k k) (+ (* i i) (* j j))))))))


