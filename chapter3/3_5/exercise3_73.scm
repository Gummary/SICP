(load "streamfunc")
(load "integral")

(define (RC r c dt)
  (lambda (i v0)
    (add-streams (scale-stream i r)
		 (integral (scale-stream i (/ 1 c)) v0 dt))))


