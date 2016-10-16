(load "streamfunc")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit x tolerance)
  (let ((first (stream-car x))
	(second (stream-car (stream-cdr x))))
    (if (< (abs (- first second)) tolerance)
      first
      (stream-limit (stream-cdr x) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
