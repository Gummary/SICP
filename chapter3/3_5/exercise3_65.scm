(load "euler")

(define (ln2 n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2 (+ n 1)))))

(define ln-stream
  (partial-sums (ln2 1)))


