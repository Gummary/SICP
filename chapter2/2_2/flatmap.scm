(load "funclist")
(load "seq_operations")

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
