(load "exercise2_36")

(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define vector (list 1 2 3 4))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x)
	 (dot-product x v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (vec)
	   (matrix-*-vector cols vec))
	 m)))
