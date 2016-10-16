(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
  (cons-stream n
	       (integers-starting-from (+ n 1))))

(define integers
  (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (display-stream s n)
  (if (= n 0)
    'done
    (begin (newline)
	   (display (stream-car s))
	   (display-stream (stream-cdr s) (- n 1)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
		 (interleave s2 (stream-cdr s1)))))


