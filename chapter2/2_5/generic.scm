(define (add x y) 
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(define (equ? x y)
  (apply-generic 'equ x y))

(define (apply-generic op . args)
  (let ((tags (map tag-type args)))
    (let ((proc (get op tags)))
      (if proc
	(apply proc (map contents args))
	(error
	  "No method for these types -- APPLY-GENERIC"
	  (list op args))))))

(define (tag-type item)
  (car item))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (contents datum)
  (cdr datum))
