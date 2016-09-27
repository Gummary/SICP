(load "exercise2_12")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
 
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
 
; (define ina (make-interval 9 11))
; (define inb (make-center-percent 18 20))
; (define inc (make-center-percent 20 20))