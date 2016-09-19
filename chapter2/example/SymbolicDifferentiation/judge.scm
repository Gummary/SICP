(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
   (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num))) 

;; (define (make-sum a b)
;;  (list '+ a b))

(define (make-sum a b)
  (cond ((=number? a 0) b)
	((=number? b 0) a)
	((and (number? a) (number? b)) (+ a b))
	(else (list '+ a b))))

;; (define (make-product a b)
;;  (list '* a b))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
	((=number? a 1) b)
	((=number? b 1) a)
	((and (number? a) (number? b)) (* a b))
	(else (list '* a b))))


(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))

(define (addend exp)
  (cadr exp))

(define (augend exp)
  (caddr exp))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))


