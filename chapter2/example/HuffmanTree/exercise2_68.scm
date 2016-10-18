(load "huffmantree")

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
	    (encode (cdr message) tree))))

(define (encode-symbol message tree)
  (let ((lb (left-branch tree))
	(rb (right-branch tree)))
    (cond ((intree? message lb)
	   (if (leaf? lb)
	     '(0)
	     (cons 0 (encode-symbol message lb))))
	  ((intree? message rb)
	   (if (leaf? rb)
	     '(1)
	     (cons 1 (encode-symbol message rb))))
	  (else error "bad symbol -- ENCODE-SYMBOL" message))))
  
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (intree? message branch)
  (cond ((leaf? branch) 
	 (equal? message (symbol-leaf branch)))
	(else (element-of-set? message (symbols branch)))))
    