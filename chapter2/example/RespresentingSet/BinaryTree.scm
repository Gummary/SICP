
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x tree)
  (cond ((null? tree) false)
	((= x (entry tree)) true)
	((> x (entry tree))
	 (element-of-set? x (right-branch tree)))
	((< x (entry tree))
	 (element-of-set? x (left-branch tree)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set)) 
	 (make-tree (entry set)
	   	    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))
	 
