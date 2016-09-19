(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))


(define (symbol-leaf x) (cadr x))

(define (symbol-weight x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols object)
  (if (leaf? object)
    (list (symbol-leaf object))
    (caddr object)))

(define (weight object)
  (if (leaf? object)
    (symbol-weight object)
    (cadddr object)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
	(if (leaf? next-branch)
	  (cons (symbol-leaf next-branch)
	 	(decode-1 (cdr bits) tree))
	  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
	((= bit 1) (right-branch tree))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set)))
	 (cons x set))
	(else (cons (car set) 
		    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
			     (cadr pair))
		  (make-leaf-set (cdr pairs))))))
