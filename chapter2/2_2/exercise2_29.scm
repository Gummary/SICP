(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (is-mobile? structure)
  (pair? structure))

; some test valve

(define level-1-mobile 
  (make-mobile (make-branch 2 1)
	       (make-branch 1 2)))

 (define level-2-mobile 
   (make-mobile (make-branch 3 level-1-mobile) 
		(make-branch 9 1)))

 (define level-3-mobile 
   (make-mobile (make-branch 4 level-2-mobile) 
		(make-branch 8 2))) 

;; ----------------------------------------------

; a)

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; ----------------------------------------------

; b)

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (is-mobile? s)
      (total-weight s)
      s)))

(define (total-weight tree)
  (+ (branch-weight (left-branch tree))
     (branch-weight (right-branch tree))))

;; ----------------------------------------------

; c)

(define (branch-balanced? branch)
  (let ((s (branch-structure branch)))
    (if (is-mobile? s)
      (balanced? s)
      true)))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced? tree)
  (let ((left (left-branch tree))
	(right (right-branch tree)))
    (and (= (branch-torque left)
	    (branch-torque right))
	 (branch-balanced? left)
	 (branch-balanced? right))))


;; ----------------------------------------------

; d)

(define (left-branch mobile) 
     (car mobile)) 
 (define (right-branch mobile) 
      (cdr mobile)) 
  
 (define (branch-length branch) 
      (car branch)) 
 (define (branch-structure branch) 
      (cdr branch)) 
  
 (define (structure-is-mobile? structure) 
      (pair? structure)) 
