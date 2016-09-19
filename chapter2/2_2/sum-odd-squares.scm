(load "seq_operations")

; Enumerate all leaves

(define (enummerate-tree tree)
	(cond ((null? tree) nil)
	 	((not (pair? tree)) (list tree))
		(else (append (enummerate-tree (car tree))
			   			(enummerate-tree (cdr tree))))))


(define (sum-odd-squares tree)
	(accumulate +
	 			0
				(map (lambda (x) (* x x))
					(filter odd? (enummerate-tree tree)))))	
