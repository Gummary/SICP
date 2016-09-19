(load "huffmantree")

(define (successive-merge leafs)
  (if (= (length leafs) 1)
    leafs
    (successive-merge (adjoin-set (make-code-tree (car leafs) (cadr leafs))
				  (cddr leafs)))))
