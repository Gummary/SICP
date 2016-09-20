(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (varliable p1)
	       (add-poly p1 (negate p2)))
    (error "Polys not in the same var -- SUB-POLY"
	   (list p1 p2))))
(define (negate-poly p)
  (make-polynomaial (variable p)
		    (negate-terms (term-list p))))
(put 'negate '(polynomial)
     negate-poly)


(put 'sub '(polynomial polynomial)
     sub-poly)

(define (negate-terms termlist)
  (if (empty-termlist? termlsit)
    the-empty-termlist
    (let ((t (first-term termlist)))
      (adjoin-term (make-term (order term)
			      (negate (coeff term)))
		   (negate-terms (rest-terms termlist))))))


(define (negate-terms termlist)
  (if (empty-termlist? termlsit)
    the-empty-termlist
    (map (lambda (term) (make-term (order term)
				   (negate term)))
	 (termlist))))
