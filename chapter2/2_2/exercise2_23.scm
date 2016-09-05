; (define (for-each proc items)
; 	(if (null? items)
; 		#t
; 		((proc (car items))
; 		(for-each proc (cdr items)))))

 ; (define (for-each unary-proc seq) 
 ;        (if (null? seq) 
 ;            (newline) 
 ;            ((unary-proc (car seq)) (for-each unary-proc (cdr seq))) ) )  

(define (for-each proc list)
	(if (null? list)
		#t
		(proc (car list))
		(for-each proc (cdr list))))