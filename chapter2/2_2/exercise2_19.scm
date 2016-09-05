; (define (cc amount coin-values)
;   (cond ((= amount 0) 1)
;         ((or (< amount 0) (no-more? coin-values)) 0)
;         (else
;          (+ (cc amount
;                 (except-first-denomination coin-values))
;             (cc (- amount
;                    (first-denomination coin-values))
;                 coin-values)))))

(define (first-denomination coin-values)
	(car coin-values))

(define (except-first-denomination coin-values)
	(cdr (coin-values)))

(define (no-more? coin-values)
	(null? coin-values))

; (define (first-denomination denominations) (car denominations)) 
;  (define (except-first-denom denominations) (cdr denominations)) 
;  (define (no-more? denominations) (null? denominations)) 
  
 (define (cc amount denominations) 
   (cond  
    ;; If there's no change left, we have a solution 
    ((= amount 0) 1) 
     
    ;; If we're gone -ve amount, or there are no more kinds of coins 
    ;; to play with, we don't have a solution. 
    ((or (< amount 0) (no-more? denominations)) 0) 
     
    (else 
     ;; number of ways to make change without the current coin type 
     ;; plus the number of ways after subtracting the amount of the 
     ;; current coin. 
     (+ (cc amount (except-first-denom denominations)) 
        (cc (- amount  
               (first-denomination denominations))  
            denominations))))) 