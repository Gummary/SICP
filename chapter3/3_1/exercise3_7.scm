(define (make-joint account old-passwd new-passwd)
  (lambda (p m)
    (if (eq? p new-passwd)
      (account old-passwd m)
      (error "New-Passwd is wrong" new-passwd))))


(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ amount balance))
    balance)
  (define (dispatch password m)
    (if (eq? password passwd)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unkonw request -- MAKE-ACCOUNT"
			 m)))
      (error "Old-Passwd is wrong" password)))
  dispatch)
      
