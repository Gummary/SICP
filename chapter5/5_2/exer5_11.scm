;; a


;; b
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst)
			(get-contents reg)))
      (advance-pc pc))))


(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let (stack-top (pop stack))
	(let ((reg-name (car stack-top))
	      (reg-value (cadr stack-top)))
	  (if (equal? reg-name (stack-inst-reg-name inst)) 
	    (set-contents! reg (pop stack))
	    (error "REG name is not equal to this MAKE-RESTORE" reg-name))
	  (advance-pc pc))))))
