(beq <register-name> (reg <register-name>) (label <label-name>))
(beq <register-name> (const <const-value>) (label <label-name>))

(define (make-beq inst machine labels pc)
  (let ((first-reg (get-register machine (beq-first-exp inst)))
	(second-exp (beq-second-exp inst))
	(insts 
	  (lookup-label labels (label-exp-label (beq-dest inst)))))
    (let (second-value
	   (cond ((register-exp? second-exp)
		  (get-contents 
		    (get-register machine (register-exp-reg second-exp))))
		 ((constant-exp? second-exp)
		  (constant-exp-value second-exp))))
      (if (equal? (get-contents first-reg)
		  second-value)
	(set-contents pc insts)
	(advance-pc pc)))))


(define (beq-first-exp inst)
  (car inst))

(define (beq-second-exp inst)
  (cadr inst))

(define (beq-dest inst)
  (caddr inst))

