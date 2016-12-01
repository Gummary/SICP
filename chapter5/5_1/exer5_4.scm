(controller
  (assign b (op read))
  (assign n (op read))
  (assign continue (label fact-done))
 expt-loop
  (test (op =) (reg n) (const 0))
  (branch (branch base-case))
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto expt-loop)
 after-fact
  (restore n)
  (restore continue)
  (assign val (op *) (reg b) (reg n))
  (goto (reg continue))
 base-case
  (assign val (const 0))
  (goto (reg continue))
 fact-done)

(controller 
  (assign b (op read))
  (assign n (op read))
  (assign counter n)
  (assign product 1)
 expt-loop
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))
  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg product) (reg b))
  (goto (label expt-loop))
 expt-done)

  (branch (label expt-done))

