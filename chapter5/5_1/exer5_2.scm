(controller
  (assign product 1)
  (assign counter 1)
 test-c
  (test (op >) counter n)
  (branch (label fact-done))
  (assign product ((op *) counter product))
  (assign counter ((op +) counter (const 1)))
  (goto (test-c))
 fact-done)

