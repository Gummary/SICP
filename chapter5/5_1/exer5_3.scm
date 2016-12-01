(controller
 sqrt-loop
   (assign x (op read))
   (assign guess (const 1.0))
 sqrt-iter
   (test (op good-enough?) guess)
   (branch (label sqrt-done))
   (assign guess ((op improve) guess))
   (goto (label sqrt-iter))
 sqrt-done
   (goto (label sqrt-loop)))

