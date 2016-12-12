ev-cond
(save continue)
(assign unev (op cond-clauses) (reg exp))
ev-cond-loop
(test (op null?) unev)
(branch (label cond-no-val))
(assign exp (op car) (reg unev))
(test (op cond-else-clause?) exp)
(branch (label cond-true))
(save exp)
(save unev)
(assign exp (op cond-predicate exp) (reg exp))
(assign continue (label cond-test-true))
(goto (label (label ev-dispatch)))
cond-test-true
(restore exp)
(restore unev)
(test (op true?) (reg vla))
(branch (label cond-true))
(assign unev (op cdr) (reg unev))
cond-true
(assign unev (cond-actions exp))
(goto (label ev-sequence))
ev-cond-unspec
(assign val (const 'unspecified))
(restore continue)
(goto (reg continue))
