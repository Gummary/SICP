(define (eval exp env)
  (cond ((self-evaluating? exp) exp) ;; number or string
        ((variable? exp) (lookup-variable-value exp env)) ;; 
        ((quoted? exp) (text-of-quotation exp)) 
        ((assignment? exp) (eval-assignment exp env)) ;; (set! a b)
        ((definition? exp) (eval-definition exp env)) ;; (define a b)
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)  ;;构造一个复合过程
                         (lambda-body exp)    
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env)) ;; 在env中求取exp中的每一个值
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)           ;; 求取过程的运算符（可能是一个lambda表达式）
                (list-of-values (operands exp) env))) ;; 获取该过程的参数列表
        (else
         (error "Unknown expression type -- EVAL" exp))))
      

;; 基本进程 => 利用apply-primitive-procedure将参数应用到procedure上
;; 复合过程 => 1、将复合过程的body看做求值的序列
;;            2、将参数名和参数的值组成一个框架扩展当前的环境
;;            3、利用eval-sequence求取复合过程 

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)           ;; 获取过程体
           (extend-environment                  ;; 拓展当前环境
             (procedure-parameters procedure)   ;; 获取过程的参数名
             arguments                          ;; 获取形式参数
             (procedure-environment procedure))))    ;; 获取当前的环境
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;; 用于求取过程应用中的参数表
;; 每个参数都有可能是一个过程或者一个参数，所以对每一个参数都要用eval求值
;; 最终的得到的是所有参数的一个表
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))  

;; 求值表达式序列
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; 将一个序列转化为一个begin表达式
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))              

;; 判断exp是否是以tag开头的表
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))   


;; 自求值表达式的处理

;; 自求值表达式的表示
;; 自求值表达式只有字符串和数字
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


;; 变量的处理

;; 变量的表示
;; 变量可以通过是否为符号进行判断
(define (variable? exp) (symbol? exp))


;; 引号表达式的处理

;; 引号表达式的表示
;; 引号表达式就相当于以'quote开头的表
;; (quote <test-of-quotation>)
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;; if语句的处理

;; if语句的表示
;; 由if开始，有一个谓词部分，一个推论部分和一个（可缺的）替代部分,如果没有替代部分，则用false代替
;; ('if <predicate> <consequent> [alternative])
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;; if的构造函数
(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))      

;; 求值if语句
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env)) ;; true?是由解释器实现的，判断参数是否为真
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))



;; 赋值语句的处理

;; 赋值语句的表示
;; 由set!开始，后面跟一个变量名，一个值
;; ('set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; 赋值语句的执行
;; 利用eval求取<value>
;; 在当前环境中找<var> 将<var>变为<value>或建立一个新的变量<var>值为<value>
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;; 定义的处理

;; 定义语句的表示
;; 定义语句都是由'define开头的表
;; 定义有两种形式，分别为
;; (define <var> <value>)
;; 或
;; (define (<var> <parameter1> <parameter2> ... <parametern>)
;;   <body>)
;; 第二种形式可以变为
;; (define <var>
;;   (lambda (<parameter1> ... <parametern>)
;;     <body>))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
;; 如果是第二种形式将其转化成lambda表达式      
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;; 定义语句的求值
;; 与set类似
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)                   


;; lambda表达式的处理

;; lambda表达式的表示
;; lambda表达式是以lambda开始的表
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;; lambda表达式的构造函数
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; lambda表达式的求值是将lambda表达式转换为一个复合过程  


;;begin表达式的处理

;; begin表达式的表示
;; ('begin <exp1> <exp2> <exp3> ... <expn>)
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; begin表达式的构造函数
(define (make-begin seq) (cons 'begin seq))

;; begin表达式的求值
;; 利用eval-sequence求取每一个表达式的值


;; 过程应用的处理

;; 过程应用的表示
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; 过程应用的求值
;; 提取出过程的参数和操作符，利用apply求值


;; cond的处理

;; cond的表示
;; ('cond ((<predicate1> <action1>) (<predicate2> <action2>) ... (else <actionn>)))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

;; 将cond表示为if语句的派生
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))