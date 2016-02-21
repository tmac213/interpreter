#| An interpreter for a Java-like language. Uses simpleParser.scm to parse input.
   Team member:  Yang Ding yxd155 / Guanzhou Qu gxq3 / Emilio Colindres exc231
|#

(load "simpleParser.scm")
;operator
(define operator car)

; left operand
(define lop cadr)

; right operand
(define rop caddr)

; else branch in if statements
(define elsebranch cadddr)

; condition branch in if statements
(define condition lop)

; then branch in if statements
(define thenbranch rop)

; For M_state, the first list is the variables
(define variableList car)

; For M_state, the second list is the values corresponding to the variables
(define valueList cadr)



; the main function
(define interpret
  (lambda (filename)
    (lookup 'return (M_statement_list (parser filename) (newstate)))))

; lookup a variable in the current state binding 
(define lookup
  (lambda (varName state)
    (cond
     ((null? (car state)) 'undeclared)
     ((eq? (car (variableList state)) varName) (car (valueList state)))
     (else (lookup varName (cons (cdr (variableList state)) (cons(cdr(valueList state)) '())))))))

; divide parse tree to statements
(define M_statement_list
  (lambda (stmts state)
    (if (null? stmts)
        state
        (M_statement_list (cdr stmts) (M_state (car stmts) state)))))

; returns the mathematical value of a statement
(define M_value
  (lambda (stmt state)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((not (list? stmt)) (lookup stmt state))
      ((null? (cdr stmt)) (M_value (car stmt) state))
      ((or (eq? 'uninitialized (M_value (lop stmt) state)) (eq? 'undeclared (M_value (lop stmt) state))) (M_value (lop stmt) state))
      ((and (not (null? (cddr stmt))) (or (eq? 'uninitialized (M_value (rop stmt) state)) (eq? 'undeclared (M_value (rop stmt) state)))) (M_value (rop stmt) state))                                                                                                                                                                                                    
      ((eq? '+ (operator stmt)) (+ (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((and (eq? '- (operator stmt)) (null? (cddr stmt)))  (- (M_value (lop stmt) state)))
      ((eq? '- (operator stmt)) (- (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '* (operator stmt)) (* (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '/ (operator stmt)) (quotient (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '% (operator stmt)) (remainder (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '== (operator stmt))(= (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '!= (operator stmt))(not (= (M_value (lop stmt) state) (M_value (rop stmt) state))))
      ((eq? '< (operator stmt))(< (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '> (operator stmt))(> (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '<= (operator stmt))(<= (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '>= (operator stmt))(>= (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '|| (operator stmt))(or (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '&& (operator stmt))(and (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '! (operator stmt))(not(M_value (lop stmt) state)))
      (else error "unknown expression"))))

; interprets a single statement to determine its state
(define M_state
  (lambda (stmt state)
    (cond
      ((eq? '= (operator stmt)) (M_state_assign stmt state))
      ((eq? 'var (operator stmt)) (M_state_declare stmt state))
      ((eq? 'if (operator stmt)) (M_state_if stmt state))
      ((eq? 'return (operator stmt)) (M_state_return stmt state))
      ((eq? 'while (operator stmt)) (M_state_while stmt state))
      (else (M_state_expression stmt state)))))

; interprets an assign statement
(define M_state_assign
  (lambda (stmt state)
    (if (eq? 'undeclared (lookup (lop stmt) state))
      (error 'variableUndeclared "assigning to a variable without having declared it first")
      (addtostate (lop stmt) (M_value (rop stmt) state) state))))

; interprets a declaration statement
(define M_state_declare
  (lambda (stmt state)
    (cond
      ((not (eq? 'undeclared (lookup (lop stmt) state))) (error 'variableAlreadyDeclared "declaring a variable that has already been declared"))
      ((null? (cddr stmt)) (addtostate (lop stmt) 'uninitialized state))
      (else (addtostate (lop stmt) (M_value (rop stmt) state) state)))))

;interprets an if statement
(define M_state_if
  (lambda (stmt state)
    (cond
      ((or (eq? 'uninitialized (M_value (condition stmt) state)) (eq? 'undeclared (M_value (condition stmt) state))) (interpret "24.txt")(error 'variableUndeclared "undeclared or initialized variable") )
      ((M_value (condition stmt) state) (M_state (thenbranch stmt) state))
      ((null? (cdddr stmt)) state)
      (else (M_state (elsebranch stmt) state)))))

; interprets an return statement
(define M_state_return
  (lambda (stmt state)
    (cond
      ((eq? 'undeclared (M_value (lop stmt) state)) (error 'variableUndeclared "returning an undeclared variable"))
      ((eq? 'uninitialized (M_value (lop stmt) state)) (error 'variableUninitialized "returning an uninitialized variable"))
      ((eq? #t (M_value (lop stmt) state)) (addtostate 'return 'true state))
      ((eq? #f (M_value (lop stmt) state)) (addtostate 'return 'false state))
      (else (addtostate 'return (M_value (lop stmt) state) state)))))

; interprets a while statement
(define M_state_while
  (lambda (stmt state)
    (cond
      ((or (eq? 'uninitialized (M_value (condition stmt) state)) (eq? 'undeclared (M_value (condition stmt) state))) (error 'variableUndeclared "undeclared or initialized variable") )
      ((M_value (condition stmt) state) (M_state_while stmt (M_state (thenbranch stmt) state)))
      (else state))))
      

; add a variable binding to state. The binding we used is in format ((a b c) (1 2 3))
(define addtostate
  (lambda (varName value state)
    (cond
       ((null? (variableList state))
        (cons (append (variableList state) (cons varName '())) (cons (append (valueList state) (cons value '())) '())))
       ((eq? (car (variableList state)) varName)
        (cons (variableList state)  (cons (cons value (cdr (valueList state))) '())) )
       (else (cons (cons (car (variableList state)) (car (addtostate varName value (cons (cdr (variableList state))
                   (cons (cdr (valueList state)) '()))) )) (cons (cons (car (valueList state)) (car (cdr (addtostate varName value
                        (cons (cdr (variableList state)) (cons (cdr (valueList state)) '()))) ))) '()) )))))

; the initial state
(define newstate
  (lambda ()
    '((true false return) (#t #f 'undefinedReturnValue))))
                     