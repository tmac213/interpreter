#| An interpreter for a Java-like language. Uses simpleParser.scm to
    parse input.

|#

(load "simpleParser.scm")



(define condition lop)
(define thenbranch rop)

(define M_state
  (lambda (stmt state)
    (cond
      ((eq? '= (operator stmt)) (M_state_assign stmt state))
      ((eq? 'var (operator stmt)) (M_state_declare stmt state))
      ((eq? 'if (operator stmt)) (M_state_if stmt state))
      ((eq? 'return (operator stmt)) (M_state_return stmt state))
      (else (M_state_expression stmt state)))))

(define M_state_assign
  (lambda (stmt state)
    (if (eq? 'undeclared (lookup (lop stmt) state))
      (error 'variableUndeclared "assigning to a variable without having declared it first")
      (addtostate (lop stmt) (rop stmt) (removefromstate (lop stmt) state)))))

(define M_state_declare
  (lambda (stmt state)
    (if (not (eq? 'undeclared (lookup (lop stmt) state)))
      (error 'variableAlreadyDeclared "declaring a variable that has already been declared")
      (addtostate (lop stmt) 'uninitialized state))))

(define M_state_if
  (lambda (stmt state)
    (cond
      ((M_boolean (condition stmt) state) (M_state (thenbranch stmt) state))
      ((null? (elsebranch stmt)) state)
      (else (M_state (elsebranch stmt) state)))))

(define M_state_return
  (lambda (stmt state)
    (cond
      ((eq? 'undeclared (M_value (lop stmt))) (error 'variableUndeclared "returning an undeclared variable"))
      ((eq? 'uninitialized (M_value (lop stmt))) (error 'variableUninitialized "returning an uninitialized variable"))
      ((eq? #t (M_value (lop stmt))) 'true)
      ((eq? #f (M_value (lop stmt))) 'false)
      (else (M_value (lop stmt))))))
