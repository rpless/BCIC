#lang racket

(require "data.rkt")

;; Symbolic Derivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This module provides procedures for symbolic derivation of MathExpressions.

(provide 
 (contract-out
  ;; Take the symbolic Derivative of the given Math-Expression
  [deriv (-> math-expr? constant? math-expr?)]
  ;; Is the given value a Math-Expression?
  [math-expr? (-> any/c boolean?)]))

;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deriv expr var)
  (match expr 
    [(matches-constant? var) 0]
    [(== var) 1]
    [(list (? plus-or-minus-sign? op) a b)
     (list op (deriv a var) (deriv b var))]
    [(cons '* (list-no-order (matches-constant? var a) b))
     (list '* a (deriv b var))]
    [(list '/ (function-of? var a) (function-of? var b))
     (list '/ (list '- (list '* (deriv a var) b) (list '* (deriv b var) a)) (list '^ b 2))]
    [(list '^ (function-of? var a) power)
     (list '* power (list '^ a (list '- power 1)))]
    [else (error "Unrecognized expression: " expr)]))

;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plus-or-minus-sign? op)
  (or (eq? op '-) (eq? op '+)))

;; Math-Expression Symbol -> Boolean
;; Does the given expression contain the given symbol as a variable?
(define (contains-variable? expr var)
  (match expr
    [(? number?) #f]
    [(== var) #t]
    [(list _ subexpr) (contains-variable? subexpr var)]
    [(list _ left right) (or (contains-variable? left var)
                             (contains-variable? right var))]))

;; Match Expanders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Checks if the given variable exists in the subexpression.
(define-match-expander function-of? 
  (λ (stx)
    (syntax-case stx ()
      [(_ var id)
       #'(? (curryr contains-variable? var) id)])))
   

;; An Expression that matches a constant is either a number or a symbol that 
;; is not eq? to the given symbol.
(define-match-expander matches-constant?
  (λ (stx)
    (syntax-case stx ()
      [(_ var)
       #'(or (? number?)
             (and (? symbol?) (== var (negate eq?))))]
      [(_ var id) 
       #'(or (? number? id)
             (and (? symbol? id) (== var (negate eq?))))])))

;; Tests

(module+ test 
  (require rackunit)
  (define tderiv (curryr deriv 'x))
  
  ;; derivative of a number
  (check-equal? (tderiv 5) 0)
  
  ;; derivative of a constant symbol
  (check-equal? (tderiv 'a) 0)
  
  ;; constant division Rule
  (check-equal? (tderiv '(* 5 x)) '(* 5 1))
  (check-equal? (tderiv '(* x 5)) '(* 5 1))
  (check-equal? (tderiv '(* x a)) '(* a 1))
  
  ;; Sum Rule
  (check-equal? (tderiv '(+ x x)) `(+ ,(tderiv 'x) ,(tderiv 'x)))
  (check-equal? (tderiv '(- x x)) `(- ,(tderiv 'x) ,(tderiv 'x)))
  
  ;; Power Rule
  (check-equal? (tderiv '(^ x 2)) '(* 2 (^ x (- 2 1))))
  
  ;; Quotient Rule
  (check-equal? (tderiv '(/ x (+ x 1))) '(/ (- (* 1 (+ x 1)) (* (+ 1 0) x)) (^ (+ x 1) 2)))
  
  ;; Check the error case for the sake of coverage
  (check-exn exn:fail? (thunk (tderiv (list)))))
      