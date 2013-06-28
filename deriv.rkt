#lang racket

(require rackunit "data.rkt")

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
    [else (error "Unrecognized expression: " expr)]))

;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plus-or-minus-sign? op)
  (or (eq? op '-) (eq? op '+)))

;; Match Expanders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Expression that matches a constant is either a number or a symbol that 
;; is not eq? to the given symbol.
(define-match-expander matches-constant?
  (λ (stx)
    (syntax-case stx ()
      [(_ var)
       #'(or (? number?)
             (and (? symbol?) (== var (negate eq?))))]
      [(_ var a) 
       #'(or (? number? a)
             (and (? symbol? a) (== var (negate eq?))))])))

;; Tests

(module+ test 
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
  
  ;; Check the error case for the sake of coverage
  (check-exn exn:fail? (thunk (tderiv (list)))))
      