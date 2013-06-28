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

(define (deriv expr var) expr)

;; Tests

(module+ test 
  (define tderiv (curryr deriv 'x))
  
  
  ;; constant division Rule
  (check-equal? (tderiv '(* 5 x)) '(* 5 1))
  
  ;; Sum Rule
  (check-equal? (tderiv '(+ x x)) `(+ ,(tderiv 'x) ,(tderiv 'x)))
  (check-equal? (tderiv '(- x x)) `(- ,(tderiv 'x) ,(tderiv 'x))))
      