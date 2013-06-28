#lang racket
(require "data.rkt")

;; Arithmetic Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Arithmetic Module provides utilities for simplifying arithmetic 
;; expressions.

;; Simplify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify expr)
  (match expr
    [(cons '* (list-no-order a (? (λ (x) (and (number? x) (zero? x)))))) 0]
    [(cons '* (list-no-order a (== 1))) a]
    [(list '/ numerator (== 1)) numerator]
    [else expr]))

;; Simplification Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  ;; Zero Elimination
  (check-equal? (simplify '(* 0 x)) 0)
  
  ;; One Elimination
  (check-equal? (simplify '(* 1 x)) 'x)
  (check-equal? (simplify '(/ x 1)) 'x))