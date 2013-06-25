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
    [(? constant? ex) (if (symbol=? expr var) 1 0)]
    [(? number?) 0]
    [(list (== '+) (? math-expr? a) (? math-expr? b)) (addition-rule a b var)]
    [else (error "Unrecognized expression " expr ".")]))

(define (addition-rule a b var)
  `(+ ,(deriv a var) ,(deriv b var)))
      

;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (math-expr? v) 
  (or (constant? v)
      (number? v)
      (unary-expression? v)
      (binary-expression? v)))

;; Any -> Boolean
;; Is the given value a Unary-Expression?
(define (unary-expression? v)
  (expr-with-pred-and-n-args? v unary-operator? 2))

;; Any -> Boolean
;; Is the given value a Binary-Expression?
(define (binary-expression? v)
  (expr-with-pred-and-n-args? v binary-operator? 3))

;; Any [Any -> Boolean] Integer -> Boolean
;; Does the given v have n arguments that are math expressions and its first 
;; expression satisifies the given predicate?
(define (expr-with-pred-and-n-args? v pred n)
  (and (cons? v)
       (= (length v) n)
       (pred (first v))
       (for/and ([i (sub1 n)])
         (math-expr? (list-ref v (add1 i))))))

;; Any -> Boolean
;; Is the given value a Constant?
(define (constant? v) 
  (and (symbol? v)
       (not (unary-operator? v))
       (not (binary-operator? v))))

;; Any -> Boolean
;; Is the given value a Unary-Operator?
(define (unary-operator? v)
  (cons? (member v UNARY-OPERATORS)))

;; Any -> Boolean
;; Is the given value a Binary-Operator?
(define (binary-operator? v)
  (cons? (member v BINARY-OPERATORS)))

;; Predicate Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define uexpr1 '(- 1))
  (define bexpr1 '(+ 1 2))
  (define fexpr1 '(sin 1))
  
  ;; test what symbols are valid constants
  (check-true (constant? 'a))
  (check-true (constant? 'x))
  (check-true (constant? 'rho))
  (check-false (constant? (first BINARY-OPERATORS)))
  (check-false (constant? (first UNARY-OPERATORS)))
  
  ;; unary-operator? tests
  (check-true (unary-operator? (first UNARY-OPERATORS)))
  (check-false (unary-operator? (third BINARY-OPERATORS)))
  (check-false (unary-operator? 'a))
  
  ;; binary-operator? tests
  (check-true (binary-operator? (third BINARY-OPERATORS)))
  (check-false (binary-operator? 'a))
  
  ;; unary-expression? tests
  (check-true (unary-expression? uexpr1))
  (check-false (unary-expression? bexpr1))
  (check-true (unary-expression? fexpr1))
  
  ;; binary-expression? tests
  (check-false (binary-expression? uexpr1))
  (check-true (binary-expression? bexpr1))
  (check-false (binary-expression? fexpr1)))