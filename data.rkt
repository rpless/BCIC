#lang racket

;; Because I Can't Calculus (BCIC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some of people are masters of calculus. This library is not for those people.

;; This module provides a data defintion for MathExpressions, which are a 
;; symbolic representation of mathematical expressions.

(provide UNARY-OPERATORS BINARY-OPERATORS math-expr? constant?)

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A MathExpression is oneof:
;; - Constant
;; - Number
;; - Unary Operation
;; - Binary Operation

;; A Constant is a symbol that is not a member of UNARY-OPERATORS or BINARY-OPERATORS

;; A Number is any valid number? in Racket

;; A UnaryOperation is a (list Unary-Operator Math-Expression)

;; A UnaryOperator is any member of the UNARY-OPERATORS list.
(define UNARY-OPERATORS '(- / sin cos tan))

;; A BinaryOperation is a (list Binary-Operator Math-Expression Math-Expression)

;; A BinaryOperator is any member of the BINARY-OPERATORS list.
(define BINARY-OPERATORS '(+ - * / ^))

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
  (require rackunit)
  
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
