#lang racket
(provide UNARY-OPERATORS BINARY-OPERATORS)
;; Because I Can't Calculus (BCIC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some of people are masters of calculus. This library is not for those people.

;; This module provides a data defintion for MathExpressions, which are a 
;; symbolic representation of mathematical expressions.

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
(define BINARY-OPERATORS '(+ - * /))

;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
