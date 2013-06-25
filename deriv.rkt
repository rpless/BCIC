#lang racket

(require rackunit)

;; Because I Can't Calculus (BCIC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some of people are masters of calculus. This library is not for those people.

(provide 
 (contract-out
  ;; Take the symbolic Derivative of the given Math-Expression
  [deriv (-> math-expr? math-expr?)]
  ;; Is the given value a Math-Expression?
  [math-expr? (-> any/c boolean?)]))

;; Data Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; A Math-Expression is oneof:
;; - Constant
;; - Number
;; - Unary Operation
;; - Binary Operation
;; - Function Application

;; A Constant is a symbol that is not a member of UNARY-OPERATORS, 
;; BINARY-OPERATORS, or FUNTIONS lists

;; A Number is any valid number? in Racket

;; A Unary Operation is a (list Unary-Operator Math-Expression)
(define UNARY-OPERATORS '(-))

;; A Binary Operation is a (list Binary-Operator Math-Expression Math-Expression)
(define BINARY-OPERATORS '(+ - * /))

;; A Function Application is a (list Function-Name Math-Expression)
(define FUNCTIONS '(sin cos tan))

;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deriv x) x)

;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (math-expr? v) 
  (or (constant? v)
      (number? v)
      (unary-expression? v)
      (binary-expression? v)
      (function-application? v)))

;; Any -> Boolean
;; Is the given value a Unary-Expression?
(define (unary-expression? v)
  (expr-with-n-args-and-pred? v unary-operator? 2))

;; Any -> Boolean
;; Is the given value a Binary-Expression?
(define (binary-expression? v)
  (expr-with-n-args-and-pred? v binary-operator? 3))

;; Any -> Boolean
;; Is the given value a Function-Application?
(define (function-application? v)
  (expr-with-n-args-and-pred? v function-name? 2))

;; Any [Any -> Boolean] Integer -> Boolean
;; Does the given v have n arguments that are math expressions and its first 
;; expression satisifies the given predicate?
(define (expr-with-n-args-and-pred? v pred n)
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
       (not (binary-operator? v))
       (not (function-name? v))))

;; Any -> Boolean
;; Is the given value a Unary-Operator?
(define (unary-operator? v)
  (cons? (member v UNARY-OPERATORS)))

;; Any -> Boolean
;; Is the given value a Binary-Operator?
(define (binary-operator? v)
  (cons? (member v BINARY-OPERATORS)))

;; Any -> Boolean
;; Is the given value a Function-Name?
(define (function-name? v)
  (cons? (member v FUNCTIONS)))

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
  (check-false (constant? (first FUNCTIONS)))
  
  ;; unary-operator? tests
  (check-true (unary-operator? (first UNARY-OPERATORS)))
  (check-false (unary-operator? (third BINARY-OPERATORS)))
  (check-false (unary-operator? 'a))
  (check-false (unary-operator? (first FUNCTIONS)))
  
  ;; binary-operator? tests
  (check-true (binary-operator? (third BINARY-OPERATORS)))
  (check-false (binary-operator? 'a))
  (check-false (binary-operator? (first FUNCTIONS)))
  
  ;; function name tests
  (check-true (function-name? (first FUNCTIONS)))
  (check-false (function-name? 'a))
  (check-false (function-name? (first BINARY-OPERATORS)))
  
  ;; unary-expression? tests
  (check-true (unary-expression? uexpr1))
  (check-false (unary-expression? bexpr1))
  (check-false (unary-expression? fexpr1))
  
  ;; binary-expression? tests
  (check-false (binary-expression? uexpr1))
  (check-true (binary-expression? bexpr1))
  (check-false (binary-expression? fexpr1))
  
  ;; function-application? tests
  (check-false (function-application? uexpr1))
  (check-false (function-application? bexpr1))
  (check-true (function-application? fexpr1)))