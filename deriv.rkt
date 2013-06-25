#lang racket

(require rackunit)

;; Because I Can't Calculus (BCIC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

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
  (constant? v))

;; Any -> Boolean
;; Is the given value a Constant?
(define (constant? v) 
  (and (symbol? v)
       (not (member v UNARY-OPERATORS))
       (not (member v BINARY-OPERATORS))
       (not (member v FUNCTIONS))))

;; Predicate Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  
  ;; test what symbols are valid constants
  (check-true (constant? 'a))
  (check-true (constant? 'x))
  (check-true (constant? 'rho))
  
  (check-false (constant? '-))
  (check-false (constant? '+))
  (check-false (constant? 'cos)))
  
  