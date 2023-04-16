;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw10-problem3 (done+fixSig again)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Design the data SimpleExpression that allows you to represent
;           arithmetic expressions given the following requirements:
;           - There are two valid operations: add (+) and multiply (*)
;           - Each operation is applied to at least one operand, which may be a
;             number or another expression
;
;           Some example expressions include...
;            5
;            3.14 * 2
;            (3 + 4) * (1 + -2 + 7) * (5)
;            
;           Make sure to follow all steps of the design recipe for data and
;           define at least the examples above.

; + and *
; one simple expression

; An operator is one of:
; +
; *

; Interpretation: An operation

(define add "+")

(define multiply "*")

(define (operator-temp x)
  (...
   (cond
     [(string=? add x) ...]
     [(string=? multiply x) ...])))


(define-struct simpleExpression [operator losExp])

(define (simpleExpression-temp x)
  (...
   (cond
     [(number? x) ...]
     [(simpleExpression? x)
      (operator-temp (simpleExpression-operator x))                  
      (simpleExpression-losExp x) ...])))

; A SimpleExpression is One of:
; number
; (make-simpleExpression (operator [List-of simpleExpression])

; Interpretation : Expression

(define exp1number 5)
(define exp2number 7)

(define simpExp1 (make-simpleExpression multiply (list 3.14 2)))
(define simpExp2 (make-simpleExpression add (list (make-simpleExpression multiply (list 3 4))
                                                  (make-simpleExpression multiply (list 11 8)))))




; TODO 2/2: Now design the function evaluate that takes a SimpleExpression and
;           produces its numerical result. So for the examples above (where <=
;           means that the expression on the right evaluates to the value on the
;           left...
;
;            5    <= 5
;            6.28 <= 3.14 * 2
;            210  <= (3 + 4) * (1 + -2 + 7) * (5)

(check-expect (evaluate exp1number) 5)
(check-expect (evaluate exp2number) 7)
(check-expect (evaluate simpExp1) 6.28)
(check-expect (evaluate simpExp2) 100)

; evaluate : SimpleExpression -> Num
; produces its numerical result based on operator
(define (evaluate x)
  (cond
    [(number? x) x]
    [(simpleExpression? x)
     (foldr (newoperator-temp (simpleExpression-operator x))
            (operater-help (simpleExpression-operator x))
            (map evaluate (simpleExpression-losExp x)))]))


; newoperator-temp : String -> operator
; does proper operation based on operator used

(define (newoperator-temp x)
  (cond
    [(string=? add x) +]
    [(string=? multiply x) *]))

;operater-help : String -> num
; provides proper base case for evaluate function based on operation used

(define (operater-help n)
  (cond
    [(string=? add n) 0]
    [(string=? multiply n) 1]))



    
  


