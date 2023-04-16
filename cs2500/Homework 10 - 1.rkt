;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw10-problem1 (done)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Finish designing the function countdown that takes a natural number
;           and a non-empty list of strings and produces a countdown message.
;
;           Note: you are NOT allowed to use list abstractions for this
;                 function; doing so will result in ZERO code credit :(


; countdown : Nat [NEList-of String] -> String
; produces a message of counting down the numbers and then
; listing out the messages

(check-expect (countdown 3 (list "go")) "3!2!1!go!")
(check-expect (countdown 0 (list "howdy")) "howdy!")
(check-expect (countdown 2 (list "check" "expect")) "2!1!check!expect!")

(define (countdown num 1list)
  (cond
    [(<= num 0) (listhelp 1list)]
    [(> num 0) (string-append (string-append (number->string num) "!") (countdown (- num 1) 1list))]))

; listhelp :[NEList-of String] -> String
; produces a listing of the messages when no number is given in countdown

(check-expect (listhelp (list "go")) "go!")
(check-expect (listhelp (list "howdy")) "howdy!")
(check-expect (listhelp (list "check" "expect")) "check!expect!")


(define (listhelp 1list)
  (cond
    [(empty? (rest 1list)) (string-append (first 1list) "!")]
    [(cons? (rest 1list))
     (string-append (string-append (first 1list) "!") (listhelp (rest 1list)))]))




; TODO 2/2: Finish designing the function lists-same? that determines if two
;           supplied lists have the "same" contents, as determined by a supplied
;           predicate.
;
;           Note: you are NOT allowed to use list abstractions for this
;                 function; doing so will result in ZERO code credit :(


; lists-same? : (X Y) [List-of X] [List-of Y] [X Y -> Boolean] -> Boolean
; determines if the two lists are the "same" based upon the predicate

(check-expect (lists-same? '() '() =) #t)
(check-expect (lists-same? '() (list 1) =) #f)
(check-expect (lists-same? (list 1 2) '() =) #f)

(check-expect (lists-same? (list 1) (list 1) =) #t)
(check-expect (lists-same? (list 1) (list 100) =) #f)
(check-expect (lists-same? (list 1 100) (list 1 2) =) #f)
(check-expect (lists-same? (list "a" "b" "c") (list "a" "b" "c") string=?) #t)
(check-expect (lists-same? (list "a" "b" "c") (list "c" "b" "a") string=?) #f)
(check-expect (lists-same? (list "a" "b" "c") (list "a" "b" "a") string=?) #f)

(check-expect (lists-same? (list 1 2 3) (list "1" "2" "3")
                           (λ (x y) (string=? (number->string x) y))) #t)

(check-expect (lists-same? (list "howdy" "world") (list "Howdy" "WORLD")
                           (λ (x y) (string=? (string-upcase x)
                                              (string-upcase y)))) #t)


(define (lists-same? 1l 2l func)
  (cond
    [(and (empty? 1l) (empty? 2l)) #t]
    [(and (cons? 1l) (empty? 2l)) #f]
    [(and (cons? 2l) (empty? 1l)) #f]
    [(and (cons? 1l) (cons? 2l))
     (and (func (first 1l) (first 2l))
          (lists-same? (rest 1l) (rest 2l) func))]))



