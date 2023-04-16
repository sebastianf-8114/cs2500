;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw6-problem2(1+1 +1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; It's super useful to be able to answer the question: does a value appear
; in a list? But of course that question can be phrased multiple ways...

; TODO 1/3: Design two functions: string-in-list? and string-in-list-ci?.
;           The first returns #true if a supplied string appears
;           *exactly* in a list of strings, whereas the second returns
;           #true if a supplied string occurs in a list of strings if
;           we ignore lower/upper-case. You have been supplied some tests
;           for clarity (which you can use in your design, but should
;           supplement). Make sure your code follows the list template!

; string-in-list?/old : String [List-of String] -> Boolean
; returns #true if a supplied string appears *exactly* in a list of strings
(define (string-in-list?/old str los)
  (cond
    [(empty? los) #f]
    [(cons? los)
     (if
      (string=? str (first los)) #t
      (string-in-list? str (rest los)))]))

; string-in-list-ci?/old : String [List-of String] -> Boolean
; returns #true if a supplied string occurs in a list of strings if we ignore lower/upper-case
(define (string-in-list-ci?/old str los)
  (cond
    [(empty? los) #f]
    [(cons? los)
     (if
      (string-ci=? str (first los)) #t
      (string-in-list? str (rest los)))]))

(check-expect (string-in-list?/old "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list?/old "A" (list "a" "b" "c")) #f)
(check-expect (string-in-list-ci?/old "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci?/old "A" (list "a" "b" "c")) #t)




; TODO 2/3: Those two functions probably feel rather similar - so now
;           design the abstraction value-in-list? based on these two
;           functions.

;           Notes:
;           - Think through your signature to make sure it is as general
;             as possible, while still not making promises your abstraction
;             cannot keep!
;           - Don't forget to improve your implementations for the last
;             step! (Importantly: keep the old code by renaming the
;             functions string-in-list?/old and string-in-list-ci?/old;
;             you do not need to change/reproduce any parts of the function
;             design recipe for these old function implementations.)

; value-in-list? : (X) X [List-of X] [X X -> Boolean] -> Boolean
; check if the predicate is true for any of the values in the given list 
(check-expect (value-in-list? "a" (list "a" "b" "c") string=?) #t)
(check-expect (value-in-list? "C" (list "a" "b" "c") string=?) #f)
(check-expect (value-in-list? "f" (list "a" "b" "c") string=?) #f)
(check-expect (value-in-list? "A" (list "a" "b" "c") string-ci=?) #t)
(check-expect (value-in-list? "c" (list "a" "b" "c") string-ci=?) #t)
(check-expect (value-in-list? "e" (list "a" "b" "c") string-ci=?) #f)
(check-expect (value-in-list? "" (list "a" "b" "c") string-ci=?) #f)
(check-expect (value-in-list? "" (list "a" "b" "c") string=?) #f)

(define (value-in-list? val lox func)
  (cond
    [(empty? lox) #f]
    [(cons? lox)
     (or
      (func val (first lox))
      (value-in-list? val (rest lox) func))]))

; string-in-list? : String [List-of String] -> Boolean
; returns #true if a supplied string appears *exactly* in a list of strings
(check-expect (string-in-list? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list? "d" (list "a" "b" "c")) #f)
(check-expect (string-in-list? "A" (list "a" "b" "c")) #f)
(check-expect (string-in-list? "" (list "a" "b" "c")) #f)

(define (string-in-list? str los)
  (value-in-list? str los string=?))

; string-in-list-ci? : String [List-of String] -> Boolean
; returns #true if a supplied string occurs in a list of strings if we ignore lower/upper-case
(check-expect (string-in-list-ci? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci? "A" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci? "" (list "a" "b" "c")) #f)
(check-expect (string-in-list-ci? "d" (list "a" "b" "c")) #f)

(define (string-in-list-ci? str los)
  (value-in-list? str los string-ci=?))




; TODO 3/3: Now put your fancy new abstraction to good use! Design the function
;           anything-bigger? that determines if any of a list of numbers is
;           bigger than a supplied number. You have been supplied some tests
;           for clarity (which you can use in your design, but should supplement).

; anything-bigger? : Num [List-of Numbers] -> Boolean
; determines if any of a list of numbers is bigger than a supplied number
(check-expect (anything-bigger? 5 (list 10 -1 3)) #t)
(check-expect (anything-bigger? 100 (list 10 -1 3)) #f)

(define (anything-bigger? num lon)
  (value-in-list? num lon <))

