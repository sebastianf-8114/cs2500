;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw1-problem1 (new)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Understanding code that doesn't have effective naming and documentation
; can be really challenging! (That's part of the reason that this class, and
; most organizations with software-development teams, employ style guides.)

; As an example, consider the confusing functions defined below...

; Hints:
;  - For built-in functions that aren’t familiar to you,
;    be sure to look them up in the DrRacket documentation!
;  - Once you have a theory about what a function is doing, try
;    running it in the interactions window, then try changing
;    parameters to confirm how it all works!
;  - You might start with smaller, simpler functions, and
;    then that can help understand bigger ones (that use them!)
(define a "dog")
(define b 9)
(define c "red") 

; SIGNATURE HERE: String + String -> String (fixed)

; PURPOSE HERE: Combine two strings and uppercase a (once declared) since it is a placeholder for a
; string. Also add four "!" to the end of the combined string
(define (nonplus1 a b) ; (define (name variable variable ...)
  (string-append b; adding strings b then a
   (string-upcase a) ; Uppercase "a" (string)
   (replicate 4 "!"))) ; amount (natural number) of ! (string)

(define (NONPLUS1-ACTUAL value)
  ("abc" "def"))

(define (NONPLUS1-EXPECTED value)
  ("DEF!!!!"))
                    

; SIGNATURE HERE: int, string, int -> int
  
; PURPOSE HERE: will compare int values of a b c and print the smallest value
(define (nonplus2 a b c) ; (define (name variable variable ...)
  (min ; minimum between (max(string-length b) (a) and (c)
   (max ; max between (string-length b) and (a)
    (string-length b) ; length of B
    a) ;refering to nonplus2
   c)) ; refering to nonplus2

(define (NONPLUS2-ACTUAL value)
  (string-length("String" "String")) int)

(define (NONPLUS2-EXPECTED value)
  (max int))
                    

; SIGNATURE HERE string -> string
; PURPOSE HERE  Convert a string and then input the emoji as a text, below original text
(define (nonplus3 a b c)
  (above
   (text (nonplus1 a b) 30 c)
   (text (replicate (nonplus2 b a 5) "⭐") 20 c)))

(define (NONPLUS3-ACTUAL value)
  (above
   ("String1")
   ("String2")))

(define (NONPLUS3-EXPECTED value)
  ("String1")
  (⭐⭐⭐⭐⭐⭐))
                    

; TODO 1/2: Replace each "SIGNATURE HERE" and "PURPOSE HERE" with signature
;           and purpose statements for that function. Remember, a purpose
;           statement should say *what* a function does, not just re-state
;           the signature; the signature precisely *how* to use the function,
;           that is, what type(s) of data need to be supplied (in what order)
;           and what type of data will be returned. In some sense, the purpose
;           helps someone figure out if they will find a function useful, and
;           then the signature helps them use it.

; TODO 2/2: Confirm the above TODO by defining at least one pair of constants
;           per function, according to the following rules...
;           - In each pair, end the name of one -ACTUAL and the other -EXPECTED;
;             for example, NONPLUS1-ACTUAL and NONPLUS1-EXPECTED
;           - The "ACTUAL" value should call the function with a set of arguments
;             you select, which must adhere to your signature; the "EXPECTED"
;             should be the value (like "hi!") or expression (like (text "hi!" 5 "black"))
;             you expect ACTUAL to be when executed;
;             for example, the value for NONPLUS1-ACTUAL could be (nonplus1 3 4)
;             and the value for NONPLUS1-EXPECTED could be 7, if you thought the
;             function added two numeric arguments
;           - If you would like to demonstrate multiple actual/expected pairs, you are
;             welcome, in which case just be reasonable in how you name them (e.g.,
;             NONPLUS1-ACTUAL-A, NONPLUS1-EXPECTED-A)
;           - Now run your program and make sure each ACTUAL/EXPECTED pair is equal
;             (you can do this in the intermediate window, or using a check-expect)
;
;           Note: as we proceed in the course, you'll be doing this a lot in the form
;           of tests, since they are helpful to document your code, as well as make
;           sure it works the way you think it does!


; I was told by the TA that the "ACTUAL" and "EXPECTED" Values should not be as a comment 


