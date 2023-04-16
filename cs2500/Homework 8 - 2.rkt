;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw8-problem2 done|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IMPORTANT:
; 1. The functions that you design for this problem *must* use the ISL list
;    abstraction(s); you MAY NOT use recursion: doing so will lead you to get
;    no code credit for the function :(
;
; 2. The planning part of this problem is in the SAME file used for ALL parts
;    of Homework 8.


; Your task here will be to design the function elim-contains-char, which takes
; a list of strings and produces a list of the same strings, in the same order,
; but excluding those strings that contain a supplied character (represented as
; a 1String). For clarity, here is the intended signature:

; elim-contains-char : 1String [List-of String] -> [List-of String]


; Note: For purposes of this problem, you should NOT use string-contains? (or
;       similar). Instead, use the explode function to treat the supplied
;       string as a list of characters (each represented as a 1String).


; TODO 1/2: Plan your solution, using the planning interface described on the
;           canvas page for this assignment. ALL PLANNING FOR THIS HW WILL BE
;           DONE IN THE SAME PLACE, AND SUBMITTED TOGETHER.

; TODO 2/2: Design the function elim-contains-char using the ISL list
;           abstractions. YOUR CODE SHOULD NOT USE ANY RECURSION.

; elim-contains-char : 1String [List-of String] -> [List-of String]
; Produces a list of the same strings, in the same order,
; but excluding those strings that contain a supplied character



(check-expect (elim-contains-char "a" (list "apple" "banana" "apricot")) '())
(check-expect (elim-contains-char "t" (list "the" "table" "john")) (list "john"))
(check-expect (elim-contains-char "c" (list "Chris" "Ditch" "Harris")) (list "Chris" "Harris"))


(define (elim-contains-char OneString LoS)
  (local [

          ; funcExplode : String -> Boolean
          ; Determines if any string in the list doesn't match the given
          ; OneString and returns it
          (define (funcExplode S)
            (not (ormap (lambda (letters) (string=? letters OneString)) (explode S))))]
         

    (filter funcExplode LoS)))
          




