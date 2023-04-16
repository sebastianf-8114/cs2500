;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw8-problem1 done+1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; explode : Consumes a string and returns a list of one string 

; IMPORTANT:
; 1. The functions that you design for this problem *must* use the ISL list
;    abstraction(s); you MAY NOT use recursion: doing so will lead you to get
;    no code credit for the function :(
;
; 2. The planning part of this problem is in the SAME file used for ALL parts
;    of Homework 8.


; Recall Homework 6, where you designed a function to help bookstores - you
; are now going to solve the same problem, but this time using ISL list
; abstractions.

; As before, assume that bookstores keep all authors whose last name starts
; with the same letter on the same shelf, and those shelves are labeled with
; that letter. A record of which authors are on a given shelf would be
; represented using the following data definitions:


(define-struct shelf [letter authors])

; A Shelf is a (make-shelf 1String [List-of String])
; Interpretation: a record of the letter the authors' last name *should* start
; with, and the list of the *actual* last names on the shelf.

(define SHELF-1 (make-shelf "A" (list "Austen" "Hurston" "Angelou" "Butler" "Alvarez")))
(define SHELF-2 (make-shelf "B" (list)))
(define SHELF-3 (make-shelf "C" (list "Carle" "Coates")))


(define (shelf-temp s)
  (... (shelf-letter s) ...
       (list-of-string-temp (shelf-authors s)) ...))




; As before, your task will be to design the function fix-shelves, which takes
; a list of Shelf records and produces a list of Shelf records where at least
; one author does not belong on the Shelf. The output Shelf records should
; only contain the authors who don't belong on that shelf. Shelf records and
; the authors within those records should be in the same order in the output
; as they appear in the input. Do not generate empty Shelf records; this
; generates needlessly long reports, which annoys the employees.


; TODO 1/2: Plan your solution, using the planning interface described on the
;           canvas page for this assignment. ALL PLANNING FOR THIS HW WILL BE
;           DONE IN THE SAME PLACE, AND SUBMITTED TOGETHER.



; TODO 2/2: Design the function fix-shelves using the ISL list abstractions.
;           You have been supplied a test for clarity (which you can use in
;           your design, but should supplement). YOUR CODE SHOULD NOT USE ANY
;           RECURSION.

; fix-shelves : [List-of Shelf] -> [List-of Shelf]
; Takes a list of Shelf records and produces a list of Shelf records where
; at least one author does not belong on the Shelf.

(check-expect (fix-shelves (list SHELF-1 SHELF-2 SHELF-3))
              (list (make-shelf "A" (list "Hurston" "Butler"))))
(check-expect (fix-shelves (list SHELF-1 SHELF-2))
              (list (make-shelf "A" (list "Hurston" "Butler"))))
(check-expect (fix-shelves (list SHELF-2))
              (list))

(define (fix-shelves ListOfShelves)
  [local [
          
          ; middle : Shelf -> Shelf
          ; Goes through each shelf in the given list of shelves and makes a new shelf
          ; consisting of shelf-letter of each shelf and calling fix-author in each shelf by
          ; passing in two parameters ([List-of String] 1String)
          
          (define (middle shelf)
            (make-shelf
             (shelf-letter shelf) (fix-author (shelf-authors shelf) (shelf-letter shelf))))
          
          ; fix-author : [List-of String] 1String -> [List-of String]
          ; filters out a list of authors who's first letter doesn't
          ; match the supplied shelf-letter
          (define (fix-author ListOfAuthor OneString)
            (filter
             (lambda (author) (not (string=? (substring author 0 1) OneString))) ListOfAuthor))]
    (filter (lambda (shelf) (not (empty? (shelf-authors shelf)))) (map middle ListOfShelves))])


; (lamba
; (variable that is undefined but used - general function parameter (given item)) create function)

; Takes in first function and returns function in the second set 



     

; filter list of authirs that are mislpaced then u need to
; filter again the list of shelves that have a new list of authors
; do need map since you need a local

; 1filter  is going to check the string=? substring instead of 





