;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw6-problem1 (1+1+12)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In this problem you'll practice with lists by designing a function to
; help bookstores!

; Most bookstores sort the books on their shelves by the author’s last
; name. Unfortunately, some bookstore patrons do not preserve this order
; when browsing books, and simply place books back wherever they fit.

; Assume that bookstores keep all authors whose last name starts with the
; same letter on the same shelf, and those shelves are labeled with that
; letter. A record of which authors are on a given shelf would be represented
; using the following data definitions:

(define-struct shelf [letter authors])

; A Shelf is a (make-shelf 1String [List-of String])
; Interpretation: a record of the letter the authors' last name *should* start
; with, and the list of the *actual* last names on the shelf.

(define SHELF-1 (make-shelf "A" (list "Austen" "Hurston" "Angelou" "Butler" "Alvarez")))
(define SHELF-2 (make-shelf "B" (list)))
(define SHELF-3 (make-shelf "C" (list "Carle" "Coates")))

(define LOSh-0 (list))
(define LOSh-1 (list SHELF-1))
(define LOSh-2 (list SHELF-1 SHELF-2))
(define LOSh-3 (list SHELF-1 SHELF-2 SHELF-3))

(define (shelf-temp s)
  (... (shelf-letter s) ...
       (list-of-string-temp (shelf-authors s)) ...))


; TODO 1/1: Design the function fix-shelves that takes a list of Shelf records
;           and produces a list of Shelf records where at least one author does
;           not belong on the Shelf. The output Shelf records should only contain
;           the authors who don't belong on that shelf. Shelf records and the authors
;           within those records should be in the same order in the output as they
;           appear in the input. Do not generate empty Shelf records; this generates
;           needlessly long reports, which annoys the employees. You have been
;           supplied a test for clarity (which you can use in your design, but
;           should supplement). Make sure your solution follows the (list) templates!

; fix-shelves : [List-of Shelf] -> [List-of Shelf]
; Takes a list of Shelf records and produces a list of Shelf records where
; at least one author does not belong on the Shelf.


(check-expect (fix-shelves LOSh-0)
              (list))

(check-expect (fix-shelves LOSh-1)
              (list (make-shelf "A" (list "Hurston" "Butler"))))

(check-expect (fix-shelves LOSh-2)
              (list (make-shelf "A" (list "Hurston" "Butler"))))

(check-expect (fix-shelves LOSh-3)
              (list (make-shelf "A" (list "Hurston" "Butler"))))


(define (fix-shelves ListOfShelf)
  (cond
    [(empty? ListOfShelf) ListOfShelf]
    [(cons? ListOfShelf)
     (if (ShelfValid (first ListOfShelf))
         (cons
          (make-shelf (shelf-letter (first ListOfShelf))
                      (LetterHelper (shelf-authors (first ListOfShelf))
                                    (shelf-letter (first ListOfShelf))))
          (fix-shelves (rest ListOfShelf)))
         (fix-shelves (rest ListOfShelf)))]))

; Notes: Recursion, make sure it goes down if it isn't valid
         


; ShelfValid : Shelf -> Boolean
; Checking to see if a single-shelf has a misplaced author

(check-expect (ShelfValid SHELF-1) #t)
(check-expect (ShelfValid SHELF-2) #f)
(check-expect (ShelfValid SHELF-3) #f)

(define (ShelfValid s)
  (cons? (LetterHelper (shelf-authors s) (shelf-letter s))))



; LetterHelper : [List-of String] 1String -> [List-of String]
;Returns a new List of authors that doesnt match with the first letter
(check-expect (LetterHelper (list "Austen" "Hurston" "Angelou" "Butler" "Alvarez") "A")
              (list "Hurston" "Butler"))

(check-expect (LetterHelper (list "Dog" "James" "Jonathan" "Kingston" "Juan") "J")
              (list "Dog" "Kingston"))

(check-expect (LetterHelper (list) "A") LOSh-0) ;LOSh-0 = '()


(define (LetterHelper ListOfString shelf-letter)
  (cond
    [(empty? ListOfString) ListOfString]
    [(cons? ListOfString)
     (if (string=? (substring (first ListOfString) 0 1) shelf-letter)
         (LetterHelper (rest ListOfString) shelf-letter)
         (cons (first ListOfString) (LetterHelper (rest ListOfString) shelf-letter)))]))
       
    
    
    


