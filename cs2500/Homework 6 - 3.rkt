;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw6-problem3 (1+1+1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Back to Wordle (https://www.nytimes.com/games/wordle/)!

; We are going to put together some work you did in prior assignments...
; - In HW1, you made code for a boxed-letter function, which could create a
;   letter in a box, given lots of specific details (about color, size, etc).
; - In HW2, you designed an ls->color function, which would return the color
;   associated with the status of a letter.
; - In HW3, you designed the LetterStatusPair data type, which could represent
;   the pairing of a letter with its status.

; Moving towards the full game, it's going to be handy to be able to more
; simply draw various boxed letters (e.g., those that are part of the current
; guess, as well as those that were previously guessed). So let's build & use
; some abstractions :)

; TODO 1/2: Re-design boxed-letter to work a bit differently: instead of being
;           supplied details of how to draw the box (e.g., background/border
;           colors), it takes a function to make one (given a size), as well as
;           a letter, and then places the letter onto the background produced
;           by the function (with a font size proportional to the box size).
;
;           For clarity, you have been provided a function to produce a "blank"
;           background (useful while typing a guess) - when combined with your
;           new boxed-letter function, the supplied test should pass. You should
;           supplement this test with others in your design.


(define BG-COLOR "white")
(define BORDER-COLOR "dimgray")
(define GUESS-COLOR "black")

; A LetterStatus (LS) is one of:
; - "wrong"
; - "misplaced"
; - "right"
; Interpretation: status of a guessed letter

(define LS-WRONG "wrong")
(define LS-MISPLACED "misplaced")
(define LS-RIGHT "right")

(define (ls-temp ls)
  (...
   (cond
     [(string=? ls LS-WRONG) ...]
     [(string=? ls LS-MISPLACED) ...]
     [(string=? ls LS-RIGHT) ...])))



(define-struct lsp [letter status])

; A LetterStatusPair (LSP) is a (make-lsp 1String LetterStatus)
; Interpretation: the status associated with a letter

(define LSP-A-R (make-lsp "A" LS-RIGHT))
(define LSP-B-W (make-lsp "B" LS-WRONG))
(define LSP-C-M (make-lsp "C" LS-MISPLACED))

(define (lsp-temp lsp)
  (... (lsp-letter lsp) ...
       (ls-temp (lsp-status lsp)) ...))


; blank : NonNegReal -> Image
; produces a blank box in the appropriate size

(check-expect (blank 5)
              (overlay (square 5 "outline" BORDER-COLOR)
                       (square 5 "solid" GUESS-COLOR)))

(check-expect (blank 7)
              (overlay (square 7 "outline" BORDER-COLOR)
                       (square 7 "solid" GUESS-COLOR)))

(define (blank size)
  (overlay (square size "outline" BORDER-COLOR)
           (square size "solid" GUESS-COLOR)))



; createImage : LetterStatus -> [NonNegReal -> Image]
; Helper function to produce a color based on string of LetterStatus.

; Cannot Compare Functions
 
(define (createImage ls)
  (cond
    [(string=? ls "right")
     createImageHelperRight]
    [(string=? ls "wrong")
     createImageHelperWrong]
    [(string=? ls "misplaced")
     createImageHelperMisPlaced]))


; createImageHelperRight : NonNegReal -> Image
; Produces a right guess image given size

(check-expect (createImageHelperRight 70) (square 70 "solid" "darkgreen"))
(check-expect (createImageHelperRight 10) (square 10 "solid" "darkgreen"))

(define (createImageHelperRight size)
  (square size "solid" "darkgreen"))

; createImageHelperWrong : NonNegReal -> Image
; Produces a wrong guess image given size

(check-expect (createImageHelperWrong 70) (square 70 "solid" "dimgray"))
(check-expect (createImageHelperWrong 10) (square 10 "solid" "dimgray"))

(define (createImageHelperWrong size)
  (square size "solid" "dimgray"))

; createImageHelperMisPlaced : NonNegReal -> Image
; Produces a misplaced guess image given size

(check-expect (createImageHelperMisPlaced 70) (square 70 "solid" "goldenrod"))
(check-expect (createImageHelperMisPlaced 10) (square 10 "solid" "goldenrod"))

(define (createImageHelperMisPlaced size)
  (square size "solid" "goldenrod"))


;; Checking two functions at a time


; image-Right : [Number -> Image] -> Image
; Creates the "right" "background" of Wordle based on the status of letter from 'createImage'

(define image-Right (createImage "right"))
(check-expect (image-Right 50) (square 50 "solid" "darkgreen"))
;

; image-Right : [Number -> Image] -> Image
; Creates the "wrong" "background" of Wordle based on the status of letter from 'createImage'
(define image-Wrong (createImage "wrong"))
(check-expect (image-Wrong 50) (square 50 "solid" "dimgray"))
;
; image-Right : [Number -> Image] -> Image
; Creates the "misplaced" "background" of Wordle based on the status of letter from 'createImage'
(define image-MisPlaced  (createImage "misplaced"))
(check-expect (image-MisPlaced 50) (square 50 "solid" "goldenrod"))



; boxed-letter: 1String Num [NonNegReal -> Image] -> Image
; Used to vizualize a single letter within a box

(check-expect
 (boxed-letter "B" 64 blank)
 (overlay
  (text "B" 32 BG-COLOR)
  (square 64 "outline" BORDER-COLOR)
  (square 64 "solid" GUESS-COLOR)))

(check-expect
 (boxed-letter "a" 20 blank)
 (overlay
  (text "a" 10 BG-COLOR)
  (square 20 "outline" BORDER-COLOR)
  (square 20 "solid" GUESS-COLOR)))



;(define (boxed-letter letter size blank)
;  (overlay
;   (text letter (/ size 2) BG-COLOR)
;   (square size "outline" BORDER-COLOR)
;   (square size "solid" GUESS-COLOR)))



(define (boxed-letter letter size func)
  (overlay (text letter (/ size 2) BG-COLOR) (func size)))





; TODO 2/2: Now, use your boxed-letter abstraction to design two useful
;           functions: guess-letter->image and lsp->image. The former
;           produces a visualization of a letter on a blank background,
;           while the latter produces a visualization of a LetterStatusPair
;           (LSP) on a background associated with he letter's status.
;           You have been supplied some tests for clarity (which you can use
;           in your design, but should supplement).
;
;           You are welcome to use your own (correct) design of LetterStatus,
;           LetterStatusPair, or ours; either way, include it below as a part
;           of your solution to this problem. (Note that you might have to
;           adjust your constants and structure name to accommodate the
;           supplied tests.)





(define LT-SIZE 64)

; guess-letter->image : 1String -> Image
; Produces a visualization of a letter on a blank background.

(check-expect
 (guess-letter->image "A")
 (overlay
  (text "A" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(check-expect
 (guess-letter->image "B")
 (overlay
  (text "B" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(check-expect
 (guess-letter->image "C")
 (overlay
  (text "C" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))


 
(define (guess-letter->image letter)
  (boxed-letter letter LT-SIZE blank))
 

; lsp->image : LetterStatusPair -> Image
; Produces a visualization of a LetterStatusPair (LSP) on a background
; associated with the letter's status.


(define (lsp->image lsp)
  (boxed-letter (lsp-letter lsp) LT-SIZE (createImage (lsp-status lsp))))



(check-expect
 (lsp->image (make-lsp "A" "right"))
 (overlay
  (text "A" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "darkgreen")))

(check-expect
 (lsp->image (make-lsp "B" "wrong"))
 (overlay
  (text "B" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "dimgray")))

(check-expect
 (lsp->image (make-lsp "C" "misplaced"))
 (overlay
  (text "C" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "goldenrod")))


