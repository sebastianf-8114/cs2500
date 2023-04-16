;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab1-starter (edited1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For any word of at least one character that starts with a letter,
; let’s say that its "bingo word" is the uppercase version of the
; first letter, followed by a space, and then followed by the number
; of characters in the word. For example, the bingo word of "bingo"
; is "B 5" and the bingo word of "Win" is "W 3".

; TODO 1/1: Define a function, bingo-word, that takes a string as an argument
;           and returns its bingo word. You may assume that the argument is a
;           valid word as described above.
;
;           Don't forget to include a signature and reasonable purpose statement!!
;
;           Hint: if you don't remember ALL the string functions in BSL, that's
;           ok!! :) Remember that you can right-click on a function and search
;           the Help Desk - as a start, string-append will be quite handy...
;           and now you just need some help with isolating substrings,
;           converting strings to upper case, getting the length of a string, and
;           converting a number to a string. Good luck!!

;PURPOSE STATEMENT: Uppercase first letter and print out amount of letters in string
;SIGNATURE: word -> String + String-length


(define (bingo-word word) (string-append(string-upcase (substring word 0 1))
                         (number->string (string-length word))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's make a pretty animated scene with a house, in parts!

; TODO 1/4: Use the triangle, square, rectangle, above, and overlay/align
;           functions to define a constant HOUSE that is the image of a
;           house with a roof and door (and circle if you’re feeling bold
;           enough for a door handle). Be creative :)

;(define house-base (overlay (rectangle 50 30 "solid" "blue")
 ;          (triangle 60 "solid" "purple")))

(define house-roof (place-image/align (triangle 100 "solid" "yellowgreen")
                     190 100 "right" "bottom"
                     (empty-scene 200 300)))
(define SQUARE (rectangle 100 200 "solid" "pink"))

(define a (place-image SQUARE 140 200 house-roof))

(define door (rectangle 50 100 "solid" "black"))

(define all (place-image door 139 275 a))

(define knob (circle 5 "solid" "goldenrod" ))

(define newCrib (place-image knob 160 275 all))

(define window (rectangle 30 40 "solid" "gray"))

(define window1 (place-image window 120 179 newCrib))

(define window2 (rectangle 30 40 "solid" "gray"))


(define HOUSE-WITH-WINDOWS (place-image window2 168 150 window1))



; TODO 2/4: Define a constant WINDOW, as the image of a window, and place
;           two of them on your HOME, defining HOUSE-WITH-WINDOWS.
;           Note how in using a constant we only have to draw it once and
;           get to use it twice!



; The next step is a bit tricky, and will require you to understand a bit
; about how colors are represented in DrRacket (and other languages!).

; Colors in DrRacket can either defined via a name (like "blue" and "red"),
; or by numbers, representing the amount of red, green, and blue (each a
; number from 0-255) using the color function...

; (color red-val green-val blue-val)

; For example, a bright red square could be created as either of the following...

; (square 100 "solid" (color 255 0 0))
; (square 100 "solid" "red")

; Now consider the following function, which uses a mathematical formula to
; produce a range of blues...

; SIGNATURE HERE: Int -> Color (Replica)
(define (sky-color t)
  (color 0 0 (abs (- (remainder t 510) 255))))



; This function always uses 0's for red and green, but differs in the amount
; of blue. If it helps, here is an infix representation of the equation...

; |(t remainder 510) - 255|

; and here is a visual depiction of how the amount of blue changes as a function
; of the value of t: https://www.desmos.com/calculator/ntq43wwjpg

; As you can see, the amount of blue moves linearly up and down between 0 and 255.

; TODO 3/4: Replace "SIGNATURE HERE" above with a signature for this function.
;           Note: ordinarily we'd also have a purpose statement... but that's
;           kinda what this whole set of comments was about ;)


; TODO 4/4: Now finally we are ready to put it all together :)
;           The goal is that when you uncomment the final line below, you will see
;           a movie of your house, with the sky getting darker and then lighter
;           behind it. SO, define a function scene that uses your HOUSE-WITH-WINDOWS
;           constant, as well as the sky-color function, to visualize a single
;           frame of the movie, where the frame number determines the sky color,
;           and so...
;
 ;(scene 0)
;
;           should have your house in front of a bright blue background, whereas...
;
;(scene 255)
;           
;           should have your house in front of a black background (night!).

; scene : Nat -> Image
; visualizes a house where the supplied number determines
; the amount of blue in the sky behind the house
(define (scene t)(overlay HOUSE-WITH-WINDOWS(rectangle 300 300 "solid" (sky-color t))))


(animate scene)


; The empty scene that is surrounding the house. I was trying to convert the empty scene to "scene"
; I spoke with a TA and they said as long as I have the house with background I am good. Since I used
; an empty scene, they informed me that the next lines wouldn't work so I just creates the scene
; bigger than the empty scene. I hope you understand if you need anymore clarification please
; reach out to me