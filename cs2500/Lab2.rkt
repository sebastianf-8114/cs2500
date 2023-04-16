;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab2-starter (fixed)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





; Design the data type "Fruit"

; a fruit is one of 

; "orange"
; "apple"
; "pineapple"
; Interpretation: Types of fruits at the fundies 1 groceries

; examples

(define FRUIT-ORANGE "orange")
(define FRUIT-APPLE "apple")
(define FRUIT-PINEAPPLE "pineapple")

;Templete

(define (fruit-temp f)
  (... (cond
         [(string=? FRUIT-ORANGE f)...]
         [(string=? FRUIT-APPLE f)...]
         [(string=? FRUIT-PINEAPPLE f)...])))






; Design a function "fruit-price" that provides one of item of the given fruit.

; Orange 1.20
; Apple 5
; Pineapple 25.30

; fruit-price : Fruit -> Real
; To provide the price of a given fruit

(check-expect (fruit-price FRUIT-ORANGE) 1.20)
(check-expect (fruit-price FRUIT-APPLE) 5)
(check-expect (fruit-price FRUIT-PINEAPPLE) 20.30)


(define (fruit-price f)
  (cond
         [(string=? FRUIT-ORANGE f)1.20]
         [(string=? FRUIT-APPLE f) 5]
         [(string=? FRUIT-PINEAPPLE f) 20.30]))



; TODO 1/1: Design the function string-starts-with?, which takes two
;           Strings and determines if the first begins with the second.
;           This could happen in three situations...
;           - The second string is empty ("")
;           - The strings are actually the same
;           - The first string is longer than the second,
;             and the beginning of the first string is the same as
;             the second

;           Provide a signature, uncomment the supplied tests, and
;           write the code for the function that matches the description.


; string-starts-with? : ???
; Does the first string start with the second?

(define (string-starts-with? 1string 2string)
  (if (>=(string-length 1string) (string-length 2string))
      (string=? (substring 1string 0 (string-length 2string)) 2string)
  #false))
  


(check-expect (string-starts-with? "fundies" "fun") #true)
(check-expect (string-starts-with? "fun" "fundies") #false)
(check-expect (string-starts-with? "fundies" "fundies") #true)
(check-expect (string-starts-with? "any" "") #true)
(check-expect (string-starts-with? "cat" "dog") #false)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; You are to design a small door-simulator program...
;
; - A door can either be open, closed, or locked. Your program
;   will take in a representation of one of these states as its
;   starting position.
;
; - The user can open a closed door by pressing the "o"
;   key on their keyboard. You cannot open a locked door,
;   and attempting to open an already open door will do nothing.
;
; - The user can close an open door by pressing the "c" key
;   on their keyboard. Attempting to close an already closed
;   (or closed and locked) door will do nothing.
;
; - The user can lock a closed door by pressing the "l" key
;   on their keyboard. Attempting to lock an open door or an
;   already locked door will do nothing.
;
; - The user can unlock a locked door by pressing the "u" key
;   on their keyboard. Attempting to unlock a closed door that
;   is already unlocked, or an open door, will do nothing.

; When finished, you'll uncomment the following code and run
; something like ... (door-simulator "open")


; door-simulator : DoorState -> DoorState
; Simulates a lockable door


(define (door-simulator ds)
  (big-bang ds
    [to-draw draw-door]
    [on-key change-door]))


; TODO 1/3: Finish the Design Recipe for data for DoorState
;           (so provide examples and a template).


; A DoorState is one of:
; - "closed"
; - "locked"
; - "open"
; Interpretation: state of a lockable door

(define DS-CLOSED "Closed")
(define DS-LOCKED "Locked")
(define DS-OPEN "Open")

(define (DoorState-temp x)
  (...
   (cond
         [(string=? DOORSTATE-CLOSED x)...]
         [(string=? DOORSTATE-LOCKED x)...]
         [(string=? DOORSTATE-OPEN x)...])))





; TODO 2/3: Design the function draw-door takes a DoorState and
;           produces a corresponding image. To help get you
;           started, you have been provided...
;           - Example constants (DOOR-CLOSED, DOOR-LOCKED, DOOR-OPEN);
;             feel free to be creative if you'd like to make a more
;             interesting door :)
;           - A signature, purpose statement, and tests (commented out)

;(define (draw-door x)
;   (cond
 ;        [(string=? DOORSTATE-CLOSED x)DOOR-CLOSED]
  ;       [(string=? DOORSTATE-LOCKED x)DOOR-LOCKED]
   ;      [(string=? DOORSTATE-OPEN x)DOOR-OPEN]))


  
(define BG (rectangle 400 200 "solid" "blue"))

(define DOOR-W (/ (image-width BG) 5))
(define DOOR-H (- (image-height BG) 40))

(define KNOB-X (* .8 DOOR-W))
(define KNOB-Y (/ DOOR-H 2))

(define DOOR
  (place-image
   (circle (/ DOOR-W 10) "solid" "gray")
   KNOB-X KNOB-Y
   (rectangle DOOR-W DOOR-H "solid" "brown")))

(define DOOR-LOCK
  (place-image
   (text "x" 10 "black")
   KNOB-X KNOB-Y
   DOOR))

(define DOOR-X (* 0.6 (image-width BG)))
(define DOOR-Y (+ (/ DOOR-H 2) (- (image-height BG) DOOR-H)))

(define DOOR-CLOSED
  (place-image
   DOOR
   DOOR-X DOOR-Y
   BG))

(define DOOR-LOCKED
  (place-image
   DOOR-LOCK
   DOOR-X DOOR-Y
   BG))

(define DOOR-OPEN
  (place-image
   (beside (flip-horizontal DOOR)
           (rectangle DOOR-W DOOR-H "solid" "lightblue"))
   (- DOOR-X (/ DOOR-W 2)) DOOR-Y
   BG))


; draw-door : DoorState -> Image
; Draw the current state of the door


(check-expect (draw-door DS-CLOSED) DOOR-CLOSED)
(check-expect (draw-door DS-LOCKED) DOOR-LOCKED)
(check-expect (draw-door DS-OPEN) DOOR-OPEN)


; CODE HERE


(define (draw-door x)
   (cond
         [(string=? DS-CLOSED x)DOOR-CLOSED]
         [(string=? DS-LOCKED x)DOOR-LOCKED]
         [(string=? DS-OPEN x)DOOR-OPEN]))





; TODO 3/3: Uncomment the following function - you are not to change it,
;           but rather design the helper functions it references.
;           The first (change-closed-door) produces a DoorState based upon
;           the key pressed when the door is closed - this has been provided
;           for you. Your job is to implement the remaining two, which
;           determine the next state when the door is locked (change-locked-door)
;           and then open (change-open-door).


; change-door : DoorState KeyEvent -> DoorState
; Open/close/lock/unlock the door as necessary

(check-expect (change-door DS-CLOSED "o") DS-OPEN)
(check-expect (change-door DS-LOCKED "o") DS-LOCKED)
(check-expect (change-door DS-OPEN "o") DS-OPEN)

(check-expect (change-door DS-CLOSED "l") DS-LOCKED)
(check-expect (change-door DS-LOCKED "l") DS-LOCKED)
(check-expect (change-door DS-OPEN "l") DS-OPEN)

(check-expect (change-door DS-CLOSED "c") DS-CLOSED)
(check-expect (change-door DS-LOCKED "c") DS-LOCKED)
(check-expect (change-door DS-OPEN "c") DS-CLOSED)

(check-expect (change-door DS-CLOSED "u") DS-CLOSED)
(check-expect (change-door DS-LOCKED "u") DS-CLOSED)
(check-expect (change-door DS-OPEN "u") DS-OPEN)

(check-expect (change-door DS-CLOSED "a") DS-CLOSED)
(check-expect (change-door DS-LOCKED "a") DS-LOCKED)
(check-expect (change-door DS-OPEN "a") DS-OPEN)

(define (change-door ds ke)
  (cond [(string=? ds DS-CLOSED) (change-closed-door ke)]
        [(string=? ds DS-LOCKED) (change-locked-door ke)]
        [(string=? ds DS-OPEN) (change-open-door ke)]))


; change-closed-door : KeyEvent -> DoorState
; applies a key event to a closed door

(check-expect (change-closed-door "o") DS-OPEN)
(check-expect (change-closed-door "l") DS-LOCKED)
(check-expect (change-closed-door "c") DS-CLOSED)
(check-expect (change-closed-door "u") DS-CLOSED)
(check-expect (change-closed-door "a") DS-CLOSED)

(define (change-closed-door ke)
  (cond
    [(key=? ke "o") DS-OPEN]
    [(key=? ke "l") DS-LOCKED]
    [else DS-CLOSED]))

; change-locked-door : KeyEvent -> DoorState
; applies a key event to a locked door

(check-expect (change-locked-door "u") DS-CLOSED)
(check-expect (change-locked-door "l") DS-LOCKED)
(check-expect (change-locked-door "c") DS-LOCKED)

(check-expect (change-locked-door "a") DS-LOCKED)

(define (change-locked-door ke)
  (cond
    [(key=? ke "u") DS-CLOSED]
    [else DS-LOCKED]))


; change-open-door : KeyEvent -> DoorState
; applies a key event to a opened door

(check-expect (change-open-door "o") DS-OPEN)
(check-expect (change-open-door "l") DS-OPEN)
(check-expect (change-open-door "c") DS-CLOSED)
(check-expect (change-open-door "u") DS-OPEN)
(check-expect (change-open-door "a") DS-OPEN)

(define (change-open-door ke)
  (cond
    [(key=? ke "o") DS-OPEN]
    [(key=? ke "c") DS-CLOSED]
    [else DS-OPEN]))


