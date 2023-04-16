;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw5-problem1(oct 18) (1+1+123)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You are going to make yourself a useful interactive app: flash cards
; (https://en.wikipedia.org/wiki/Flashcard).

; To begin, consider the following data definition...


(define-struct flashcard [front back])

; A FlashCard is a (make-flashcard String String)
; Interpretation: the front and back of a card


; TODO 1/4: Complete the design recipe for FlashCard.
 
(define card1 (make-flashcard "Dog" "An animal"))
(define card2 (make-flashcard "Pizza" "A type of food"))
(define card3 (make-flashcard "Car" "Type of transportation"))
(define card4 (make-flashcard "Gatorade" "Type of drink"))


(define (FlashCard-temp card)
  (...
   (cond
     [(string=? card card1) ...]
     [(String=? card card2) ...]
     [(String=? card card3) ...])))


; Now a single flash card wouldn't be super useful, and so...

; TODO 2/4: Design ListOfFlashCard (LoFC) to support an arbitrarily sized
;           sequence of flash cards. Importantly...
;           - These should be proper lists (i.e., using cons and '()).
;           - Make sure to give yourself a few example lists, of different sizes;
;             hopefully they are useful in your classes!
;           - Remember that your LoFC template should reflect that your list
;             elements are themselves designed types (FlashCard). 

; A ListOfFlashCard (LoFC) is one of:
; - '()
; - (cons FlashCard LoFC)
; Interpretation: a list of flashcards
 
(define LoFC-0 '())
(define LoFC-1 (cons card1 LoFC-0))
(define LoFC-2 (cons card2 LoFC-1))
(define LoFC-3 (cons card3 LoFC-2))
(define LoFC-4 (cons card4 LoFC-3))

(define (lofc-temp lofc)
  (...
   (cond
     [(empty? lofc) ...]
     [(cons? lofc)
      (first lofc) ...
      (rest los) ...])))


; Now, for practice...

; TODO 3/4: Design the function has-text?, which determines if a list of flash
;           cards contains any card that contains a supplied text.
;
;           Hint: the string-contains? function is very useful for determining
;           if one string contains another :)

; has-text? : ListOfFlashCard String -> Boolean
; Determines if a list of flash cards contains any card that contains a supplied text.
(define (has-text? lofc st)
  (cond
    [(empty? lofc) #f]
    [(cons? lofc)
     (if (or (string-contains? st (flashcard-front (first lofc)))
             (string-contains? st (flashcard-back (first lofc)))) #t
                                                                  (has-text? (rest lofc) st))]))

(check-expect (has-text? LoFC-0 "Pizza") #f)
(check-expect (has-text? LoFC-1 "Pizza") #f)
(check-expect (has-text? LoFC-2 "A type of food") #t)
(check-expect (has-text? LoFC-4 "Type of transportation") #t)


; Finally, let's put this list to use :)

; TODO 4/4: Design the program go-cards, which helps you study with a supplied list
;           of cards. It starts on the first card and then flips it when a key is
;           pressed, and then goes to the front of the next card when another key is
;           pressed. The program should end when the last card has been flipped, and
;           the go-cards function should return how many cards were in the original
;           list. Some hints...
;           - To get you started, you have been supplied the data definition of a
;             way to represent the state of the program (don't forget to uncomment
;             the structure definition and finish the design recipe for data!).
;           - The return value of this function is a bit challenging, since the list
;             you get at the end is empty! So uncomment the code we've given you below,
;             but to understand: you can *add* the length of the originally supplied
;             list to that of the (empty) final list and still get the right answer :)
;           - Be sure to follow the templates for all your data, which will typically
;             entail helpers for the FS, the LoFC, and the FC.
;           - As long as the program operates as described, you are welcome to make it
;             look as simple or as creative as you would like - we hope it helps you
;             in your classes!! :)


(define (fs-temp fs)
  (... (fs-cards fs) ...
       ... (fs-front? fs) ...))
                 
(define-struct fs [cards front?])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (nextCard LoFC-0) LoFC-0)

(check-expect (nextCard LoFC-1) LoFC-0)

(check-expect (nextCard LoFC-2) LoFC-1)

(check-expect (nextCard LoFC-3) LoFC-2)


; nextCard : ListOfFlashCard -> ListOfFlashCard
; Checks to see if there is another card in the list and goes on to it

(define (nextCard lofc)
  (cond
    [(empty? lofc) lofc]
    [(cons? lofc) 
     (rest lofc)]))



; A FlashState (FS) is a (make-fs LoFC Boolean)
; Interpretation: a list of cards, and whether
; the front is face up

(define FS-1 (make-fs LoFC-0 #f))
(define FS-2 (make-fs LoFC-1 #t))
(define FS-3 (make-fs LoFC-2 #t))
(define FS-4 (make-fs LoFC-3 #f))

(define BoFC (rectangle 800 400 "outline" "black"))


; draw-fsList : ListOfFlashCard Boolean -> Image of ListOfFlashCard
; draw the ListOfFlashCard

 
(check-expect (draw-fsList LoFC-0 #t) BoFC)

(check-expect (draw-fsList LoFC-1 #t) (overlay (text (flashcard-front (first LoFC-1))
                                                     50 "black") BoFC))

(check-expect (draw-fsList LoFC-2 #t) (overlay (text (flashcard-front (first LoFC-2))
                                                     50 "black") BoFC))

(check-expect (draw-fsList LoFC-3 #t) (overlay (text (flashcard-front (first LoFC-3))
                                                     50 "black") BoFC))

(check-expect (draw-fsList LoFC-4 #t) (overlay (text (flashcard-front (first LoFC-4))
                                                     50 "black") BoFC))




(define (draw-fsList lofc front?)
  (cond
    [(empty? lofc) BoFC]
    [(cons? lofc)
     (overlay (text (if front? (flashcard-front (first lofc))
                        (flashcard-back (first lofc))) 50 "black") BoFC)]))



; draw-fs : FlashState -> ???
; draw the ListOfFlashCard

(check-expect (draw-fs FS-1) BoFC)

(check-expect (draw-fs FS-2) (overlay (text (flashcard-front card1) 50 "black") BoFC))

(check-expect (draw-fs FS-3) (overlay (text (flashcard-front card2) 50 "black") BoFC))

(define (draw-fs fs)
  (draw-fsList
   (fs-cards fs)
   (fs-front? fs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (flip-fs FS-2 "x") (make-fs
                                  (list
                                   (make-flashcard "Dog" "An animal"))
                                  #false))

(check-expect (flip-fs FS-3 "x") (make-fs
                                  (list
                                   (make-flashcard
                                    "Pizza"
                                    "A type of food")
                                   (make-flashcard "Dog" "An animal"))
                                  #false))

(check-expect (flip-fs FS-4 "x") (make-fs
                                  (list
                                   (make-flashcard
                                    "Pizza"
                                    "A type of food")
                                   (make-flashcard "Dog" "An animal"))
                                  #true))


; flip-fs : FlashState KeyEvent -> FlashState
; flips it when a key is pressed
(define (flip-fs fs ke)
  (make-fs (if (fs-front? fs)
               (fs-cards fs)
               (nextCard (fs-cards fs)))
           (not (fs-front? fs))))



; done-fs? : FlashState -> Boolean
; stops when the last card has been flipped

(check-expect (done-fs? FS-2) #f)
(check-expect (done-fs? FS-3) #f)
(check-expect (done-fs? FS-4) #f)


(define (done-fs? fs)
  (empty? (fs-cards fs)))


; go-cards : LoFC -> Nat
; displays the cards in sequence (flip via key),
; returning the number of cards
(define (go-cards lofc)
  (+
   (length lofc)
   (length (fs-cards
            (big-bang (make-fs lofc #t)
              [to-draw draw-fs]
              [on-key flip-fs]
              [stop-when done-fs?])))))

(go-cards LoFC-4)