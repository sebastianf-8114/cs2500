;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw4-problem2 (FINISH3)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's think about what goes into making a particularly useful app, Weather!
;
; Consider the following data definition:


(define-struct cloudy [morn? eve?])
(define-struct rain [chance])
(define-struct snow [inches])

; A Prediction is one of:
; - "sunny"
; - (make-cloudy Boolean Boolean)
; - (make-rain Nat[1, 100])
; - (make-snow Nat)
; Intepretation: weather prediction, either...
; - Sunny!
; - Cloudy (either in the morning, evening, both, or unsure)
; - Raining (with provided % chance as 1-100)
; - Snow (with provided accumulation)


; TODO 1/2: Complete the data design recipe for Prediction.

(define PREDICTION-1 "sunny")
(define PD-MORNCLOUDY (make-cloudy #true #false))
(define PD-EVECLOUDY (make-cloudy #false #true))
(define PD-BOTHCLOUDY (make-cloudy #true #true))
(define PD-NOTCLOUDY (make-cloudy #false #false))
(define PREDICTION-3 (make-rain 70))
(define PREDICTION-4 (make-snow 4))

(define (cloudy-temp PD)
  (... (cloudy-morn? PD) ...
       (cloudly-eve? PD) ...))

(define (rain-temp PD)
  (... (rain-chance) ...))

(define (snow-temp PD)
  (... (snow-inches PD) ...))

; PD-> String
; Helper Function

(define (CloudyFunction PD)
  (cond
    [(and
      (cloudy-morn? PD) (cloudy-eve? PD)) "It's going to be cloudy the whole day."]
    [(cloudy-morn? PD) "It's going to be cloudy in the morning."]
    [(cloudy-eve? PD) "It's going to be cloudy in the evening."]
    [(not (and
           (cloudy-morn? PD) (cloudy-eve? PD))) "It's not going to be cloudy."]))



; TODO 2/2: Design the function announcement, which given
;           a prediction (e.g., "sunny"), produces a short
;           text announcement to display (e.g., "It's going
;           to be sunny!").

; announcement : Prediction -> String
; Accepts a Prediction and returns a new sentence made.

(define (announcement PD)
  (cond
    [(string? PD) "It is sunny outside"]
    [(cloudy? PD) (CloudyFunction PD)]
    [(rain? PD) (string-append
                 "There's a " (number->string (rain-chance PD))
                 " chance of rain")]
    [(snow? PD) (string-append
                 "It's going to snow, with " (number->string (snow-inches PD))
                 " inches on the ground.")]))
    
;           Some other example announcements include:
;           - "It's going to be cloudy in the morning."
;           - "There's a 60% chance of rain."
;           - "It's going to snow, with 2 inches on the ground."

(check-expect (announcement PREDICTION-1) "It is sunny outside")
(check-expect (announcement PD-NOTCLOUDY) "It's not going to be cloudy.")
(check-expect (announcement PREDICTION-3) "There's a 70 chance of rain")
(check-expect (announcement PREDICTION-4) "It's going to snow, with 4 inches on the ground.")


