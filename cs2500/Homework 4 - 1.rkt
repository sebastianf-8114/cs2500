;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw4-problem1 (FINISH2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's think about what goes into designing notifications for a mobile device.
;
; Consider the following data definitions...

; TODO 1/2: Complete the design recipe for InfoMessage,
;           Badge, and Confirmation. You should come up
;           with reasonable examples, but are welcome
;           to be creative :)

(define APP-1 "Instagram")
(define APP-2 "Twitter")
(define APP-3 "Youtube")

; Template
; Since there is no cond, it cannot be a full data template

(define-struct info [app message])


; An InfoMessage is a (make-info String String)
; Interpretation: a message from an app
(define INFOMESSAGE-1 (make-info APP-1 "Someone followed you"))
(define INFOMESSAGE-2 (make-info APP-2 "Someone just posted"))
(define INFOMESSAGE-3 (make-info APP-3 "KSI Uploaded New Video"))

(define (InfoMessage-temp a)
  (... (info-app a) ...
       (info-message a) ...))

; A Badge is a (make-badge String Nat)
; Interpretation: a numeric indicator for an app

(define-struct badge [app num])


(define BADGE-1 (make-badge APP-1 1))
(define BADGE-2 (make-badge APP-2 2))
(define BADGE-3 (make-badge APP-3 3))

(define (badge-temp b)
  (... (badge-app b) ...
       (badge-num b) ...))

; A Confirmation is a (make-confirm String String String)
; Interpretation: a yes/no question from an app, with
; associated text to display for each option

(define-struct confirm [app yestxt notxt])


(define CONFIRM-1 (make-confirm APP-1 "APPROVE" "DENY"))
(define CONFIRM-2 (make-confirm APP-2 "DO YOU WANT TO VIEW IT?" "CANCEL"))
(define CONFIRM-3 (make-confirm APP-3  "DO YOU WANT TO SEE THE VIDEO?" "IGNORE"))

(define (confirm-temp c)
  (... (confirm-app c) ...
       (confirm-yestxt c) ...
       (confirm-notxt c)))
 
; TODO 2/2: Design the data type Notification, which represents
;           a single notification that could be of any of the
;           types described above.

; A noficiation (Noti) is one of the following:
; - (make-info String String)
; - (make-badge String Number)
; - (make-confirm String String String)

; Interpretation: A notification represents the type of notification,
; info represents the message from an app, badge represents the interface,
; confirm represents the options to interact.

#|
; make-Notification :

(define Noti-1 INFOMESSAGE-1)
(define Noti-2 INFOMESSAGE-2)
(define Noti-3 INFOMESSAGE-3)
|#

; Signature : Info, Badge, or Confirm -> Notification
; Purpose : To correctly produce the notification for the appropriate app
(define (noti-temp s)
  (...
   (cond
     [(info? s) InfoMessage-temp]
     [(badge? s) badge-temp]
     [(confirm? s) confirm-temp])))



         


  


