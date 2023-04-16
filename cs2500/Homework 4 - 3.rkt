;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw4-problem3 (FINISH2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's think about what goes into making an app that works with purchase
; receipts.
;
; Consider the following data definition:


(define-struct item [desc qty unit sale? next])

; A ReceiptItem is one of:
; - "nothing"
; - (make-item String Nat PosReal Boolean ReceiptItem)
; Intepretation: either end of the receipt or
; an item's description, quantity purchased,
; unit price (in $), whether it was on sale,
; and the next item on the receipt

; TODO 1/4: Complete the data design recipe for ReceiptItem.
;           You *must* have examples that (at least) represent the following
;           three receipts...
;           - An empty receipt
;           - A grocery receipt...
;             (1 box of cereal, $4.28),
;             (2 apples on sale, $1.67 each)
;           - A computer invoice...
;             (2 RaspberryPi on sale, $32 each),
;             (1 monitor, $135),
;             (2 wireless touch keyboards, $27 each)

; Examples

(define EMPTY-RECIEPT  "nothing")
(define GROCERY-RECIEPT-CEREAL (make-item "cereal" 1 4.28 #false EMPTY-RECIEPT))
(define GROCERY-RECIEPT-APPLE (make-item "apple" 2 1.67 #true GROCERY-RECIEPT-CEREAL))

(define EMPTY-INVOICE "nothing")
(define RASPBERRY-INVOICE (make-item "RaspberryPi" 2 32 #true EMPTY-INVOICE))
(define MONITOR-INVOICE (make-item "Monitor" 1 135 #false RASPBERRY-INVOICE))
(define KEYBOARD-INVOICE (make-item "Keyboard" 2 27 #false MONITOR-INVOICE))

; Temp

(define (ri-temp ri)
  (... (cond
         [(string? ri) ...]
         [(item? ri)
          (item-desc ri) ...
          (item-qty ri) ...
          (item-unit ri) ...
          (item-sale? ri) ...
          (ri-temp (item-next ri))])))


; TODO 2/4: Design the function total-cost, which calculates the total cost
;           of a receipt. For instance, the empty receipt is 0; the grocery
;           is (1 x 4.28) + (2 x 1.67) = 7.62; and the computer receipt is
;           (2 x 32) + (1 x 135) + (2 x 27) = 253.

; total-cost: RecieptItem -> PosInt
; Calculates the total cost of a receipt

(define (total-cost ri)
  (cond
    [(string? ri) 0]
    [(item? ri)
     (+ (* (item-qty ri) (item-unit ri))
        (total-cost (item-next ri)))]))

(check-expect (total-cost RASPBERRY-INVOICE) 64)
(check-expect (total-cost EMPTY-INVOICE) 0)
(check-expect (total-cost KEYBOARD-INVOICE) 253)
(check-expect (total-cost GROCERY-RECIEPT-APPLE) 7.62)
              

; TODO 3/4: Design the function any-sale?, which determines if any item in the
;           receipt is on sale. For example, the empty receipt does not have
;           any sale items, but both other examples do.

; any-sale? : RecieptItem -> Boolean
; Determines if any item in the receipt is on sale

(define (any-sale? ri)
  (cond
    [(string? ri) #f]
    [(item? ri)
     (or (item-sale? ri)
         (any-sale? (item-next ri)))]))

(check-expect (any-sale? EMPTY-INVOICE) #f)
(check-expect (any-sale? RASPBERRY-INVOICE) #t)
(check-expect (any-sale? MONITOR-INVOICE) #t)


; TODO 4/4: Design the function expensive, which produces a new receipt that only
;           contains items that are greater than $100 (unit cost). For example,
;           both the empty and grocery receipts would produce empty receipts,
;           whereas the computer receipt would produce a new list only containing
;           the monitor.

; expensive : RecieptItem -> RecieptItem
; Produces a new receipt that only contains items that are greater than $100 (unit cost)

(define (expensive ri)
  (cond
    [(string? ri) "nothing"]
    [(item? ri)
     (if (>= (item-unit ri) 100)
         (make-item
          (item-desc ri) 
          (item-qty ri) 
          (item-unit ri) 
          (item-sale? ri)
          (expensive (item-next ri)))
         (expensive (item-next ri)))]))

(check-expect (expensive RASPBERRY-INVOICE) EMPTY-INVOICE)
(check-expect (expensive EMPTY-INVOICE) EMPTY-INVOICE)
(check-expect (expensive MONITOR-INVOICE) (make-item "Monitor" 1 135 #false "nothing"))
(check-expect (expensive KEYBOARD-INVOICE) (make-item "Monitor" 1 135 #false "nothing"))
(check-expect (expensive GROCERY-RECIEPT-APPLE) EMPTY-RECIEPT)

