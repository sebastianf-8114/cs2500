;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab4-starter (finished)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Design the data necessary to represent a book, which can
;           either be physical or electronic. All books have a title
;           and author. Physical books are either paperback or hardcover,
;           and have some number of pages. Electronic (e-books) have a
;           format (pdf, epub, txt) and a source URL.
;
;           We've gotten you started with the design of a PhysicalBook
;           and EFormat :) So your task is an EBook and a Book!


(define-struct physbook [title author paperback? pages])

; A PhysicalBook is a (make-physbook String String Boolean PosInteger)
; Interpretation: a physical book
; - title is the title of the book
; - author is the author of the book
; - paperback? is #true if paperback, #false if hardcover
; - pages is the number of pages in the book

(define
  PHYSBOOK-DUNE
  (make-physbook "Dune" "Frank Herbert" #true 896))

(define
  PHYSBOOK-JUSTICE
  (make-physbook
   "Doing Justice: A Prosecutor's Thoughts on Crime, Punishment, and the Rule of Law"
   "Preet Bharara"
   #false
   368))


(define (physbook-temp pb)
  (... (physbook-title pb) ...
       (physbook-author pb) ...
       (physbook-paperback? pb) ...
       (physbook-pages pb) ...))



(define-struct ebook [title author eformat url])

; A EBook is a (make-ebook title author EFormat String)
; Interpretation: an electronic book
; - title is the title of the book
; - author is the author of the book
; - eformat is the format of the book
; - url is the sourceURL (string) of the book


(define (ebook-temp eb)
  (... (ebook-title eb) ...
       (ebook-author eb) ...
       (ebook-eformat eb) ...
       (ebook-url eb) ...))

; An EFormat is one of:
; - "pdf"
; - "epub"
; - "txt"
; Interpretation: e-book formats

(define EFORMAT-PDF "pdf")
(define EFORMAT-EPUB "epub")
(define EFORMAT-TXT "txt")

(define (eformat-temp ef)
  (...
   (cond
     [(string=? ef EFORMAT-PDF) ...]
     [(string=? ef EFORMAT-EPUB) ...]
     [(string=? ef EFORMAT-TXT) ...])))

(define
  EBOOK-DOGS
  (make-ebook "Dogs" "Bdd Bunny" EFORMAT-PDF "www.BadBunny.com"))

(define
  EBOOK-CATS
  (make-ebook "CATS" "Anuel AA" EFORMAT-EPUB "www.AnuelAA.com"))


; A book is one of
; - Ebook
; - PhysicalBook

; Interpretation: A type of book

(define BOOK-PHY PHYSBOOK-DUNE)
(define BOOK-ELEC EBOOK-DOGS)

(define (book-temp b)
  (...
   (cond
     [(physbook? b) ... (physbook-temp b) ...]
     [(ebook? b) ... (ebook-temp b)] ...)))



; TODO 2/2: Now design the function where-to-find that accepts a book
;           and returns where you can find it: physical books are either
;           in the "hardcover section" or "paperback section", whereas
;           electronic books are found at their URL.


; where-to-find : book -> String
; Accepts a book and returns where you can find it

(check-expect (where-to-find PHYSBOOK-JUSTICE) "hardcover section")
(check-expect (where-to-find EBOOK-DOGS) "www.BadBunny.com")
(check-expect (where-to-find PHYSBOOK-DUNE) "paperback section")



(define (where-to-find b)
  (cond
    [(physbook? b)
       (if (physbook-paperback? b)
           "paperback section"
           "hardcover section")]
    [(ebook? b) (ebook-url b)]
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definitions...

; A Genre is one of:
; - "comedy"
; - "drama"
; - "action"
; - "education"
; Interpretation: genre for a video

(define GENRE-COMEDY "comedy")
(define GENRE-DRAMA "drama")
(define GENRE-ACTION "action")
(define GENRE-EDUCATION "education")

(define (genre-temp g)
  (...
   (cond
     [(string=? g GENRE-COMEDY)...]
     [(string=? g GENRE-DRAMA)...]
     [(string=? g GENRE-ACTION) ...]
     [(string=? g GENRE-EDUCATION) ...])))


(define-struct video [name duration hd? genre next])


; A StreamingQueue is one of:
; - #false
; - (make-video String PosInteger Boolean Genre StreamingQueue)
; Interpretation: either an empty queue (#false) or a video
; with a name, duration in minutes, whether it's available in HD,
; and its genre.

(define QUEUE-EMPTY #false)

(define QUEUE-CRASH
  (make-video "Crash Course Organic Chemistry #5"
              14 #true GENRE-EDUCATION
              QUEUE-EMPTY))

(define QUEUE-OLIVER
  (make-video
   "Prisons & Jails: Last Week Tonight with John Oliver"
   18 #true GENRE-COMEDY
   QUEUE-CRASH))

(define QUEUE-DUEL
  (make-video
   "Duel" 2 #false GENRE-ACTION QUEUE-OLIVER))

(define QUEUE-STORM
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))

(define (sq-temp sq)
  (...
   (cond
     [(boolean? sq) ...]
     [(video? sq)
      (...
       (video-name sq) ...
       (video-duration sq) ...
       (video-hd? sq) ...
       (genre-temp (video-genre sq)) ...
       (sq-temp (video-next sq)) ...)])))

; Design a function only-15min that takrs a StreamingQueue and
; returns only the video that are 15min or less

; only-15min : StreamingQueue -> StreamingQueue
; Returns a StreamingQueue with only the videos that are not greater than 15 min in duration

(check-expect (only-15min QUEUE-EMPTY) QUEUE-EMPTY)
(check-expect (only-15min QUEUE-CRASH) QUEUE-CRASH)
(check-expect (only-15min QUEUE-OLIVER) QUEUE-CRASH)
(check-expect (only-15min QUEUE-DUEL)
              (make-video "Duel" 2 #false GENRE-ACTION QUEUE-CRASH))


(define (only-15min sq)
  (cond
    [(boolean? sq) sq]
    [(video? sq)
     (if (<= (video-duration sq) 15)
         (make-video
          (video-name sq) 
          (video-duration sq)
          (video-hd? sq) 
          (video-genre sq)
          (only-15min (video-next sq )))
         (only-15min (video-next sq ) ))]))

; TODO 1/1: Design the following functions. For clarity, we've provided
;           (commented out) tests for each. Don't forget to follow the
;           templates!!!
;
;           Note: for full credit, you only need to submit your attempt
;           for ONE function; however, we recommend trying them all! :)


; Design the function good-for-friday? that determines if a streaming queue
; contains any content that is comedy or action.


; comedy/action? :  Genre -> Boolean
; Helper funtion for good-for-friday? which returns true if and only if genre is comedy or action

(define (comedy/action? g)
  (cond
    [(string=? g GENRE-COMEDY)#t]
    [(string=? g GENRE-DRAMA)#f]
    [(string=? g GENRE-ACTION) #t]
    [(string=? g GENRE-EDUCATION) #f]))


; good-for-friday? : StreamingQueue -> Boolean
; Determines if a streaming queue contains any content that is comedy or action.
(define (good-for-friday? sq)
  (cond
    [(boolean? sq) #f]
    [(video? sq) (or (comedy/action? (video-genre sq))
                     (good-for-friday? (video-next sq)))]))
    

(check-expect (good-for-friday? QUEUE-EMPTY) #false)
(check-expect (good-for-friday? QUEUE-STORM) #true)




; Design the function duration that calculates the total number of minutes
; of content in a streaming queue. For example, an empty queue has 0 minutes
; of content, whereas QUEUE-STORM has 45 minutes (14 + 18 + 2 + 11).

; duration : StreamingQueue -> PositiveInteger
; Calculates the total number of minutes of content in a streaming queue.

(define (duration sq)
  (cond
    [(boolean? sq) 0]
    [(video? sq)
     (+ (video-duration sq) (duration (video-next sq)))]))

(check-expect (duration QUEUE-EMPTY) 0)
(check-expect (duration QUEUE-STORM) 45)




; Design the function upgrade that takes a streaming queue and produces a
; new queue containing HD versions of all the videos in the original queue.

; upgrade : StreamingQueue -> StreamingQueue
; Produces a new queue containing HD versions of all the videos in the original queue

(define (upgrade sq)
  (cond
    [(boolean? sq) #f]
    [(video? sq)
     (make-video (video-name sq) (video-duration sq) #t
                 (video-genre sq) (upgrade (video-next sq)))]))
       

(check-expect (upgrade QUEUE-EMPTY) QUEUE-EMPTY)

(check-expect
 (upgrade QUEUE-STORM)
 (make-video
  "Tim Minchin's Storm the Animated Movie"
  11 #true GENRE-DRAMA
  (make-video
   "Duel" 2 #true GENRE-ACTION
   QUEUE-OLIVER)))


