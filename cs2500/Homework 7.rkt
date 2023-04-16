;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw7 comp 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This problem asks you to design several functions that employ the
; following data definitions. The functions that you design *must* use
; list abstraction(s) when appropriate; you MAY NOT use recursion: doing
; so will lead you to get no code credit for the function :(
;
; NOTE #1: Part of the credit for each problem will be based on the choice
; of list abstractions, so make sure that they are a good match for the
; problem.
;
; NOTE #2: For certain problems, you will have to design helper functions
; that do not use list abstractions. You should follow the full design
; recipe (including appropriate use of templates) for all problems. Be sure
; to do this, even if it feels a bit tedious - listen to your templates!!
;
; Data Definitions (do not modify these)


; A Weekday is one of:
; - "Monday"
; - "Tuesday"
; - "Wednesday"
; - "Thursday"
; - "Friday"
; Interpretation: a day that excludes the weekend

(define WEEKDAY-M "Monday")
(define WEEKDAY-T "Tuesday")
(define WEEKDAY-W "Wednesday")
(define WEEKDAY-R "Thursday")
(define WEEKDAY-F "Friday")

(define (weekday-temp w)
  (...
   (cond
     [(string=? w WEEKDAY-M) ...]
     [(string=? w WEEKDAY-T) ...]
     [(string=? w WEEKDAY-W) ...]
     [(string=? w WEEKDAY-R) ...]
     [(string=? w WEEKDAY-F) ...])))


(define-struct meeting [day bname rnum hstart mstart duration])

; A ClassMeeting is a (make-meeting Weekday String String PosInt[8, 18] NonNegInt[0, 59] PosInt)
; Interpretation: when a class is scheduled to meet weekly
; - day: which day of the week
; - bname: name of the building
; - rnum: room number
; - hstart: starting hour (24hr)
; - mstart: starting minute
; - duration: length of the class (in minutes)

(define CM-FUNDIES-M (make-meeting WEEKDAY-M "WVH" "210A" 10 30 65))
(define CM-FUNDIES-W (make-meeting WEEKDAY-W "WVH" "210A" 10 30 65))
(define CM-FUNDIES-R (make-meeting WEEKDAY-R "WVH" "210A" 10 30 65))
;
(define CM-FUNDIES-LAB (make-meeting WEEKDAY-T "WVH" "212" 8 0 100))
;
(define CM-DISCRETE-T (make-meeting WEEKDAY-T "ISEC" "102" 13 35 100))
(define CM-DISCRETE-F (make-meeting WEEKDAY-F "ISEC" "102" 13 35 100))
;
(define CM-DISCRETE-SEM (make-meeting WEEKDAY-W "Hastings" "110" 16 35 65))
;
(define CM-CREATURES-T (make-meeting WEEKDAY-T "Forbidden Forest" "Hut" 13 0 200))
(define CM-POTIONS-R (make-meeting WEEKDAY-R "Hogwarts" "Dungeon" 13 0 200))

(define (classmeeting-temp cm)
  (... (weekday-temp (meeting-day cm)) ...
       (meeting-bname cm) ...
       (meeting-rnum cm) ...
       (meeting-hstart cm) ...
       (meeting-mstart cm) ...
       (meeting-duration cm) ...))


(define-struct course [prefix num name prof meetings])

; A Course is a (make-course String String String String [List-of ClassMeeting])
; Interpretation: a weekly class
; - prefix: the course prefix
; - num: the course number
; - name: the course name
; - prof: name of the professor
; - meetings: weekly meeting times

(define COURSE-EASY-A
  (make-course "SCHED" "101" "Easy A" "Lazy"
               '()))

(define COURSE-FUNDIES-LECTURE
  (make-course "CS" "2500" "Fundies" "Howdy"
               (list CM-FUNDIES-M CM-FUNDIES-W CM-FUNDIES-R)))

(define COURSE-FUNDIES-LAB
  (make-course "CS" "2501" "Fundies Lab" "Awesome TAs"
               (list CM-FUNDIES-LAB)))

(define COURSE-DISCRETE-LECTURE
  (make-course "DS" "1800" "Discrete Structures" "Dr Strange"
               (list CM-DISCRETE-T CM-DISCRETE-F)))

(define COURSE-DISCRETE-SEM
  (make-course "DS" "1802" "Seminar for CS 1800" "Park"
               (list CM-DISCRETE-SEM)))

(define COURSE-CREATURES
  (make-course "HPTR" "2000" "Care of Magical Creatures" "Hagrid"
               (list CM-CREATURES-T)))

(define COURSE-POTIONS
  (make-course "HPTR" "2650" "Potions" "Snape"
               (list CM-POTIONS-R)))

(define (course-temp c)
  (... (course-prefix c) ...
       (course-num c) ...
       (course-name c) ...
       (course-prof c) ...
       (locm-temp (course-meetings c)) ...))


; A CourseSchedule is a [List-of Course]
; Interpretation: a list of weekly courses!

(define SCHEDULE-OOPS '())

(define SCHEDULE-KHOURY
  (list COURSE-FUNDIES-LECTURE
        COURSE-FUNDIES-LAB
        COURSE-DISCRETE-LECTURE
        COURSE-DISCRETE-SEM))

(define SCHEDULE-MAGIC
  (list COURSE-CREATURES
        COURSE-POTIONS))

(define SCHEDULE-CS+MAGIC
  (list COURSE-FUNDIES-LECTURE
        COURSE-FUNDIES-LAB
        COURSE-CREATURES
        COURSE-POTIONS))



; TODO 1/8: Part of healthy course scheduling is making sure to build in time for
;           food, and so you are to design the function lunch-course that produces
;           a "Lunch" course!
;
;           The function should take in a prefix & number (e.g., "FOOD" "101"),
;           a name & professor (e.g., "Exciting Baking" with "Alderton"), as
;           well as a list of weekdays. The function will then makes sure that
;           déjeuner occurs on all of those days at noon (for one hour) in a
;           single location of your choice (e.g., Hogwarts Great Hall).
;
;           Note: make sure to test your function on at least two sets of inputs!

; lunch-course : String String String String [List-of Weekday] -> Course
; produces a course named "Lunch"
(check-expect (lunch-course "FOOD"  "101" "Exciting Baking" "Alderton" '())
              (make-course "FOOD"  "101" "Exciting Baking" "Alderton" '())) 
(check-expect (lunch-course "FOOD"  "101" "Exciting Baking" "Alderton"
                            (list WEEKDAY-M WEEKDAY-W WEEKDAY-F))
              (make-course "FOOD"  "101" "Exciting Baking" "Alderton"
                           (list
                            (make-meeting WEEKDAY-M "Richards Hall" "102" 9 30 60)
                            (make-meeting WEEKDAY-W "Richards Hall" "102" 9 30 60)
                            (make-meeting WEEKDAY-F "Richards Hall" "102" 9 30 60))))


(define (lunch-course pre num name prof weekd)
  (make-course pre num name prof (map weekd->meeting weekd)))


; weekd->meeting : Weekday -> ClassMeeting
; converts a weekday to a classmeeting
(check-expect (weekd->meeting WEEKDAY-M) (make-meeting WEEKDAY-M "Richards Hall" "102" 9 30 60))
(check-expect (weekd->meeting WEEKDAY-W) (make-meeting WEEKDAY-W "Richards Hall" "102" 9 30 60))
(check-expect (weekd->meeting WEEKDAY-F) (make-meeting WEEKDAY-F "Richards Hall" "102" 9 30 60))

(define (weekd->meeting weekd)
  (make-meeting weekd "Richards Hall" "102" 9 30 60))



; TODO 2/8: Design the function long-weekend? that determines if a
;           course schedule avoids all classes on Mondays & Fridays.
;           In the examples above, this is true of OOPS and MAGIC.
;           Note: make sure to follow all the templates and
;           sufficiently test all your functions!

; long-weekend? : [List-of Course] -> Boolean
; Checks if course schedule avoids all classes on Mondays & Fridays
(check-expect (long-weekend? SCHEDULE-OOPS) #t)
(check-expect (long-weekend? SCHEDULE-MAGIC) #t)
(check-expect (long-weekend? SCHEDULE-CS+MAGIC) #f)

(define (long-weekend? listOfCourses)
  (andmap long-weekend-helper? listOfCourses))

; long-weekend-helper? : Course -> Boolean
; Checks if a course is on Monday or Friday
(check-expect (long-weekend-helper? COURSE-EASY-A) #t)
(check-expect (long-weekend-helper? COURSE-FUNDIES-LECTURE) #f)
(check-expect (long-weekend-helper? COURSE-FUNDIES-LAB) #t)
(check-expect (long-weekend-helper? COURSE-CREATURES) #t)
               
(define (long-weekend-helper? course)
  (andmap courseFM? (course-meetings course)))

; courseFM? : ClassMeeting -> Boolean
; Checks if the course meeting is Monday or Friday
(check-expect (courseFM? CM-FUNDIES-LAB) #t)
(check-expect (courseFM? CM-CREATURES-T) #t)
(check-expect (courseFM? CM-DISCRETE-F) #f)
(check-expect (courseFM? CM-FUNDIES-M) #f)

(define (courseFM? courseMeeting)
  (weekdayMF (meeting-day courseMeeting)))

; weekdayMF : Weekday -> Boolean
; Checks if the weekday is Monday or Friday
(check-expect (weekdayMF WEEKDAY-M) #f)
(check-expect (weekdayMF WEEKDAY-F) #f)
(check-expect (weekdayMF WEEKDAY-W) #t)
               
(define (weekdayMF w)
  (or
   (string=? w WEEKDAY-T)
   (string=? w WEEKDAY-W)
   (string=? w WEEKDAY-R)))

; TODO 3/8: Design the function only-khoury that takes a course schedule
;           and produces a new schedule only containing classes that
;           have the prefix "CS", "DS", or "CY". So supplying OOPS and
;           KHOURY would result in unaffected schedules, but MAGIC would
;           result in an empty schedule and CS+MAGIC would result in a
;           schedule with only Fundies :)
;
;           Note: since we didn't include any DS/CY courses in the
;           examples, make may need to create example courses to properly
;           test your helper function(s)! Some course suggestions include
;           DS2000 (Programming with Data) and CY2550 (Foundations of
;           Cybersecurity).

; only-khoury : [List-of Course] -> [List-of Course]
; Interpretation : produces a new schedule only containing classes
; that have the prefix "CS", "DS", or "CY"

(define COURSE-DS-LECTURE
  (make-course "DS" "2000" "Programming with Data" "Dr Feliciano"
               (list CM-DISCRETE-T CM-DISCRETE-F)))

(define COURSE-CY-LECTURE
  (make-course "CY" "2550" "Foundations of Cybersecurity" "Dr.Phil"
               (list CM-DISCRETE-T CM-DISCRETE-F)))

(check-expect (only-khoury SCHEDULE-OOPS) SCHEDULE-OOPS)
(check-expect (only-khoury SCHEDULE-KHOURY) SCHEDULE-KHOURY)
(check-expect (only-khoury SCHEDULE-MAGIC) '())
(check-expect (only-khoury SCHEDULE-CS+MAGIC)
              (list
               (make-course
                "CS"
                "2500"
                "Fundies"
                "Howdy"
                (list (make-meeting "Monday" "WVH" "210A" 10 30 65)
                      (make-meeting "Wednesday" "WVH" "210A" 10 30 65)
                      (make-meeting "Thursday" "WVH" "210A" 10 30 65)))
               (make-course "CS" "2501" "Fundies Lab" "Awesome TAs"
                            (list (make-meeting "Tuesday" "WVH" "212" 8 0 100)))))

(define (only-khoury listOfCourses)
  (filter khoury-temp listOfCourses))

; khoury-temp : Course -> Boolean
; Checks if the given course matches the supplied prefix
(check-expect (khoury-temp COURSE-EASY-A) #f)
(check-expect (khoury-temp COURSE-FUNDIES-LECTURE) #t)
(check-expect (khoury-temp COURSE-POTIONS) #f)

(define (khoury-temp name)
  (or
   (string=? (course-prefix name) "DS")
   (string=? (course-prefix name) "CS")
   (string=? (course-prefix name) "CY")))



; TODO 4/8: Design the function time-in-class that calculates total
;           time spent in class (in minutes each week) for a supplied  
;           course schedule. For example, OOPS requires 0 minutes and
;           KHOURY is 560.

; time-in-class : [List-of Course] -> NonNegReal
; calculates total time spent in class (in minutes each week) for a supplied course schedule.
(check-expect (time-in-class SCHEDULE-OOPS) 0)
(check-expect (time-in-class SCHEDULE-KHOURY) 560)
(check-expect (time-in-class SCHEDULE-MAGIC) 400)
(check-expect (time-in-class SCHEDULE-CS+MAGIC) 695)

(define (time-in-class listOfCourses)
  (foldr + 0 (map example-helper listOfCourses)))

; example-helper : Course -> NonNegReal
; takes in course and returns the time
(check-expect (example-helper COURSE-EASY-A) 0)
(check-expect (example-helper COURSE-FUNDIES-LECTURE) 195)
(check-expect (example-helper COURSE-FUNDIES-LAB) 100)
(check-expect (example-helper COURSE-POTIONS) 200)

(define (example-helper course)
  (foldr + 0 (map meeting-duration (course-meetings course))))


; TODO 5/8: Design the function bring-water? that takes a course schedule
;           and determines if any course has even a single meeting that
;           lasts for longer than two hours. For example, this is true
;           for either of the magic schedules, but none of the others.

; bring-water? : [List-of Course] -> Boolean
; determines if any course has even a single meeting that lasts for longer than two hours.
(check-expect (bring-water? SCHEDULE-OOPS) #f)
(check-expect (bring-water? SCHEDULE-KHOURY) #f)
(check-expect (bring-water? SCHEDULE-CS+MAGIC) #t)
(check-expect (bring-water? SCHEDULE-MAGIC) #t)

(define (bring-water? listOfCourses)
  (ormap water-helper listOfCourses))

; water-helper : Course -> Boolean
; Checks the meeting duration of a list of ClassMeetings
(check-expect (water-helper COURSE-EASY-A) #f)
(check-expect (water-helper COURSE-FUNDIES-LECTURE) #f)
(check-expect (water-helper COURSE-FUNDIES-LAB) #f)
(check-expect (water-helper COURSE-POTIONS) #t)

(define (water-helper course)
  (ormap newtemp (course-meetings course)))

; newtemp : ClassMeeting -> Boolean
; Checks if the meeting duration is greater than 120
(check-expect (newtemp CM-FUNDIES-M) #f)
(check-expect (newtemp CM-FUNDIES-LAB) #f)
(check-expect (newtemp CM-CREATURES-T) #t)
(check-expect (newtemp CM-POTIONS-R) #t)

(define (newtemp meeting)
  (water-temp (meeting-duration meeting)))

; water-temp : PosInt -> Boolean
; Checks if the given meeting time is greater than 120
(check-expect (water-temp 100) #f)
(check-expect (water-temp 120) #f)
(check-expect (water-temp 140) #t)

(define (water-temp meetingtime)
  (> meetingtime 120))



; TODO 6/8: Design the function course->days-abbrev that takes a course
;           and produces a single string that has abbreviations of all
;           days of the week that course meets. For instance, Fundies
;           lecture would produce "MWR", Fundies lab would produce "T",
;           Discrete lecture would be "TF", and the "easy A" class would
;           produce "" (since the lazy prof never wants to meet!).

; course->days-abbrev : Course -> String
; Produces a single string that has abbreviations of all
; days of the week that course meets
(check-expect (course->days-abbrev COURSE-EASY-A) "")
(check-expect (course->days-abbrev COURSE-FUNDIES-LECTURE) "MWR")
(check-expect (course->days-abbrev COURSE-FUNDIES-LAB) "T")
(check-expect (course->days-abbrev COURSE-DISCRETE-LECTURE) "TF")
(check-expect (course->days-abbrev COURSE-DISCRETE-SEM) "W")
(check-expect (course->days-abbrev COURSE-CREATURES) "T")

(define (course->days-abbrev course)
  (foldr string-append "" (map next-temp (course-meetings course))))

; next-temp : ClassMeeting -> String
; Checks which day of the week is the class meeting
(check-expect (next-temp CM-FUNDIES-LAB) "T")
(check-expect (next-temp CM-DISCRETE-T) "T")
(check-expect (next-temp CM-DISCRETE-F) "F")
(check-expect (next-temp CM-POTIONS-R) "R")

(define (next-temp meeting)
  (weekdaytempl (meeting-day meeting)))


; weekdaytempl : String -> String
; Checks if the supplied day of the week matches the Weekday
(check-expect (weekdaytempl "Monday") "M")
(check-expect (weekdaytempl "Tuesday") "T")
(check-expect (weekdaytempl "Friday") "F")

(define (weekdaytempl w)
  (cond
    [(string=? w WEEKDAY-M) "M"]
    [(string=? w WEEKDAY-T) "T"]
    [(string=? w WEEKDAY-W) "W"]
    [(string=? w WEEKDAY-R) "R"]
    [(string=? w WEEKDAY-F) "F"]))
  
         

; TODO 7/8: Design the functions stack/h and stack/v, to stack a supplied
;           list of images horizontally and vertically, with a bit of buffer
;           between each image (see the GAP we've defined for you). You have
;           been supplied tests for clarity.

(define GAP (square 5 "solid" "white"))

; stack/h : [List-of Image] -> Image
; Stack a supplied list of images horizontally
(check-expect (stack/h '()) GAP)
(check-expect
 (stack/h
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (beside
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))


(define (stack/h listOfImages)
  (foldr horizontal-function GAP listOfImages))  ; function , basecase, variable

; horizontal-function : Image Image  -> Image
; Function to place GAP and current Image beside previous image
(define (horizontal-function image base)
  (beside GAP image base))



; stack/v : [List-of Image] -> Image
; Stack a supplied list of images vertically
(check-expect (stack/v '()) GAP)
(check-expect
 (stack/v
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (above
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/v listOfImages)
  (foldr vertical-function GAP listOfImages))  ; function , basecase, variable

; vertical-function : Image Image -> Image
; Function to place GAP and current Image above previous image
(define (vertical-function image base)
  (above GAP image base))





; TODO 8/8: Now using your solutions to the previous two parts, design the
;           function viz-schedule, which produces a visual representation
;           of a supplied course schedule, such that each course is a row
;           (with the prefix, num, name, prof, and day abbreviations) and
;           the rows are vertically stacked. You have been supplied tests
;           for clarity.

; viz-schedule : [List-of Course] -> Image
; Produces a visual representation of a supplied course schedule
(check-expect (viz-schedule SCHEDULE-OOPS) GAP)
(check-expect (viz-schedule SCHEDULE-KHOURY)
              (above GAP
                     (text "CS 2500 (Fundies, Howdy): MWR" 50 "black")
                     GAP
                     (text "CS 2501 (Fundies Lab, Awesome TAs): T" 50 "black")
                     GAP
                     (text "DS 1800 (Discrete Structures, Dr Strange): TF" 50 "black")
                     GAP
                     (text "DS 1802 (Seminar for CS 1800, Park): W" 50 "black")
                     GAP))

(define (viz-schedule listOfCourse)
  (stack/v (map viz-helper listOfCourse))) ; function , basecase, variable


; viz-helper : Course -> String
; Helper function that produces the string for image in viz-schedule

(define (viz-helper course)
  (text (string-append (course-prefix course) " " (course-num course) " (" (course-name course) ", "
                       (course-prof course) "): " (course->days-abbrev course)) 50 "black"))


                  


