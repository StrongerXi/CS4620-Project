#lang racket

(require "structs.rkt")


;; Result -> String
;; stringify the plan Result as final output of a traveler program
(define (lopp->string lopp)
  "")


;; Processed-Path -> String
;; ...
(define (pp->string pp)
  "")


;; Edge -> String
;; ...
(define (edge->string eg)
  "")


;; A -> B t1 t2 $1000
(define los '("Boston" "JFK" "Shanghai"))
(define lod '("JFK" "Shanghai" "Beijing"))

(define n1 (string-length (argmax string-length (cons "Origin" los))))
(define n2 (string-length (argmax string-length (cons "Destination" lod))))



(define (out ori n1 des n2 others)
  (printf 
    (string-append
      (~a ori #:min-width n1)
      "        "
      (~a des #:min-width n2)
      others)))

(define sep*
  "****************************************************************************************\n")
(define sep=
  "========================================================================================\n")

(printf sep=)
(printf "Plan A\n")
(printf "From Boston to Shanghai\n\n")
(printf sep*)
(printf "Number of Intermediate Stops: 2\n")
(printf "Total Price:     $4602\n")
(printf "Total Duration   46 hours\n\n")
(out "Origin" n1 "Destination" n2 "   Departure Time    Arrival Time      Cost \n")
(out "Boston" n1 "JFK" n2 "   16:14 02/02/2018  19:00 02/03/2018  $1534\n")
(out "JFK" n1 "Shanghai" n2  "   22:00 02/02/2018  02:00 02/04/2018  $1534\n")
(out "Shanghai" n1 "Beijing" n2  "   20:00 02/04/2018  02:00 02/05/2018  $1534\n")
(printf sep*)
(printf sep=)
