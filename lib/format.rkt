#lang racket

(provide
 res->string
 pp->string)
 
(require "structs.rkt"
         "date.rkt")




;; Result -> String
;; stringify the plan Result as final output of a traveler program
(define (res->string res)
  (string-append
   sep=
   (symbol->string (result-name res))
   (foldr (λ (pp str) (string-append str "\n" (pp->string pp))) "" (result-lop res))
   sep=))


;; Processed-Path -> String
;; ...
(define (pp->string pp)
  (define path (path-loe (processed-path-path pp)))
  (define list-ori (map (λ (p) (symbol->string (edge-from p))) path))
  (define list-des (map (λ (p) (symbol->string (edge-to p))) path))
  (define list-start (map edge-start path))
  (define list-end (map edge-start path))
  (define list-cost (map edge-cost path))
  (define col1 (string-length (argmax string-length (cons "From" list-ori))))
  (define col2 (string-length (argmax string-length (cons "To" list-des))))
  (define (combine ori des start end cost)
    (string-append
     (~a ori #:min-width col1)
     "   "
     (~a des #:min-width col2)
     "   "
     (seconds->date-str start)
     "  "
     (seconds->date-str end)
     "  "
     (number->string cost)
     "\n"))
  (define main (foldr string-append "" (map combine list-ori list-des list-start list-end list-cost)))
  (string-append
   sep*
   "Number of intermediate stops : " (number->string (processed-path-stop-count pp)) "\n"
   "Total Price : $" (number->string (processed-path-cost pp)) "\n"
   "Total Duration : " (number->string (processed-path-duration pp)) "\n\n"
   (string-append (~a "From" #:min-width col1) "   " (~a "To" #:min-width col2)
                  "   Departure Time      Arrival Time        Cost\n")
   main))

(define sep*
  "****************************************************************************************\n")
(define sep=
  "========================================================================================\n")


(printf
 (pp->string (processed-path
  (path 'Boston 'Beijing
        (list (edge 'Boston 'Shanghai 980 1518127200 1518265800)
              (edge 'Shanghai 'Beijing 300 1518399000 1518413400)))
  -10
  8
  1280
  221400
  133200
  1)))



#|
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
(printf sep=)|#
