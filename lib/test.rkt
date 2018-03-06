
#lang racket

;; Even though lang does not explicitly provide the macro "foo",
;; the output of mb is able to use it, and racket expander will expand
;; it once it sees the foo macro.
;; This means that, the run module cannot explicitly use "foo", yet
;; if it's expression gets transformed into a "foo" macro, racket is
;; able to expand it.
(module lang racket

  (require (for-syntax syntax/parse))
  (provide 
    (except-out (all-from-out racket) #%module-begin)
    (rename-out [mb #%module-begin]))

  (define b 5)

  (define-syntax mb
    (syntax-parser
      [(_ e ...)
       #'(#%module-begin (foo e) ...)]))

  (define-syntax foo
    (syntax-parser
      [(_ e ...)
       #'b]))
  )


(module run (submod ".." lang)

  (define a 5)
  a
  )





;; Path -> String
(define (path->string p)
  "")


;; A -> B t1 t2 $1000
(define los '("Boston" "JFK" "Shanghai"))
(define lod '("JFK" "Shanghai" "Beijing"))

(define n1 (string-length (argmax string-length (cons "Origin" los))))
(define n2 (string-length (argmax string-length (cons "Destination" lod))))

#;(string-append
(~a "Origin" #:min-width n1)
"        "
(~a "Destination" #:min-width n2)
"   Departure Time " "   Arrival Time " "     Cost \n")

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
(printf "Number of Stops: 2\n")
(printf "Total Price:     $4602\n")
(printf "Total Duration   46 hours\n\n")
(out "Origin" n1 "Destination" n2 "   Departure Time    Arrival Time      Cost \n")
(out "Boston" n1 "JFK" n2 "   16:14 02/02/2018  19:00 02/03/2018  $1534\n")
(out "JFK" n1 "Shanghai" n2  "   22:00 02/02/2018  02:00 02/04/2018  $1534\n")
(out "Shanghai" n1 "Beijing" n2  "   20:00 02/04/2018  02:00 02/05/2018  $1534\n")
(printf sep*)
(printf sep=)
