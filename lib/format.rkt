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
   (foldr (λ (pp str) 
             (string-append str "\n" 
                            (pp->string pp)))
          "" 
          (result-lop res))
   sep=))


;; Processed-Path -> String
;; gets all the information for this processed path as a string
(define (pp->string pp)
  ;; the list of edges for this path
  (define path (path-loe (processed-path-path pp)))
  ;; the length of the first column
  (define col1 (string-length
                (argmax string-length
                        (cons "From"
                              (map (λ (p) (symbol->string (edge-from p))) path)))))
  ;; the length of the second column
  (define col2 (string-length
                (argmax string-length
                        (cons "To"
                              (map (λ (p) (symbol->string (edge-to p))) path)))))
  ;; edge -> string
  ;; makes a string for the edge
  (define (combine e)
    (string-append
     (~a (edge-from e) #:min-width col1)
     "   "
     (~a (edge-to e) #:min-width col2)
     "   "
     (seconds->date-str (edge-start e))
     "  "
     (seconds->date-str (edge-end e))
     "  "
     (number->string (edge-cost e))
     "\n"))

  ;; the table of iformation for this path
  (define main (foldr string-append 
                      "" 
                      (map combine path)))
  ;; the header column for the table
  (define header (string-append (~a "From" #:min-width col1) "   " (~a "To" #:min-width col2)
                  "   Departure Time      Arrival Time        Cost\n"))
  ;; finally append all the data
  (string-append
   sep*
   "Number of intermediate stops : " (number->string (processed-path-stop-count pp)) "\n"
   "Total Price : $" (number->string (processed-path-cost pp)) "\n"
   "Total Duration : " (real->decimal-string (/ (processed-path-duration pp) 3600)) " hrs\n\n"
   header
   main))

(define sep*
  "****************************************************************************************\n")
(define sep=
  "========================================================================================\n")


#;(printf
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