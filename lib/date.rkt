#lang racket

(provide
 date-time->number
 hour->seconds
 time->seconds
 increment-day
 (rename-out [date->secs date->seconds]))

(require racket/string
         racket/date)

(define (date-time->number date time)
  (define secs (date->seconds (syntax->date date time)))
  (datum->syntax date secs))


;; Syntax -> Syntax
;; #'12345 -> #'12345...
;; Treat input syntax as a representation of a time-date in seconds
;; increment the time-date by a day and return the reulst seconds as a syntax
(define (increment-day time-date-secs)
  (datum->syntax time-date-secs 0))


;; Syntax -> (Syntax N)
;; #'2 -> 7200
(define (hour->seconds hour)
  (datum->syntax hour 0))

;; Syntax -> (Syntax N)
;; #'20:00 -> 72000
(define (time->seconds time)
  (datum->syntax time 0))

;; Syntax -> (Syntax N)
;; #'02/10/2018 -> ...
(define (date->secs date)
  (datum->syntax date 0))


;; Syntax Syntax -> date
;; checks validity and returns a date struct
;; raises a syntax error if it is not possible
(define (syntax->date date time)
  (define date-list (get-date-list date))
  (define time-list (get-time-list time))
  (unless date-list (raise-syntax-error #f "incorrect date format" date))
  (unless time-list (raise-syntax-error #f "incorrect time format" time))
  (make-date 0 (second time-list) (first time-list) (second date-list) (first date-list) (third date-list) 0 0 #f 0))

;; syntax-> [maybe list of numbers]
;; returns a list of umbers representing the time if the time is valid else returns false
(define (get-time-list time)
  (define t (symbol->string (syntax->datum time)))
  (define time-list (map string->number (string-split t ":")))
  (if (and
       (andmap number? time-list)
       (= (length time-list) 2)
       (< (first time-list) 24)
       (< (second time-list) 60)) time-list #f))

;; syntax-> [maybe list of numbers]
;; returns a list of numbers representing the date if the date is valid else returns false
(define (get-date-list date)
  (define d (symbol->string (syntax->datum date)))
  (define date-list (map string->number (string-split d "/")))
  (define month-31 '(1 3 5 7 8 10 12))
  (define month-30 '(4 6 9 11))
  (if
   (and
    (andmap number? date-list)
    (= (length date-list) 3)
    (or (and (member (first date-list) month-31) (<= (second date-list) 31))
        (and (member (first date-list) month-30) (<= (second date-list) 30))
        (and (= (first date-list) 2) (not (is-leap-year (third date-list))) (<= (second date-list) 28))
        (and (= (first date-list) 2) (is-leap-year (third date-list)) (<= (second date-list) 29))))
   date-list #f))

;; checks if the given year is a leap year
(define (is-leap-year year)
  (or (and (= (modulo year 4) 0) 
           (not (= (modulo year 100) 0)))
      (= (modulo year 400) 0)))

