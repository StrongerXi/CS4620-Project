#lang racket

(require rackunit
         "structs.rkt"
         "format.rkt"
         "traveler-lib.rkt"
         (for-syntax syntax/parse
                     "date.rkt"
                     racket))

(provide 
  #%app
  #%datum
  depart-time
  depart-date
  wait-time
  price
  duration
  go-through
  bypass
  (rename-out [traveler-mb #%module-begin]))



; 1. Require and provide hygiene 
; 2. recurring flight schedule


;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;;
;;;;;;;;;;; ;;;;;;;;;;;   Syntaxes  ;;;;;;;;;;; ;;;;;;;;;;;
;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;;



;; The entire client file will be parsed into:
;; (define database ...)
;; (find-all-path from to database)

;; email to leif@leif.pl
(define-syntax traveler-mb
  (syntax-parser
    #:datum-literals (data-from database plan)
    [(_ database city ...)
     (check-origin-dups #'(city ...))
     #'(#%module-begin
        (provide db)
        (define db
          (graph (list (city-clause city) ...))))]

    [(_ (data-from file:str)
        (plan pe ...) ...)

     ;; This associated this macro's scope with the file syntax
     ;; which had a user-side scope. 
     ;; Now the db introduced in template can be associated with the
     ;; db provided in file because they have the same scope set.
     #:with foo (datum->syntax #'nomatter (syntax-e #'file))

     #'(#%module-begin
        (require foo)
        ;(module->exports 'file)
        (make-plan db (pe ...))
        ...)]))




;; Turn city-clause into a runtime Node struct
(define-syntax city-clause
  (syntax-parser
    [(_ (ori to-clause ...))
     #'(node 'ori (list (make-edge ori to-clause) ...))]))

;; Turn a given origin name and to-clause into an edge struct
;; Ex:
;; (Boston (SA207  19:00 02/09/2018 13:00 02/10/2018 1500 Beijing))
(define-syntax make-edge
  (syntax-parser
    [(_ ori 
        (id depart-time depart-date arrive-time arrive-date cost des))
     #:with start (date-time->number #'depart-date #'depart-time)
     #:with end (date-time->number #'arrive-date #'arrive-time)
     #'(edge 'ori 'des cost start end)]))


(define-syntax make-plan 
  (syntax-parser
    #:datum-literals (--> timezone)
    [(_ db 
        (name:id
          (ori -> des)
          (timezone ori-tz:number des-tz:number)
          conditional-clause 
          ...))
     (check-constraints #'(conditional-clause ...) #'ori)
     #'(begin
         (define paths (find-all-path 'ori 'des db 0))
         (define processed (lop->lopp paths ori-tz des-tz))
         (define filtered (filter-lopp processed (list conditional-clause ...)))
         (define output (result 'name filtered))
         (displayln (res->string output)))]))
         


(define-for-syntax constraints '(bypass depart-time depart-date 
                                        arrive-time arrive-date
                                        wait-time duration price
                                        go-through))


;; Syntax Syntax -> Void
;; Check whether the constraint clauses are unique (no dups)
;; and whether (first clause) is a member of constraints
;; Raise syntax error if either fails
(define-for-syntax (check-constraints stx ctx)
  ;; List of Constraint Clause
  (define loc (syntax->list stx))

  ;; Check clause name (must be a member of constraints)
  ;; This also ensures that each (syntax->datum clause) is a list
  (map (λ (clause) 
          (unless (and (list? (syntax->datum clause))
                       (member (first (syntax->datum clause))
                               constraints))
            (raise-syntax-error #f "Constraint clause name not recognized" clause)))
       loc)

  (unless (not (check-duplicates (map first (syntax->datum stx))))
    (raise-syntax-error #f "Constraint clauses in plan must have no duplicated type" ctx)))

;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Constraint Clauses Syntax ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;

(define-syntax depart-time
  (syntax-parser
    #:datum-literals (~)
    [(_ start ~ end)
    #:with lower (time->seconds #'start)
    #:with upper (time->seconds #'end)
    (check-bounds #'lower #'upper #'start)
    #'(λ (pp) (< lower (pp-depart-time pp) upper))]))


(define-syntax depart-date
  (syntax-parser
    #:datum-literals (~)
    [(_ start ~ end)
    #:with lower (date->seconds #'start)
    #:with upper (increment-day (date->seconds #'end))
    (check-bounds #'lower #'upper #'start)
    #'(λ (pp) (< lower (pp-depart-timedate pp) upper))]))


(define-syntax wait-time
  (syntax-parser
    #:datum-literals (~)
    [(_ start ~ end)
    #:with lower (hour->seconds #'start)
    #:with upper (hour->seconds #'end)
    (check-bounds #'lower #'upper #'start)
    #'(λ (pp) (< lower (pp-wait-time pp) upper))]))


(define-syntax price
  (syntax-parser
    #:datum-literals (~)
    [(_ lower ~ upper)
    (check-bounds #'lower #'upper #'lower)
    #'(λ (pp) (< lower (pp-price pp) upper))]))


(define-syntax duration
  (syntax-parser
    #:datum-literals (~)
    [(_ start ~ end)
    #:with lower (hour->seconds #'start)
    #:with upper (hour->seconds #'end)
    (check-bounds #'lower #'upper #'start)
    #'(λ (pp) (< lower (pp-duration pp) upper))]))


(define-syntax go-through
  (syntax-parser
    [(_ name ...)
     #'(λ (pp) (sublist? (list 'name ...)
                         (pp-cities pp)))]))



(define-syntax bypass
  (syntax-parser
    [(_ name ...)
     #'(λ (pp) (disjoint? (pp-cities pp) 
                          (list 'name ...)))]))


;; Syntax -> Void
;; Given a syntax that represents a list of city clauses,
;; raise a syntax error if there are any duplicates in the
;; origin names
(define-for-syntax (check-origin-dups stx)
  ;; [List-of city-syntax]
  (define loc (syntax->datum stx))
  (define names (map first loc))
  (unless (not (check-duplicates names))
    (raise-syntax-error #f "duplicates in origin names")))


;; Syntax Syntax Syntax -> Void
;; Determine whether the given bounds are logical
(define-for-syntax (check-bounds lower upper ctx)
  (unless (> (syntax->datum upper)
             (syntax->datum lower))
    (raise-syntax-error #f
                        "Illogical constraint, upper bound must be later than lower bound"
                         ctx)))



