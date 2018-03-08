#lang racket

(require rackunit
         "structs.rkt"
         (for-syntax syntax/parse
                     "date.rkt"
                     racket))

(provide 
  #%app
  #%datum
  (rename-out [traveler-mb #%module-begin]))



;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;;
;;;;;;;;;;; ;;;;;;;;;;;   Syntaxes  ;;;;;;;;;;; ;;;;;;;;;;;
;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;;



;; The entire client file will be parsed into:
;; (define database ...)
;; (find-all-path from to database)
(define-syntax traveler-mb
  (syntax-parser
    [(_ p city ...)
     (check-origin-dups #'(city ...))
     #'(#%module-begin
        (define database
          (graph (list (city-clause city) ...)))
        (make-plan database p))]))

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



;; Turn city-clause into a runtime Node struct
(define-syntax city-clause
  (syntax-parser
    [(_ (ori to-clause ...))
     #'(node 'ori (list (make-edge 'ori to-clause) ...))]))

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
    #:datum-literals (plan --> timezone)
    [(_ db 
        (plan (ori -> des)
              (timezone ori-tz des-tz)
              conditional-clause 
              ...))
     #'(begin
         (define paths (find-all-path 'ori 'des db 0))
         paths)]))
         




;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;;
;;;;;;;;;;; ;;;;;;;;;;; Runtime Functions  ;;;;;;;;;;; ;;;;;;;;;;;
;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;; ;;;;;;;;;;;

;; -------------------------------------------------------------------
;; How to hide utility functions' scopes
;; Utility Functions for Graph
;; -------------------------------------------------------------------
#|

A -> B 100 500 700
A -> B 150 400 610
B -> C 30 800 950
B -> C 70 600 750


|#
(define A (node 'A (list (edge 'A 'B 100 500 700)
                         (edge 'A 'B 150 400 610))))
(define B (node 'B (list (edge 'B 'C 30 800 950)
                         (edge 'B 'C 70 550 750))))
(define C (node 'C '()))
(define sample-G (graph (list A B C)))

;; Symbol Graph -> [Maybe Node]
;; Retrive the Node that has corresponding name from graph G
;; Return #f if the node does not exist in graph
(module+ test
  (check-equal? (get-node 'B sample-G) B)
  (check-equal? (get-node 'H sample-G) #f))

(define (get-node name G)
  (findf (lambda (a-node)
           (symbol=? name (node-name a-node)))
         (graph-nodes G)))


;; Symbol Graph -> [List-of Symbol]
;; Return a list of name of neighbors 
;; of the node with given name  in graph G
(define (get-edges name G)
  (define the-node (get-node name G))
  (if the-node
    (node-edges the-node)
    '()))


;; 
;; Adds an edge to existing path
;; Effectively returning an updated path including that edge
(define (update-path eg p)
  (path (path-ori p)
        (path-des p)
        (cons eg (path-loe p))))

;; Edge [List-of Path] -> [List-of Path]
;; Similar to make-path
;; Adds an edge to update each path in given list of path lop
(define (add-to-paths eg lop)
  (map (lambda (a-path)
         (update-path eg a-path))
       lop))


; Symbol Symbol Graph Number -> [List-of Path]
; finds a all paths from name of node ori to name of node des in graph G
; current time set to be curr
; If no such path exist, the list contains names of both origin and destination,
; and name of all the intermediate nodes.
(define (find-all-path origin des G start-time)
  (local (;; Accumulator seen:
          ;; A list of symbols that represents the name of nodes
          ;; which have been visited
          ;; curr is a number that reprents the current time
          ;; ASSUME:
          ;; ori is not in seen
          (define (find-all-path-acc curr-pos seen curr)
            (cond
              [(symbol=? curr-pos des) (list (path origin des '()))]
              [else (define neighbors (get-edges curr-pos G))

                    ;; Filter out neighbors that have been seen
                    ;; [List-of Edges]
                    (define valid-neighbors 
                      (filter (lambda (an-edge)
                                (and (not (member (edge-to an-edge) seen))
                                     (< curr (edge-start an-edge))))
                              neighbors))


                    ;; [List-of [List-of Path]]
                    (define results 
                      (map (lambda (an-edge) 
                             (add-to-paths an-edge
                                          (find-all-path-acc (edge-to an-edge) 
                                                             (cons (edge-to an-edge) seen)
                                                             (edge-end an-edge))))
                           valid-neighbors))


                    ;; [List-of Path]
                    ;; Paths from all of ori's valid neighbors to
                    ;; des
                    (define flattened-results 
                      (foldr (lambda (this-lop lop-sofar)
                               (append this-lop
                                       lop-sofar))
                             '()
                             results))

                    flattened-results])))
    (find-all-path-acc origin '() start-time)))

;(find-all-path 'A 'C sample-G 350)

