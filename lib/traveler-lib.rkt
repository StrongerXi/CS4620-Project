#lang racket


(require rackunit
         "structs.rkt")
(provide find-all-path
         lop->lopp
         sublist?
         disjoint?
         filter-lopp
         pp-depart-time
         pp-depart-timedate
         pp-price
         pp-cities
         pp-duration
         pp-wait-time)



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
(define A (node 'A (list (edge 'ab1 'A 'B 100 500 700)
                         (edge 'ab2 'A 'B 150 400 610))))
(define B (node 'B (list (edge 'bc1 'B 'C 30 800 950)
                         (edge 'bc2 'B 'C 70 550 750))))
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


;; [List-of Path] Number Number -> [List-of Processed-Path]
;; Convert a list of path into a list of processed path, with given timezone info
(define (lop->lopp lop ori-tz des-tz)
  (map (λ (p) 
          (processed-path p ori-tz des-tz
                          (path-cost p)
                          (path-duration p ori-tz des-tz)
                          (path-wait-time p)
                          (path-stop-count p)))
       lop))


;; Path Number Number -> Number
;; Given a path, and origin's timezone and destination's timezone
;; Calculate and return the total duration of this travel
;; Return 0 if the path is empty (no route found)
;; Time is represented by seconds
(define (path-duration p ori-tz des-tz)
  (define ori-tz-secs (* 60 60 ori-tz))
  (define des-tz-secs (* 60 60 des-tz))
  (define loe (path-loe p))
  (if (empty? loe)
    0
    (- (- (edge-end (last loe)) des-tz-secs)
       (- (edge-start (first loe)) ori-tz-secs))))

       
;; Path -> Number
;; Calculate and return the total cost of this travel
(define (path-cost p)
  (foldr (λ (an-edge cost)
            (+ (edge-cost an-edge) cost))
         0
         (path-loe p)))

;; Path -> Nunber
;; Calculate and return the total time spent waiting between flights in this path
;; Time is represented by seconds
(define (path-wait-time p)
  ;; [List-of Edge] Number Number -> Number
  ;; Accumulator:
  ;; arrive-time is the arrive time of last flight at current city
  ;; curr is the total wait time accumulated so far
  (define (helper edges-left arrive-time curr)
    (cond
      [(empty? edges-left) curr]
      [(cons? edges-left) 
       (define this (first edges-left))
       (helper (rest edges-left)
               (edge-end this)
               (+ curr (- (edge-start this) arrive-time)))]
      [else (error "Ill input in path-wait-time's helper: " edges-left arrive-time curr)]))
  (define loe (path-loe p))
  (if (empty? loe)
    0
    (helper (rest loe) (edge-end (first loe)) 0)))


;; Path -> N
;; Calculate the number of intermediate stops in this travel path,
;; direct flight has 0 stop count
(define (path-stop-count p)
  (define loe (path-loe p))
  (if (empty? loe)
    0
    (- (length loe) 1)))



;; Processed-Path -> Number
;; Retrive the initial depart time of this path, represented in seconds
(define (pp-depart-time pp)
  (define secs-in-day (* 60 60 24))
  (define output (modulo (- (pp-depart-timedate pp) 18000) ;; Mystery
                         secs-in-day))
  output)


;; Processed-Path -> Number
;; Retrive the initial depart timedate of this path, represented in seconds
(define (pp-depart-timedate pp)
  (define loe (path-loe (processed-path-path pp)))
  (unless (cons? loe) 
    (error "Cannot attain depart timedate for an empty path"))
  (edge-start (first loe)))


;; Processed-Path -> Number
;; Retrive the total cost of this path.
(define (pp-price pp)
  (processed-path-cost pp))

;; Processed-Path -> Number
;; Retrive the total duration
(define (pp-duration pp)
  (processed-path-duration pp))

;; Processed-Path -> Number
;; Retrive the total wait-time of this path.
(define (pp-wait-time pp)
  (processed-path-wait-time pp))


;; Processed-Path -> [List-of Symbol]
;; Retrive names of all the cities that the given path go through
(define (pp-cities pp)
  (define p (processed-path-path pp))
  (cons (path-ori p)
        (map edge-to (path-loe p))))



;; A Filter is a [Processed-Path -> Boolean]

;; [List-of Processed-Path] [List-of Filter] -> [List-of Processed-Path]
;; Filter out the PP in given lopp that do not satisfy all the conditions in lof
(define (filter-lopp lopp lof)
  (filter (λ (pp)
             (andmap (λ (fit) (fit pp))
                     lof))
             lopp))



;; List List -> Boolean
;; Determine whether l1 and l2 are disjoint sets(lists)
(define (disjoint? l1 l2)
  (not (ormap (lambda (item)
             (member item l2))
           l1)))


;; List List -> Boolean
;; Determine whether l1 is a sublist of l2
;; meaning, all elements in l1 are in l2 as well
(define (sublist? l1 l2)
  (andmap (lambda (item)
            (member item l2))
          l1))


