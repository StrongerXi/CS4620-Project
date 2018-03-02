#lang racket

(provide (all-from-out racket)
         define-graph
         define-nodes
         add-nodes
         print-graph)

(require rackunit)



;"(\d{2}):(\d\{2})"
;"(\d\{2})/(\d\{2})/(\d\{4})"
;; Date-String -> Number
;; Time -> Number

;(define 

(define-syntax (define-graph stx)
  (syntax-case stx (name directed?)
    ((_ (name name-id))
     #'(define name-id (graph 'name-id '() #f)))
    ((_ (name name-id) (directed? bool))
     #'(define name-id (graph 'name-id '() bool)))))



(define-syntax (define-nodes stx)
  (syntax-case stx (->)
    ((_ (name-id -> nb-ids ...) ...)
     #'(begin (define name-id (node 'name-id '(nb-ids ...)))
              ...))))



(define-syntax (add-nodes stx)
  (syntax-case stx (->)
    ((_ 
        (node-id ...)
        ->
        graph-id)

     #'(set! graph-id (graph (graph-name graph-id)
                             (append (list node-id ...)
                                     (graph-nodes graph-id))
                             (graph-directed graph-id))))))


(define (print-graph G)
  (displayln (graph-name G))
  (void (map (lambda (node)
               (displayln (list (node-name node)
                                (node-edges node))))
             (graph-nodes G))))


(struct graph [name nodes directed] #:transparent)
;; A Graph is a (graph Symbol [List-of Node] Boolean)
(struct node [name edges] #:transparent)
;; A Node is a (node Symbol [List-of Edge]))
(struct edge [from to cost start end] #:transparent)
;; An Edge is a (edge Symbol Number Number Number)
;; to represents the destination of this edge
;; cost represents the monetary cost of this route/edge
;; start represents the starting time (converted to a single number)
;; end represents the time of arrival
(struct path [ori loe] #:transparent)
;; A Path is a (path Symbol [List-of Edge])
;; ori is a symbol representing origin


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
(define sample-G (graph 'sample (list A B C) #t))

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
              [(symbol=? curr-pos des) (list (path origin '()))]
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

(find-all-path 'A 'C sample-G 350)

