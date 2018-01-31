#lang racket


;; ----------------------------DATA DEFINITION-------------------------------------
;; A Graph is a [List-of Node]
;; Assume the graph is directed.
;; No duplicates are allowed

(define-struct node [name data edges])
;; A Node is a (make-node Symbol Number [List-of Edge])
;; An Edge is a [Pair Number Symbol]

;; A Directed-Edge is a (list Number Origin-Node-Name End-Node-Name)

;; INTERPRETATION:
;; For a graph (define G `(,a ,b ,c ,d))
;; a, b, c, d are all the nodes in this graph

;; For a node (make-node sb 10 `((5 ,c) (13 ,d)) )
;; sb represents this node's name
;; 10 represents the value it contains
;; (5 c) and (13 d) are the edges extending from a to other nodes

;; For an edge (list 17 b)
;; 17 represents the cost of this edge
;; b represents the end point of this edge


;; ----------------------------EXAMPLES-------------------------------------

(define graph0 '())


(define A (make-node 'a 20 (list 10 'b)))
(define B (make-node 'b 15 (list 5 'c)))
(define C (make-node 'c 30 (list 25 'd)))
(define D (make-node 'd 5 (list 15 'a)))

(define graph1 (list A B C D))

;;  20    15   5
;;   A<--------D
;;   |         /\
;; 10|         |  25
;;   \/        |
;;   B-------->C 30
;;  15     5


;; ----------------------------Selectors, Predicates, Helper functions-------------------------------

;; --------------------GRAPH----------------------------
;; Any -> Boolean
;; Check whether any is a graph
(define (graph? any)
  (and (list? any)
       (andmap node? any)))

;; Symbol Graph -> Boolean
;; Determine whether the g contains a node with name s
(define (graph-contains? s g)
  (ormap (λ (node)
           (symbol=? s (node-name node)))
         g))

;; Symbol Graph -> Node
;; Return the node in g with name as s
;; If not found, raise an error
(define (graph-get-node s g)
  (cond
    [(empty? g) 
     (error "node named " (symbol->string s) "not found")]
    [else (define this (first g))
          (if (is-node? s this)
              this
              (graph-get-node s (rest g)))]))

;; Node [List-of Directed-Edge] Graph -> Graph
;; Add node into graph, raise an error if already exists
;; loe corresponds to the edges that extends FROM the endpoint to Node
;; Add them to their origin nodes in graph respectively
(define (add-node-to-graph node loe graph)
  (cond
    [(graph-contains? (node-name node) graph) (error "cannot add a node that already exists : "
                                                     (symbol->string (node-name node)))]
    [else (foldr add-edge-to-graph
                 graph
                 loe)]))

;; Directed-Edge Graph -> Graph
;; Add a directed edge de into graph
;; Raise an error if already exists
;; or if any of the node in this edge does not exists
(define (add-edge-to-graph de graph)
  (define origin (directed-edge-origin de))
  (define end (directed-edge-end de))
  (cond
    [(not (and (graph-contains? origin graph)
               (graph-contains? end graph)))
     (error "end or origin node in this de cannot be found in graph given")]
    [else (define edge-to-add (cons (directed-edge-cost de) end))
          (foldr (λ (a-node checked-graph)
                   (if (is-node? origin a-node)
                       (cons (node-add-edge edge-to-add a-node)
                             checked-graph)
                       (cons a-node checked-graph))))]))
              
;; --------------------NODE----------------------------
;; Symbol Node -> Boolean
;; Determine whether node has name s
(define (is-node? s node)
  (symbol=? (node-name node) s))

;; Edge Node -> Node
;; Add edge to node
;; raise an error if such an edge already exists
(define (node-add-edge edge node)
  (define old-edges (node-edges node))
  (define this-name (node-name node))
  (if (member edge old-edges)
      (error "edge already exists between nodes"
             (symbol->string (edge-end edge))
             "and"
             (symbol->string this-name))
             
      (make-node this-name
                 (node-data node)
                 (cons edge old-edges))))

;; --------------------EDGE----------------------------
;; Any -> Boolean
;; Check whether any is an edge
(define (edge? any)
  (and (cons? any)
       (= 2 (length any))
       (number? (first any))
       (symbol? (second any))))

;; Edge -> Number
;; Return the cost of this edge
(define (edge-cost edge)
  (first edge))

;; Edge -> Symbol
;; Return the end node's name on this edge
(define (edge-end edge)
  (second edge))

;; --------------------DIRECTED-EDGE----------------------------
;; Any -> Boolean
;; Check whether any is a directed-edge
(define (directed-edge? any)
  (and (cons? any)
       (= 3 (length any))
       (number? (first any))
       (symbol? (second any))
       (symbol? (third any))))
;; Directed-Edge -> Symbol
;; return the name of the origin in de
(define (directed-edge-origin de)
  (second de))

;; Directed-Edge -> Number
;; Return the cost of this de
(define (directed-edge-cost de)
  (first de))

;; Directed-Edge -> Symbol
;; Return the name of the end node in de
(define (directed-edge-end de)
  (third de))


  
;; ----------------------------FUNCTIONS-------------------------------------







