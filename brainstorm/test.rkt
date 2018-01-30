#lang racket

(require (for-syntax syntax/parse))

(struct graph [name nodes directed])
;; A Graph is a (graph Symbol [List-of Node] Boolean)
(struct node [name dess])
;; A Node is a (node Symbol [List-of Symbol]))


(define-syntax (define-graph stx)
  (syntax-case stx (name directed?)
    ((_ (name name-id))
     #'(define name-id (graph 'name '() #f)))
    ((_ (name name-id) (directed? bool))
     #'(define name-id (graph 'name '() bool)))))
     

(define-graph (name Boston-map)
              (directed? #t))


(define-syntax (define-nodes stx)
  (syntax-case stx (name neighbors)
    ((_ ((name name-id) (neighbors nb-ids ...)) ...)
     #'(begin (define name-id (node 'name-id '(nb-ids ...)))
              ...))))

(define-nodes ((name Harvard) (neighbors Cambridge Potter))
              ((name Cambridge) (neighbors Harvard))
              ((name Potter) (neighbors Harvard)))



;; -------------------------------------------------------------------
;; Utility Functions for Graph
;; -------------------------------------------------------------------
(define A (node 'A '(B E)))
(define B (node 'B '(A)))
(define E (node 'E '(A)))
(define G (graph 'G (list A B E) #t))

;; Symbol Graph -> [Maybe Node]
;; Retrive the Node that has corresponding name from graph G
;; Return #f if the node does not exist in graph
(module+ test
  (check-equal? (get-node 'B G) B)
  (check-equal? (get-node 'E G) E)
  (check-equal? (get-node 'C G) #f))

(define (get-node name G)
  (findf (lambda (a-node)
           (symbol=? name (node-name a-node)))
         G))

;;TODO Circular Path??
; Symbol Symbol Graph -> [List-of Node]
; finds a path from name of node ori to name of node des in graph G
(define (find-path ori des G)
  (cond
    [(symbol=? ori des) (list des))]
    [else (define neighbors (get-neighbors ori G))
          (define results (map (lambda (node) 
                                 (find-path node des G)) 
                               neighbors))
          (define valid-results (filter cons? results))
          (if (empty? valid-results)
            '() 
            (cons ori (first valid-results)))])


(define-syntax (foo stx)
  (syntax-case stx (name G)
    ((_ name) #''name)
    ((_ G) #''G)
    ((_ abc) #''abc)))

