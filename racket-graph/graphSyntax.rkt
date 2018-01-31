#lang racket

(provide define-graph
         define-nodes
         add-nodes
         print-graph)

(require rackunit)


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
                                (node-neighbors node))))
             (graph-nodes G))))


(struct graph [name nodes directed])
;; A Graph is a (graph Symbol [List-of Node] Boolean)
(struct node [name neighbors])
;; A Node is a (node Symbol [List-of Symbol]))

;; A Path is a [List-of Symbol]
;; Where symbols are names of nodes that connect
;; (first path) to (last path)


;; -------------------------------------------------------------------
;; How to hide utility functions' scopes
;; Utility Functions for Graph
;; -------------------------------------------------------------------
(define A (node 'A '(B E)))
(define B (node 'B '(E F)))
(define C (node 'C '(B D)))
(define D (node 'D '()))
(define E (node 'E '(C)))
(define F (node 'F '(D G)))
(define G (node 'G '()))
(define sample-G (graph 'sample (list A B C D E F G) #t))

;; Symbol Graph -> [Maybe Node]
;; Retrive the Node that has corresponding name from graph G
;; Return #f if the node does not exist in graph
(module+ test
  (check-equal? (get-node 'B sample-G) B)
  (check-equal? (get-node 'E sample-G) E)
  (check-equal? (get-node 'H sample-G) #f))

(define (get-node name G)
  (findf (lambda (a-node)
           (symbol=? name (node-name a-node)))
         (graph-nodes G)))


;; Symbol Graph -> [List-of Symbol]
;; Return a list of name of neighbors 
;; of the node with given name  in graph G
(module+ test
  (check-equal? (get-neighbors-name 'B sample-G)
                '(E F))
  (check-equal? (get-neighbors-name 'A sample-G)
                '(B E))
  (check-equal? (get-neighbors-name 'C sample-G)
                '(B D)))

(define (get-neighbors-name name G)
  (define the-node (get-node name G))
  (if the-node
    (node-neighbors the-node)
    '()))



; Symbol Symbol Graph -> [List-of Path]
; finds a all paths from name of node ori to name of node des in graph G
; If no such path exist, the list contains names of both origin and destination,
; and name of all the intermediate nodes.
(define (find-all-path ori des G)
  (local (;; Accumulator seen:
          ;; A list of symbols that represents the name of nodes
          ;; which have been visited
          ;; ASSUME:
          ;; ori is not in seen
          (define (find-all-path-acc ori des G seen)
            (cond
              [(symbol=? ori des) `((,des))]
              [else (define neighbors (get-neighbors-name ori G))
                    ;; Filter out neighbors that have been seen
                    (define valid-neighbors (filter (lambda (name)
                                                      (not (member name seen)))
                                                    neighbors))
                    ;; [List-of [List-of Path]]
                    (define results (map (lambda (name) 
                                           (find-all-path-acc name des G (cons name seen))) 
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
                    ;; [List-of Path]
                    ;; All Possible Paths from ori to des
                    (define final-results
                      (map (lambda (path) (cons ori path)) flattened-results))

                    final-results])))
    (find-all-path-acc ori des G '())))

(module+ test
  (check-equal? (find-all-path 'A 'G sample-G)
                '((A B F G)
                  (A E C B F G)))

  (check-equal? (find-all-path 'G 'A sample-G)
                '())

  (check-equal? (find-all-path 'D 'C sample-G)
                '())

  (check-equal? (find-all-path 'A 'D sample-G)
                '((A B E C D)
                  (A B F D)
                  (A E C B F D)
                  (A E C D))))






