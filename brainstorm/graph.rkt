

(struct graph [name nodes directed])
;; A Graph is a (graph Symbol [List-of Node] Boolean)
(struct node [name destinations])
;; A Node is a (node Symbol [List-of Symbol]))



(define-graph (name G)
              (directed? #t)) ;; Default #f

(define-nodes ((name A) (neighbors B E))
              ((name B) (neighbors A))
              ((name E) (neighbors A)))

(add-node (graph G)
          (nodes A B E))


(find-path (graph G)
           (nodes A B))

(contains-node (graph G)
               (nodes A))


==>

(define G (graph 'G '() #t))
(define A (node 'A '(B E)))
(define B (node 'B '(A)))
(define E (node 'E '(A)))

(set! G (graph (graph-name G)
               (list A B E)
               (graph-directed G)))

(find-path A B G)

;;TODO Circular Path??

; Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination))]
    [else (define neighbors (get-neighbors origination G))
          (define results (map (lambda (node) 
                                 (find-path node destination G)) 
                               neighbors))
          (define valid-results (filter cons? results))
          (if (empty? valid-results)
            '() 
            (cons origination (first valid-results)))])



; Error at compile time or run time?
; error message -> (duplicate in define)
; but we want our customized error message (duplicate in node-init, name conflict)


(contains-node? i)



(define g ...)

(define new-g (add-node g n))

(contains-node? new-g n)


;Graph Node Node -> Path
(define (find-path graph fromA toB)
  (cond
    []
    [... find-path sub-graph from... toB]))

(graph-make (get-nodes i))


(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

