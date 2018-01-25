(define-graph (name i)
            (list nodes....))


(define i graph [....])

(define nodeA)



(add-node (graph i)
          (node b)
          (cost 5))




(node-init (name a)
           (...))




==>
(define a node [...])
(define a node [...])
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

; Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination))]
    [else (define neighbors (get-neighbors origination G))
               (define results (map (lambda (node) (find-path node destination G))))
               (define valid-results (filter cons? results))
               (if (empty? valid-results)
                 '() 
                 (cons origination (first valid-results)))])

(define-node A (neighbors B E))
(define-node B (neighbors E F))


(define-graph G (nodes A B)
              (directed? #t))

(find-path A B G)
(contains-node G A)





