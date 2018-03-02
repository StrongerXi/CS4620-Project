#lang s-exp "graphSyntax.rkt"


(define-graph (name Boston-map)
              (directed? #t))


(define-nodes (Harvard -> Cambridge Potter)
              (Cambridge -> Harvard)
              (Potter -> Harvard))


(add-nodes (Harvard Cambridge Potter)
           ->
           Boston-map)

;(find-all-paths ...)


(print-graph Boston-map)


(subprocess ...)

