#lang racket 


(require (for-syntax syntax/parse))

(define-syntax foo
  (syntax-parser
    [(_ e ...) 
     #'(begin
         (define a 5)
         a)]))


(define-syntax bar
  (syntax-parser
    [(_ e)
     #:with name (datum->syntax #'e 'a)
     #'name]))

(foo 5)
(foo 5)

