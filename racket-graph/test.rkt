
#lang racket

(module lang racket

  (require (for-syntax syntax/parse))
  (provide 
    (except-out (all-from-out racket) #%module-begin)
    (rename-out [mb #%module-begin]))

  (define b 5)

  (define-syntax mb
    (syntax-parser
      [(_ e ...)
       #'(#%module-begin (foo e) ...)]))

  (define-syntax foo
    (syntax-parser
      [(_ e ...)
       #'b]))
  )


(module run (submod ".." lang)

  (define a 5)
  a
  )

(require 'run)
