
#lang racket

;; Even though lang does not explicitly provide the macro "foo",
;; the output of mb is able to use it, and racket expander will expand
;; it once it sees the foo macro.
;; This means that, the run module cannot explicitly use "foo", yet
;; if it's expression gets transformed into a "foo" macro, racket is
;; able to expand it.
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





;; Path -> String
(define (path->string p)
  "")


