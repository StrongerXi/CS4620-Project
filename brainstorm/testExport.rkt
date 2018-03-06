#lang racket


#|
This program is intended to test the feasibility of using
macro to make a program "provide" identifier defined as a result
of other macro transformation, and whether a program that
requires this provider module would be able to use that specific
identifier.

In this case, the identifier chosen is named "database", and
we have not encountered any bugs so far.
|#

(module lang racket
  
  (require (for-syntax syntax/parse))

  (provide 
    ;(except-out (all-from-out racket) #%module-begin)
    (rename-out [mb #%module-begin]))
  
  (define-syntax mb
    (syntax-parser
      [(_ e ...)
       #'(#%module-begin 
          (provide database)
          (define database '(1 2 3)))]))
  )


;; Provider module contains no explicit provide;
;; It only uses lang as its language
(module provider (submod ".." lang)
  5)


;; By requiring provider, the user module is able to access
;; the variable database, defined by the "mb" macro in lang
(module user racket
  (require (submod ".." provider))

  database)

(require 'user)


