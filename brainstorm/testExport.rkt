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
    submod
    (rename-out [mb #%module-begin]))
  
  (define-syntax mb
    (syntax-parser
      [(_ e)
       #'(#%module-begin 
          (provide database)
          (define database '(1 2 3)))]

      [(_ file extra ...)
      #'(#%module-begin
         (require (submod ".." provider))
         database)]))
  )


;; Provider module contains no explicit provide;
;; It only uses lang as its language
(module provider (submod ".." lang)
  5)


(module macro racket
  (provide (rename-out [mb #%module-begin]))
  (require (for-syntax syntax/parse))

  (define-syntax mb
    (syntax-parser 
      [(_ loc e ...) 
       ;#:with db (datum->syntax #'(e ...) 'database)
       #'(#%module-begin 
          (require (submod ".." provider))
          database)])))

;; By requiring provider, the user module is able to access
;; the variable database, defined by the "mb" macro in lang
(module user (submod ".." lang)
;  (require (submod ".." provider))
  provider
  ahaha)

(require 'user)


