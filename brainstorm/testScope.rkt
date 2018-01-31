#lang racket 

(module foo-mod racket
  (provide foo)
  (require (for-syntax syntax/parse))

  (define (play x)
    (* 10 x))
  
  (define-syntax (foo stx)
    (syntax-parse stx
      ((_ exp) #'(play exp)))))


(module test racket
  (require (submod ".." foo-mod))

  (displayln (foo 5))

  (define (play x)
    (add1 x))
  (play 2)
  (displayln   (foo 5)))


