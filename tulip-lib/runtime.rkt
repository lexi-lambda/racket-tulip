#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/format
         racket/function
         racket/match
         racket/string)

(provide @%lambda @%tag @%chain)

(define-syntax @%lambda
  (syntax-parser
    [(_ [(formal ...) expr] ...+)
     ; Cheat a little with curry. It might be a better idea to turn [ a b => ... ] into
     ; (λ (x) (λ (y) (match* (x y) [(a b) ...]))) for purity and performance, but a naïve
     ; desugaring implemented by generating nested match-lambda** forms won’t work because
     ; pattern-matching needs to be able to backtrack to previous arguments. Nested match-lambda**
     ; forms will create undesirable committing once a single match succeeds.
     #'(curry (match-lambda** [(formal ...) expr] ...))]))

(struct tag (name fields)
  #:transparent
  #:property prop:procedure
  (λ (t new-field)
    (tag (tag-name t) (append (tag-fields t) (list new-field))))
  #:methods gen:custom-write
  [(define (write-proc t out mode)
     (if (null? (tag-fields t))
         (fprintf out ".~a" (tag-name t))
         (fprintf out "(.~a ~a)" (tag-name t) (string-join (map ~a (tag-fields t))))))])

(define-match-expander @%tag
  (syntax-parser
    [(_ name field ...)
     #'(tag 'name (list field ...))])
  (syntax-parser
    [(_ name)
     #'(tag 'name '())]))

(define-syntax-rule (@%chain a b)
  (b a))
