#lang tulip/private/configured-runtime-lang

(require (for-syntax racket/base
                     syntax/parse)
         racket/format
         racket/function
         racket/match
         racket/string
         (prefix-in base: racket/base))

(provide #%app #%datum #%top #%top-interaction #%require #%provide
         @%define-multiple-binders @%lambda @%namespaced @%tag @%block @%chain
         (rename-out [@%module-begin #%module-begin]
                     [begin @%begin]
                     [define @%define]))

(define-syntax-rule (@%module-begin form ...)
  (base:#%module-begin
   (module configure-runtime racket/base
     (require tulip/lang/configure-runtime)
     (configure-runtime!))
   form ...))

(define-syntax @%define-multiple-binders
  (syntax-parser
    [(_ id:id [id*:id ...] expr:expr)
     (syntax-property #'(define id expr)
                      'disappeared-binding
                      (map syntax-local-introduce (attribute id*)))]))

(define-syntax @%lambda
  (syntax-parser
    [(_ [(formal ...) expr] ...+)
     ; Cheat a little with curry. It might be a better idea to turn [ a b => ... ] into
     ; (λ (x) (λ (y) (match* (x y) [(a b) ...]))) for purity and performance, but a naïve
     ; desugaring implemented by generating nested match-lambda** forms won’t work because
     ; pattern-matching needs to be able to backtrack to previous arguments. Nested match-lambda**
     ; forms will create undesirable committing once a single match succeeds.
     #'(curry (match-lambda** [(formal ...) expr] ...))]))

(define-syntax @%namespaced
  (syntax-parser
    [(_ namespace:id id:id)
     ; This soup is effectively equivalent to (syntax-local-lift-require #'namespace #'id), which is
     ; what this is doing. As suggested by the documentation, however, we need to apply
     ; syntax-local-introduce to everything to prevent macro-introduction scopes from getting in the
     ; way. Additionally, we need to add the 'original-for-check-syntax property to both syntax
     ; objects, since original-ness does not seem to be preserved through the process.
     (syntax-local-introduce
      (syntax-local-lift-require
       (syntax-local-introduce (syntax-property #'namespace 'original-for-check-syntax #t))
       (syntax-local-introduce (syntax-property #'id 'original-for-check-syntax #t))))]))

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

(define-syntax-rule (@%block expr ...)
  (let () expr ...))

(define-syntax-rule (@%chain a b)
  (b a))
