#lang racket/base

(require racket/match
         racket/syntax
         syntax/parse
         syntax/strip-context)

(provide emit-module)

(define/with-syntax -@%lambda (strip-context #'@%lambda))
(define/with-syntax -@%tag (strip-context #'@%tag))
(define/with-syntax -@%chain (strip-context #'@%chain))

(define (emit-module stx)
  (syntax-parse stx
    [(expr:tulip-expr ...)
     #'(begin expr.emitted ...)]))

(define-syntax-class tulip-expr
  #:attributes [emitted]
  [pattern #s(identifier name:id)
           #:attr emitted #'name]
  [pattern #s(tag-word name:id)
           #:attr emitted #'(-@%tag name)]
  [pattern #s(flag-word name:id)
           #:attr emitted #'(@%flag name)]
  [pattern #s(flag-pair word:tulip-expr value:tulip-expr)
           #:attr emitted #'(@%flag-pair word.emitted value.emitted)]
  [pattern #s(number value)
           #:attr emitted #'value]
  [pattern #s(application fn:tulip-expr arg:tulip-expr)
           #:attr emitted (datum->syntax #f (list #'fn.emitted #'arg.emitted) #f #'fn.emitted)]
  [pattern #s(block [expr:tulip-expr ...])
           #:attr emitted #'(let () expr.emitted ...)]
  [pattern #s(chain left:tulip-expr right:tulip-expr)
           #:attr emitted #'(-@%chain left.emitted right.emitted)]
  [pattern #s(lambda-full [clause:tulip-lambda-clause ...])
           #:attr emitted #'(-@%lambda clause.emitted ...)]
  [pattern #s(definition id:tulip-expr expr:tulip-expr)
           #:attr emitted #'(define id.emitted expr.emitted)])

(define-syntax-class tulip-lambda-clause
  #:attributes [emitted]
  [pattern #s(lambda-clause (pat:tulip-pattern ...) expr:tulip-expr)
           #:attr emitted #'[(pat.emitted ...) expr.emitted]])

(define-syntax-class tulip-pattern
  #:attributes [emitted]
  [pattern #s(hole)
           #:attr emitted #'_]
  [pattern #s(tag-pattern #s(tag-word name:id) [value-pat:tulip-pattern ...])
           #:attr emitted #'(-@%tag name value-pat.emitted ...)]
  [pattern other-expr:tulip-expr
           #:attr emitted #'other-expr.emitted])
