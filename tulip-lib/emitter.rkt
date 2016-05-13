#lang racket/base

(require racket/match
         racket/syntax
         syntax/parse
         syntax/strip-context)

(provide emit-module)

(define/with-syntax -@%define-multiple-binders (strip-context #'@%define-multiple-binders))
(define/with-syntax -@%lambda (strip-context #'@%lambda))
(define/with-syntax -@%tag (strip-context #'@%tag))
(define/with-syntax -@%chain (strip-context #'@%chain))

(define (emit-module stx)
  (syntax-parse stx
    [(expr-or-def:tulip-expr-or-defn ...)
     #'(begin expr-or-def.emitted ...)]))

(define-splicing-syntax-class tulip-expr-or-defn
  #:attributes [emitted]
  ; Function definitions next to one another with the same name should be parsed as a single function
  ; definition with multiple pattern clauses. For example, this:
  ;   is-zero 0 = .t
  ;   is-zero _ = .f
  ; Should be parsed like this:
  ;   is-zero = [ 0 => .t; _ => .f ]
  [pattern (~seq #s(function-definition id:tulip-id pats expr)
                 #s(function-definition id*:tulip-id
                                        ; require each id* to be the same as id (otherwise, backtrack)
                                        (~fail #:unless (free-identifier=? #'id.emitted
                                                                           #'id*.emitted))
                                        pats* expr*)
                 ...)
           #:with [clause ...] #'[#s(lambda-clause pats expr)
                                  #s(lambda-clause pats* expr*)
                                  ...]
           #:with lambda:tulip-expr #'#s(lambda-full [clause ...])
           #:attr emitted #'(-@%define-multiple-binders id.emitted [id*.emitted ...] lambda.emitted)]
  [pattern #s(definition id:tulip-expr expr:tulip-expr)
           #:attr emitted #'(define id.emitted expr.emitted)]
  [pattern expr:tulip-expr
           #:attr emitted #'expr.emitted])

(define-syntax-class tulip-id
  #:attributes [emitted]
  [pattern #s(identifier name:id)
           #:attr emitted #'name])

(define-syntax-class tulip-expr
  #:attributes [emitted]
  [pattern id:tulip-id
           #:attr emitted #'id.emitted]
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
  [pattern #s(block [expr:tulip-expr-or-defn ...])
           #:attr emitted #'(let () expr.emitted ...)]
  [pattern #s(chain left:tulip-expr right:tulip-expr)
           #:attr emitted #'(-@%chain left.emitted right.emitted)]
  [pattern #s(lambda-full [clause:tulip-lambda-clause ...])
           #:attr emitted #'(-@%lambda clause.emitted ...)])

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
