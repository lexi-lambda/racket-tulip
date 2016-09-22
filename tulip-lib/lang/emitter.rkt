#lang racket/base

(require racket/list
         racket/match
         racket/syntax
         syntax/parse
         syntax/strip-context
         tulip/private/util/srcloc)

(provide emit-module emit-interaction)

(define (emit-module stx)
  (syntax-parse stx
    #:context '|error while parsing module|
    [((~var expr-or-def (tulip-top-level-form #f)) ...)
     (strip-context #'(#%module-begin expr-or-def.emitted ...))]))

(define (emit-interaction stx)
  (syntax-parse stx
    #:context '|error while parsing interaction|
    [((~var expr-or-def (tulip-top-level-form #t)) ...)
     (strip-context #'(@%begin expr-or-def.emitted ...))]))

(define-splicing-syntax-class (tulip-top-level-form interaction?)
  #:attributes [emitted]
  #:description "top level form"
  [pattern #s(import module-name:tulip-require-spec)
           #:attr emitted #'(#%require module-name.emitted)]
  [pattern expr-or-defn:tulip-expr-or-defn
           #:attr emitted (if (and (not interaction?) (attribute expr-or-defn.defined-id))
                              #'(@%begin (#%provide expr-or-defn.defined-id)
                                         expr-or-defn.emitted)
                              #'expr-or-defn.emitted)])

(define-splicing-syntax-class tulip-expr-or-defn
  #:attributes [emitted defined-id]
  #:description #f
  ; Function definitions next to one another with the same name should be parsed as a single function
  ; definition with multiple pattern clauses. For example, this:
  ;   is-zero 0 = .t
  ;   is-zero _ = .f
  ; Should be parsed like this:
  ;   is-zero = [ 0 => .t; _ => .f ]
  [pattern (~seq {~and def #s(function-definition id:tulip-unnamespaced-id pats expr)}
                 {~and def* #s(function-definition
                               id*:tulip-unnamespaced-id
                               ; require each id* to be the same as id (otherwise, backtrack)
                               (~fail #:unless (free-identifier=? #'id.emitted
                                                                  #'id*.emitted))
                               pats* expr*)}
                 ...)
           #:do [(define this-srcloc
                   (let ([matched-syntax #'(def def* ...)])
                     (join-srclocs (first (syntax->list matched-syntax))
                                   (last (syntax->list matched-syntax)))))]
           #:with [clause ...] #'[#s(lambda-clause pats expr)
                                  #s(lambda-clause pats* expr*)
                                  ...]
           #:with lambda:tulip-expr (datum->syntax #f (syntax-e #'#s(lambda-full [clause ...]))
                                                   this-srcloc)
           #:attr emitted #'(@%define-multiple-binders id.emitted [id*.emitted ...] lambda.emitted)
           #:attr defined-id #'id.emitted]
  [pattern #s(definition id:tulip-unnamespaced-id expr:tulip-expr)
           #:attr emitted #'(@%define id.emitted expr.emitted)
           #:attr defined-id #'id.emitted]
  [pattern expr:tulip-expr
           #:attr emitted #'expr.emitted
           #:attr defined-id #f])

(define-syntax-class tulip-id
  #:attributes [namespace name]
  [pattern #s(identifier (~or namespace-stx:id (~and #f namespace-stx)) name:id)
           #:attr namespace (and (syntax->datum #'namespace-stx) #'namespace-stx)])

(define-syntax-class tulip-unnamespaced-id
  #:attributes [emitted]
  #:description "identifier"
  [pattern id:tulip-id
           #:fail-when (attribute id.namespace)
           "expected an unnamespaced identifier, but a namespace was provided"
           #:attr emitted #'id.name])

(define-syntax-class tulip-expr
  #:attributes [emitted]
  [pattern id:tulip-id
           #:attr emitted (if (attribute id.namespace)
                              (syntax/loc this-syntax
                                (@%namespaced id.namespace id.name))
                              #'id.name)]
  [pattern #s(chain-slot)
           #:attr emitted (syntax/loc this-syntax @%chain-slot)]
  [pattern #s(autovar)
           #:attr emitted (syntax/loc this-syntax @%auto-var)]
  [pattern #s(tag-word name:id)
           #:attr emitted (syntax/loc this-syntax
                            (@%tag name))]
  [pattern #s(flag-word name:id)
           #:attr emitted (syntax/loc this-syntax
                            (@%flag name))]
  [pattern #s(flag-pair word:tulip-expr value:tulip-expr)
           #:attr emitted (syntax/loc this-syntax
                            (@%flag-pair word.emitted value.emitted))]
  [pattern #s(number value)
           #:attr emitted #'value]
  [pattern #s(string value)
           #:attr emitted #'value]
  [pattern #s(application fn:tulip-expr arg:tulip-expr)
           #:attr emitted (datum->syntax #f (list #'fn.emitted #'arg.emitted)
                                         this-syntax #'fn.emitted)]
  [pattern #s(application! fn:tulip-expr)
           #:attr emitted (datum->syntax #f (list #'fn.emitted) this-syntax #'fn.emitted)]
  [pattern #s(block [expr:tulip-expr-or-defn ...])
           #:attr emitted (syntax/loc this-syntax
                            (@%block expr.emitted ...))]
  [pattern #s(chain left:tulip-expr right:tulip-expr)
           #:attr emitted (syntax/loc this-syntax
                            (@%chain left.emitted right.emitted))]
  [pattern #s(lambda-full [clause:tulip-lambda-clause ...])
           #:attr emitted (syntax/loc this-syntax
                            (@%lambda clause.emitted ...))]
  [pattern #s(lambda-full clause:tulip-expr)
           #:attr emitted (syntax/loc this-syntax
                            (@%auto-lambda clause.emitted))])

(define-syntax-class tulip-require-spec
  #:attributes [emitted]
  [pattern #s(string value)
           #:attr emitted #'value]
  [pattern id:tulip-id
           #:attr emitted (if (attribute id.namespace)
                              (format-id #f "~a/~a" #'id.namespace #'id.name
                                         #:source (join-srclocs #'id.namespace #'id.name)
                                         #:props #'id.name)
                              #'id.name)])

(define-syntax-class tulip-lambda-clause
  #:attributes [emitted]
  [pattern #s(lambda-clause (pat:tulip-pattern ...) expr:tulip-expr)
           #:attr emitted #'[(pat.emitted ...) expr.emitted]])

(define-syntax-class tulip-pattern
  #:attributes [emitted]
  [pattern #s(hole)
           #:attr emitted #'_]
  [pattern #s(tag-pattern #s(tag-word name:id) [value-pat:tulip-pattern ...])
           #:attr emitted #'(@%tag name value-pat.emitted ...)]
  [pattern other-expr:tulip-expr
           #:attr emitted #'other-expr.emitted])
