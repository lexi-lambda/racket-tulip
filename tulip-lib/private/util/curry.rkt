#lang racket/base

; This module implements static currying, which is useful for providing Racket functions to Tulip
; code. It is simpler than tulip/racket/ffi because it does not need to cooperate with Tulip’s
; always-curried function application syntax.

(require (for-syntax racket/base
                     racket/provide-transform
                     syntax/parse))

(provide curry-n curry-out)

(define-syntax-rule (curry-n n proc)
  (do-curry-n n proc))

(define-syntax do-curry-n
  (syntax-parser
    [(_ 0 proc arg ...) #'(proc arg ...)]
    [(_ n:nat proc arg ...) #`(λ (x) (do-curry-n #,(sub1 (syntax-e #'n)) proc arg ... x))]))

(begin-for-syntax
  (define-syntax-class curry-out-clause
    #:attributes [provide-spec]
    [pattern [name:id arity:nat]
             #:with desugared:curry-out-clause #'[(name name) arity]
             #:attr provide-spec #'desugared.provide-spec]
    [pattern [(internal-name:id external-name:id) arity:nat]
             #:with curried (syntax-local-lift-expression #'(curry-n arity internal-name))
             #:attr provide-spec #'(rename-out [curried external-name])]))

(define-syntax curry-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ clause:curry-out-clause ...)
        (pre-expand-export #'(combine-out clause.provide-spec ...) modes)]))))
