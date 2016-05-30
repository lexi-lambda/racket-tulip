#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide curry-n)

(define-syntax-rule (curry-n n)
  (λ (proc) (do-curry-n n proc ())))

(define-syntax do-curry-n
  (syntax-parser
    [(_ 0 proc (arg ...)) #'(proc arg ...)]
    [(_ n:nat proc (arg ...)) #`(λ (x) (do-curry-n #,(sub1 (syntax-e #'n)) proc (arg ... x)))]))
