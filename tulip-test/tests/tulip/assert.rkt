#lang racket/base

(require rackunit)

(provide (rename-out [check-true assert-true])
         assert-equal)

(define ((assert-equal a) b)
  (check-equal? a b))
