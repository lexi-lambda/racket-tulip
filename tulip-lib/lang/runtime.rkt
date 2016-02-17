#lang racket/base

(require "../runtime.rkt")
(provide (all-from-out "../runtime.rkt")
         #%module-begin #%datum
         add subtract)

(define ((add x) y)      (+ x y))
(define ((subtract x) y) (- x y))
