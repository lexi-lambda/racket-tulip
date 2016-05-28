#lang racket/base

(require "../runtime.rkt")
(provide (all-from-out "../runtime.rkt")
         #%module-begin #%app #%datum #%top #%require
         (rename-out [begin @%begin]
                     [define @%define])
         add subtract)

(define ((add x) y)      (+ x y))
(define ((subtract x) y) (- x y))
