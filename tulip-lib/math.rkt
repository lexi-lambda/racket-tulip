#lang racket/base

(require racket/math
         tulip/base
         tulip/private/util/curry)

(provide
 abs
 round
 floor
 sqrt
 exp
 log
 random

 pi
 sqr
 sgn

 sin asin sinh
 cos acos cosh
 tan atan tanh

 eq gt lt gte lte

 (rename-out
  [add1 incr]
  [sub1 decr]
  [ceiling ceil])
 
 (curry-out
  [(+ add) 2]
  [(- sub) 2]
  [(* mul) 2]
  [(/ div) 2]

  [(modulo mod) 2]

  [max 2]
  [min 2]

  [expt 2]

  [(atan atan2) 2]))

(define ((eq  a) b) (if (=  a b) (@%tag t) (@%tag f)))
(define ((gt  a) b) (if (>  a b) (@%tag t) (@%tag f)))
(define ((lt  a) b) (if (<  a b) (@%tag t) (@%tag f)))
(define ((gte a) b) (if (>= a b) (@%tag t) (@%tag f)))
(define ((lte a) b) (if (<= a b) (@%tag t) (@%tag f)))
