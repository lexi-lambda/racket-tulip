#lang tulip

@import racket/base
@import racket/function

@import "assert.rkt"

kons = curry cons

kons 1 null > assert-equal (list 1)
