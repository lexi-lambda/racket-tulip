#lang tulip

@import "assert.rkt"

kons = racket/function/curry racket/base/cons

kons 1 racket/base/null > assert-equal (racket/base/list 1)
