#lang tulip

@import tulip/list
@import tulip/math

@import "assert.rkt"

list-123 = .cons 1 (.cons 2 (.cons 3 .nil))

list-123 > map incr > assert-equal (.cons 2 (.cons 3 (.cons 4 .nil)))
list-123 > foldl add 0 > assert-equal 6
list-123 > reverse > assert-equal (.cons 3 (.cons 2 (.cons 1 .nil)))
