#lang tulip

@import tulip/math
@import "assert.rkt"

5 > sub - 3 > assert-equal 2
add > - 1 2 > assert-equal 3

5 > sub - (3 > add - 1) > assert-equal 1
