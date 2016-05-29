#lang tulip

@import "assert.rkt"

decr x = subtract x 1

test v = {
  is-even = [ 0 => .t; x => decr x > is-odd ]
  is-odd  = [ 0 => .f; x => decr x > is-even ]
  v > [ .even x => is-even x; .odd x => is-odd x ]
}

test (.even 3) > assert-equal .f
test (.odd 3)  > assert-equal .t
