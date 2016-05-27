#lang tulip

decr x = subtract x 1

test v = {
  is-even = [ 0 => .t; x => decr x > is-odd ]
  is-odd  = [ 0 => .f; x => decr x > is-even ]
  v > [ .even x => is-even x; .odd x => is-odd x ]
}

test (.even 3) # => .f
test (.odd 3)  # => .t
