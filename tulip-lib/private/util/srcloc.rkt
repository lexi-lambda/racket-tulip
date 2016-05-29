#lang racket/base

(provide split-srcloc
         join-srclocs)

;; ---------------------------------------------------------------------------------------------------
;; srcloc management

; The split-srcloc and join-srclocs functions are used to break apart and join source location
; information from identifiers. This is useful in Tulip because we have things like namespaced
; identifiers, of the form `racket/base/cons`, which should be broken up into two separate pieces
; of source location information along these lines:
;
;             racket/base/cons
;             ^^^^^^^^^^^ ^^^^
;                  |        |
;            namespace    identifier
;
; Additionally, the forward slash between each segment needs to be omitted from both srclocs, so
; the `skip` argument of split-srcloc is used for that purpose.

(define (split-srcloc stx index [skip 0])
  (values (list (syntax-source stx) (syntax-line stx) (syntax-column stx)
                (syntax-position stx) index)
          (list (syntax-source stx) (syntax-line stx) (syntax-column stx)
                (+ index skip (syntax-position stx)) (- (syntax-span stx) index skip))))

(define (join-srclocs a b)
  (list (syntax-source a) (syntax-line a) (syntax-column a)
        (syntax-position a) (+ (syntax-span a) (syntax-span b)
                               (- (syntax-position b) (+ (syntax-position a) (syntax-span a))))))
