#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide syntax/loc/props quasisyntax/loc/props)

;; ---------------------------------------------------------------------------------------------------
;; [quasi]syntax/loc/props

; The syntax/loc and quasisyntax/loc forms from racket/syntax are useful, but they don’t copy over
; syntax properties, even when they probably should. This module provides two forms that act just
; like syntax/loc and quasisyntax/loc, except that they also copy properties.

(define-for-syntax (*/loc/props *)
  (syntax-parser
    [(_ src-expr template)
     #`(let ([src src-expr])
         (datum->syntax (quote-syntax #,this-syntax)
                        (syntax-e (#,* template))
                        src src))]))

(define-syntaxes [syntax/loc/props quasisyntax/loc/props]
  (values (*/loc/props #'syntax)
          (*/loc/props #'quasisyntax)))
