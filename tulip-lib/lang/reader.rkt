#lang s-exp syntax/module-reader tulip/lang/runtime

#:whole-body-readers? #t

#:read        tulip:read
#:read-syntax tulip:read-syntax

(require "../parser.rkt"
         "../emitter.rkt")

(define (tulip:read-syntax module-name in)
  (let* ([ast (parse in)]
         [mod (emit-module ast)])
    (println mod)
    (list mod)))

(define (tulip:read in)
  (syntax->datum (tulip:read-syntax in)))
