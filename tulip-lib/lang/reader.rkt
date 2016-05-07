#lang s-exp syntax/module-reader tulip/lang/runtime

#:whole-body-readers? #t

#:read        tulip:read
#:read-syntax tulip:read-syntax

(require megaparsack
         "../parser.rkt"
         "../emitter.rkt")

(define (tulip:read-syntax module-name in)
  (let* ([ast (parse-result! (parse-tulip in module-name))]
         [mod (emit-module ast)])
    (list mod)))

(define (tulip:read in)
  (syntax->datum (tulip:read-syntax in)))
