#lang s-exp syntax/module-reader tulip

#:whole-body-readers? #t

#:read        tulip:read
#:read-syntax tulip:read-syntax
#:info        get-info

(require megaparsack
         "lexer.rkt"
         "parser.rkt"
         "emitter.rkt")

(define (tulip:read-syntax module-name in)
  (let* ([ast (parse-result! (parse-tulip in module-name))]
         [mod (emit-module ast)])
    (list mod)))

(define (tulip:read in)
  (syntax->datum (tulip:read-syntax in)))

(define (get-info key default lookup-default)
  (case key
    [(color-lexer) lex-for-colorizer]
    [else (lookup-default key default)]))
