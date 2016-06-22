#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [@%module-begin #%module-begin]))

(module reader syntax/module-reader
  tulip/private/configured-runtime-lang)

(define-syntax-rule (@%module-begin form ...)
  (#%module-begin
   (module configure-runtime racket/base
     (require tulip/lang/configure-runtime)
     (configure-runtime!))
   form ...))
