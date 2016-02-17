#lang racket

(require racket/syntax
         syntax/strip-context
         "parser.rkt")

(provide emit-module)

(define/with-syntax -@%lambda (strip-context #'@%lambda))
(define/with-syntax -@%tag (strip-context #'@%tag))

(define (emit-module in)
  (with-syntax ([(expr ...) (map emit in)])
    #'(begin expr ...)))

(define/match (emit in)
  [((identifier name))
   (datum->syntax #f (string->symbol name))]
  [((tag-word name))
   (with-syntax ([nm (string->symbol name)])
     #'(-@%tag nm))]
  [((flag-word name))
   (with-syntax ([nm (string->symbol name)])
     #'(@%flag nm))]
  [((flag-pair word value))
   (with-syntax ([w (emit word)]
                 [v (emit value)])
     #'(@%flag-pair w v))]
  [((number value))
   (datum->syntax #f value)]
  [((application fn arg))
   (with-syntax ([f (emit fn)]
                 [a (emit arg)])
     #'(f a))]
  [((block expressions))
   (with-syntax ([(expr ...) (map emit expressions)])
     #'(begin expr ...))]
  [((chain left right))
   (with-syntax ([l (emit left)]
                 [r (emit right)])
     #'(%chain l r))]
  [((lambda-full clauses))
   (emit-lambda clauses)]
  [((definition id expression))
   (with-syntax ([nm (emit id)]
                 [expr (emit expression)])
     #'(define nm expr))])

(define (emit-lambda clauses)
  (with-syntax ([(cl ...) (map emit-lambda-clause clauses)])
    #'(-@%lambda cl ...)))

(define/match (emit-lambda-clause clause)
  [((lambda-clause patterns expression))
   (with-syntax ([(pat ...) (map emit-pattern patterns)]
                 [expr (emit expression)])
     #'[(pat ...) expr])])

(define/match (emit-pattern pattern)
  [((== hole))
   #'_]
  [((tag-pattern (tag-word name) value-patterns))
   (with-syntax ([nm (string->symbol name)]
                 [(pat ...) (map emit-pattern value-patterns)])
     #'(-@%tag nm pat ...))]
  [(x) (emit x)])
