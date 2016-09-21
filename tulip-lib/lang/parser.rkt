#lang curly-fn racket

(require data/applicative
         data/monad
         data/maybe
         (prefix-in monad: data/monad)
         tulip/private/util/srcloc
         megaparsack
         megaparsack/parser-tools/lex
         (prefix-in lex: parser-tools/lex)
         "lexer.rkt")

(provide parse-tulip)

(define (chain-left+/p p op)
  (define (rest x)
    (or/p (try/p (do [f <- op]
                     [y <- p]
                     (rest (f x y))))
          (pure x)))
  (monad:chain rest p))

;; ---------------------------------------------------------------------------------------------------

; The many/trailing-sep/end/p and some/trailing-sep/end/p combinators are like many/sep*/p and
; many/sep+/p, but they handle trailing separators, like in the following expression:
;
;   [ 0 => 1; 1 => 2; ]
;
; Due to how the parser model works, that last semicolon needs to be handled with care to preserve
; good error messages without completely breaking the parser. These wordy combinators handle that by
; providing information about how the expression terminates, so they can explicitly check for the
; trailing separator.

(define (many/trailing-sep/end/p p sep end)
  (or/p (some/trailing-sep/end/p p sep end)
        (do end (pure '()))))

(define (some/trailing-sep/end/p p sep end)
  (do [x <- p]
      [xs <- (or/p (try/p (do sep end (pure '())))
                   (do sep (some/trailing-sep/end/p p sep end))
                   (do end (pure '())))]
      (pure (cons x xs))))

;; ---------------------------------------------------------------------------------------------------

(struct import (module-name) #:prefab)

(struct identifier (namespace name) #:prefab)
(struct tag-word (name) #:prefab)
(struct flag-word (name) #:prefab)
(struct flag-pair (word value) #:prefab)

(struct number (value) #:prefab)
(struct string (value) #:prefab)

(struct application (fn arg) #:prefab)
(struct application! (fn) #:prefab)
(struct block (body) #:prefab)
(struct chain (left right) #:prefab)

(define chain-slot #s(chain-slot))
(define hole #s(hole))

(struct tag-pattern (tag value-patterns) #:prefab)

(struct lambda-clause (formals expression) #:prefab)
(struct lambda-full (clauses) #:prefab)

(struct definition (identifier expression) #:prefab)
(struct function-definition (identifier formals expression) #:prefab)

;; ---------------------------------------------------------------------------------------------------

;; 1. Datums and Operators

(define (wrap-token/p label constructor token)
  (label/p label ((pure constructor) (syntax/p (token/p token)))))

(define tag-word/p  (wrap-token/p "tag-word"   tag-word   'TAG-WORD))
(define flag-word/p (wrap-token/p "flag-word"  flag-word  'FLAG-WORD))
(define number/p    (wrap-token/p "number"     number     'NUMBER))
(define string/p    (wrap-token/p "string"     string     'STRING))

(define chain-slot/p
  (label/p "hole" (syntax/p (do (token/p 'OP-CHAIN-SLOT)
                                (pure chain-slot)))))

(define identifier/p
  (label/p
   "identifier"
   (do [stx <- (syntax/p (token/p 'IDENTIFIER))]
       (match (syntax->datum stx)
         [(list name)
          (pure (identifier #f (datum->syntax #f name stx stx)))]
         [(list namespaces ... name)
          (let*-values ([(namespace) (string-join (map symbol->string namespaces) "/")]
                        [(namespace-srcloc name-srcloc) (split-srcloc stx (string-length namespace) 1)])
            (pure (identifier (and (not (empty? namespaces))
                                   (datum->syntax #f (string->symbol namespace) namespace-srcloc stx))
                              (datum->syntax #f name name-srcloc stx))))]))))

(define sequence-delimiter/p (token/p 'OP-SEQUENCE))
(define sequence-delimiter?/p (or/p (hidden/p sequence-delimiter/p) void/p))

(define (keyword/p keyword-name)
  (label/p (format "@~a" keyword-name)
           (satisfy/p (λ (tok) (and (lex:token? tok)
                                    (eq? (lex:token-name tok) 'KEYWORD)
                                    (equal? (lex:token-value tok) keyword-name))))))

;; 2. Expressions

(define group/p
  (do (token/p 'GROUP-OPEN)
      [expr <- expression/p]
      (token/p 'GROUP-CLOSE)
      (pure expr)))

(define expression-term/p
  (or/p group/p
        (lazy/p lambda/p)
        (lazy/p block/p)
        tag-word/p
        number/p
        string/p
        identifier/p
        chain-slot/p))

(define application/p
  (syntax/p
   (or/p (try/p (do [expr <- expression-term/p]
                  (token/p 'EMPTY-ARGS)
                  (pure (application! expr))))
         (chain-left+/p expression-term/p (pure application)))))

(define chain/p
  (chain-left+/p application/p (do (token/p 'OP-CHAIN) (pure chain))))

(define block/p
  (do (token/p 'BLOCK-OPEN)
      sequence-delimiter?/p
      [exprs <- (some/trailing-sep/end/p (or/p definition/p chain/p)
                                         sequence-delimiter/p
                                         (token/p 'BLOCK-CLOSE))]
      (pure (block exprs))))

(define expression/p
  (label/p
   "expression"
   (or/p (try/p chain/p)
         expression-term/p)))

;; 2.1 Lambdas

(define hole/p
  (label/p "hole"
           (syntax/p (do (token/p 'OP-HOLE)
                         (pure hole)))))

(define pattern/p
  (label/p
   "pattern"
   (or/p (lazy/p tag-pattern/p)
         (lazy/p grouped-pattern/p)
         hole/p
         identifier/p
         number/p)))

(define grouped-pattern/p
  (do (token/p 'GROUP-OPEN)
      [pat <- pattern/p]
      (token/p 'GROUP-CLOSE)
      (pure pat)))

(define tag-pattern/p
  ((pure tag-pattern) tag-word/p (many*/p pattern/p)))

(define lambda-formals/p
  (or/p (do (token/p 'EMPTY-ARGS) (pure '()))
        (many+/p pattern/p)))

(define lambda-clause/p
  (do [formals <- lambda-formals/p]
      (token/p 'OP-CLAUSE)

      ; the => token eats semicolons/newlines
      sequence-delimiter?/p

      [expr <- expression/p]
      (pure (lambda-clause formals expr))))

(define lambda/p
  (do (token/p 'LAMBDA-OPEN)
      sequence-delimiter?/p
      [clauses <- (some/trailing-sep/end/p lambda-clause/p
                                           sequence-delimiter/p
                                           (token/p 'LAMBDA-CLOSE))]
      (pure (lambda-full clauses))))

;; 3. Definitions

(define declaration/p
  (do [id <- identifier/p]
      [maybe-formals
       <- (or/p (do (token/p 'OP-DEFINE)
                    (pure nothing))
                (do [formals <- lambda-formals/p]
                    (token/p 'OP-DEFINE)
                    (pure (just formals))))]
      (pure (list id maybe-formals))))

(define definition/p
  (label/p
   "definition"
   (syntax/p (do [decl <- (try/p declaration/p)]
               ; the = token eats semicolons/newlines
               sequence-delimiter?/p
               [expr <- expression/p]
               (match decl
                 [(list id (just formals)) (pure (function-definition id formals expr))]
                 [(list id (nothing))      (pure (definition id expr))])))))

;; 4. Whole Programs

(define directive/p
  (do (keyword/p 'import)
      [module-name <- (or/p string/p identifier/p)]
      (pure (import module-name))))

(define top-level-form/p
  (or/p directive/p
        definition/p
        expression/p))

(define eof/p (label/p "end of file" (token/p 'EOF)))

(define program/p
  (syntax/p
   (do sequence-delimiter?/p
       (many/trailing-sep/end/p top-level-form/p
                                sequence-delimiter/p
                                eof/p))))

;; ---------------------------------------------------------------------------------------------------

(define (parse-tulip in [source-name (object-name in)])
  (parse-tokens program/p (lex in) source-name))
