#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide tulip tulip* lex)

(define-tokens tulip
  [IDENTIFIER TAG-WORD FLAG-WORD NUMBER])
(define-empty-tokens tulip*
  [EOF AUTOVAR EMPTY-ARGS OP-SEQUENCE
   OP-CHAIN OP-DEFINE OP-CLAUSE
   GROUP-OPEN GROUP-CLOSE
   LAMBDA-OPEN LAMBDA-CLOSE
   BLOCK-OPEN BLOCK-CLOSE])

(define-lex-abbrevs
  [space (:or whitespace blank iso-control)]
  
  ; 1. Datums and Operators
  [identifier (:: letter (:* (:or letter digit)))]
  [tag-word (:: #\. identifier)]
  [flag-word (:: #\- identifier)]
  [number (:or (:: (:* digit) #\. (:+ digit))
               (:: (:+ digit) (:? #\.)))]
  [letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [digit (:/ #\0 #\9)]
  [sequence-delimiter #\;]
  
  ;[space (:& (:~ #\newline) (:or whitespace blank iso-control))]
  ;[comment (:: #\# (:* (:~ #\newline)))]
  ;[operator (:or #\$ #\> #\= "=>")]
  ;[sequence (:or #\; #\newline)]
  ;[delimiter (:or (char-set "()[]") space sequence #\!)]
  #;[identifier (:& (complement operator)
                  (:+ (:~ delimiter)))]
  #;[number (:or (:+ (:/ #\0 #\9))
               (:: (:* (:/ #\0 #\9)) #\. (:+ (:/ #\0 #\9))))]
  #;[tag (:: #\. identifier)])

(define tulip-lexer
  (lexer-src-pos
   [#\( (token-GROUP-OPEN)]
   [#\) (token-GROUP-CLOSE)]
   [#\[ (token-LAMBDA-OPEN)]
   [#\] (token-LAMBDA-CLOSE)]
   [#\{ (token-BLOCK-OPEN)]
   [#\} (token-BLOCK-CLOSE)]
   [#\$ (token-AUTOVAR)]
   [#\! (token-EMPTY-ARGS)]
   [#\> (token-OP-CHAIN)]
   [#\= (token-OP-DEFINE)]
   ["=>" (token-OP-CLAUSE)]
   
   [tag-word (token-TAG-WORD (string->symbol (substring lexeme 1)))]
   [flag-word (token-FLAG-WORD (string->symbol (substring lexeme 1)))]
   [identifier (token-IDENTIFIER (string->symbol lexeme))]

   [number (token-NUMBER (real->double-flonum (string->number lexeme)))]
   [sequence-delimiter (token-OP-SEQUENCE)]

   [(:+ space) (void)]
   [(eof) (token-EOF)]))

(define (lex in)
  (port-count-lines! in)
  (for*/list ([_ (in-naturals)]
              [v (in-value (tulip-lexer in))]
              #:unless (void? (position-token-token v))
              #:final (eq? 'EOF (token-name (position-token-token v))))
    v))

