#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/list)

(provide tulip tulip* lex)

(define-tokens tulip
  [IDENTIFIER TAG-WORD FLAG-WORD NUMBER STRING])
(define-empty-tokens tulip*
  [EOF AUTOVAR EMPTY-ARGS OP-SEQUENCE
   OP-CHAIN OP-DEFINE OP-CLAUSE OP-HOLE
   GROUP-OPEN GROUP-CLOSE
   LAMBDA-OPEN LAMBDA-CLOSE
   BLOCK-OPEN BLOCK-CLOSE])

(define-lex-abbrevs
  [space (:& (:~ #\newline) (:or whitespace blank iso-control))]
  
  [identifier (:: letter (:* (:or letter digit #\-)))]
  [tag-word (:: #\. identifier)]
  [flag-word (:: #\- identifier)]
  [number (:or (:: (:* digit) #\. (:+ digit))
               (:: (:+ digit) (:? #\.)))]
  [single-quote-string (:: #\' identifier)]
  [double-quote-string (:: #\" (:* (:~ #\")) #\")]
  [letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [digit (:/ #\0 #\9)]
  [sequence-delimiter (:or #\; #\newline)]

  [comment (:: #\# (:* (:~ #\newline)))])

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
   [#\_ (token-OP-HOLE)]
   
   [tag-word (token-TAG-WORD (string->symbol (substring lexeme 1)))]
   [flag-word (token-FLAG-WORD (string->symbol (substring lexeme 1)))]
   [identifier (token-IDENTIFIER (string->symbol lexeme))]

   [number (token-NUMBER (real->double-flonum (string->number lexeme)))]

   [single-quote-string (token-STRING (substring lexeme 1))]
   [double-quote-string (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))]

   [sequence-delimiter (token-OP-SEQUENCE)]

   [comment (void)]
   [(:+ space) (void)]
   [(eof) (token-EOF)]))

(define (lex in)
  (port-count-lines! in)
  (reverse
   (let loop ([acc '()])
     (let ([v (tulip-lexer in)])
       (cond
         ; do some post-processing for special tokens that lex can’t quite handle on its own
         [(or ; ignore #<void> tokens
              (void? (position-token-token v))
              ; ignore consecutive OP-SEQUENCE tokens
              (and (not (empty? acc))
                   (eq? 'OP-SEQUENCE (token-name (position-token-token (first acc))))
                   (eq? 'OP-SEQUENCE (token-name (position-token-token v)))))
          (loop acc)]
         ; once we hit the EOF token, stop lexing
         [(eq? 'EOF (token-name (position-token-token v)))
          (cons v acc)]
         ; otherwise, use the token and keep lexing
         [else
          (loop (cons v acc))])))))

