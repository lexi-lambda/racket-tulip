#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/list
         racket/match
         racket/port
         racket/string)

(provide tulip tulip* lex lex-for-colorizer)

(define-tokens tulip
  [IDENTIFIER KEYWORD TAG-WORD FLAG-WORD NUMBER STRING])
(define-empty-tokens tulip*
  [EOF AUTOVAR EMPTY-ARGS OP-SEQUENCE
   OP-CHAIN OP-DEFINE OP-CLAUSE OP-HOLE
   GROUP-OPEN GROUP-CLOSE
   LAMBDA-OPEN LAMBDA-CLOSE
   BLOCK-OPEN BLOCK-CLOSE
   WHITESPACE COMMENT])

(define-lex-abbrevs
  [space (:& (:~ #\newline) (:or whitespace blank iso-control))]
  
  [identifier (:: letter (:* (:or letter digit #\-)))]
  [namespaced-identifier (:: (:* (:: identifier #\/)) identifier)]
  [keyword (:: #\@ identifier)]
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

   [namespaced-identifier (token-IDENTIFIER (map string->symbol (string-split lexeme "/")))]
   [keyword (token-KEYWORD (string->symbol (substring lexeme 1)))]
   [tag-word (token-TAG-WORD (string->symbol (substring lexeme 1)))]
   [flag-word (token-FLAG-WORD (string->symbol (substring lexeme 1)))]

   [number (token-NUMBER (real->double-flonum (string->number lexeme)))]

   [single-quote-string (token-STRING (substring lexeme 1))]
   [double-quote-string (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))]

   [sequence-delimiter (token-OP-SEQUENCE)]

   [comment (token-COMMENT)]
   [(:+ space) (token-WHITESPACE)]
   [(eof) (token-EOF)]))

(define (lex in)
  (port-count-lines! in)
  (reverse
   (let loop ([acc '()])
     (let ([v (tulip-lexer in)])
       (cond
         ; do some post-processing for special tokens that lex can’t quite handle on its own
         [(or ; ignore whitespace tokens
              (eq? 'WHITESPACE (position-token-token v))
              ; ignore comment tokens
              (eq? 'COMMENT (position-token-token v))
              ; ignore consecutive OP-SEQUENCE tokens
              (and (not (empty? acc))
                   (eq? 'OP-SEQUENCE (position-token-token (first acc)))
                   (eq? 'OP-SEQUENCE (position-token-token v))))
          (loop acc)]
         ; once we hit the EOF token, stop lexing
         [(eq? 'EOF (token-name (position-token-token v)))
          (cons v acc)]
         ; otherwise, use the token and keep lexing
         [else
          (loop (cons v acc))])))))

(define (lex-for-colorizer in)
  (with-handlers (; if the lexer fails, just mark that character as an error and hobble along
                  [exn:fail:read?
                   (λ (exn)
                     (values (read-string 1 in) 'error #f
                             (file-position in) (add1 (file-position in))))])
    (match-let* ([peek-in (peeking-input-port in #:init-position (add1 (file-position in)))]
                 [(position-token tok (position start _ _) (position end _ _)) (tulip-lexer peek-in)]
                 [read-str (read-string (- end start) in)])
      (define-values (token-type paren-type)
        (match tok
          [(app token-name 'NUMBER)
           (values 'constant #f)]
          [(app token-name 'STRING)
           (values 'string #f)]
          [(app token-name 'IDENTIFIER)
           (values 'symbol #f)]
          [(app token-name 'KEYWORD)
           (values 'hash-colon-keyword #f)]
          [(app token-name (or 'TAG-WORD 'FLAG-WORD))
           (values 'keyword #f)]
          
          ['GROUP-OPEN
           (values 'parenthesis '|(|)]
          ['GROUP-CLOSE
           (values 'parenthesis '|)|)]
          ['LAMBDA-OPEN
           (values 'parenthesis '|[|)]
          ['LAMBDA-CLOSE
           (values 'parenthesis '|]|)]
          ['BLOCK-OPEN
           (values 'parenthesis '|{|)]
          ['BLOCK-CLOSE
           (values 'parenthesis '|}|)]
          
          [(or 'EMPTY-ARGS 'OP-SEQUENCE 'OP-DEFINE 'OP-CHAIN 'OP-CLAUSE 'OP-HOLE)
           (values 'other #f)]
          
          ['COMMENT
           (values 'comment #f)]
          ['WHITESPACE
           (values 'white-space #f)]
          ['EOF
           (values 'eof #f)]))
      (values read-str token-type paren-type start end))))
