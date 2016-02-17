#lang curly-fn racket

(require (prefix-in r: racket/base)
         parsack)

(provide parse
         (struct-out identifier)
         (struct-out tag-word)
         (struct-out flag-word)
         (struct-out flag-pair)
         (struct-out number)
         (struct-out application)
         (struct-out block)
         (struct-out chain)
         (struct-out tag-pattern)
         (struct-out lambda-clause)
         (struct-out lambda-full)
         (struct-out definition)
         hole)

;; ---------------------------------------------------------------------------------------------------

(define-syntax-rule (lazy p)
  (λ (in) (p in)))

(define (maybe p)
  (<any> (try p) (return null)))

(define (chainl1 p op)
  (define (rest x)
    (<or> (try (parser-compose (f <- op)
                               (y <- p)
                               (rest (f x y))))
          (return x)))
  (p . >>= . rest))

;; ---------------------------------------------------------------------------------------------------

(struct identifier (name) #:transparent)
(struct tag-word (name) #:transparent)
(struct flag-word (name) #:transparent)
(struct flag-pair (word value) #:transparent)

(struct number (value) #:transparent)

(struct application (fn arg) #:transparent)
(struct block (expressions) #:transparent)
(struct chain (left right) #:transparent)

(define hole
  (let ()
    (struct hole ())
    (hole)))

(struct tag-pattern (tag value-patterns) #:transparent)

(struct lambda-clause (formals expression) #:transparent)
(struct lambda-full (clauses) #:transparent)

(struct definition (identifier expression) #:transparent)

;; ---------------------------------------------------------------------------------------------------

(define $$whitespace  $space)
(define $$whitespace* (many $$whitespace))

;; 1. Datums and Operators

(define $$identifier
  (parser-seq $letter (many (<or> $letter $digit (char #\-)))
              #:combine-with (λ (x xs) (identifier (apply r:string x xs)))))

(define $$tag-word
  (parser-seq (~ (char #\.)) $$identifier
              #:combine-with (compose1 tag-word identifier-name)))

(define $$flag-word
  (parser-seq (~ (char #\-)) $$identifier
              #:combine-with (compose1 flag-word identifier-name)))

(define $$number
  (<or> (try (parser-seq (many $digit) (~ (char #\.)) (many1 $digit)
                         #:combine-with
                         (λ (xs ys) (number (string->number (string-append (list->string xs)
                                                                           "."
                                                                           (list->string ys)))))))
        (parser-seq (many1 $digit) (~ (maybe (char #\.)))
                    #:combine-with (compose1 number string->number list->string))))

(define $$sequence-delimiter
  (<or> (char #\;) (char #\newline)))

;; 2. Expressions

(define $$group
  (parser-seq (~ (char #\()) (~ $$whitespace*) $$expression (~ $$whitespace*) (~ (char #\)))
              #:combine-with values))

(define $$flag-pair
  (parser-seq $$flag-word (~ (char #\:)) $$expression
              #:combine-with flag-pair))

(define $$expression-term
  (>> $$whitespace*
      (<?> (<or> (try $$group)
                 (try (lazy $$lambda))
                 (try (lazy $$block))
                 (try $$identifier)
                 (try $$tag-word)
                 (try $$flag-pair)
                 $$number)
           "expression term")))

(define $$application
  (chainl1 $$expression-term (>> (many1 $$whitespace)
                                 (return application))))

(define $$chain
  (chainl1 $$application (>> (>> $$whitespace* (char #\>))
                                 (return chain))))

(define $$block
  (parser-compose (char #\{)
                  (exprs <- (chainl1 $$chain (>> (>> $$whitespace* (char #\;))
                                                 (return list))))
                  $$whitespace*
                  (char #\})
                  (return (block (flatten exprs)))))

(define $$expression
  (>> $$whitespace*
      (<?> (<or> (try $$chain) $$expression-term)
           "expression")))

;; 2.1 Lambdas

(define $$pattern
  (>> $$whitespace*
      (<?> (<or> (try (lazy $$tag-pattern))
                 (try (lazy $$grouped-pattern))
                 $$identifier
                 $$number
                 (>> (char #\_) (return hole)))
           "pattern")))

(define $$grouped-pattern
  (parser-seq (~ (char #\()) $$pattern (~ $$whitespace*) (~ (char #\)))
              #:combine-with values))

(define $$tag-pattern
  (parser-seq $$tag-word (many (try $$pattern))
              #:combine-with tag-pattern))

(define $$lambda-formals
  (>> $$whitespace*
      (<or> (try (many1 (try $$pattern)))
            (>> (char #\!) (return '())))))

(define $$lambda-clause
  (parser-seq $$lambda-formals (~ $$whitespace*) (~ (string "=>")) $$expression
              #:combine-with lambda-clause))

(define $$full-lambda
  (>>= (chainl1 $$lambda-clause (>> (>> $$whitespace* (char #\;))
                                    (return list)))
       (compose1 return lambda-full flatten)))

(define $$lambda-body
  (<or> $$full-lambda))

(define $$lambda
  (parser-seq (~ $$whitespace*) (~ (char #\[)) $$lambda-body (~ $$whitespace*) (~ (char #\]))
              #:combine-with values))

;; 3. Definitions

(define $$binding-declaration $$identifier)

(define $$declaration
  (>> $$whitespace*
      (<or> $$binding-declaration)))

(define $$definition
  (parser-seq $$declaration (~ $$whitespace*) (~ (char #\=)) $$expression
              #:combine-with definition))

;; 4. Whole Programs
(define $$top-level-form
  (<or> (try $$definition)
        $$expression))

(define $$program
  (>>= (chainl1 $$top-level-form (>> (>> $$whitespace* (char #\;))
                                     (return list)))
       (compose1 return flatten)))

;; ---------------------------------------------------------------------------------------------------

(define (parse in)
  (parse-result $$program in))

#;(parse-result $$program
              "fib = [ 0 => 0; 1 => 1; n => add (fib (subtract n 1)) (fib (subtract n 2)) ];
               fib 6")
