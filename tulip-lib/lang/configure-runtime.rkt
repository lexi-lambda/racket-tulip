#lang racket/base

(require megaparsack
         tulip/lang/parser
         tulip/lang/emitter)

(provide configure-runtime!)

(define (configure-runtime!)
  (current-read-interaction
   (λ (src in)
     ; The REPL works pretty differently when used at the terminal and when used from within DrRacket.
     ; Therefore, it’s necessary to branch on the result of terminal-port? so we can check what
     ; behavior to expect.
     ;
     ; Additionally, when readline is loaded, it installs itself in place of the terminal port with
     ; the name 'readline-input. Therefore, we should handle that case the same way.
     (if (or (terminal-port? in)
             (eq? (object-name in) 'readline-input))
         ; At the terminal, input is delimited by newlines. Therefore, we should read a line at a time
         ; before handing things off to the lexer and parser. If we ever get #<eof>, we should pass it
         ; through. That way, the user can exit the REPL by sending ^D.
         (let ([line (read-line in)])
           (if (eof-object? line)
               eof
               (emit-interaction (parse-result! (parse-tulip (open-input-string line) src)))))
         ; In DrRacket, multi-line input is completely possible, so #<eof>s are inserted between each
         ; interaction within the port. Therefore, we should just lex/parse the whole thing. We need
         ; to actually return #<eof> in order for the REPL to advance to the next prompt, though (for
         ; whatever reason), so we’ll also pass lone #<eof>s through here.
         (if (and (char-ready? in)
                  (not (eof-object? (peek-char in))))
             (emit-interaction (parse-result! (parse-tulip in src)))
             eof)))))
