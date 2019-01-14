(load "parser.scm")
(load "utils.scm")

;;-----------

;
; Helper function which is useful for testing the scanner:
; given a lexer-state, scan all the remaining tokens and return them in a list.
;
(define (scan-all lexer-state acc)
  (let* ((new-lexer-state (scan lexer-state))
         (token           (token-of new-lexer-state)))
    (if (equal? token 'EOF)
      (reverse acc)
      (scan-all new-lexer-state (cons token acc)))))

;;-----------

(test scan-1
  (scan (initial-lexer-state "hi there"))
  '("hi there" 2 "hi" identifier)
)

(test scan-2a
  (scan (initial-lexer-state-at-pos "hi there" 2))
  '("hi there" 8 "there" identifier)
)

(test scan-2b
  (scan (initial-lexer-state-at-pos "hi there" 8))
  '("hi there" 8 EOF eof)
)

(test scan-3
  (scan (initial-lexer-state-at-pos "hi   (there)" 2))
  '("hi   (there)" 6 "(" parenthesis)
)

;;-----------

(test scan-all-1
  (scan-all (initial-lexer-state "hi there,    you") '())
  '("hi" "there" "," "you")
)

(test scan-all-2
  (scan-all (initial-lexer-state "hi \"the re\", you") '())
  '("hi" "the re" "," "you")
)

;;-----------

(test parser-1
  (parse "atom")
  "atom"
)

(test parser-2
  (parse "(list a b)")
  '("list" "a" "b")
)

(test parser-3
  (parse "  (  li (nes ted   ) st  )  ")
  '("li" ("nes" "ted") "st")
)

(test parser-4
  (parse "()")
  '()
)

(test parser-5
  (parse "() extra")
  '()
)

; "!" is not a legal token
(test parser-6
  (parse "(li!st)")
  #f
)

; "-" is a legal token, and we parse it specially (mainly to demonstrate)
(test parser-7
  (parse "(li-st)")
  '("li" #(999) "st")
)

(test parser-8
  (parse "( quo \"te d\" string)")
  '("quo" (quoted-string "te d") "string")
)
