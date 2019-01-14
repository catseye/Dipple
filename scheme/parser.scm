(load "lexer.scm")

;
; Recursive descent parser, using the lexer in lexer.scm, for the following
; grammar (similar to S-Expressions, but gratuitously different, to demonstrate):
;
;     Expr ::= identifier | quoted-string | "-" | List.
;     List ::= "(" {Expr} ")".
;
; A parser-state is a 2-element list: (lexer-state ast) - or #f on error.
; All parsing functions take a lexer-state and and return a parser-state.
;

(define (parse str)
  (let* ((lexer-state     (scan (initial-lexer-state str)))
         (result          (parse-expr lexer-state)))
    (if result
      (let* ((ast (cadr result)))
        ast)
      #f)))

(define (parse-expr lexer-state)
  (cond
    ((equal? (token-of lexer-state) "(")
      (parse-list (scan lexer-state) '()))
    ((equal? (toktype-of lexer-state) 'identifier)
      (list (scan lexer-state) (token-of lexer-state)))
    ((equal? (toktype-of lexer-state) 'quoted-string)
      (list (scan lexer-state) (list 'quoted-string (token-of lexer-state))))
    ((equal? (toktype-of lexer-state) 'punctuation)
      (list (scan lexer-state) #(999)))
    (else
      #f)))

(define (parse-list lexer-state acc)
  (cond
    ((equal? (token-of lexer-state) ")")
      (list (scan lexer-state) (reverse acc)))
    (else
      (let* ((result          (parse-expr lexer-state)))
        (if result
          (let* ((new-lexer-state (car result))
                 (ast             (cadr result))
                 (new-acc         (cons ast acc)))
            (parse-list new-lexer-state new-acc))
          #f)))))
