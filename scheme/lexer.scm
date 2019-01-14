;
; This file defines a lightweight (not to say crude) lexical analyser.
; in R5RS Scheme.
;
; No combinators, no regular expressions, no set of allowable token
; symbols, no explicit finite automata (deterministic or otherwise).
; It's just some code that consumes tokens from the start of a string.
; Is that so wrong?
;
; It's in the public domain, so feel free to just copy it into your code
; and modify it as you see fit.
;

;
; A lexer-state is a 4-element list: (string pos token toktype).  Code outside the
; lexer doesn't need to know about the internals of it.  Instead, it should use
; these functions to create lexer-states and to examine their properties.
;
(define (initial-lexer-state str)
  (list str 0 "" 'unknown))

(define (initial-lexer-state-at-pos str pos)
  (list str pos "" 'unknown))

(define (token-of lexer-state)
  (caddr lexer-state))

(define (toktype-of lexer-state)
  (cadddr lexer-state))

;
; Here is a helper function to retrieve "invalid" characters past end of string.
;
(define (string-ref-within str pos)
  (if (>= pos (string-length str))
    'EOF
    (string-ref str pos)))

;
; `scan` takes a lexer-state and returns a new lexer-state.
; The token may be the symbol EOF, in which case you should stop scanning (you will
; not get anything new if you keep scanning.)
;
; `scan` basically figures out what kind of token it should scan next,
; and (if necessary) calls one of the functions below it to actually consume it.
;
; Note that the token so scanned will be in the resulting new lexer-state.
; Use `token-of` to retrive it.  Note also that, after creating an initial
; lexer-state, you must scan once in order to get the first token.
;
(define (scan lexer-state)
  (let* ((str              (car lexer-state))
         (pos              (cadr lexer-state))
         (fst-char         (string-ref-within str pos)))
    (cond
      ((equal? fst-char 'EOF)
        (list str pos 'EOF 'eof))
      ((char-whitespace? fst-char)
        (let* ((new-lexer-state         (list str (+ 1 pos) (token-of lexer-state) (toktype-of lexer-state))))
          (scan new-lexer-state)))
      ((member fst-char '( #\( #\) ))
        (let* ((new-lexer-state         (list str (+ 1 pos) (string fst-char) 'parenthesis)))
          new-lexer-state))
      ((member fst-char '( #\- ))
        (let* ((new-lexer-state         (list str (+ 1 pos) (string fst-char) 'punctuation)))
          new-lexer-state))
      ((member fst-char '( #\" ))
        (scan-quoted-string str (+ 1 pos) '()))
      ((char-alphabetic? fst-char)
        (scan-identifier str pos '()))
      (else
        (let* ((new-lexer-state         (list str (+ 1 pos) (string fst-char) 'unknown)))
          new-lexer-state)))))

(define (scan-identifier str pos acc)
  (let* ((fst-char         (string-ref-within str pos))
         (lexer-state      (list str pos)))
    (cond
      ((equal? fst-char 'EOF)
        (let* ((new-lexer-state         (list str pos (list->string (reverse acc)) 'identifier)))
          new-lexer-state))
      ((char-alphabetic? fst-char)
        (scan-identifier str (+ 1 pos) (cons fst-char acc)))
      (else
        (let* ((new-lexer-state         (list str pos (list->string (reverse acc)) 'identifier)))
          new-lexer-state)))))

(define (scan-quoted-string str pos acc)
  (let* ((fst-char         (string-ref-within str pos))
         (lexer-state      (list str pos)))
    (cond
      ((equal? fst-char #\")
        (let* ((new-lexer-state         (list str (+ 1 pos) (list->string (reverse acc)) 'quoted-string)))
          new-lexer-state))
      (else
        (scan-quoted-string str (+ 1 pos) (cons fst-char acc))))))
