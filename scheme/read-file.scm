; Much sketching must occur.

; This module works on several kinds of "tagged list structures":

; (chars ...) -- a list of Scheme character objects representing a text
; (words ...) -- a list of "chars"es, where each "chars" is a word OR whitespace chunk
; (lines ...) -- a list of FOOs, where each FOO is a "chars" or a "words"

;
; Utilties -- ignore these for now -- skip down for the good stuff
;

(define whitespace?
  (lambda (char)
    (or (equal? char #\space)
        (equal? char #\newline)
        (equal? char #\tab))))

(define newline?
  (lambda (char)
    (equal? char #\newline)))

(define consume-one
  (lambda (pred chars)
    (if (null? chars) chars)
      (if (pred (car chars) (cdr chars))
        chars)))

;
; Utility: Convert a Scheme string to a "chars".
;
(define string->chars
  (lambda (s)
    (cons 'chars (string->list s))))

;
; Open and read a text file as a "chars".
;
(define read-file
  (lambda (filename)
    (let* ((port (open-input-file filename)))
      (read-chars port '()))))

(define read-chars
  (lambda (port acc)
    (let* ((c (read-char port)))
      (cond
        ((eof-object? c) (cons 'chars (reverse acc)))
        (else            (read-chars port (cons c acc)))))))

;
; Regroup a "chars" into a "lines" of "chars".
;
(define extract-lines
  (lambda (chars)
    (if (equal? (car chars) 'chars)
      (extract-lines-rec (cdr chars) '()))))

(define extract-lines-rec
  (lambda (chars acc)
    (if (null? chars) (cons 'lines (reverse acc))
      (let* ((result  (extract-line chars '()))
             (line    (car result))
             (rest    (cadr result)))
        (extract-lines-rec rest (cons line acc))))))

(define extract-line
  (lambda (chars acc)
    (if (null? chars) (list (cons 'chars (reverse acc)) chars)
      (let* ((char (car chars))
             (rest (cdr chars)))
        (if (newline? char)
          (list (cons 'chars (reverse acc)) rest)
          (extract-line rest (cons char acc)))))))

;
; Regroup a "chars" into a "words" of "chars".
;
(define extract-words
  (lambda (chars)
    (if (equal? (car chars) 'chars)
      (extract-words-rec (cdr chars) '()))))

(define extract-words-rec
  (lambda (chars acc)
    (if (null? chars) (cons 'words (reverse acc))
      (let* ((result  (extract-word chars '()))
             (word    (list->string (car result)))
             (rest    (cadr result))
             (result2 (extract-whitespace rest '()))
             (word2   (list->string (car result2)))
             (rest2   (cadr result2)))
        (extract-words-rec rest2 (cons word2 (cons word acc)))))))

(define extract-whitespace
  (lambda (chars acc)
    (if (null? chars) (list (reverse acc) chars)
      (let* ((char (car chars))
             (rest (cdr chars)))
        (if (whitespace? char)
          (extract-whitespace rest (cons char acc))
          (list (reverse acc) chars))))))

(define extract-word
  (lambda (chars acc)
    (if (null? chars) (list (reverse acc) chars)
      (let* ((char (car chars))
             (rest (cdr chars)))
        (if (whitespace? char)
          (list (reverse acc) chars)
          (extract-word rest (cons char acc)))))))

;
; Regroup a "lines" of "chars" into a "lines" of "words" of "chars".
;
(define lines-chars->words
  (lambda (lines)
    (if (equal? (car lines) 'lines)
      (cons 'lines (map extract-words (cdr lines))))))
