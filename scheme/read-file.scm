; Much sketching must occur.

(define read-chars
  (lambda (port acc)
    (let* ((c (read-char port)))
      (cond
        ((eof-object? c) (reverse acc))
        (else            (read-chars port (cons c acc)))))))

(define read-file
  (lambda (filename)
    (let* ((port (open-input-file filename)))
      (read-chars port '()))))

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

; ----- untagged lists -----

; (#\a #\b ...) -- a list of chars representing a text (or portion thereof)

; ----- tagged-list style ----- ;

; (lines ...)  -- a list of strings, where each string is a line
; (words ...)  -- a list of strings, where each string is a word

(define extract-words
  (lambda (chars acc)
    (if (null? chars) (cons 'words (reverse acc))
      (let* ((result  (extract-word chars '()))
             (word    (list->string (car result)))
             (rest    (cadr result))
             (result2 (extract-whitespace rest '()))
             (word2   (list->string (car result2)))
             (rest2   (cadr result2)))
        (extract-words rest2 (cons word2 (cons word acc)))))))

(define extract-line
  (lambda (chars acc)
    (if (null? chars) (list (reverse acc) chars)
      (let* ((char (car chars))
             (rest (cdr chars)))
        (if (newline? char)
          (list (reverse acc) rest)
          (extract-line rest (cons char acc)))))))

(define extract-lines
  (lambda (chars acc)
    (if (null? chars) (cons 'lines (reverse acc))
      (let* ((result  (extract-line chars '()))
             (line    (list->string (car result)))
             (rest    (cadr result)))
        (extract-lines rest (cons line acc))))))

