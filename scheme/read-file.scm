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

(define extract-words
  (lambda (chars acc)
    (if (null? chars) (reverse acc)
      (let* ((result  (extract-word chars '()))
             (word    (list->string (car result)))
             (rest    (cadr result))
             (result2 (extract-whitespace rest '()))
             (word2   (list->string (car result2)))
             (rest2   (cadr result2)))
        (extract-words rest2 (cons word acc))))))
