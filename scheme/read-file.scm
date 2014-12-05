
(define read-chars
  (lambda (port acc)
    (let* ((c (read-char port)))
      (cond
        ((eof-object? c) (reverse acc))
        (else            (read-chars port (cons c acc)))))))

(define read-file
  (lambda (name)
    (let* ((port (open-input-file name)))
      (read-chars port '()))))
