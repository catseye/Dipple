(define get/cc (lambda ()
  (call/cc
    (lambda (continuation)
      continuation))))

(define get/cc (lambda ()
  (call-with-current-continuation
    (lambda (continuation)
      continuation))))

(define test (lambda ()
  (let ((a (get/cc))
        (b 1))
    (display "hi") (newline)
    a)))

((test) (lambda (x) (display "lo")))

(define test2 (lambda ()
  (let ((a (get/cc)))
    (a (get/cc)))))

(define test3 (lambda ()
  (let ((a (get/cc))
        (b (lambda (x) (display "x is") (display x) (newline))))
    (display "a is") (display a) (newline)
    (display "b is") (display b) (newline)
    (a b))))

(define test4 (lambda ()
  (let ((a (get/cc)))
    (display "a is") (display a) (newline)
    (let ((b (lambda (x) (display "x is") (display x) (newline))))
      (display "b is") (display b) (newline)
      (a 7)))))
      
