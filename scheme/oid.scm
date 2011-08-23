; The 'oid' Control Structure: let* meets cond.
; Sometime in early 2009, Chris Pressey, Cat's Eye Technologies
; $Id: oid.scm 842 2010-11-28 09:17:01Z cpressey $

; The idea here is that I observed that so much of my Scheme code
; followed this idiom:

; (let* ((foo blah) (bar blah))
;   (cond
;     ((> 23 foo)
;        (let* ((moo blah) (goo blah))
;           ...))
;     ((eq? bar baaz)
;        ...))
;     (else gar)))

; That is to say, cond's nested in let*'s nested in cond's
; nested in let*'s, etc.  So, I thought, why not devise a
; single structure that encompasses this pattern?  Here it is:

(define-syntax oid
  (syntax-rules (else)
    ((oid ((var expr)) branch ...)
      (let ((var expr)) (oid () branch ...)))
    ((oid ((var expr) binding ...) branch ...)
      (let ((var expr)) (oid (binding ...) branch ...)))
    ((oid () (else ((var expr) binding ...) inner-branch ...))
      (oid ((var expr) binding ...) inner-branch ...))
    ((oid () (else expr))
      expr)
    ((oid () (test ((var expr) binding ...) inner-branch ...) branch ...)
      (if test (oid ((var expr) binding ...) inner-branch ...) (oid () branch ...)))
    ((oid () (test expr) branch ...)
      (if test expr (oid () branch ...)))))

(oid ((a 3) (b 2))
  ((eq? b 2) ((c 4) (d 3))
      ((> c b) "goo")
      (else "gar"))
  ((eq? a 3) "hello")
  (else "nada")
)
