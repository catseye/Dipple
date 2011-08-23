; World's Most Awesome Topological Sort Implementation
; November 26 2010, Chris Pressey, Cat's Eye Technologies
; $Id: topo.scm 842 2010-11-28 09:17:01Z cpressey $

(define topo-sort
  (lambda (nodes)
    (eval
    `(let ((result '()))
      (letrec ,(map (lambda (node)
        (let*
          ((element (car node))
           (dependencies (cadr node))
           (depcalls (map (lambda (dep)
                       `(,dep)
                     ) dependencies))
           (depcalls (append depcalls
                     `((set! result (cons (quote ,element) result)))))
          )
          `(,element (lambda ()
                       (if (not (memv (quote ,element) result))
                         (begin . ,depcalls))))
        )
      ) nodes) (begin (begin . ,(map (lambda (node)
                                 `(,(car node))
                                ) nodes)) (reverse result))))
     (scheme-report-environment 5))
  )
)

(topo-sort (quote
  ((shoes (socks pants))
   (socks ())
   (underwear ())
   (pants (underwear))
   (shirt ())
   (coat (shirt)))))
