(module weakptr
 (import (main "main.scm"))
 (include "test.sch")
 (export (test-weakptr)))

(define (has-collected list)
 (any (lambda (ptr) (eq? #unspecified (weakptr-data ptr))) list))

(define (test-weakptr)
 (test-module "test-weakptr" "weakptr.scm")
 (let ((ptr (make-weakptr 32)))
  (test "weakptr?.1" (weakptr? ptr) #t)
  (test "weakptr?.2" (weakptr? #f) #f)
  (test "weakptr?.3" (weakptr? 32) #f)
  (test "weakptr?.4" (weakptr? "foo") #f)
  (test "weakptr-data" (weakptr-data ptr) 32)
;  (test "weakptr-collection " 
;        (let loop ((leak '())
;                   (i 0))
;         (cond ((> i 10000) #f)
;               ((has-collected leak) #t)
;               (else
;                (loop (cons (make-weakptr (make-vector 20000)) leak)
;                      (+ i 1)))))
;        #t)
  ))