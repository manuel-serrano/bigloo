(module import1
   (export (import-test1))
   (export x
	   (y::obj)
	   (inline z::int)))

(define x 1)
(define (y) 1)
(define-inline (z) 1)

(define (import-test1)
   'import-test1)
