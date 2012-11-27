(module import2
   (export x)
   (export (import-test2)))

(define x 2)

(define (import-test2)
   'import-test2)
