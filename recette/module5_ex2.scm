(module ex2
   (export (EX2a ex2a))
   (export ex2b))

(define (ex2a a b)
   (list 'ex2a a b))

(define (ex2b x)
   (list 'ex2b x))
