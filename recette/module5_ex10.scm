(module ex10
   (export ex10i))

(define-macro (ex10m x)
   `(cons 'ex10m ,x))

(define-inline (ex10i z)
   (ex10m z))
