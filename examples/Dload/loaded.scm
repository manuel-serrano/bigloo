(module loaded
   (export (foo))
   (extern (export foo "foo")))

(define (foo)
   (print "foo: " (bar '(1 2 3 "toto" #(1 2 3)))))

(define (bar x)
   (cons 'bar x))

(print "Module foo: ")
(foo)
