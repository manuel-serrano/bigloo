(module foreign2
   (extern (bar::int (::int) "bar"))
   (export (inline bis x)))

(define-inline (bis x)
   (bar x))
