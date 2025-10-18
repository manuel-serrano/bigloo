(module ex12
   (export (ex12 x)
	   (inline ex12i a::long b::long c::long)
	   (generic to-string o)
	   G)
  (static (class C12 a))
   )

(define G 45)

(define (ex12 x)
   (instantiate::C12 (a x)))

(define-inline (ex12i a b c)
   (+fx a (-fx b c)))

(define-generic (to-string a)
   (format "==> ~a " (typeof a)))

(define-method (to-string a::C12)
   (format "~a ~a" (call-next-method) (-> a a)))
