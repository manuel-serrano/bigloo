(module foo)


(define-struct int-point x y)
(define-struct real-point x y)

(define (is-in-circle? r p)
   (match-case p
      (#{int-point ?x ?y}
       (<=fx (+fx (*fx x x) (*fx y y)) (*fx r r)))
      (#{real-point ?x ?y}
       (<= (+fl (*fl x x) (*fl y y)) (* r r)))
      (else
       (error "is-in-circle?" "argument not a point" p))))

(define p-int (make-int-point))

(int-point-x-set! p-int 4)
(int-point-y-set! p-int -4)

(define p-real (make-real-point))

(real-point-x-set! p-real 4.4)
(real-point-y-set! p-real -4.4)

(print "is-in-circle? : " (is-in-circle? 10 p-int))
(print "is-in-circle? : " (is-in-circle? 10 p-real))


