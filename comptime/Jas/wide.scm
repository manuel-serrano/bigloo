(module jas_wide
   (import jas_lib jas_classfile)
   (export resolve-wide) )

;;
;; Wide or narrow instructions and do the pooling
;;
(define (resolve-wide classfile code)
   (map (lambda (x) (if (pair? x) (wide classfile (car x) (cdr x)) x))
	code ))

(define (wide classfile cop args)
   (case cop
      ((17) ; SIPUSH
       (cons cop (u2 (car args))) )
      ((18) ; LDC
       (let ( (v (car args)) )
	  (define (w n) (if (<fx n #x100) (list 18 n) (cons 19 (u2 n))))
	  (cond ((fixnum? v) (w (pool-int    classfile v)))
		((flonum? v)  (w (pool-float  classfile v)))
		((string? v)  (w (pool-string classfile v)))
		(else (jas-error classfile "bad immediate value" v)) )))
      ((20) ; LDC2
       (let ( (v (car args)) )
	  (cond ((fixnum? v) (cons 20 (u2 (pool-long   classfile v))))
		((flonum? v)  (cons 20 (u2 (pool-double classfile v))))
		((elong? v)  (cons 20 (u2 (pool-elong classfile v))))
		((llong? v)  (cons 20 (u2 (pool-llong classfile v))))
		(else (jas-error classfile "bad immediate value" v)) )))
      ((21 22 23 24 25 54 55 56 57 58) ; XLOAD XSTORE
       (let* ( (index (car args))
	       (base  (if (>fx cop 50) 54 21))
	       (ismall (+fx (+fx base 5) (*fx (-fx cop base) 4))) )
	  (cond ((<fx index 4)       (list (+ ismall index)))
		((<fx index #x100)   (list cop index))
		((<fx index #x10000) (cons* 196 cop (u2 index)))
		(else                (jas-error classfile "too much locals"
						index )))))
      ((132) ; IINC
       (let ( (index (car args)) (n (cadr args)) )
	  (cond
	     ((and (<fx index #x100) (>fx n -129) (<fx n 128))
	      (list 132 index n) )
	     ((>=fx index #x10000)
	      (jas-error classfile "too much locals" index) )
	     ((>=fx (abs n) #x8000)
	      (jas-error classfile "too large increment in iinc" n) )
	     (else (cons* 196 132 (u2 index) (u2 n))) )))
      ((169) ; RET
       (let ( (index (car args)) )
	  (cond ((<fx index #x100)   (list 169 index))
		((<fx index #x10000) (cons* 196 169 (u2 index)))
		(else                (jas-error classfile "too much locals"
						index )))))
      ((178 179 180 181) ; [GET|PUT][FIELD|STATIC]
       (cons cop (u2 (pool-field classfile (car args)))) )
      ((182 183 184) ; INVOKE**
       (cons cop (u2 (pool-method classfile (car args)))) )
      ((185) ; INVOKEINTERFACE
       (let ( (m (car args)) )
        (with-access::fun (method-type m) (targs)
           (cons cop (append (u2 (pool-interface-method classfile m))
                             (list (+fx 1 (apply + (map type-size targs)))
                                   0) )))))
      ((187) ; NEW
       (cons cop (u2 (pool-class classfile (car args)))) )
      ((189 192 193) ; ANEWARRAY CHECKCAST INSTANCEOF
       (cons cop (u2 (pool-class-by-reftype classfile (car args)))) )
      ((197) ; MULTINEWARRAY
       (cons cop (append (u2 (pool-class-by-reftype classfile (car args)))
			 (list (cadr args)) )))
      ((205) ; LOCALVAR
       (match-case args
	  ((?beg ?end ?name ?type ?index)
	   (list cop beg end (pool-name classfile name)
		 (pool-name classfile (type-code type))
		 index ))
	  (else (jas-error classfile "bad localvar args" args)) ))
      (else (cons cop args)) ))
