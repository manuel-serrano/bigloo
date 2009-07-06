(module saw_lib
   (export (subst::pair-nil l::pair-nil x y)
	   (subst-append::pair-nil l::pair-nil x y::pair-nil)))

(define (subst::pair-nil l::pair-nil x y) ;()
   (define (walk l)
      (if (null? l)
	  l
	  (let ( (o (car l)) )
	     (if (eq? o x)
		 (cons y (walk (cdr l)))
		 (cons o (walk (cdr l))) ))))
   (walk l) )

(define (subst-append::pair-nil l::pair-nil x y::pair-nil) ;()
   (define (walk l)
      (if (null? l)
	  l
	  (let ( (o (car l)) )
	     (if (eq? o x)
		 (append y (walk (cdr l)))
		 (cons o (walk (cdr l))) ))))
   (walk l) )


 
