(module saw_cast
   (import type_type ast_var ast_node
	   type_env type_cache
	   tools_shape
	   object_class
	   backend_backend
	   saw_defs )
   (export (add-casts b::backend l::pair-nil)) )

;; CARE better if we can generate expressions

(define (add-casts back::backend l::pair-nil) ;(list block)
   (for-each (lambda (b)
		(let ( (hook (cons 'hook '())) )
		   (let loop ( (l (block-first b)) (last hook) )
		      (if (null? l)
			  (block-first-set! b (cdr hook))
			  (let ( (ins (car l)) (r (cdr l)) )
			     (let ( (cins (add-casts-ins back ins)) )
				(set-cdr! last cins)
				(loop (cdr l) (last-pair cins)) ))))))
	     l ))

(define (add-cast from to) ;()
   ;; CARE BPS obsolete (11jan2013)
   (let ( (type (rtl_reg-type to)) )
      (instantiate::rtl_ins (dest to)
			    (fun (instantiate::rtl_cast
				    (totype type)
				    (fromtype (rtl_reg-type from))))
			    (args (cons from '())) )))

(define (add-casts-ins back ins) ;()
   (with-access::rtl_ins ins (dest fun args)
      (let ( (casts (add-cast-dest back ins)) )
	 (add-cast-args back fun args (type-args fun) (cons ins casts)) )))

(define (add-cast-dest back ins) ;()
   (with-access::rtl_ins ins (dest fun args)
      (if dest
	  (let ( (real (type-dest fun args)) (declared (rtl_reg-type dest)) )
	     (if (assign-type? back real declared)
		 '()
		 (let ( (rr (instantiate::rtl_reg
			       (type real)
			       (var #f)
			       (name (gensym)))) )
		    (let ( (x dest) )
		       (rtl_ins-dest-set! ins rr)
		       (cons (add-cast rr x) '()) ))))
	  '() )))

(define (add-cast-args back fun args types casts);()
   (if (null? types)
       casts
       (let ( (casts (add-cast-args back fun (cdr args) (cdr types) casts)) )
	  (let ( (r (car args)) (t (car types)) )
	     (if (assign-type? back (rtl_reg-type r) t)
		 casts
		 (let ( (rr (instantiate::rtl_reg
			       (type t)
			       (var #f)
			       (name (gensym)))) )
		    (set-car! args rr)
		    (cons (add-cast r rr) casts) ))))))

(define (assign-type? back t1 t2)
   (or (eq? t1 t2)
       (numeric? t1)
       (numeric? t2)
       ;; Give a chance to the backend
       (backend-subtype? back t1 t2) ))

(define (numeric? t)
   (memq (type-id t) '(bool byte ubyte short ushort
			    char uchar ucs2 void int long ulong 
			    elong llong uelong ullong float double
			    )) )

;;
(define-generic (type-dest::type fun::rtl_fun args::pair-nil) ;(list expr)
   (find-type 'obj) )
   
(define-method (type-dest::type fun::rtl_nop args::pair-nil) ;(list expr)
   (find-type 'unspecified) )

(define-method (type-dest::type fun::rtl_mov args::pair-nil) ;(list expr)
   (rtl_reg-type (car args)) )

(define-method (type-dest::type fun::rtl_loadi args::pair-nil) ;(list expr)
   (node-type (rtl_loadi-constant fun)) )

(define-method (type-dest::type fun::rtl_loadfun args::pair-nil) ;(list expr)
   ; MANU
   ; (find-type 'function)
   (find-type 'procedure) )

(define-method (type-dest::type fun::rtl_loadg args::pair-nil) ;(list expr)
   (global-type (rtl_loadg-var fun)) )

(define-method (type-dest::type fun::rtl_getfield args::pair-nil) ;(list expr)
   (rtl_getfield-type fun) )

(define-method (type-dest::type fun::rtl_new args::pair-nil) ;(list expr)
   (rtl_new-type fun) )

(define-method (type-dest::type fun::rtl_valloc args::pair-nil) ;(list expr)
   (rtl_valloc-vtype fun) )

(define-method (type-dest::type fun::rtl_vref args::pair-nil) ;(list expr)
   (rtl_vref-type fun) )

(define-method (type-dest::type fun::rtl_vlength args::pair-nil) ;(list expr)
   (find-type 'int) )

(define-method (type-dest::type fun::rtl_instanceof args::pair-nil) ;(list expr)
   (find-type 'int) )

(define-method (type-dest::type fun::rtl_cast_null args::pair-nil) ;(list expr)
   (with-access::rtl_cast_null fun (type)
      type) )

(define-method (type-dest::type fun::rtl_call args::pair-nil) ;(list expr)
   (global-type (rtl_call-var fun)) )

(define-method (type-dest::type fun::rtl_cast args::pair-nil) ;(list expr)
   (rtl_cast-totype fun) )

;;
(define-generic (type-args fun::rtl_fun);
   '() )

(define-method (type-args fun::rtl_getfield);
   (cons (rtl_getfield-objtype fun) '()) )

(define-method (type-args fun::rtl_setfield);
   (cons (rtl_setfield-objtype fun) (cons (rtl_setfield-type fun) '())) )

(define-method (type-args fun::rtl_vref);
   (cons (rtl_vref-vtype fun) '()) )

(define-method (type-args fun::rtl_vset);
   (cons (rtl_vset-vtype fun) '()) )

(define-method (type-args fun::rtl_vlength);
   (cons (rtl_vlength-vtype fun) '()) )

(define-method (type-args fun::rtl_storeg);
   (cons (global-type (rtl_storeg-var fun)) '()) )

(define-method (type-args fun::rtl_call);
   (let* ( (g (rtl_call-var fun))
	   (f (global-value (rtl_call-var fun))) )
      (if (cfun? f)
	  (cfun-args-type f)
	  (let ( (r (sfun-args f)) )
	     (if (or (null? r) (type? (car r)))
		 r
		 (map local-type r) )))))

(define-method (type-args fun::rtl_return);
   (cons (rtl_return-type fun) '()) )
		 
(define-method (type-args fun::rtl_funcall);
   (cons (find-type 'procedure) '()) )

(define-method (type-args fun::rtl_lightfuncall);
   (cons (find-type 'procedure) '()) )

(define-method (type-args fun::rtl_apply);
   (cons (find-type 'procedure) '()) )

(define-method (type-args fun::rtl_boxref);
   (cons (find-type 'cell) '()) )

(define-method (type-args fun::rtl_boxset);
   (cons (find-type 'cell) '()) )

