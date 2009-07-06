(module saw_remove
   (import type_type ast_var ast_node
	   saw_lib
	   saw_defs
	   )
   (export (remove::block b::block))
   (static (wide-class defcollect::block)
	   (wide-class rcollect::block)
	   (wide-class ucollect::block)
	   (wide-class removed::rtl_ins)
	   (wide-class visited::block)
	   (wide-class bremoved::block)
	   (wide-class creg::rtl_reg defs nbuses) ))


(define (remove::block b::block) ;()
   (make-def-use b)
   (fix-remove (get-first-unused b) (get-first-removable b))
   (let dfs ( (b b) )
      (widen!::visited b)
      (let ( (l (block-first b)) )
	 (let ( (nl (filter! (lambda (ins) (not (removed? ins))) l)) )
	    (if (null? nl)
		(block-remove b)
		(block-first-set! b nl) )))
      (for-each (lambda (s) (if (not (or (visited? s) (bremoved? s))) (dfs s)))
		(block-succs b) ))
   (let find-entry ( (b b) )
      (if (not (bremoved? b))
	  b
	  (let ( (s (car (block-succs b))) )
	     (if (eq? b s)
		 b
		 (find-entry s) )))))

(define (block-remove b::block) ;()
   (with-access::block b (preds succs)
      (widen!::bremoved b)
      (if (or (null? succs) (pair? (cdr succs)))
	  (error 'unlink! "must have only one successor" b) )
      (let ( (s (car succs)) )
	 (for-each (lambda (p) (with-access::block p (succs)
				     (set! succs (subst succs b s)) ))
		   preds )
	 (block-preds-set! s (subst-append (block-preds s) b preds)) )))

(define (make-def-use b::block) ;()
   (define (reg->creg r)
      (if (not (creg? r))
	  (widen!::creg r (defs '()) (nbuses '0)) )
      r )
   (define (reg/read r)
      (with-access::creg (reg->creg r) (nbuses)
	 (set! nbuses (+fx 1 nbuses)) ))
   (define (reg/write r ins)
      (with-access::creg (reg->creg r) (defs)
	 (set! defs (cons ins defs)) ))
   (let defcollect ( (b b) )
      (widen!::defcollect b)
      (for-each (lambda (ins)
		   (with-access::rtl_ins ins (dest fun args)
		      (if dest (reg/write dest ins))
		      (for-each reg/read  args) ))
		(block-first b))
      (for-each (lambda (s) (if (not (defcollect? s)) (defcollect s)))
		(block-succs b) )))

(define (get-first-removable b::block) ;()
   (let ( (rm '()) )
      (let rcollect ( (b b) )
	 (widen!::rcollect b)
	 (for-each (lambda (ins) (if (removable? ins) (set! rm (cons ins rm))))
		   (block-first b) )
	 (for-each (lambda (s) (if (not (rcollect? s)) (rcollect s)))
		   (block-succs b) ))
      rm ))

(define (get-first-unused b::block) ;()
   (let ( (unused '()) )
      (let ucollect ( (b b) )
	 (widen!::ucollect b)
	 (for-each (lambda (ins)
		      (with-access::rtl_ins ins (dest fun args)
			 ;; Collect unused regs
			 (if (and dest
				  (=fx (creg-nbuses dest) 0)
				  (not (memq dest unused)) )
			     (set! unused (cons dest unused)) )))
		   (block-first b))
	 (for-each (lambda (s) (if (not (ucollect? s)) (ucollect s)))
		   (block-succs b) ))
      unused ))

;;
;; fixpoint between useless functional call and useless variable
;;
(define (fix-remove unused rm) ;()
   ;; remove the unused functional call
   (for-each (lambda (ins)
		(widen!::removed ins)
		(for-each (lambda (r)
			     (let ( (n (creg-nbuses r)) )
				(creg-nbuses-set! r (-fx n 1))
				(if (=fx n 1)
				    (set! unused (cons r unused)) )))
			  (rtl_ins-args ins) ))
	     rm )
   (set! rm '())
   ;; remove the affectation of unused variables
   (for-each (lambda (r)
		(for-each (lambda (ins)
			     (rtl_ins-dest-set! ins #f)
			     (if (removable? ins) (set! rm (cons ins rm))) )
			  (creg-defs r) ))
	     unused )
   ;; fixpoint
   (if (not (null? rm)) (fix-remove '() rm)) )

;;
;;
;;
(define (removable?::bool ins::rtl_ins) ;()
   (with-access::rtl_ins ins (fun dest)
      (and (not dest)
	   (rtl_pure? fun) )))
