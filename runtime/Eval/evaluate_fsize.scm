;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate_fsize.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  8 16:42:27 2011                          */
;*    Last change :  Wed May 11 18:31:40 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compute the size of stack needed for an abstraction              */
;*    of the space for free variables is not included.                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate_fsize

   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __dsssl
	    __bit
	    __param
	    __bexit
	    __object
	    __thread
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r5_control_features_6_4
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __pp
	    __reader
	    __progn
	    __expand
	    __evenv
	    __evcompile
	    __everror
	    __evmodule
	    
	    __evaluate_uncomp
	    __evaluate_types)

   (export  (frame-size::int ::ev_expr)
	    (extract-loops::ev_expr ::ev_expr)))

(define (frame-size::int e::ev_expr);
   (fsize e 0) )

(define-generic (fsize::int e::ev_expr n::int);
   (error "eval" "internal error: not defined for" e) )

(define-method (fsize::int e::ev_var n::int);
   n )

(define-method (fsize::int var::ev_global n::int);
   n )

(define-method (fsize::int e::ev_litt n::int);
   n )

(define-method (fsize::int e::ev_if n::int);
   (with-access::ev_if e (p t e)
      (max (fsize p n) (fsize t n) (fsize e n)) ))

(define-method (fsize::int e::ev_list n::int);
   (with-access::ev_list e (args)
      (let rec ( (l args) (r n) )
	 (if (null? l)
	     r
	     (rec (cdr l) (max r (fsize (car l) n))) ))))

(define-method (fsize::int e::ev_prog2 n::int);
   (with-access::ev_prog2 e (e1 e2)
      (max (fsize e1 n) (fsize e2 n)) ))

(define-method (fsize::int e::ev_hook n::int);
   (with-access::ev_hook e (e)
      (fsize e n) ))

(define-method (fsize::int e::ev_bind-exit n::int);
   (with-access::ev_bind-exit e (body)
      (fsize body (+fx n 1)) ))

(define-method (fsize::int e::ev_unwind-protect n::int);
   (with-access::ev_unwind-protect e (e body)
      (max (fsize e n) (fsize body n)) ))

(define-method (fsize::int e::ev_with-handler n::int);
   (with-access::ev_with-handler e (handler body)
      (max (fsize handler n) (fsize body n)) ))

(define-method (fsize::int e::ev_synchronize n::int);
   (with-access::ev_synchronize e (mutex prelock body)
      (max (fsize mutex n) (fsize prelock n) (fsize body n)) ))

(define-method (fsize::int e::ev_let n::int);
   (with-access::ev_let e (vals body)
      (let rec ( (l vals) (n n) (r n) )
	 (if (null? l)
	     (max (fsize body n) r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::int e::ev_let* n::int);
   (with-access::ev_let* e (vals body)
      (let rec ( (l vals) (n n) (r n) )
	 (if (null? l)
	     (max (fsize body n) r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::int e::ev_letrec n::int);
   (with-access::ev_letrec e (vals body)
      (let ( (n (+fx n (length vals))) )
	 (let rec ( (l vals) (r n) )
	    (if (null? l)
		(max (fsize body n) r)
		(rec (cdr l) (max (fsize (car l) n) r)) )))))
	 
(define-method (fsize::int e::ev_labels n);
   (with-access::ev_labels e (vars vals body)
      (let rec ( (l vals) (r n) )
	 (if (null? l)
	     (max (fsize body n) r)
	     (rec (cdr l) (max (fsize (cdar l) (+fx n (length (caar l)))) r)) ))))
	 
(define-method (fsize::int e::ev_goto n);
   (with-access::ev_goto e (args)
      (let rec ( (l args) (n n) (r n) )
	 (if (null? l)
	     (max n r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::int e::ev_app n::int);
   (with-access::ev_app e (fun args)
      (let rec ( (l args) (n n) (r (fsize fun n)) )
	 (if (null? l)
	     (max n r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::int e::ev_abs n::int);
   (with-access::ev_abs e (arity vars body size)
      (let ( (nn (fsize body (length vars))) )
	 (set! size nn)
	 n )))



;;;
(define (extract-loops::ev_expr e::ev_expr)
   (if #t
       (search-letrec e)
       e ))

(define (search-letrec* l)
   (let rec ( (l l) )
      (unless (null? l)
	 (set-car! l (search-letrec (car l)))
	 (rec (cdr l)) )))

(define-generic (search-letrec e::ev_expr)
   (error 'search-letrec "not defined for" e) )

(define-method (search-letrec e::ev_var);
   e )

(define-method (search-letrec e::ev_global);
   e )

(define-method (search-letrec e::ev_litt);
   e )

(define-method (search-letrec expr::ev_if);
   (with-access::ev_if expr (p t e)
      (set! p (search-letrec p))
      (set! t (search-letrec t))
      (set! e (search-letrec e))
      expr ))

(define-method (search-letrec e::ev_list);
   (with-access::ev_list e (args)
      (search-letrec* args)
      e ))

(define-method (search-letrec e::ev_prog2);
   (with-access::ev_prog2 e (e1 e2)
      (set! e1 (search-letrec e1))
      (set! e2 (search-letrec e2))
      e ))

(define-method (search-letrec expr::ev_hook);
   (with-access::ev_hook expr (e)
      (set! e (search-letrec e))
      expr ))
      
(define-method (search-letrec e::ev_bind-exit);
   (with-access::ev_bind-exit e (var body)
      (set! body (search-letrec body))
      e ))
      
(define-method (search-letrec expr::ev_unwind-protect);
   (with-access::ev_unwind-protect expr (e body)
      (set! e (search-letrec e))
      (set! body (search-letrec body))
      expr ))

(define-method (search-letrec e::ev_with-handler);
   (with-access::ev_with-handler e (handler body)
      (set! handler (search-letrec handler))
      (set! body (search-letrec body))
      e ))

(define-method (search-letrec e::ev_synchronize);
   (with-access::ev_synchronize e (mutex prelock body)
      (set! mutex (search-letrec mutex))
      (set! prelock (search-letrec prelock))
      (set! body (search-letrec body))
      e ))

(define-method (search-letrec e::ev_binder);
   (with-access::ev_binder e (vars vals body)
      (search-letrec* vals)
      (set! body (search-letrec body))
      e ))

(define-method (search-letrec e::ev_letrec);
   (with-access::ev_letrec e (vars vals body)
      (search-letrec* vals)
      (set! body (search-letrec body))
      (let ( (ok? (letrectail? vars vals body)) )
	 ;;(tprint "FOUND LETREC " ok?)
	 ;(pp (uncompile e))
	 (if ok?
	  (modify-letrec vars vals body)
	  e ))))

(define-method (search-letrec e::ev_app);
   (with-access::ev_app e (fun args)
      (set! fun (search-letrec fun))
      (search-letrec* args)
      e ))

(define-method (search-letrec e::ev_abs);
   (with-access::ev_abs e (arity vars body)
      (set! body (search-letrec body))
      e ))


;;;

(define (modify-letrec vars vals body)
   (let ( (r (instantiate::ev_labels (vars vars) (vals '())
				     (body (instantiate::ev_litt (value 0))) )) )
      (with-access::ev_labels r ((nbody body) (nvals vals))
	 (set! nbody (subst_goto body vars r))
	 (set! nvals (map (lambda (val)
			    (with-access::ev_abs val ((vvars vars) (vbody body))
			       (cons vvars (subst_goto vbody vars r)) ))
		       vals ))
	 r )))
 
(define (letrectail? vars vals body)
   (every (lambda (v)
	      (and (tailpos body v)
		   (every (lambda (e)
			      (when (isa? e ev_abs)
				 (with-access::ev_abs e (arity body)
				    (and (>=fx arity 0)
					 (tailpos body v) ))))
		      vals )))
      vars ))


;;
(define (subst_goto* l vars lbls)
   (let rec ( (l l) )
      (unless (null? l)
	 (set-car! l (subst_goto (car l) vars lbls))
	 (rec (cdr l)) )))

(define-generic (subst_goto e::ev_expr vars lbls)
   (error 'subst_goto "not defined for" e) )

(define-method (subst_goto e::ev_var vars lbls);
   e )

(define-method (subst_goto e::ev_global vars lbls);
   e )

(define-method (subst_goto e::ev_litt vars lbls);
   e )

(define-method (subst_goto expr::ev_if vars lbls);
   (with-access::ev_if expr (p t e)
      (set! p (subst_goto p vars lbls))
      (set! t (subst_goto t vars lbls))
      (set! e (subst_goto e vars lbls))
      expr ))

(define-method (subst_goto e::ev_list vars lbls);
   (with-access::ev_list e (args)
      (subst_goto* args vars lbls)
      e ))

(define-method (subst_goto e::ev_prog2 vars lbls);
   (with-access::ev_prog2 e (e1 e2)
      (set! e1 (subst_goto e1 vars lbls))
      (set! e2 (subst_goto e2 vars lbls))
      e ))

(define-method (subst_goto expr::ev_hook vars lbls);
   (with-access::ev_hook expr (e)
      (set! e (subst_goto e vars lbls))
      expr ))
      
(define-method (subst_goto e::ev_bind-exit vars lbls);
   (with-access::ev_bind-exit e (var body)
      (set! body (subst_goto body vars lbls))
      e ))
      
(define-method (subst_goto expr::ev_unwind-protect vars lbls);
   (with-access::ev_unwind-protect expr (e body)
      (set! e (subst_goto e vars lbls))
      (set! body (subst_goto body vars lbls))
      expr ))

(define-method (subst_goto e::ev_with-handler vars lbls);
   (with-access::ev_with-handler e (handler body)
      (set! handler (subst_goto handler vars lbls))
      (set! body (subst_goto body vars lbls))
      e ))

(define-method (subst_goto e::ev_synchronize vars lbls);
   (with-access::ev_synchronize e (mutex prelock body)
      (set! mutex (subst_goto mutex vars lbls))
      (set! prelock (subst_goto prelock vars lbls))
      (set! body (subst_goto body vars lbls))
      e ))

(define-method (subst_goto e::ev_binder vars lbls);
   (with-access::ev_binder e (vals body)
      (subst_goto* vals vars lbls)
      (set! body (subst_goto body vars lbls))
      e ))

(define-method (subst_goto e::ev_labels vars lbls);
   (with-access::ev_labels e (vals body)
      (let rec ( (l vals) )
	 (unless (null? l)
	    (set-cdr! (car l) (subst_goto (cdar l) vars lbls))
	    (rec (cdr l)) ))
      (set! body (subst_goto body vars lbls))
      e ))

(define-method (subst_goto e::ev_goto vars lbls);
   (with-access::ev_goto e (args)
      (subst_goto* args vars lbls)
      e ))

;* (define-method (subst_goto e::ev_app vars lbls);                    */
;*    (with-access::ev_app e (fun args loc)                            */
;*       (subst_goto* args vars lbls)                                  */
;*       (if (memq fun vars)                                           */
;* 	  (instantiate::ev_goto (label fun) (args args) (labels lbls) (loc loc)) */
;* 	  (begin (set! fun (subst_goto fun vars lbls))                 */
;* 		 e ))))                                                */
(define-method (subst_goto e::ev_app vars lbls);
   ;; BPS: bug fix 11 may 2016
   (with-access::ev_app e (fun args loc)
      (subst_goto* args vars lbls)
      (if (memq fun vars)
      (instantiate::ev_goto (label fun) (args args) (labels lbls) (loc loc))
      (begin (set! fun (subst_goto fun vars lbls))
         (with-access::ev_app e (tail?) (set! tail? #f))
         e ))))

(define-method (subst_goto e::ev_abs vars lbls);
   (with-access::ev_abs e (arity body)
      (set! body (subst_goto body vars lbls))
      e ))


;;; (TAILPOS e x) x must appear only in functional position (x ...) in
;; a direct tail position (not closed under a lambda)

(define-generic (tailpos e::ev_expr v::ev_var)
   (error 'tailpos "not defined for" e) )

(define-method (tailpos e::ev_var v::ev_var);
   (not (eq? e v)) )

(define-method (tailpos e::ev_global v::ev_var);
   #t )

(define-method (tailpos e::ev_litt v::ev_var);
   #t )

(define-method (tailpos e::ev_if v::ev_var);
   (with-access::ev_if e (p t e)
      (and (not (hasvar? p v)) (tailpos t v) (tailpos e v)) ))

(define-method (tailpos e::ev_list v::ev_var);
   (with-access::ev_list e (args)
      (let rec ( (l args) )
	 (if (null? (cdr l))
	     (tailpos (car l) v)
	     (and (not (hasvar? (car l) v))
		  (rec (cdr l)) )))))

(define-method (tailpos e::ev_prog2 v::ev_var);
   (with-access::ev_prog2 e (e1 e2)
      (and (not (hasvar? e1 v)) (tailpos e2 v)) ))

(define-method (tailpos e::ev_hook v::ev_var);
   (with-access::ev_hook e (e)
      (not (hasvar? e v)) ))

(define-method (tailpos e::ev_setlocal var::ev_var);
   (with-access::ev_setlocal e (v e)
      (and (not (eq? v var)) (not (hasvar? e var))) ))
      
(define-method (tailpos e::ev_bind-exit v::ev_var);
   (with-access::ev_bind-exit e (var body)
      (not (hasvar? body v)) ))
      
(define-method (tailpos e::ev_unwind-protect v::ev_var);
   (with-access::ev_unwind-protect e (e body)
      (and (not (hasvar? e v)) (not (hasvar? body v))) ))
      
(define-method (tailpos e::ev_with-handler v::ev_var);
   (with-access::ev_with-handler e (handler body)
      (and (not (hasvar? handler v)) (not (hasvar? body v))) ))

(define-method (tailpos e::ev_synchronize v::ev_var);
   (with-access::ev_synchronize e (mutex prelock body)
      (and (not (hasvar? mutex v)) (not (hasvar? prelock v)) (not (hasvar? body v))) ))

(define-method (tailpos e::ev_let v::ev_var);
   (with-access::ev_let e (vars vals body)
      (and (every (lambda (e) (not (hasvar? e v))) vals)
	   (tailpos body v) )))

(define-method (tailpos e::ev_let* v::ev_var);
   (with-access::ev_let* e (vars vals body)
      (and (every (lambda (e) (not (hasvar? e v))) vals)
	   (tailpos body v) )))

(define-method (tailpos e::ev_letrec v::ev_var);
   (with-access::ev_letrec e (vars vals body)
      (and (every (lambda (e) (not (hasvar? e v))) vals)
	   (tailpos body v) )))

(define-method (tailpos e::ev_labels v::ev_var);
   (with-access::ev_labels e (vals body)
      (and (every (lambda (e) (tailpos (cdr e) v)) vals)
	   (tailpos body v) )))

(define-method (tailpos e::ev_goto v::ev_var);
   (with-access::ev_goto e (args)
      (every (lambda (e) (not (hasvar? e v))) args) ))

(define-method (tailpos e::ev_app v::ev_var);
   (with-access::ev_app e (fun args)
      (and (every (lambda (e) (not (hasvar? e v))) args)
	   (or (eq? fun v)
	       (not (hasvar? fun v)) ))))

(define-method (tailpos e::ev_abs v::ev_var);
   (with-access::ev_abs e (arity vars body)
      (not (hasvar? body v)) ))

;;; (hasvar? e x) return true iff x appear as a subexpression of e

(define-generic (hasvar? e::ev_expr v::ev_var))

(define-method (hasvar? e::ev_var v::ev_var);
   (eq? e v) )

(define-method (hasvar? e::ev_global v::ev_var);
   #f )

(define-method (hasvar? e::ev_litt v::ev_var);
   #f )

(define-method (hasvar? e::ev_if v::ev_var);
   (with-access::ev_if e (p t e)
      (or (hasvar? p v) (hasvar? t v) (hasvar? e v)) ))

(define-method (hasvar? e::ev_list v::ev_var);
   (with-access::ev_list e (args)
      (any (lambda (a) (hasvar? a v)) args) ))

(define-method (hasvar? e::ev_prog2 v::ev_var);
   (with-access::ev_prog2 e (e1 e2)
      (or (hasvar? e1 v) (hasvar? e2 v)) ))

(define-method (hasvar? e::ev_hook v::ev_var);
   (with-access::ev_hook e (e)
      (hasvar? e v) ))

(define-method (hasvar? e::ev_setlocal var::ev_var);
   (with-access::ev_setlocal e (v e)
      (or (eq? v var) (hasvar? e var)) ))
      
(define-method (hasvar? e::ev_bind-exit v::ev_var);
   (with-access::ev_bind-exit e (var body)
      (hasvar? body v) ))
      
(define-method (hasvar? e::ev_unwind-protect v::ev_var);
   (with-access::ev_unwind-protect e (e body)
      (or (hasvar? e v) (hasvar? body v)) ))
      
(define-method (hasvar? e::ev_with-handler v::ev_var);
   (with-access::ev_with-handler e (handler body)
      (or (hasvar? handler v) (hasvar? body v)) ))

(define-method (hasvar? e::ev_synchronize v::ev_var);
   (with-access::ev_synchronize e (mutex prelock body)
      (or (hasvar? mutex v) (hasvar? prelock v) (hasvar? body v)) ))

(define-method (hasvar? e::ev_binder v::ev_var);
   (with-access::ev_binder e (vals body)
      (or (any (lambda (e) (hasvar? e v)) vals)
	  (hasvar? body v) )))

(define-method (hasvar? e::ev_labels v::ev_var);
   (with-access::ev_labels e (vals body)
      (or (any (lambda (e) (hasvar? (cdr e) v)) vals)
	  (hasvar? body v) )))

(define-method (hasvar? e::ev_goto v::ev_var);
   (with-access::ev_goto e (args)
      (any (lambda (e) (hasvar? e v)) args) ))

(define-method (hasvar? e::ev_app v::ev_var);
   (with-access::ev_app e (fun args)
      (or (hasvar? fun v) (any (lambda (e) (hasvar? e v)) args)) ))

(define-method (hasvar? e::ev_abs v::ev_var);
   (with-access::ev_abs e (vars body)
      (hasvar? body v) ))

;;;

