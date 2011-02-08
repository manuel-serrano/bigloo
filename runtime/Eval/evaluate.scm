;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Bernard Serpette                                  */
;*    Creation    :  Fri Jul  2 10:01:28 2010                          */
;*    Last change :  Tue Feb  8 10:48:11 2011 (serrano)                */
;*    Copyright   :  2010-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    New Bigloo interpreter                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate
   
   (option (set! *unsafe-type* #f)
	   (set! *unsafe-range* #f)
	   (set! *unsafe-arity* #f)
	   (set! *unsafe-struct* #f)
	   (set! *unsafe-eval* #f))
   
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
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
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
	    __evmodule)

   (extern (macro $evmeaning-evstate::obj (::dynamic-env)
		  "BGL_ENV_EVSTATE")
	   (macro $evmeaning-evstate-set!::obj (::dynamic-env ::obj)
		  "BGL_ENV_EVSTATE_SET"))
   
   (java   (class foreign
	      (method static $evmeaning-evstate::obj (::dynamic-env)
		      "BGL_ENV_EVSTATE")
	      (method static $evmeaning-evstate-set!::obj (::dynamic-env ::obj)
		      "BGL_ENV_EVSTATE_SET")))
   
   (static 
    ;;
    ;; Ast types
    ;;
    (class ev_expr)
    
    (class ev_var::ev_expr name::symbol (eff (default #f)))
    (class ev_global::ev_expr name::symbol mod (loc read-only))
    
    (class ev_litt::ev_expr value)
    (class ev_if::ev_expr p::ev_expr t::ev_expr e::ev_expr)
    (class ev_list::ev_expr args)
    (class ev_or::ev_list)
    (class ev_and::ev_list)
    (class ev_prog2::ev_expr e1::ev_expr e2::ev_expr)
    (class ev_hook::ev_expr e::ev_expr)
    (class ev_trap::ev_hook)
    (class ev_setlocal::ev_hook v::ev_var)
    (class ev_setglobal::ev_hook name::symbol mod (loc read-only))
    (class ev_defglobal::ev_setglobal)
    (class ev_bind-exit::ev_expr var::ev_var body::ev_expr)
    (class ev_unwind-protect::ev_expr e::ev_expr body::ev_expr)
    (class ev_with-handler::ev_expr handler::ev_expr body::ev_expr)
    (class ev_binder::ev_expr vars vals body::ev_expr)
    (class ev_let::ev_binder (boxes (default '())))
    (class ev_let*::ev_binder (boxes (default '())))
    (class ev_letrec::ev_binder)
    (class ev_app::ev_expr loc fun::ev_expr args tail?)
    (class ev_abs::ev_expr
       (loc read-only)
       (where read-only)
       (arity read-only)
       vars
       body::ev_expr
       (size::bint (default 0))
       (bind (default '()))
       (free (default '()))
       (inner (default '()))
       (boxes (default '())) )
    )
   
   (export  (evaluate2 sexp env loc)
	    (get-evaluation-contexte)
	    (set-evaluation-contexte! v)))

(define **call/cc-compliant** #f)
;(define (call/cc proc)
;   (call-cc (lambda (cont)
;	       (let ( (evc (get-evaluation-contexte)) )
;		  (let ( (cont (lambda (x)
;				  (set-evaluation-contexte! evc)
;				  (cont x) )) )
; ...

;;
;; Macros
;;
(define-macro (prog1 e . l)
   `(let ( (**res** ,e) )
       (begin ,@l
	      **res** )))

(define-macro (step s bp size . l)
   (if #f
       `(when (> (bigloo-debug) 0)
	   (display (list ,bp ,size (vector-ref ,s 0)))
	   (display " ")
	   (display (dump ,s ,bp ,size))
	   (map (lambda (x) (display " ") (display x)) (list ,@l))
	   (print) )
       '#unspecified ))

(define **prof_results** (list 'all (fixnum->llong 0)))

(define-macro (prof path)
   (if #f
       `(do_profile ,path)
       '#unspecified ))

(define (do_profile path);
   (let rec ( (t **prof_results**) (p path) )
      ;; t = (last_node_name nb ... <sub_tree> ...)
      (set-car! (cdr t) (+ 1 (cadr t)))
      (when (pair? p)
	 (let ( (slot (assq (car p) (cddr t))) )
	    (unless slot
		(begin (set! slot (list (car p) (fixnum->llong 0)))
		       (set-cdr! (cdr t) (cons slot (cddr t))) ))
	    (rec slot (cdr p)) ))))

(define (reset_profile)
   (set! **prof_results** (list 'all (fixnum->llong 0))) )

(define (<lexint t1 t2)
   (define (o->s v)
      (cond
	 ((integer? v) (integer->string v))
	 ((symbol? v) (symbol->string v))
	 ((string? v) v)
	 (else "non non non") ))
   (let ( (v1 (car t1)) (v2 (car t2)) )
      (if (and (integer? v1) (integer? v2))
	  (< v1 v2)
	  (string<? (o->s v1) (o->s v2)) )))

(define (get_profile)
   (let rec ( (t **prof_results**) )
      (set-cdr! (cdr t) (sort <lexint (cddr t)))
      (for-each (lambda (t) (rec t)) (cddr t)) )
   **prof_results** )


;*---------------------------------------------------------------------*/
;*    evaluate2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (evaluate2 sexp env loc)
   (let ( (ast (convert sexp env loc)) )
      (when (> (bigloo-debug) 10) (pp (uncompile ast)))
      (analyse-vars ast)
      (let ( (n (frame-size ast)) )
	 (let ( (f (compile ast)) )
	    (let ( (s (find-state)) )
	       (let ( (bp (vector-ref s 0)) )
		  (unwind-protect (f s)
				  (vector-set! s 0 bp) )))))))

(define (loc-type-error f m v loc)
   (match-case loc
      ((at ?fname ?loc)
       (bigloo-type-error/location f m v fname loc) )
      (else (bigloo-type-error f m v)) ))

;;
;; Taken elsewhere...
;;


;*---------------------------------------------------------------------*/
;*    get-location ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-location exp loc)
   (or (get-source-location exp) loc))

;;
;; Lib
;;
(define (index x l)
   (define (walk l n)
      (cond ((null? l) #f)
	    ((eq? x (car l)) n)
	    (else (walk (cdr l) (+fx n 1))) ))
   (walk l 0) )

(define (_index x l)
   (or (index x l) (error "eval" "internal error: not found" (list x " in " (map lname l)))) )

(define (union l1 l2)
   (if (null? l1)
       l2
       (let ( (x (car l1)) )
	  (union (cdr l1) (if (memq x l2) l2 (cons x l2))) )))

(define (inter l1 l2)
   (if (null? l1)
       '()
       (let ( (x (car l1)) )
	  (if (memq x l2)
	      (cons x (inter (cdr l1) l2))
	      (inter (cdr l1) l2) ))))

(define (diff l1 l2)
   (if (null? l1)
       '()
       (let ( (x (car l1)) )
	  (if (memq x l2)
	      (diff (cdr l1) l2)
	      (cons x (diff (cdr l1) l2)) ))))

;;
;; Convert cons to Ast
;;
(define (convert e globals loc)
   (conv e '() globals #f 'nowhere loc #t) )

(define (conv-var v locals)
   (let rec ( (l locals) )
      (if (null? l)
	  #f
	  (let ( (rv (car l)) )
	     (if (eq? v (ev_var-name rv))
		 rv
		 (rec (cdr l)) )))))

(define (lname v)
   (if (ev_var? v)
       (ev_var-name v)
       v ))

(define (conv-begin l locals globals tail? where loc top?)
   (match-case l
      (() (instantiate::ev_litt (value #unspecified)))
      ((?e) (conv e locals globals tail? where loc top?))
      ((?e1 . ?r) (instantiate::ev_prog2 (e1 (conv e1 locals globals #f where loc top?))
					 (e2 (conv-begin r locals globals tail? where loc top?)) ))
      (else (evcompile-error loc "eval" "bad syntax" l)) ))

(define (conv e locals globals tail? where loc top?)
   (define (rconv e) (conv e locals globals tail? where (get-location e loc) #f))
   (define (uconv e) (conv e locals globals #f where (get-location e loc) #f))
   (define (conv-lambda formals body where)
      (define (split-formals l)
	 (let rec ( (r l) (flat '()) (arity 0) )
	    (cond
	       ((null? r) (values (reverse! flat) arity))
	       ((not (pair? r)) (values (reverse! (cons (untype-ident r) flat)) (-fx -1 arity)))
	       (else (rec (cdr r) (cons (untype-ident (car r)) flat) (+fx arity 1))) )))
      (multiple-value-bind (vars arity) (split-formals (dsssl-formals->scheme-formals formals error))
	(let ( (vars (map (lambda (v) (instantiate::ev_var (name v))) vars))
	       (body (make-dsssl-function-prelude e formals body error))
	       (nloc (get-location body loc)) )
	   (instantiate::ev_abs (loc loc)
				(where where)
				(arity arity)
				(vars vars)
				(body (conv body (append vars locals) globals #t where nloc #f)) ))))
   (match-case e
      ((atom ?x)
       (if (symbol? x)
	   (or (conv-var x locals)
	       (instantiate::ev_global (loc loc) (name x) (mod (if (evmodule? globals) globals ($eval-module)))) )
	   (instantiate::ev_litt (value x)) ))
      ((module . ?bah)
       (if top?
	   (conv (expand (evmodule e (get-location e loc))) locals globals where #f loc #t)
	   (evcompile-error loc "eval" "Illegal non toplevel module declaration" e) ))
      ((@ (and ?id (? symbol?)) (and ?mod (? symbol?)))
       (instantiate::ev_global (loc loc) (name id) (mod mod)))
      (((and (? symbol?)
	     (? (lambda (x) (conv-var x locals)))
	     ?fun)
	. ?args)
       (let ( (fun (uconv fun)) (args (map uconv args)) )
	  (instantiate::ev_app (loc loc) (fun fun) (args args) (tail? tail?)) ))
      ((trap ?e)
       (instantiate::ev_trap (e (uconv e))) )
      ((quote ?v)
       (instantiate::ev_litt (value v)) )
      ((if ?p ?t ?e)
       (instantiate::ev_if (p (uconv p)) (t (rconv t)) (e (rconv e))) )
      (((kwote or) . ?args)
       (instantiate::ev_or (args (map uconv args))) )
      (((kwote and) . ?args)
       (instantiate::ev_and (args (map uconv args))) )
      ((begin . ?l)
       (conv-begin l locals globals tail? where loc top?) )
      ((let ?binds . ?body)
       (let ( (vars (map (lambda (b) (instantiate::ev_var (name (untype-ident (car b))))) binds)) )
	  (instantiate::ev_let (vars vars)
			       (vals (map (lambda (b) (uconv (cadr b))) binds))
			       (body (conv-begin body (append vars locals) globals tail? where loc #f)) )))
      ((let* ?binds . ?body)
       (define (conv-vals l vars locals)
	  (if (null? l)
	      '()
	      (cons (conv (cadar l) locals globals #f where loc #f)
		    (conv-vals (cdr l) (cdr vars) (cons (car vars) locals)) )))
       (let ( (vars (map (lambda (b) (instantiate::ev_var (name (untype-ident (car b))))) binds)) )
	  (instantiate::ev_let* (vars vars)
				(vals (conv-vals binds vars locals))
				(body (conv-begin body (append vars locals) globals tail? where loc #f)) )))
      ((letrec ?binds . ?body)
       (let ( (vars (map (lambda (b) (instantiate::ev_var (name (untype-ident (car b))))) binds)) )
	  (let ( (locals (append vars locals)) )
	     (instantiate::ev_letrec (vars vars)
				     (vals (map (lambda (b) (conv (cadr b) locals globals #f where loc #f)) binds))
				     (body (conv-begin body locals globals tail? where loc #f)) ))))
      ((set! (@ (and ?id (? symbol?)) (and ?mod (? symbol?))) ?e)
       (instantiate::ev_setglobal (loc loc)
				  (name id)
				  (mod mod)
				  (e (uconv e))))
      ((set! ?v ?e)
       (let ( (cv (conv-var v locals)) (e (uconv e)) )
	  (if cv
	      (instantiate::ev_setlocal (v cv) (e e))
	      (instantiate::ev_setglobal (loc loc)
					 (name v)
					 (mod (if (evmodule? globals) globals ($eval-module)))
					 (e e)) )))
      ((define ?gv (lambda ?formals ?body))
       (instantiate::ev_defglobal (loc loc)
				  (name (untype-ident gv))
				  (mod (if (evmodule? globals) globals ($eval-module)))
				  (e (conv-lambda formals body gv)) ))
      ((define ?gv ?ge)
       (instantiate::ev_defglobal (loc loc)
				  (name (untype-ident gv))
				  (mod (if (evmodule? globals) globals ($eval-module)))
				  (e (uconv ge)) ))
      ((bind-exit (?v) . ?body)
       (let ( (var (instantiate::ev_var (name v))) )
	  (instantiate::ev_bind-exit (var var)
				     (body (conv-begin body (cons var locals) globals #f where loc #f)) )))
      ((unwind-protect ?e . ?body)
       (instantiate::ev_unwind-protect (e (uconv e))
				       (body (conv-begin body locals globals #f where loc #f)) ))
      ((with-handler ?h . ?body)
       (instantiate::ev_with-handler (handler (uconv h))
				     (body (conv-begin body locals globals #f where loc #f)) ))
      ((lambda ?formals ?body)
       (conv-lambda formals body 'nowhere) )
      ((?f . ?args)
       (let ( (fun (uconv f)) (args (map uconv args)) )
	  (instantiate::ev_app (loc loc) (fun fun) (args args) (tail? tail?)) ))
      (else (evcompile-error loc "eval" "bad syntax" e)) ))

;;
;; Compute free/closed variables
;;
(define (analyse-vars e::ev_expr);
   (let ( (fake (instantiate::ev_abs (loc 'never) (where 'fake) (arity 0) (vars '()) (body e))) )
      ;; We can check something in fake.
      (avar e '() fake) ))

(define (check-var var::ev_var local abs::ev_abs);
   (unless (memq var local)
      (let ( (free (ev_abs-free abs)) )
	 (unless (memq var free) (ev_abs-free-set! abs (cons var free))) )))

(define-generic (avar e::ev_expr local abs);
   (error "eval" "internal error: not defined for" e) )

(define-method (avar var::ev_var local abs);
   (check-var var local abs) )

(define-method (avar var::ev_global local abs);
   #unspecified )

(define-method (avar e::ev_litt local abs);
   #unspecified )

(define-method (avar e::ev_if local abs);
   (with-access::ev_if e (p t e)
      (avar p local abs)
      (avar t local abs)
      (avar e local abs) ))

(define-method (avar e::ev_list local abs);
   (with-access::ev_list e (args)
      (for-each (lambda (e) (avar e local abs)) args) ))

(define-method (avar e::ev_prog2 local abs);
   (with-access::ev_prog2 e (e1 e2)
      (avar e1 local abs)
      (avar e2 local abs) ))

(define-method (avar e::ev_hook local abs);
   (with-access::ev_hook e (e)
      (avar e local abs) ))

(define-method (avar e::ev_setlocal local abs);
   (with-access::ev_setlocal e (v e)
      (check-var v local abs)
      (ev_var-eff-set! v #t)
      (avar e local abs) ))

(define-method (avar e::ev_bind-exit local abs);
   (with-access::ev_bind-exit e (var body)
      (avar body (cons var local) abs)
      (bind-and-reset-effect abs (cons var '())) ))

(define-method (avar e::ev_unwind-protect local abs);
   (with-access::ev_unwind-protect e (e body)
      (avar e local abs)
      (avar body local abs) ))

(define-method (avar e::ev_with-handler local abs);
   (with-access::ev_with-handler e (handler body)
      (avar handler local abs)
      (avar body local abs) ))

(define-method (avar e::ev_let local abs);
   (with-access::ev_let e (vars vals body)
      (for-each (lambda (e) (avar e local abs)) vals)
      (avar body (append vars local) abs)
      (bind-and-reset-effect abs vars)
      (ev_let-boxes-set! e (filter ev_var-eff vars)) ))

(define-method (avar e::ev_let* local abs);
   (with-access::ev_let* e (vars vals body)
      (let ( (local (append vars local)) )
	 (for-each (lambda (e) (avar e local abs)) vals)
	 (avar body local abs)
	 (bind-and-reset-effect abs vars)
	 (ev_let*-boxes-set! e (map ev_var-eff vars)) )))
	 
(define-method (avar e::ev_letrec local abs);
   (with-access::ev_letrec e (vars vals body)
      (let ( (local (append vars local)) )
	 (for-each (lambda (e) (avar e local abs)) vals)
	 (avar body local abs)
	 (ev_abs-bind-set! abs (append vars (ev_abs-bind abs)))
	 (for-each (lambda (v) (ev_var-eff-set! v #t)) vars) )))

(define-method (avar e::ev_app local abs);
   (with-access::ev_app e (fun args)
      (avar fun local abs)
      (for-each (lambda (e) (avar e local abs)) args) ))


(define (bind-and-reset-effect abs::ev_abs vars);
   ;; clean the effect flag for non-closed variables
   (define (funion l)
      (if (null? l)
	  '()
	  (union (ev_abs-free (car l)) (funion (cdr l))) ))
   (let ( (ifree (funion (ev_abs-inner abs))) )
      (ev_abs-bind-set! abs (append vars (ev_abs-bind abs)))
      (unless **call/cc-compliant**
	 (for-each (lambda (v) (ev_var-eff-set! v #f)) (diff vars ifree) ))
      ifree ))

(define-method (avar e::ev_abs local abs);
   (with-access::ev_abs e (arity vars body)
      (ev_abs-inner-set! abs (cons e (ev_abs-inner abs)))
      (avar body vars e)
      (let ( (ifree (bind-and-reset-effect e vars)) )
	 (ev_abs-free-set! e (diff (union ifree (ev_abs-free e)) (ev_abs-bind e)))
         (ev_abs-boxes-set! e (filter ev_var-eff vars)) )))

;;
;; Compute the size of stack needed for an abstraction / the space for free variables is not included
;;
(define (frame-size::bint e::ev_expr);
   (fsize e 0) )

(define-generic (fsize::bint e::ev_expr n::bint);
   (error "eval" "internal error: not defined for" e) )

(define-method (fsize::bint e::ev_var n::bint);
   n )

(define-method (fsize::bint var::ev_global n::bint);
   n )

(define-method (fsize::bint e::ev_litt n::bint);
   n )

(define-method (fsize::bint e::ev_if n::bint);
   (with-access::ev_if e (p t e)
      (max (fsize p n) (fsize t n) (fsize e n)) ))

(define-method (fsize::bint e::ev_list n::bint);
   (let rec ( (l (ev_list-args e)) (r n) )
      (if (null? l)
	  r
	  (rec (cdr l) (max r (fsize (car l) n))) )))

(define-method (fsize::bint e::ev_prog2 n::bint);
   (with-access::ev_prog2 e (e1 e2)
      (max (fsize e1 n) (fsize e2 n)) ))

(define-method (fsize::bint e::ev_hook n::bint);
   (with-access::ev_hook e (e)
      (fsize e n) ))

(define-method (fsize::bint e::ev_bind-exit n::bint);
   (with-access::ev_bind-exit e (body)
      (fsize body (+fx n 1)) ))

(define-method (fsize::bint e::ev_unwind-protect n::bint);
   (with-access::ev_unwind-protect e (e body)
      (max (fsize e n) (fsize body n)) ))

(define-method (fsize::bint e::ev_with-handler n::bint);
   (with-access::ev_with-handler e (handler body)
      (max (fsize handler n) (fsize body n)) ))

(define-method (fsize::bint e::ev_let n::bint);
   (with-access::ev_let e (vals body)
      (let rec ( (l vals) (n n) (r n) )
	 (if (null? l)
	     (max (fsize body n) r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::bint e::ev_let* n::bint);
   (with-access::ev_let* e (vals body)
      (let rec ( (l vals) (n n) (r n) )
	 (if (null? l)
	     (max (fsize body n) r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::bint e::ev_letrec n::bint);
   (with-access::ev_letrec e (vals body)
      (let ( (n (+fx n (length vals))) )
	 (let rec ( (l vals) (r n) )
	    (if (null? l)
		(max (fsize body n) r)
		(rec (cdr l) (max (fsize (car l) n) r)) )))))

(define-method (fsize::bint e::ev_app n::bint);
   (with-access::ev_app e (fun args)
      (let rec ( (l args) (n n) (r (fsize fun n)) )
	 (if (null? l)
	     (max n r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::bint e::ev_abs n::bint);
   (with-access::ev_abs e (arity vars body)
      (let ( (nn (fsize body (length vars))) )
	 (ev_abs-size-set! e nn)
	 n )))

;;
;; States
;;
(define-struct mcell value)

(define **size-stack** (* 8 1024))

(define (make-state)
   (let ( (stk (make-vector **size-stack** "")) )
      (vector-set! stk 0 2)
      stk ))
   
(define (find-state)
   (let ( (s ($evmeaning-evstate (current-dynamic-env))) )
      (if (not (vector? s))
	  (let ( (state (make-state)) )
	     ($evmeaning-evstate-set! (current-dynamic-env) state)
	     state )
	  (begin
	     (tprint "find-state: " (vector-length s))
	     s ))))

(define (push-state s)
   (let ( (stk (make-vector **size-stack** "")) )
      (vector-set! s 1 stk)
      (vector-set! s 0 2)
      stk ))
   

(define (get-evaluation-contexte)
   (let ( (s (find-state)) )
      (let ( (bp (vector-ref s 0)) )
	 (let ( (r (make-vector bp "")) )
	    (let rec ( (i 0) )
	       (when (<fx i bp)
		  (vector-set! r i (vector-ref s i))
		  (rec (+fx i 1)) ))
	    r ))))

(define (set-evaluation-contexte! v)
   (let ( (s (find-state)) )
      (let ( (bp (vector-ref v 0)) )
	 (let rec ( (i 0) )
	    (when (<fx i bp)
	       (vector-set! s i (vector-ref v i))
	       (rec (+fx i 1)) )))))

(define (dump s bp size)
   (if (not (vector? s))
       "INVALID STATE"
       (let rec ( (i 0) )
	  (if (<fx i size)
	      (let ( (sp (+fx bp i)) )
		 (cons
		  (if (or (<fx sp 0) (>=fx sp (vector-length s)))
		      'OUT
		      (dvalue (vector-ref s sp) (bigloo-debug)) )
		  (rec (+fx i 1)) ))
	      '() ))))

(define (dvalue v level)
   (if (=fx level 1)
       (get-type v)
       (let ( (level (-fx level 1)) )
	  (cond
	     ((or (string? v) (symbol? v) (number? v))
	      v )
	     ((mcell? v)
	      (cons "#" (dvalue (mcell-value v) level)) )
	     ((pair? v)
	      (cons (dvalue (car v) level) (dvalue (cdr v) level)) )
	     ((vector? v)
	      (let ( (n (vector-length v)) )
		 (let ( (r (make-vector n "")) )
		    (let rec ( (i 0) )
		       (when (<fx i n)
			  (vector-set! r i (dvalue (vector-ref v i) level)) ))
		    r )))
	      (else (get-type v)) ))))

(define (get-type v);
   (cond ((or (string? v) (symbol? v) (number? v))
	  "A" )
	 ((pair? v)
	  "P" )
	 ((vector? v)
	  "V" )
	 ((procedure? v)
	  "F" )
	 (else "?") ))

;;
;; User functions
;;

;;
;; Ast back to list
;;
(define (uncompile e::ev_expr);
   (uncomp e) )

(define-generic (uncomp e::ev_expr);
   (error "eval" "internal error: not defined for" e) )

(define-method (uncomp var::ev_var);
   (ev_var-name var) )

(define-method (uncomp var::ev_global);
   (ev_global-name var) )

(define-method (uncomp e::ev_litt);
   `',(ev_litt-value e) )

(define-method (uncomp e::ev_if);
   (with-access::ev_if e (p t e)
      `(if ,(uncomp p) ,(uncomp t) ,(uncomp e)) ))

(define-method (uncomp e::ev_or);
   (with-access::ev_or e (args)
      `(or ,@(map uncomp args)) ))

(define-method (uncomp e::ev_and);
   (with-access::ev_and e (args)
      `(and ,@(map uncomp args)) ))

(define-method (uncomp e::ev_prog2);
   (with-access::ev_prog2 e (e1 e2)
      (let ( (e1 (uncomp e1)) (e2 (uncomp e2)) )
	 (if (and (pair? e2) (eq? (car e2) 'begin))
	     `(begin ,e1 ,@(cdr e2))
	     `(begin ,e1 ,e2) ))))

(define-method (uncomp e::ev_trap);
   (with-access::ev_trap e (e)
      `(trap ,(uncomp e)) ))

(define-method (uncomp e::ev_setglobal);
   (with-access::ev_setglobal e (name e)
      `(set! ,name ,(uncomp e)) ))

(define-method (uncomp e::ev_defglobal);
   (with-access::ev_defglobal e (name e)
      `(define ,name ,(uncomp e)) ))

(define-method (uncomp e::ev_setlocal);
   (with-access::ev_setlocal e (v e)
      `(set! ,(uncomp v) ,(uncomp e)) ))
      
(define-method (uncomp e::ev_bind-exit);
   (with-access::ev_bind-exit e (var body)
      `(bind-exit (,(uncomp var)) ,(uncomp body)) ))
      
(define-method (uncomp e::ev_unwind-protect);
   (with-access::ev_unwind-protect e (e body)
      `(unwind-protect ,(uncomp e) ,(uncomp body)) ))
      
(define-method (uncomp e::ev_with-handler);
   (with-access::ev_with-handler e (handler body)
      `(with-handler ,(uncomp handler) ,(uncomp body)) ))

(define-method (uncomp e::ev_let);
   (with-access::ev_let e (vars vals body)
      `(let ,(map (lambda (v a) `(,(uncomp v) ,(uncomp a))) vars vals) ,(uncomp body)) ))

(define-method (uncomp e::ev_let*);
   (with-access::ev_let* e (vars vals body)
      `(let* ,(map (lambda (v a) `(,(uncomp v) ,(uncomp a))) vars vals) ,(uncomp body)) ))

(define-method (uncomp e::ev_letrec);
   (with-access::ev_letrec e (vars vals body)
      `(letrec ,(map (lambda (v a) `(,(uncomp v) ,(uncomp a))) vars vals) ,(uncomp body)) ))

(define-method (uncomp e::ev_app);
   (with-access::ev_app e (fun args)
      `(,(uncomp fun) ,@(map uncomp args)) ))

(define-method (uncomp e::ev_abs);
   (with-access::ev_abs e (arity vars body)
      (define (redovars n l)
	 (if (>=fx n 0)
	     l
	     (let rec ( (l l) )
		(if (null? (cdr l))
		    (car l)
		    (cons (car l) (rec (cdr l))) ))))
      `(lambda ,(redovars arity (map ev_var-name vars)) ,(uncomp body)) ))

;;
;; Compute the vars used by an expression
;;
(define-generic (use e::ev_expr done);
   (error "eval" "internal error: not defined for" e) )

(define (use* l done)
   (if (null? l)
       done
       (use* (cdr l) (use (car l) done)) ))

(define-method (use var::ev_var done);
   (unless (memq var done) (set! done (cons var done)))
   done )

(define-method (use var::ev_global done);
   done )

(define-method (use e::ev_litt done);
   done )

(define-method (use e::ev_if done);
   (with-access::ev_if e (p t e)
      (use p (use t (use e done))) ))

(define-method (use e::ev_list done);
   (with-access::ev_list e (args)
      (use* args done) ))

(define-method (use e::ev_prog2 done);
   (with-access::ev_prog2 e (e1 e2)
      (use e1 (use e2 done)) ))

(define-method (use e::ev_hook done);
   (with-access::ev_hook e (e)
      (use e done) ))

(define-method (use e::ev_setlocal done);
   (with-access::ev_setlocal e (v e)
      (unless (memq v done) (set! done (cons v done)))
      (use e done) ))
      
(define-method (use e::ev_bind-exit done);
   (with-access::ev_bind-exit e (body)
      (use body done) ))
      
(define-method (use e::ev_unwind-protect done);
   (with-access::ev_unwind-protect e (e body)
      (use e (use body done)) ))
      
(define-method (use e::ev_with-handler done);
   (with-access::ev_with-handler e (handler body)
      (use handler (use body done)) ))

(define-method (use e::ev_binder done);
   (with-access::ev_binder e (vals body)
      (use body (use* vals done)) ))

(define-method (use e::ev_app done);
   (with-access::ev_app e (fun args)
      (use fun (use* args done)) ))

(define-method (use e::ev_abs done);
   (with-access::ev_abs e (body)
      (use body done) ))

;;
;; Compile Ast to closures
;;
(define (compile e::ev_expr)
   (comp e '()) )

(define-generic (comp e::ev_expr stk)
   (error "eval" "internal error: not defined for" e) )

(define-macro (EVA path step_args . body)
   `(lambda (s)
       (let ( (bp (vector-ref s 0)) )
	  (prof (cons 'node ,path))
	  (step s bp (length stk) ',path ,@step_args)
	  ,@body )))

(define-macro (EVC f)
   `(,f s) )

(define-method (comp e::ev_trap stk)
   (with-access::ev_trap e (e)
      (let ( (e (comp e stk)) )
	 (EVA '(trap) ()
	    (let ( (cmd (EVC e)) )
	       (case cmd
		  ((reset_profile)
		   (reset_profile) )
		  ((get_profile)
		   (get_profile) )
		  ((reset_stack)
		   (vector-set! (find-state) 0 2) )
		  (else (error "eval" "internal error: unknown command" cmd)) ))))))

(define-macro (generate-fix-index . indexes)
   (define (doit i)
      `(if eff
	   (EVA `(var read cell ,i) ((ev_var-name var))
	      (mcell-value (vector-ref s (+fx bp ,i))) )
	   (EVA `(var read direct ,i) ((ev_var-name var))
		(vector-ref s (+fx bp ,i)) )))
   `(case i
      ,@(map (lambda (i) `((,i) ,(doit i))) indexes)
      (else ,(doit 'i)) ))

(define-method (comp var::ev_var stk);
   (let ( (i (_index var stk)) )
      (with-access::ev_var var (eff)
	 (generate-fix-index 0 1 2 3) )))

(define-method (comp e::ev_setlocal stk);
   (with-access::ev_setlocal e (v e)
      (let ( (i (_index v stk)) (e (comp e stk)) )
	 (with-access::ev_var v (eff)
	    (if eff
		(EVA '(var write cell) ((ev_var-name v) i)
		     (mcell-value-set! (vector-ref s (+fx bp i)) (EVC e)) )
		(EVA '(var write direct) ((ev_var-name v) i)
		     (vector-set! s (+fx bp i) (EVC e))) )))))

(define-method (comp var::ev_global stk);
   (with-access::ev_global var (name mod loc)
      (let ( (g (evmodule-find-global mod name)) )
	 (if g
	     (if (eq? (eval-global-tag g) 1)
		 (EVA '(global read cell) (name)
		      (__evmeaning_address-ref (eval-global-value g)) )
		 (EVA '(global read direct) (name)
		      (eval-global-value g) ))
	     (let ( (slot #f) )
		(EVA '(global read check) (name)
		     (unless slot
			(set! slot (evmodule-find-global mod name))
			(unless slot (evmeaning-error loc "eval" "unbound variable" name)) )
		     (eval-global-value slot) ))))))

(define-method (comp e::ev_setglobal stk);
   (with-access::ev_setglobal e (name mod e loc)
      (let ( (g (evmodule-find-global mod name)) (e (comp e stk)) )
	 (if g
	     (if (eq? (eval-global-tag g) 1)
		 (EVA '(global write cell) (name)
		    (__evmeaning_address-set! (eval-global-value g) (EVC e)) )
		 (EVA '(global write direct) (name)
		    (set-eval-global-value! g (EVC e)) ))
	     (let ( (slot #f) )
		(EVA '(global write check) (name)
		     (unless slot
			(set! slot (evmodule-find-global mod name))
			(unless slot (evmeaning-error loc "eval" "unbound variable" name)) )
		     (set-eval-global-value! slot (EVC e)) ))))))

(define-method (comp e::ev_defglobal stk);
   (with-access::ev_defglobal e (name mod e loc)
      (let ( (e (comp e stk)) )
	 (EVA '(global write define) (name)
	    (let ( (g (evmodule-find-global mod name)) )
	       (if g
		   (begin
		      (if (eq? (eval-global-tag g) 1)
			  (__evmeaning_address-set! (eval-global-value g) (EVC e))
			  (begin
			     (set-eval-global-value! g (EVC e))
			     (when (and (eq? (eval-global-tag g) 0)
					(bigloo-eval-strict-module))
				(evmeaning-warning loc
						   'set! "Setting compiled read-only variable "
						   name
						   " can yield to incoherent state"))) )
		      name )
		   (let ( (g (vector 2 name (EVC e))) )
		      (evmodule-bind-global! mod name g)
		      name )))))))

(define-method (comp e::ev_litt stk);
   (with-access::ev_litt e (value)
      (EVA '(quote) (value)
	   value) ))
   
(define-method (comp e::ev_if stk);
   (with-access::ev_if e (p t e)
      (let ( (p (comp p stk)) (t (comp t stk)) (e (comp e stk)) )
	 (EVA '(control if) ()
	      (if (EVC p) (EVC t) (EVC e)) ))))
   
(define-method (comp e::ev_or stk);
   (with-access::ev_or e (args)
      (let ( (args (map (lambda (e) (comp e stk)) args)) )
	 (EVA '(control or) ()
	      (let rec ( (l args) )
		 (if (null? l)
		     #f
		     (or (EVC (car l)) (rec (cdr l))) ))))))
   
(define-method (comp e::ev_and stk);
   (with-access::ev_and e (args)
      (let ( (args (map (lambda (e) (comp e stk)) args)) )
	 (EVA '(control and) ()
	      (let rec ( (l args) (r #t) )
		 (if (null? l)
		     r
		     (let ( (v (EVC (car l))) )
			(and v (rec (cdr l) v)) )))))))

(define-method (comp e::ev_prog2 stk);
   (with-access::ev_prog2 e (e1 e2)
      (let ( (e1 (comp e1 stk)) (e2 (comp e2 stk)) )
	 (EVA '(control begin) ()
	      (begin (EVC e1) (EVC e2)) ))))

(define-method (comp e::ev_let stk);
   (with-access::ev_let e (vars vals boxes body)
      (let ( (size (length stk)) (nstk (append stk vars)) )
	 (let ( ;(vals (map (lambda (v) (comp v nstk)) vals))
	       (vals (comp-with-push vals stk))
		(iboxes (map (lambda (v) (_index v nstk)) boxes))
		(body (comp body nstk)) )
	    (EVA '(binder let) ()
		 (let rec ( (l vals) (i (+fx bp size)) )
		    (unless (null? l)
		       (vector-set! s i (EVC (car l)))
		       (rec (cdr l) (+fx i 1)) ))
		 (let rec ( (l iboxes) )
		    (unless (null? l)
		       (let ( (i (+fx bp (car l))) )
			  (vector-set! s i (mcell (vector-ref s i))) )
		       (rec (cdr l)) ))
		 (EVC body) )))))

(define-method (comp e::ev_let* stk);
   (with-access::ev_let* e (vars vals boxes body)
      (let ( (size (length stk)) (nstk (append stk vars)) )
	 (let ( (vals (map (lambda (v) (comp v nstk)) vals))
		(body (comp body nstk)) )
	    (EVA '(binder let*) ()
		 (let rec ( (l vals) (i (+fx bp size)) (b* boxes) )
		    (unless (null? l)
		       (let ( (val (EVC (car l))) )
			  (vector-set! s i (if (car b*) (mcell val) val)) )
		       (rec (cdr l) (+fx i 1) (cdr b*)) ))
		 (EVC body) )))))

(define-method (comp e::ev_letrec stk);
   (with-access::ev_letrec e (vars vals body)
      (let ( (size (length stk)) (nstk (append stk vars)) )
	 (let ( (cvals (map (lambda (v) (comp v nstk)) vals))
		(body (comp body nstk)) )
	    (if (every? ev_abs? vals)
		(EVA '(binder letrec fun) ()
		     (let rec ( (l cvals) (i (+fx bp size)) )
			(unless (null? l)
			   (vector-set! s i (mcell #unspecified))
			   (rec (cdr l) (+fx i 1)) ))
		     (let rec ( (l cvals) (i (+fx bp size)) )
			(unless (null? l)
			   (mcell-value-set! (vector-ref s i) (EVC (car l)))
			   (rec (cdr l) (+fx i 1)) ))
		     (EVC body) )
		(EVA '(binder letrec data) ()
		     (let rec ( (l cvals) (i (+fx bp size)) )
			(unless (null? l)
			   (vector-set! s i (mcell #unspecified))
			   (rec (cdr l) (+fx i 1)) ))
		     (let ( (rvals (map (lambda (v) (EVC v)) cvals)) )
			(let rec ( (l rvals) (i (+fx bp size)) )
			   (unless (null? l)
			      (mcell-value-set! (vector-ref s i) (car l))
			      (rec (cdr l) (+fx i 1)) ))
			(EVC body) )))))))

(define-method (comp e::ev_bind-exit stk);
   (with-access::ev_bind-exit e (var body)
      (let ( (size (length stk)) (nstk (append stk (cons var '()))) )
	 (let ( (body (comp body nstk)) (eff? (ev_var-eff var)) )
	    (EVA '(binder bind-exit) ()
		 (bind-exit (exit)
		    (vector-set! s (+fx bp size) (if eff? (mcell exit) exit))
		    (EVC body) ))))))

(define-method (comp e::ev_unwind-protect stk);
   (with-access::ev_unwind-protect e (e body)
      (let ( (e (comp e stk)) (body (comp body stk)) )
	 (EVA '(exception unwind-protect) ()
	      (let ( (saved-bp bp) )
		 (unwind-protect (EVC e)
				 (begin (vector-set! s 0 saved-bp)
					(EVC body)) ))))))

(define-method (comp e::ev_with-handler stk);
   (with-access::ev_with-handler e (handler body)
      (let ( (handler (comp handler stk)) (body (comp body stk)) )
	 (EVA '(exception with-handler) ()
	    (let ( (h (EVC handler)) )
	       (with-handler h (EVC body)) )))))

;;
;; Call
;;
(define-struct bounce)
(define **a-bounce** (bounce))

(define-inline (throw-trampoline f);
   f )

(define (catch-trampoline f s bp)
   (let ( (saved-bp (vector-ref s 0)) )
      (vector-set! s 0 bp)
      (let rec ( (f f) )
	 (let ( (r (f s)) )
	    (if (and (procedure? r) (bounce? (procedure-attr r)))
		(rec r)
		(begin (vector-set! s 0 saved-bp)
		       r ))))))

;;
;; Call
;;
;;
(define-struct user arity runner frame)

(define (check-stack s bp n)
   (<fx (+fx bp n) (vector-length s)) )

(define (comp-with-push e* stk)
   ;; We can save one cons with the last expression
   (if (null? e*)
       '()
       (cons (comp (car e*) stk)
	     (comp-with-push (cdr e*) (append stk (cons #f '()))) )))

(define (comp-in-place l stk n)
   (let rec ( (l l) (i n) )
      (cond ((null? l) '())
	    ((=fx i 0) (comp-with-push l stk))
	    (else (cons (comp (car l) stk)
			(rec (cdr l) (-fx i 1)) )))))

(define (subr-call-with-push s f args bp size)
   ;; CARE recheck the sp based version
   (let ( (args (map (lambda (a) (EVC a)) args)) )
      (vector-set! s 0 (+fx bp size))
      (prog1 (apply f args)
	     (vector-set! s 0 bp) )) )

(define (push-args-on-sp s args sp)
   (let rec ( (l args) (sp sp) )
      (unless (null? l)
	 (vector-set! s sp (EVC (car l)))
	 (rec (cdr l) (+fx sp 1)) )))

(define (push-nargs-on-sp arity s args sp)
   (let rec ( (l args) (sp sp) (n (-fx -1 arity)) )
      (if (=fx n 0)
	  (let rec2 ( (l l) (r '()) )
	     (if (null? l)
		 (vector-set! s sp (reverse! r))
		 (rec2 (cdr l) (cons (EVC (car l)) r)) ))
	  (begin (vector-set! s sp (EVC (car l)))
		 (rec (cdr l) (+fx sp 1) (-fx n 1)) ))))

(define-macro (comp-call-pattern pn tail? comp-call subr-call fix-expr-call notfix-expr-call);
   `(let ( (args ,comp-call) )
       (EVA ,pn ()
	    (let ( (f (EVC f)) )
	       (unless (procedure? f)
		  (loc-type-error "eval" "procedure" f loc) )
	       (let ( (uf (procedure-attr f)) )
		  (if (not (user? uf))
		      (begin
			 (prof '(call int subr))
			 (if (not (correct-arity? f nbargs))
			     (evmeaning-arity-error loc 'which-name? nbargs ($procedure-arity f))
			     ,subr-call ))
		      (let ( (arity (user-arity uf)) (run (user-runner uf)) (sf (user-frame uf)) )
			 (prof '(call int int))
			 (let ( (sp (+fx bp size)) )
			    (if (=fx arity nbargs)
				,fix-expr-call
				(if (or (>=fx arity 0) (<fx arity (-fx -1 nbargs)))
				    (error 'eval "arity mismatch" (list arity nbargs))
				    ,notfix-expr-call ))
			    (unless (check-stack s ,(if tail? 'bp 'sp) sf)
			       (error 'eval "stack overflow" bp) )
			    (let ( (!denv::dynamic-env (current-dynamic-env)) )
			       ($env-set-trace-location !denv loc) )
			    ,(if tail?
				 '(throw-trampoline run)
				 '(catch-trampoline run s sp) )))))))))

(define (need-shift args stk)
   ;; CARE must memorize the results of use.
   (let rec ( (l args) (stk stk) )
      (cond
	 ((or (null? l) (null? stk))
	  #f )
	 ((any? (lambda (e) (memq (car stk) (use e '()))) (cdr l))
	  #t )
	 (else (rec (cdr l) (cdr stk))) )))

(define (comp-old-call e stk)
   (with-access::ev_app e (loc fun args tail?)
      (let ( (size (length stk)) (nbargs (length args)) (f (comp fun stk)) )
	 (cond
	    ((not tail?)
	     (comp-call-pattern `(call nottail ,nbargs) #f
				(comp-with-push args stk)
				(subr-call-with-push s f args bp size)
				(push-args-on-sp s args sp)
				(push-nargs-on-sp arity s args sp) ))
	    ((need-shift args stk)
	     (comp-call-pattern `(call tail shift ,nbargs) #t
				(comp-with-push args stk)
				(subr-call-with-push s f args bp size)
				(begin (push-args-on-sp s args sp)
				       (vector-copy! s bp s sp (+fx sp nbargs)) )
				(begin (push-nargs-on-sp arity s args sp)
				       (vector-copy! s bp s sp (+fx sp (-fx 0 arity))) )))
	    (else
	     (comp-call-pattern `(call tail direct ,nbargs) #t
				(comp-in-place args stk size)
				(subr-call-with-push s f args bp nbargs)
				(push-args-on-sp s args bp)
				(push-nargs-on-sp arity s args bp) ))))))

;; specialized version for a fixed number of arguments
(define-macro (generate-comp-calli-body tail? args)
   (define (push-args l sp)
      (map (lambda (v i) `(vector-set! s (+fx ,sp ,i) ,v)) l (iota (length l) 0)) )
   (define (bpush-args l sp)
      `(begin ,@(push-args l sp)) )
   
   (define (split n l)
      ;; (split 2 '(a1 a2 a3 a4)) -> (a1 a2 (list a3 a4))
      (if (=fx n 0)
	  `((list ,@l))
	  (cons (car l) (split (-fx n 1) (cdr l))) ))
   (define (generate-case-neg-arity from to args sp)
      (if (=fx from to)
	  '()
	  (cons `((,from) ,(bpush-args (split (-fx -1 from) args) sp))
		(generate-case-neg-arity (-fx from 1) to args sp) )))
   (let ( (nbargs (length args)) )
      `(EVA '(call ,@(if tail? '(tail direct) '(nottail)) ,nbargs) ("nb args" ,nbargs "size" size)
	    (let ( (f (EVC fun)) ,@(map (lambda (v) `(,v (EVC ,v))) args) )
	       (unless (procedure? f)
		  (error 'eval "not a procedure" f) )
	       (let ( (uf (procedure-attr f)) )
		  (if (not (user? uf))
		      (begin
			 (prof '(call int subr))
			 (if (not (correct-arity? f ,nbargs))
			     (evmeaning-arity-error loc 'which-name? ,nbargs ($procedure-arity f))
			     (let ( (nbp (+fx bp size)) )
				(vector-set! s 0 nbp)
				(let ( (r (f ,@args)) )
				   (vector-set! s 0 bp)
				   r ))))
		      (let ( (arity (user-arity uf)) (run (user-runner uf)) (sf (user-frame uf)) )
			 (let ( (sp (+fx bp size)) )
			    (prof '(call int int))
			    (if (=fx arity ,nbargs)
				,(bpush-args args (if tail? 'bp 'sp))
				(case arity
				   ,@(generate-case-neg-arity -1 (-fx -2 nbargs) args (if tail? 'bp 'sp))
				   (else
				    (error 'eval "wrong number of arguments" (cons arity ,nbargs)) )))
			    (unless (check-stack s ,(if tail? 'bp 'sp) sf)
			       (error 'eval "stack overflow" bp) )
			    (let ( (!denv::dynamic-env (current-dynamic-env)) )
			       ($env-set-trace-location !denv loc) )
			    ,(if tail?
				 '(throw-trampoline run)
				 '(catch-trampoline run s sp) )

			 ))))))))

(define-macro (generate-comp-calli from to)
   (if (>fx from to)
       '#unspecified
       (let ( (sn (string->symbol (integer->string from)))
	      (args (map (lambda (i) (symbol-append 'a (string->symbol (integer->string i))))
			    (iota from 1) )))
	  `(begin (define (,(symbol-append 'comp-call sn) loc fun tail? stk size ,@args)
		     (if tail?
			 (generate-comp-calli-body #t ,args)
			 (generate-comp-calli-body #f ,args) ))
		  (generate-comp-calli ,(+fx 1 from) ,to) ))))

(generate-comp-calli 0 4)

;; inline
(define-macro (inline fun* loc fun vars stk);
   (let rec ( (l fun*) )
      (if (null? l)
	  '#f
	  (let* ( (slot (car l)) (type (car slot)) (pred (cadr slot)) )
	     (let rec2 ( (names (cddr slot)) )
		(if (null? names)
		    (rec (cdr l))
		    (let ( (fname (car names)) )
		       `(if (eq? ,fun ,fname)
			    (let ,(map (lambda (v) `(,v (comp ,v ,stk))) vars)
			       (EVA '(call inline ,fname) (,(symbol->string fname))
				    (let ,(map (lambda (v) `(,v (EVC ,v))) vars)
				       ,@(if pred
					     (map (lambda (v)
						     `(unless (,pred ,v)
							 (loc-type-error ',fname ',type ,v ,loc) ))
						  vars )
					     '() )
				       (,fname ,@vars) )))
			    ,(rec2 (cdr names)) ))))))))

(define (inline-call loc fun args stk);
   (when (and (ev_global? fun) (bigloo-eval-strict-module))
      (with-access::ev_global fun (name mod)
	 (let ( (g (evmodule-find-global mod name)) )
	    (when g
	       (let ( (val (eval-global-value g)) (n (length args)) )
		  (or (when (=fx n 1)
			 (let ( (a1 (car args)) )
			    (inline ((pair pair? car cdr) (pair cadr? cadr)) loc val (a1) stk) ))
		      (when (=fx n 2)
			 (let ( (a1 (car args)) (a2 (cadr args)) )
			    (inline ((number number? + - * / < > <= >= =
					     +fx -fx *fx /fx <fx >fx <=fx >=fx =fx)
				     (#f #f eq? cons)) loc val (a1 a2) stk ))))))))))

(define (cadr? l)
   (and (pair? l) (pair? (cdr l))) )

;; main method for application
(define-macro (comp-dispatch cur max vars)
   (let ( (sn (string->symbol (integer->string cur))) )
      (let ( (fn (symbol-append 'comp-call sn)) (an (symbol-append 'a sn)) )
	 (if (>fx cur max)
	     '(error 'eval "internal error" "must be dead code")
	     (let ( (vars (cons an vars)) )
		`(let ( (,an (comp (car args) stk)) (args (cdr args)) )
		    (if (null? args)
			(,fn loc f tail? stk size ,@(reverse vars))
			(comp-dispatch ,(+fx cur 1) ,max ,vars) )))))))

(define-method (comp e::ev_app stk);
   (with-access::ev_app e (loc fun args tail?)
      (or (inline-call loc fun args stk)
	  (if (>fx (length args) 4)
	      (comp-old-call e stk)
	      (let ( (f (comp fun stk)) (size (length stk)) )
		 (if (null? args)
		     (comp-call0 loc f tail? stk size)
		     (comp-dispatch 1 4 ()) ))))))

;;
;; Abstractions
;; 
(define (correct n done)
   (if (<fx n 0)
       (+fx n done)
       (-fx n done) ))

(define (bind-frame s sp arity vals)
   (if (<fx arity 0)
       (let rec ( (r (-fx -1 arity)) (sp sp) (l vals) )
	  (cond
	     ((=fx r 0)
	      (vector-set! s sp l) )
	     ((not (pair? l))
	      (error 'eval "not enought arguments" r) )
	     (else
	      (vector-set! s sp (car l))
	      (rec (-fx r 1) (+fx sp 1) (cdr l)) )))
       (let rec ( (r arity) (sp sp) (l vals) )
	  (cond
	     ((=fx r 0)
	      (unless (null? l)
		 (error 'eval "too much argument" l) ))
	     ((not (pair? l))
	      (error 'eval "not enought arguments" r) )
	     (else
	      (vector-set! s sp (car l))
	      (rec (-fx r 1) (+fx sp 1) (cdr l)) )))))

(define-macro (generate-abstraction param extra arity hasfree hasbox)
   (let ( (nbparam (length param)) )
      `(EVA '(lambda ,arity) ("nbfree " (vector-length ifrees))
	    (prof (list 'closing (vector-length ifrees)))
	    (let ( ,@(if hasfree `((val* (free-collect s bp ifrees))) '()) )
	       (let ( (run (EVA '(entry) ("param" (map lname vars)
				         "stk " (map lname nstk)
					 "nbbox " iboxes
					 "nbfree " ifrees)
				,@(if hasbox `((make-boxes s iboxes bp)) '())
				,@(if hasfree '((free-restore s val* (+fx bp size))) '())
				(step s bp (length nstk) '(entry2))
				(let ( (!denv::dynamic-env (current-dynamic-env)) (where where) (loc loc) )
				   (let ()
				      ($env-push-trace !denv where loc)
				      (prog1 (EVC body)
					     ($env-pop-trace !denv) )))) ))
		  (let ( (main (lambda (,@param . ,extra)
				  (prof '(call subr int))
				  (let ( (s (find-state)) )
				     (let ( (bp (vector-ref s 0)) )
					(step s bp 0 '(entry main))
					,@(map (lambda (i v) `(vector-set! s (+fx bp ,i) ,v))
					       (iota nbparam 0)
					       param )
					(step s bp ,nbparam '(entry main cont))
					,@(cond
					     ((null? extra) '())
					     ((eq? extra 'last) `((vector-set! s (+fx bp ,nbparam) last)))
					     (else `((bind-frame s (+ bp ,nbparam)
								 (correct ,arity ,nbparam)
								 ,extra))) )
					(step s bp size '(entry main cont2))
					(unwind-protect (catch-trampoline run s bp)
							(vector-set! s 0 bp) ))))) )
		     (procedure-attr-set! run **a-bounce**)
		     (procedure-attr-set! main (user ,arity run size-frame))
		     main ))))))

(define-macro (generate-generate-abstraction param extra arity);
   `(if (=fx (vector-length ifrees) 0)
	(if (=fx (vector-length iboxes) 0)
	    (generate-abstraction ,param ,extra ,arity #f #f)
	    (generate-abstraction ,param ,extra ,arity #f #t) )
	(if (=fx (vector-length iboxes) 0)
	    (generate-abstraction ,param ,extra ,arity #t #f)
	    (generate-abstraction ,param ,extra ,arity #t #t) )))

(define-macro (generate-case-arity params)
   (define (gen1 l)
      (let ( (n (length l)) )
	 (let ( (nn (-fx -1 n)) )
	    `(((,n) (generate-generate-abstraction ,l () ,n))
	      ((,nn) (generate-generate-abstraction ,l last ,nn)) ))))
   (define (cases l)
      (if (null? l)
	  (gen1 l)
	  (append (cases (cdr l)) (gen1 l)) ))
   `(case arity
       ,@(cases params)
       (else (generate-generate-abstraction ,params rest arity)) ))

(define-method (comp e::ev_abs stk);
   (with-access::ev_abs e (loc where arity vars free boxes body size)
      (let ( (ifrees (list->vector (map (lambda (v) (_index v stk)) free)))
	     (iboxes (list->vector (map (lambda (v) (_index v vars)) boxes)))
	     (size (length vars))
	     (size-frame (+fx size (length free)))
	     (nstk (append vars free)) )
	 (let ( (body (comp body nstk)) )
	    (generate-case-arity (a b c d)) ))))

(define (make-boxes s ib* bp)
   (let ( (n (vector-length ib*)) )
      (let rec ( (i 0) )
	 (let ( (sp (+fx bp (vector-ref ib* i))) )
	    (vector-set! s sp (mcell (vector-ref s sp)))
	    (let ( (ni (+fx i 1)) )
	       (when (<fx ni n) (rec ni)) )))))

(define (free-collect s bp i*)
   (let* ( (n (vector-length i*)) (r (make-vector n)) )
      (let rec ( (i 0) )
	 (if (>=fx i n)
	     r
	     (begin (prof '(lambda_collect))
		    (vector-set! r i (vector-ref s (+fx bp (vector-ref i* i))))
		    (rec (+fx i 1)) )))))

(define (free-restore s val* sp)
   (let ( (stop (+fx sp (vector-length val*))) )
      (let rec ( (i 0) (sp sp) )
	 (when (<fx sp stop)
	    (vector-set! s sp (vector-ref val* i))
	    (rec (+fx i 1) (+fx sp 1)) ))))


