;*=====================================================================*/
;*    .../project/bigloo/bigloo/runtime/Eval/evaluate_comp.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Bernard Serpette                                  */
;*    Creation    :  Tue Feb  8 16:49:34 2011                          */
;*    Last change :  Fri Mar 20 08:23:48 2020 (serrano)                */
;*    Copyright   :  2011-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compile AST to closures                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate_comp

   (extern (macro $evmeaning-evstate::obj (::dynamic-env)
		  "BGL_ENV_EVSTATE")
	   (macro $evmeaning-evstate-set!::obj (::dynamic-env ::obj)
		  "BGL_ENV_EVSTATE_SET"))
   
   (java   (class foreign
	      (method static $evmeaning-evstate::obj (::dynamic-env)
		      "BGL_ENV_EVSTATE")
	      (method static $evmeaning-evstate-set!::obj (::dynamic-env ::obj)
		      "BGL_ENV_EVSTATE_SET")))

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
	    __srfi4
	    
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
	    
	    __evaluate_types
	    __evaluate_uncomp
	    __evaluate_use)

   (export (find-state)
	   (compile ::ev_expr)))

;;
;; Macros
;;
(define-macro (prog1 e . l)
   `(let ( (**res** ,e) )
       (begin ,@l
	      **res** )))

(define-macro (unwind-protect-state expr denv state)
   `(let ( (e ($get-exitd-top)) )
       ((@ exitd-push-protect! __bexit) e ,state)
       (let ( (____r ,expr) )
	   (begin
	      ((@ exitd-pop-protect! __bexit) e)
	      ($evmeaning-evstate-set! ,denv ,state)
	      ____r ))))

(define-macro (unwind-protect-bp expr s bp)
   ;; see runtime __bexit
   `(let ( (e ($get-exitd-top)) )
      ((@ exitd-push-protect! __bexit) e ,bp)
      (let ( (acc ,expr) )
        (begin
          ((@ exitd-pop-protect! __bexit) e)
          (vector-set! ,s 0 ,bp)
          acc ))))

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

(define (lname v)
   (if (isa? v ev_var)
       (with-access::ev_var v (name) name)
       v ))

(define (loc-type-error f m v loc)
   (match-case loc
      ((at ?fname ?loc)
       (bigloo-type-error/location f m v fname loc) )
      (else (bigloo-type-error f m v)) ))

;;
;; States
;;
(define-struct mcell value)

(define **size-stack** (*fx 8 1024))

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
	  s )) )

(define (push-state s)
   (let ( (stk (make-vector **size-stack** "")) )
      (vector-set! s 1 stk)
      (vector-set! s 0 2)
      stk ))
   

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
;; compile
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
		  ((get_sp)
		   bp )
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

;;
(define (global-fun-value e::ev_expr);
   (when (isa? e ev_global)
      (with-access::ev_global e (name mod)
	 (let ( (g (evmodule-find-global mod name)) )
	    (when g
	       (eval-global-value g) )))))

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
		(when (evmodule? mod)
		   (let ( (g (make-eval-global name mod loc)) )
		      (eval-global-tag-set! g 3)
		      (evmodule-bind-global! mod name g loc) ))
		(EVA '(global read check) (name)
		     (unless slot
			(set! slot (evmodule-find-global mod name))
			(unless slot
			   (let ( (id (if (evmodule? mod)
					  `(@ ,name ,(evmodule-name mod))
					  name)) )
			      (everror loc "eval" "Unbound variable" id) )))
		     (let ( (v (eval-global-value slot)) )
			(if (eq? v #unspecified)
			    (let ( (t (eval-global-tag slot)) )
			       (if (or (=fx t 3) (=fx t 4))
				   (let ( (id (if (evmodule? mod)
						  `(@ ,name ,(evmodule-name mod))
						  name)) )
				      (everror loc "eval" "Uninitialized variable" id) )
				   v ))
			    v ))))))))

(define-method (comp e::ev_setglobal stk);
   (with-access::ev_setglobal e (name mod e loc)
      (let ( (g (evmodule-find-global mod name)) (e (comp e stk)) )
	 (if g
	     (case (eval-global-tag g)
		((1)
		 (EVA '(global write cell) (name)
		    (__evmeaning_address-set! (eval-global-value g) (EVC e)) ))
		((0 4 5)
		 (everror loc "set!" "read-only variable" name) )
		(else
		 (EVA '(global write direct) (name) (set-eval-global-value! g (EVC e))) ))
	     (let ( (slot #f) )
		(when (evmodule? mod)
		   (let ( (g (make-eval-global name mod loc)) )
		      (eval-global-tag-set! g 3)
		      (evmodule-bind-global! mod name g loc) ))
		(EVA '(global write check) (name)
		     (unless slot
			(set! slot (evmodule-find-global mod name))
			(unless slot (everror loc "eval" "Unbound variable" name)) )
		     (set-eval-global-value! slot (EVC e)) ))))))

(define-method (comp e::ev_defglobal stk);
   (with-access::ev_defglobal e (name mod e loc)
      (let ( (e (comp e stk)) )
	 (EVA '(global write define) (name)
	    (let ( (g (evmodule-find-global mod name)) )
	       
	       (if g
		   (begin
		      (case (eval-global-tag g)
			 ((0)
			  (everror loc "set!" "read-only variable" name) )
			 ((1)
			  (evwarning loc "eval" "\nRedefinition of compiled variable -- "
				     name)
			  (__evmeaning_address-set! (eval-global-value g) (EVC e)) )
			 ((2)
			  (set-eval-global-value! g (EVC e)) )
			 ((3)
			  (set-eval-global-value! g (EVC e))
			  (eval-global-tag-set! g 2))
			 ((4)
			  (set-eval-global-value! g (EVC e))
			  (eval-global-tag-set! g 5))
			 (else
			  (everror loc "set!" "read-only variable" name) ))
		      name )
		   (let ( (g (make-eval-global name mod loc)) )
		      (set-eval-global-value! g (EVC e))
		      (evmodule-bind-global! mod name g loc)
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
	    (if (every (lambda (v) (isa? v ev_abs)) vals)
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
	 (let ( (body (comp body nstk)) (eff? (with-access::ev_var var (eff) eff)) )
	    (EVA '(binder bind-exit) ()
		 (let ( (saved-bp bp) )
		    (prog1 (bind-exit (exit)
			      (vector-set! s (+fx bp size) (if eff? (mcell exit) exit))
			      (EVC body) )
			   (vector-set! s 0 saved-bp) )))))))

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
		 (let ( (saved-bp bp) )
		    (prog1 (with-handler h (EVC body))
			   (vector-set! s 0 saved-bp) )))))))

(define-method (comp e::ev_synchronize stk);
   (with-access::ev_synchronize e (mutex prelock body loc)
      (let ( (mutex (comp mutex stk))
	     (prelock (comp prelock stk))
	     (body (comp body stk)) )
	 (EVA '(control synchronize) ()
	      (let ( (m (EVC mutex)) )
		 (if (mutex? m)
		     (let ( (p (EVC prelock)) )
			(let ( (saved-bp bp) )
			   (prog1 (if (pair? p)
				      (synchronize m :prelock p (EVC body))
				      (synchronize m (EVC body)))
			      (vector-set! s 0 saved-bp) )))
		     (loc-type-error "synchronize" "mutex" m loc) ))))))

;;
(define-struct label-tag)
(define **a-label** (label-tag))

(define-method (comp e::ev_labels stk);
   (define (conv f)
      (procedure-attr-set! f **a-label**)
      f )
   (with-access::ev_labels e (vars vals env body (ostk stk))
      ;(print "compile labels stk=" (map uncompile stk))
      (set! ostk stk)
      (let ( (env (map (lambda (v) (cons v 'notyet)) vars)) )
	 (with-access::ev_labels e ((oenv env))
	    (set! oenv env))
	 (for-each (lambda (slot val)
		      ;(print "compile " (uncompile (car slot)) " " (uncompile (cdr val)))
		      (set-cdr! slot 
				(conv (comp (cdr val) (append stk (car val)))) ))
		   env
		   vals ))
      ;(print "compile body " (uncompile body))
      (let ( (body (comp body stk)) )
	 (EVA '(labels) ()
	      (catch-goto-trampoline body s) ))))

(define (catch-goto-trampoline f s)
   ;(print "Start goto trampoline @" (vector-ref s 0))
   (let rec ( (f f) )
      ;(print "compute for " f)
      (let ( (r (f s)) )
	 ;(print "result " r " " (and (procedure? r) (label-tag? (procedure-attr r))))
	 (if (and (procedure? r) (label-tag? (procedure-attr r)))
	     (rec r)
	     r ))))

;;
(define-method (comp e::ev_goto stk)
   (with-access::ev_goto e (label labels args loc)
      ;(print "compile goto stk" (map uncompile stk))
      (let ( (slot (assq label (with-access::ev_labels labels (env) env))) (size (length stk)) (lstk (with-access::ev_labels labels (stk) stk)) )
	 ;(print "stk at labels " (map uncompile lstk))
	 (let ( (llstk (length lstk)) (substk (sub-stk lstk stk)) (boxes (cdr (assq label (with-access::ev_labels labels (boxes) boxes)))) )
	    ;(print "sub-stk " (map uncompile substk))
	    (if (need-shift args substk)
		(let ( (args (comp-with-push args stk)) (nbargs (length args)) )
		   (EVA (goto shift) ()
			(let ( (sp (+fx bp size)) (lsp (+fx bp llstk)) )
			   (push-boxes-args-on-sp s args boxes sp loc)
			   (vector-copy! s lsp s sp (+fx sp nbargs))
			   (throw-goto-trampoline (cdr slot)) )))
		(let ( (args (comp-in-place args stk (-fx size llstk))) )
		   (EVA (goto direct) ()
			(push-boxes-args-on-sp s args boxes (+fx bp llstk) loc)
			(throw-goto-trampoline (cdr slot)) )))))))

(define (push-boxes-args-on-sp s args boxes sp loc)
   (let rec ( (l args) (bs boxes) (sp sp) )
      (if (null? l)
	  (unless (null? bs)
	     (everror loc "eval" "wrong number of argument"
		(format "expecting ~a, got ~a"
		   (length boxes) (length args))))
	  (if (null? bs)
	      (everror loc "eval" "wrong number of argument"
		 (format "expecting ~a, got ~a"
		    (length boxes) (length args)))
	      (begin
		 (vector-set! s sp
		    (let ( (v (EVC (car l))) )
		       (if (car bs)
			   (mcell v)
			   v )))
		 (rec (cdr l) (cdr bs) (+fx sp 1)) )))))

(define (throw-goto-trampoline f)
   f )

(define (sub-stk l1 l2)
   (cond ((null? l1) l2)
	 ((not (eq? (car l1) (car l2)))
	  (error "eval" 'internal 'impossible) )
	 (else (sub-stk (cdr l1) (cdr l2))) ))

;;
;; Call
;;
(define-struct bounce)
(define **a-bounce** (bounce))

(define-inline (throw-trampoline f);
   f )

;(define-macro (throw-trampoline f);
;   `(,f s) )

;(define-inline (catch-trampoline f s bp)
;   (let ( (saved-bp (vector-ref s 0)) )
;      (vector-set! s 0 bp)
;      (prog1 (f s)
;	     (vector-set! s 0 saved-bp) )))

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
(define-struct user arity runner frame where)

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

(define-macro (comp-call-pattern/location location pn tail? comp-call subr-call fix-expr-call notfix-expr-call);
   `(let ( (args ,comp-call) )
       (EVA ,pn ()
	  (let ( (f (EVC f)) )
	     (unless (procedure? f)
		(evtype-error loc "eval" "procedure" f) )
	     (let ( (uf (procedure-attr f)) )
		(if (not (user? uf))
		    (begin
		       (prof '(call int subr))
		       (if (not (correct-arity? f nbargs))
			   (evarity-error loc symb-fun nbargs ($procedure-arity f))
			   (begin
			      ,(when location `($env-set-trace-location (current-dynamic-env) loc))
			      ,subr-call )))
		    (let ( (arity (user-arity uf)) (run (user-runner uf)) (sf (user-frame uf)) )
		       (prof '(call int int))
		       (let ( (sp (+fx bp size)) )
			  (if (=fx arity nbargs)
			      ,fix-expr-call
			      (if (or (>=fx arity 0) (<fx arity (-fx -1 nbargs)))
				  (evarity-error loc (user-where uf) nbargs arity)
				  ,notfix-expr-call ))
			  (let ( (!denv::dynamic-env (current-dynamic-env)) )
			     ,(when location `($env-set-trace-location !denv loc))
			     (if (check-stack s ,(if tail? 'bp 'sp) sf)
				 ,(if tail?
				      '(throw-trampoline run)
				      '(catch-trampoline run s sp) )
				 (let ( (ns (make-state)) )
				    (let ( (start ,(if tail? 'bp 'sp)) )
				       (vector-copy! ns 2 s start (+fx start nbargs)) )
				    (vector-set! ns 1 s)
				    ($evmeaning-evstate-set! !denv ns)
				    (unwind-protect-state (catch-trampoline run ns 2) !denv s) )))))))))))

(define-macro (comp-call-pattern pn tail? comp-call subr-call fix-expr-call notfix-expr-call);
   `(if (> (bigloo-debug) 0)
	(comp-call-pattern/location #t ,pn ,tail? ,comp-call ,subr-call ,fix-expr-call ,notfix-expr-call)
	(comp-call-pattern/location #f ,pn ,tail? ,comp-call ,subr-call ,fix-expr-call ,notfix-expr-call)))

(define (need-shift args stk)
   ;; CARE must memorize the results of use.
   (let rec ( (l args) (stk stk) )
      (cond
	 ((or (null? l) (null? stk))
	  #f )
	 ((any (lambda (e) (memq (car stk) (use e '()))) (cdr l))
	  #t )
	 (else (rec (cdr l) (cdr stk))) )))

(define (comp-old-call e stk)
   (with-access::ev_app e (loc fun args tail?)
      (let ( (size (length stk)) (nbargs (length args)) (f (comp fun stk)) (symb-fun (uncompile fun)) )
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

;; specialized version for a direct call/location
(define-macro (generate-comp-constant-calli-body/location location args)
   (let ( (nbargs (length args)) )
      `(EVA '(call compiled-constant ,nbargs) ("nb args" ,nbargs "size" size)
	  (let ( ,@(map (lambda (v) `(,v (EVC ,v))) args) )
	     (let ( (nbp (+fx bp size)) )
		(vector-set! s 0 nbp)
		,(when location `($env-set-trace-location (current-dynamic-env) loc))
		(let ( (r (cfun ,@args)) )
		   (vector-set! s 0 bp)
		   r ))))))

;; specialized version for a direct call
(define-macro (generate-comp-constant-calli-body args)
   `(if (> (bigloo-debug) 0)
	(generate-comp-constant-calli-body/location #t ,args)
	(generate-comp-constant-calli-body/location #f ,args)))

;; specialized version for a fixed number of arguments
(define-macro (generate-comp-calli-body/location location tail? args)
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
		(evtype-error loc "eval" "procedure" f) )
	     (let ( (uf (procedure-attr f)) )
		(if (not (user? uf))
		    (begin
		       (prof '(call int subr))
		       (if (not (correct-arity? f ,nbargs))
			   (evarity-error loc symb-fun ,nbargs ($procedure-arity f))
			   (let ( (nbp (+fx bp size)) )
			      (vector-set! s 0 nbp)
			      (begin
				 ,(when location `($env-set-trace-location (current-dynamic-env) loc))
				 (let ( (r (f ,@args)) )
				    (vector-set! s 0 bp)
				    r )))))
		    (let ( (arity (user-arity uf)) (run (user-runner uf)) (sf (user-frame uf)) )
		       (let ( (sp (+fx bp size)) )
			  (prof '(call int int))
			  (if (=fx arity ,nbargs)
			      ,(bpush-args args (if tail? 'bp 'sp))
			      (case arity
				 ,@(generate-case-neg-arity -1 (-fx -2 nbargs) args (if tail? 'bp 'sp))
				 (else
				  (evarity-error loc (user-where uf) ,nbargs arity) )))
			  (let ( (!denv::dynamic-env (current-dynamic-env)) )
			     ,(when location `($env-set-trace-location !denv loc))
			     (if (check-stack s ,(if tail? 'bp 'sp) sf)
				 ,(if tail?
				      '(throw-trampoline run)
				      '(catch-trampoline run s sp) )
				 (let ( (ns (make-state)) )
				    (let ( (start ,(if tail? 'bp 'sp)) )
				       (vector-copy! ns 2 s start (+fx start ,nbargs)) )
				    (vector-set! ns 1 s)
				    ($evmeaning-evstate-set! !denv ns)
				    (unwind-protect-state (catch-trampoline run ns 2) !denv s) )))))))))))

(define-macro (generate-comp-calli-body tail? args)
   `(if (> (bigloo-debug) 0)
	(generate-comp-calli-body/location #t ,tail? ,args)
	(generate-comp-calli-body/location #f ,tail? ,args)))

(define-macro (generate-comp-calli from to)
   (if (>fx from to)
       '#unspecified
       (let ( (sn (string->symbol (integer->string from)))
	      (args (map (lambda (i) (symbol-append 'a (string->symbol (integer->string i))))
			    (iota from 1) )))
	  `(begin (define (,(symbol-append 'comp-call sn) loc vfun symb-fun fun tail? stk size ,@args)
		     (let ( (cfun (is-constant-fun? vfun ,(length args) loc symb-fun)) )
			(if cfun
			    (generate-comp-constant-calli-body ,args)
			    (if tail?
				(generate-comp-calli-body #t ,args)
				(generate-comp-calli-body #f ,args) ))))
		  (generate-comp-calli ,(+fx 1 from) ,to) ))))

(generate-comp-calli 0 4)

;; calling a constant function
(define (is-constant-fun? fun nbargs loc symb-fun)
   (cond
      ((isa? fun ev_global)
       (with-access::ev_global fun (name mod)
	  (let ( (g (evmodule-find-global mod name)) )
	     (when g
		(when (=fx (eval-global-tag g) 0)
		   (let ( (val (eval-global-value g)) )
		      (when (and (procedure? val)
				 (correct-arity? val nbargs) )
			 val )))))))
      ((and (isa? fun ev_litt)
	    (procedure? (with-access::ev_litt fun (value) value)))
       (with-access::ev_litt fun (value)
	  (if (correct-arity? value nbargs)
	      value
	      (evarity-error loc symb-fun nbargs ($procedure-arity value)))))))

;; expression arithmetique
(define (compile-float-arith expr stk)
   (define (CF e) (compile-float-arith e stk))
   ;(print "compile " (uncompile expr))
   (cond
      ((isa? expr ev_litt)
       (with-access::ev_litt expr (value)
	  (cond ((fixnum? value) (vector 1 (fixnum->flonum value)))
		((flonum? value) (vector 1 value))
		(else (vector 0 (comp expr stk))) )))
      ((isa? expr ev_var)
       (with-access::ev_var expr (eff)
	  (if eff
	      (vector 3 (_index expr stk))
	      (vector 2 (_index expr stk)) )))
      ((isa? expr ev_global)
       (with-access::ev_global expr (name mod loc)
	  (let ( (g (evmodule-find-global mod name)) )
	     (if g
		 (if (eq? (eval-global-tag g) 1)
		     (vector 4 g)
		     (vector 5 g) )
		 (vector 0 (comp expr stk)) ))))
      ((isa? expr ev_app)
       (with-access::ev_app expr (loc fun args tail?)
	  (let ( (fval (global-fun-value fun)) )
	     (cond
		((eq? fval +fl)
		 (vector 6 (CF (car args)) (CF (cadr args))))
		((eq? fval -fl)
		 (vector 7 (CF (car args)) (CF (cadr args))))
		((eq? fval *fl)
		 (vector 8 (CF (car args)) (CF (cadr args))))
		((eq? fval /fl)
		 (vector 9 (CF (car args)) (CF (cadr args))))
		((eq? fval fixnum->flonum)
		 (vector 10 (comp (car args) stk)))
		((eq? fval f64vector-ref)
		 (vector 11 (comp (car args) stk) (comp (cadr args) stk)))
		(else
		 (vector 0 (comp expr stk))) ))))
      (else (vector 0 (comp expr stk))) ))

(define (eval-float-arith::double bc s);
   (define (E::double bc)
      (case (vector-ref bc 0)
	 ((0) (EVC (vector-ref bc 1)))
	 ((1) (vector-ref bc 1))
	 ((2) (vector-ref s (+fx (vector-ref s 0) (vector-ref bc 1))))
	 ((3) (mcell-value (vector-ref s (+fx (vector-ref s 0) (vector-ref bc 1)))))
	 ((4) (__evmeaning_address-ref (eval-global-value (vector-ref bc 1))))
	 ((5) (eval-global-value (vector-ref bc 1)))
	 ((6) (+fl (E (vector-ref bc 1)) (E (vector-ref bc 2))))
	 ((7) (-fl (E (vector-ref bc 1)) (E (vector-ref bc 2))))
	 ((8) (*fl (E (vector-ref bc 1)) (E (vector-ref bc 2))))
	 ((9) (/fl (E (vector-ref bc 1)) (E (vector-ref bc 2))))
	 ((10) (fixnum->flonum (EVC (vector-ref bc 1))))
	 ((11) (f64vector-ref (EVC (vector-ref bc 1)) (EVC (vector-ref bc 2))))
	 (else (error "eval" "invalid byte code" (vector-ref bc 0))) ))
    (E bc) )

(define (arith-expression expr::ev_app stk);
   (with-access::ev_app expr (loc fun args tail?)
      (let ( (fval (global-fun-value fun)) )
	 (if (or (eq? fval +fl) (eq? fval -fl) (eq? fval *fl) (eq? fval /fl))
	     (let ( (bc (compile-float-arith expr stk)) )
		(EVA '(call float-arith) () (eval-float-arith bc s)) )
	     #f ))))

;; inline
(define-macro (inline fun* loc fun vars stk);
   (let rec ( (l fun*) )
      (if (null? l)
	  '#f
	  (let* ( (slot (car l))
		  (type (car slot))
		  (pred (if (pair? (cadr slot)) (car (cadr slot)) (cadr slot)))
		  (typev (when (pair? (cadr slot)) (cadr (cadr slot)))) )
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
							 (loc-type-error ',fname ',type ,(if typev `(,typev ,v) v) ,loc) ))
						  vars )
					     '() )
				       (,fname ,@vars) )))
			    ,(rec2 (cdr names)) ))))))))

(define (inline-call loc fun args stk);
   (let ( (val (global-fun-value fun)) )
      (when val
	 (let ( (n (length args)) )
	    (or (when (=fx n 1)
		   (let ( (a1 (car args)) )
		      (inline ((pair pair? car cdr) (pair (cadr? cadrerrv) cadr)) loc val (a1) stk) ))
		(when (=fx n 2)
		   (let ( (a1 (car args)) (a2 (cadr args)) )
		      (inline ((number number? + - * / < > <= >= =)
			       (fixnum fixnum? +fx -fx *fx /fx <fx >fx <=fx >=fx =fx)
			       (flonum flonum? +fl -fl *fl /fl <fl >fl <=fl >=fl =fl)
			       (#f #f eq? cons)) loc val (a1 a2) stk ))))))))

(define (cadr? l)
   (and (pair? l) (pair? (cdr l))) )

(define (cadrerrv v)
   (if (pair? v) (cdr v) v) )

;; main method for application
(define-macro (comp-dispatch cur max vars)
   (let ( (sn (string->symbol (integer->string cur))) )
      (let ( (fn (symbol-append 'comp-call sn)) (an (symbol-append 'a sn)) )
	 (if (>fx cur max)
	     '(error "eval" "internal error" "must be dead code")
	     (let ( (vars (cons an vars)) )
		`(let ( (,an (comp (car args) stk)) (args (cdr args)) )
		    (if (null? args)
			(,fn loc fun symb-fun f tail? stk size ,@(reverse vars))
			(comp-dispatch ,(+fx cur 1) ,max ,vars) )))))))

(define-method (comp e::ev_app stk);
   (with-access::ev_app e (loc fun args tail?)
      (or (and #t (arith-expression e stk))
	  (and #t (inline-call loc fun args stk))
	  (if (>fx (length args) 4)
	      (comp-old-call e stk)
	      (let ( (f (comp fun stk)) (size (length stk)) (symb-fun (uncompile fun)) )
		 (if (null? args)
		     (comp-call0 loc fun symb-fun f tail? stk size)
		     (comp-dispatch 1 4 ()) ))))))

;;
;; Abstractions
;; 
(define (correct n done)
   (if (<fx n 0)
       (+fx n done)
       (-fx n done) ))

(define (bind-frame s where sp arity vals loc)
   (if (<fx arity 0)
       (let rec ( (r (-fx -1 arity)) (sp sp) (l vals) )
	  (cond
	     ((=fx r 0)
	      (vector-set! s sp l) )
	     ((not (pair? l))
	      (evarity-error loc where arity (length vals)) )
	     (else
	      (vector-set! s sp (car l))
	      (rec (-fx r 1) (+fx sp 1) (cdr l)) )))
       (let rec ( (r arity) (sp sp) (l vals) )
	  (cond
	     ((=fx r 0)
	      (unless (null? l)
		 (evarity-error loc where arity (length vals)) ))
	     ((not (pair? l))
	      (evarity-error loc where arity (length vals)) )
	     (else
	      (vector-set! s sp (car l))
	      (rec (-fx r 1) (+fx sp 1) (cdr l)) )))))

(define-macro (generate-abstraction param extra arity hasfree hasbox)
   (let ( (nbparam (length param)) )
      `(EVA '(lambda ,arity) ("nbfree " (vector-length ifrees))
	    (prof (list 'closing (vector-length ifrees)))
	    (let ( ,@(if hasfree `((val* (free-collect s bp ifrees))) '()) )
	       (let ( (run (EVA (list 'entry (vector-length ifree)) ("param" (map lname vars)
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
					(if (check-stack s bp size-frame)
					    (begin
					       ,@(map (lambda (i v) `(vector-set! s (+fx bp ,i) ,v))
						      (iota nbparam 0)
						      param )
					       (step s bp ,nbparam '(entry main cont))
					       ,@(cond
						    ((null? extra) '())
						    ((eq? extra 'last) `((vector-set! s (+fx bp ,nbparam) last)))
						    (else `((bind-frame s where (+fx bp ,nbparam)
									(correct ,arity ,nbparam)
									,extra
									loc))) )
					       (step s bp size '(entry main cont2))
					       (unwind-protect-bp (catch-trampoline run s bp)
							       s bp))
					    (let ( (ns (make-state)) )
					       (vector-set! ns 1 s)
					       ,@(map (lambda (i v) `(vector-set! ns ,(+fx 2 i) ,v))
						      (iota nbparam 0)
						      param )
					       ,@(cond
						    ((null? extra) '())
						    ((eq? extra 'last) `((vector-set! ns ,(+fx 2 nbparam) last)))
						    (else `((bind-frame ns where ,(+fx 2 nbparam)
									(correct ,arity ,nbparam)
									,extra
									loc))) )
					       (let ( (!denv (current-dynamic-env)) )
						  ($evmeaning-evstate-set! !denv ns)
						  (unwind-protect-state (catch-trampoline run ns 2) !denv s) ))))))) )
		     (procedure-attr-set! run **a-bounce**)
		     (procedure-attr-set! main (user ,arity run size-frame where))
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
	    ; (generate-case-arity ())
	    (generate-case-arity (a b c d))
	    ))))

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
