(module saw_node2rtl
   (include "Tools/location.sch"
	    "SawMill/node2rtl.sch")
   (import type_type	;; needed for ast_var !!
	   type_typeof  ;; needed for ::new
	   ast_var	;; local/global
	   ast_node	;; node
	   ast_env
	   object_class
	   object_slots
	   sync_node
	   type_env
	   tools_shape
	   saw_defs
	   backend_cplib
	   )
   (export (global->rtl::block var::global)
	   (local->reg::rtl_reg var::local))
   (static (class area entry::block exit::block)
	   (wide-class reversed::block)
	   (wide-class rlocal::local reg code))
   )

(define *reverse-call-argument* #f)

;
; In this pass, basic blocks have only one instruction.
;

;;
;; Compiling a global definition
;;
(define (global->rtl::block var::global) ; ()
   (let ( (a (call #f (instantiate::rtl_return (type (global-type var)))
		   (sfun-body (global-value var))) ))
      ; (link (single (instantiate::rtl_entry)) a)
      (let ( (r (area-entry a)) (n '0) )
	 (let dfs ( (b r) )
	    (widen!::reversed b)
	    (block-label-set! b n)
	    (set! n (+fx n 1))
	    (for-each (lambda (s)
			 (block-preds-set! s (cons b (block-preds s)))
			 (if (not (reversed? s)) (dfs s)) )
		      (block-succs b) ))
	 r )))

(define (get-global::global var::global) ; ()
   (with-access::global var (alias module)
      (set-variable-name! var)
      (if alias
	  (let ((unalias (find-global alias module)))
	     (if (global? unalias)
		 (begin
		    (set-variable-name! unalias)
		    unalias)
		 var))
	  var)))

;;
;; Regs
;;
(define (new-reg::rtl_reg e::node) ; ()
   (if (var? e)
       (if (sfun? (variable-value (var-variable e)))
	   (instantiate::rtl_reg
	      (type (find-type 'procedure))
	      (var #f)
	      (name (gensym)))
	   (instantiate::rtl_reg
	      (type (variable-type (var-variable e)))
	      (var #f)
	      (name (gensym))) )
       (instantiate::rtl_reg (type (get-type e #f)) (var #f) (name (gensym)))) )

(define (new-ureg::rtl_reg var::local) ; ()
   (instantiate::rtl_reg (type (local-type var)) (var var)) )

(define (local->reg::rtl_reg var::local) ; ()
   (if (rlocal? var)
       (rlocal-reg var)
       (let ( (r (new-ureg var)) )
	  (widen!::rlocal var (reg r) (code #f))
	  r )))

;;
;; Blocks
;;
(define (successor! from::block to::block) ; ()
   (block-succs-set! from (cons to (block-succs from))) )

(define (bdestination! b::block reg::rtl_reg) ; ()
   (let ( (ins (car (last-pair (block-first b)))) )
      (if (or (not (rtl_mov? (rtl_ins-fun ins)))
	      (not (eq? (car (rtl_ins-args ins)) reg)) )
	  (rtl_ins-dest-set! ins reg) )))

;;
;; Area
;;
(define (single*::area e fun::rtl_fun args) ; (or #f node) (list rtl_reg)
   (let ( (ins (instantiate::rtl_ins (fun fun) (args args))) )
      (when (node? e)
	 (rtl_fun-loc-set! fun (node-loc e))
	 (rtl_ins-loc-set! ins (node-loc e)) )
      (let ( (l (cons ins '())) )
	 (let ( (b (instantiate::block (first l))) )
	    (instantiate::area (entry b) (exit b)) ))))

(define (single::area e::obj fun::rtl_fun . args) ; (list rtl_reg)
   (single* e fun args) )

(define (adestination! a::area reg::rtl_reg) ; ()
   (bdestination! (area-exit a) reg) )

(define (unlink::area a::area) ;()
   (instantiate::area (entry (area-entry a))
		      ;; This basic block is unlinked and thus is dead!
		      (exit (area-exit (single #f (instantiate::rtl_nop)))) ))

(define (link::area b1::area b2::area) ; ()
   (successor! (area-exit b1) (area-entry b2))
   (instantiate::area (entry (area-entry b1)) (exit (area-exit b2))) )

(define (link*::area l) ; l::(list area)
   (define (walk r::area l)
      (if (null? l)
	  r
	  (walk (link r (car l)) (cdr l)) ))
   (if (null? l)
       (single #f (instantiate::rtl_nop))
       (walk (car l) (cdr l)) ))

(define (fork/join::area head::area conts r::rtl_reg) ; conts::(list area)
   ; Fork
   (block-succs-set! (area-exit head) (map area-entry conts))
   ; Join
   (let ( (join (area-entry (single #f (instantiate::rtl_mov) r))) )
      (for-each (lambda (a::area)
		   (let ( (b (area-exit a)) )
		      (successor! b join)
		      (bdestination! b r) ))
		conts )
      ; Result
      (instantiate::area (entry (area-entry head)) (exit join)) ))

(define (join conts xr::rtl_reg) ;(list area) (list area)
   (let ( (r (find-register conts xr)) )
      ; Join
      (let ( (join (area-entry (single #f (instantiate::rtl_mov) r))) )
	 (for-each (lambda (a::area)
		      (let ( (b (area-exit a)) )
			 (successor! b join)
			 (bdestination! b r) ))
		   conts )
	 ; Result
	 (map (lambda (a::area)
		 (instantiate::area (entry (area-entry a)) (exit join)) )
	      conts ))))

(define (fork::area head::area joined) ;(list area)
   ; Fork
   (block-succs-set! (area-exit head) (map area-entry joined))
   ; Result
   (instantiate::area (entry (area-entry head))
		      (exit (area-exit (car joined))) ))

(define (find-register areas reg)
   (define (candidat a)
      (let ( (ins (car (last-pair (block-first (area-exit a))))) )
	 (and (rtl_mov? (rtl_ins-fun ins))
	      (let ( (r (car (rtl_ins-args ins))) )
		 (if (or (rtl_reg-var r)
			 (not (eq? (rtl_reg-type r) (rtl_reg-type reg))) )
		     #f
		     r )))))
   (let walk ( (l areas) )
      (if (null? l)
	  reg
	  (or (candidat (car l))
	      (walk (cdr l)) ))))

(define (node->rtl/in::area e::node r::rtl_reg) ; ()
   (let ( (a (node->rtl e)) )
      (adestination! a r)
      a ))

(define (call*::area e fun::rtl_fun exprs) ; exprs::(list node)
   (let ( (regs (map (lambda (e) (new-reg e)) exprs)) )
      (link (link* (map (lambda (a r) (node->rtl/in a r)) exprs regs))
	    (single* e fun regs) )))

(define (rcall*::area e fun::rtl_fun exprs) ; exprs::(list node)
   (let ( (regs (map (lambda (e) (new-reg e)) exprs)) )
      (link (link* (reverse! (map (lambda (a r) (node->rtl/in a r)) exprs regs)))
	    (single* e fun regs) )))

(define (call::area e fun::rtl_fun . exprs) ; exprs::(list node)
   (call* e fun exprs) )

;;
;; The generic function to compile a node, followed by implementations
;;
(define-generic (node->rtl::area e::node)) ; ()

;;
(define-method (node->rtl::area e::atom) ; ()
   (single e (instantiate::rtl_loadi (constant e))) )

;;
(define-method (node->rtl::area e::var) ; ()
   (with-access::var e (variable)
      (cond ((local? variable)
	     (single e (instantiate::rtl_mov) (local->reg variable)) )
	    ((sfun? (variable-value variable))
	     (single e (instantiate::rtl_loadfun (var (get-global variable)))) )
	    (else
	     (single e (instantiate::rtl_loadg (var (get-global variable)))) ))))

;;
(define-method (node->rtl::area e::let-var) ; ()
   (with-access::let-var e (bindings body)
      ;; MANU pour bien compiler les and/or...
      (if (and (conditional? body)
	       (not (null? bindings))
	       (null? (cdr bindings))
	       (var? (conditional-test body))
	       (=fx 1 (local-occurrence (caar bindings)))
	       (eq? (caar bindings) (var-variable (conditional-test body))) )
	  (begin (conditional-test-set! body (cdar bindings))
		 (node->rtl body) )
	  (link
	   (link* (map (lambda (b) (node->rtl/in (cdr b) (local->reg (car b))))
		       bindings ))
	   (node->rtl body) ))))

;;
(define-method (node->rtl::area e::setq) ; ()
   (with-access::setq e (var value)
      (with-access::var var (variable)
	 (link (if (global? variable)
		   (call e (instantiate::rtl_storeg (var (get-global variable))) value)
		   (node->rtl/in value (local->reg variable)) )
	       (single #f (instantiate::rtl_nop)) ))))

;;
(define-method (node->rtl::area e::sequence) ; ()
   (with-access::sequence e (nodes)
      (link* (map (lambda (e) (node->rtl e)) nodes)) ))

;;
(define-method (node->rtl::area e::sync) ; ()
   (node->rtl (sync->sequence e)) )

;;
(define-method (node->rtl::area e::conditional) ; ()
   (with-access::conditional e (test true false)
      (predicate test (join (list (node->rtl true) (node->rtl false))
			    (new-reg e) ))))

;;
(define-method (node->rtl::area e::switch) ; ()
   (with-access::switch e (test clauses item-type)
      (fork/join (call e (instantiate::rtl_select (type item-type)
						  (patterns (coerce clauses)) )
		       test)
		 (map (lambda (c) (node->rtl (cdr c))) clauses)
		 (new-reg e) )))

(define (coerce clauses) ;()
   (map (lambda (c)
	   (let ( (p (car c)) )
	      (if (eq? p 'else)
		  p
		  (map (lambda (n)
			  (cond ((integer? n) n)
				((char? n) (char->integer n))
				(else (error "switch" "bad constant" n)) ))
		       p ))))
	clauses ))

;;
(define-method (node->rtl::area e::let-fun) ; ()
   (with-access::let-fun e (body)
      ;; The local functions will be compiled on demand by compilation of body.
      (node->rtl body) ))

(define (compile-label-call::area v::local args) ; args::(list node)
   ;; Called when an app node have a local variable as function
   (define (move::area r::rtl_reg v::local)
      ;; Moving a register to a parameter
      (let ( (a (single #f (instantiate::rtl_mov) r)) )
	 (adestination! a (local->reg v))
	 a ))
   (let ( (regs (map (lambda (a) (new-reg a)) args)) )
      (let ( (params (sfun-args (local-value v))) )
	 ;; Prepare arguments
	 (link (link* (map (lambda (e r) (node->rtl/in e r)) args regs))
	       ;; Assign formal parameters
	       (link (link* (map move (reverse regs) (reverse params)))
		     ;; find block associated to the local function.
		     (local->code v) )))))

(define (local->code::area v::local) ; ()
   (if (not (rlocal? v))
       ;; first use of this local function.
       (let ( (nop (single #f (instantiate::rtl_nop)))
	      (body (sfun-body (local-value v))) )
	  ;; tag the local function with nop
	  (widen!::rlocal v (reg #f) (code nop))
	  ;; Link the nop to the body
	  (link nop (node->rtl body)) )
       ;; already generated
       (unlink (rlocal-code v)) ))

;;
(define-method (node->rtl::area e::app) ; ()
   (with-access::app e (fun args)
      (let ( (v (var-variable fun)) )
	 (if (local? v)
	     (compile-label-call v args)
	     (or (imperative? e v args)
		 (if *reverse-call-argument*
		     (rcall* e (instantiate::rtl_call (var (get-global v))) args)
		     (call* e (instantiate::rtl_call (var (get-global v))) args) ))))))

(define (imperative? e v::global args) ; args::(list node)
   (let ( (id (global-id v)) )
      (cond
	 ((eq? id '__evmeaning_address)
	  (let ((a (if (isa? (car args) pragma)
		       (car (pragma-expr* (car args)))
		       (car args))))
	     (call e
		(instantiate::rtl_globalref
		   (var (get-global (var-variable a))) ))))
	 (else #f) )))

;;
(define-method (node->rtl::area e::app-ly) ; ()
   (with-access::app-ly e (fun arg)
      (call e (instantiate::rtl_apply) fun arg) ))

;;
(define-method (node->rtl::area e::funcall) ; ()
   (define (args-1 args)
      (if (null? (cdr args))
	  '()
	  (cons (car args) (args-1 (cdr args))) ))
   (with-access::funcall e (fun args strength)
      ;; The last argument is always __eoa__. Forget it.
      (let ( (args (args-1 args)) )
	 (case strength
	    ((elight)
	     ;; OUPS sometime arg1 != fun but fun is a GLOBAL fun !!!!!
	     (call* e (instantiate::rtl_call (var (get-global (var-variable fun)))) args) )
	    ((light)
	     ;; Forget fun which is always the first argument of args.
	     (call* e
		    (instantiate::rtl_lightfuncall
		       (rettype (get-type e #f))
		       (name (gensym))
		       (funs (funcall-functions e)) )
		    args) )
	    (else
	     ;; Forget fun which is always the first argument of args.
	     (call* e (instantiate::rtl_funcall) args) )))))

;;
(define-method (node->rtl::area e::pragma) ; ()
   (with-access::pragma e (expr* format)
      (call* e (instantiate::rtl_pragma (format format)) expr*) ))

;;
(define-method (node->rtl::area e::getfield) ; ()
   (with-access::getfield e (expr* fname ftype otype)
      (call* e (instantiate::rtl_getfield (name fname)
					  (type ftype)
					  (objtype otype) )
	     expr*) ))

;;
(define-method (node->rtl::area e::setfield) ; ()
   (with-access::setfield e (expr* fname ftype otype)
      (link (call* e (instantiate::rtl_setfield (name fname)
						(type ftype)
						(objtype otype) )
		   expr* )
	    (single #f (instantiate::rtl_nop)) )))

;;
(define-method (node->rtl::area e::new) ; ()
   (with-access::new e (expr* args-type)
      (call* e (instantiate::rtl_new (type (get-type e #f)) (constr args-type))
	     expr* )))

;;
(define-method (node->rtl::area e::valloc) ; ()
   (with-access::valloc e (expr* ftype)
      (call* e (instantiate::rtl_valloc (type ftype) (vtype (get-type e #f)))
	     expr* )))

;;
(define-method (node->rtl::area e::vref) ; ()
   (with-access::vref e (expr* ftype vtype)
      (call* e (instantiate::rtl_vref (type ftype) (vtype vtype)) expr*) ))

;;
(define-method (node->rtl::area e::vset!) ; ()
   (with-access::vset! e (expr* ftype vtype)
      (link (call* e (instantiate::rtl_vset (type ftype) (vtype vtype)) expr*)
	    (single #f (instantiate::rtl_nop)) )))

;;
(define-method (node->rtl::area e::vlength) ; ()
   (with-access::vlength e (expr* ftype vtype)
      (call* e (instantiate::rtl_vlength (type ftype) (vtype vtype)) expr*) ))

;;
(define-method (node->rtl::area e::instanceof) ; ()
   (with-access::instanceof e (expr* class)
      (call* e (instantiate::rtl_instanceof (type class)) expr*) ))

;;
(define-method (node->rtl::area e::cast) ; ()
  (with-access::cast e (arg)
      ; CARE MANU pourquoi il y aurait des mauvais type!!
      ; (call e (instantiate::rtl_cast (type (get-type e #f))) arg)
      (node->rtl arg) ))

;;
(define-method (node->rtl::area e::cast-null) ; ()
   (with-access::cast-null e (type)
      (call* e (instantiate::rtl_cast_null (type type)) '()) ))

;;
(define-method (node->rtl::area e::set-ex-it) ; ()
   (with-access::set-ex-it e (var body)
      (link (let ( (a (single e (instantiate::rtl_protect))) )
	       (adestination! a (local->reg (var-variable var)))
	       a )
	    (call e (instantiate::rtl_protected) body) )))

;;
(define-method (node->rtl::area e::jump-ex-it) ; ()
   (with-access::jump-ex-it e (exit value)
      (unlink (call e (instantiate::rtl_jumpexit) exit value)) ))

;;
(define-method (node->rtl::area e::fail) ; ()
   (with-access::fail e (proc msg obj)
      (unlink (call e (instantiate::rtl_fail) proc msg obj)) ))

;;
(define-method (node->rtl::area e::make-box) ; ()
   (with-access::make-box e (value)
      (call e (instantiate::rtl_makebox) value) ))

;;
(define-method (node->rtl::area e::box-ref) ; ()
   (with-access::box-ref e (var)
      (call e (instantiate::rtl_boxref) var) ))

;;
(define-method (node->rtl::area e::box-set!) ; ()
   (with-access::box-set! e (var value)
      (link (call e (instantiate::rtl_boxset) var value)
	    (single #f (instantiate::rtl_nop)) )))

;;
;; The generic function to compile a predicate, followed by implementations
;;
(define-generic (predicate::area e::node joined);()
   (fork (call e (instantiate::rtl_if) e) joined) )

;;
(define-method (predicate::area e::atom l) ;()
   (with-access::atom e (value)
      (if value
	  (car l)
	  (cadr l) )))

;;
(define-method (predicate::area e::conditional l);()
   (with-access::conditional e (test true false)
      (predicate test (list (predicate true l) (predicate false l))) ))

;;
(define-method (predicate::area e::let-var l);()
   ;; MANU pour bien compiler le code généré par les grammaires (rgc)
   (with-access::let-var e (bindings body)
      (cond
	 ((and (not (null? bindings))
	       (null? (cdr bindings))
	       (var? body)
	       (eq? (caar bindings) (var-variable body)) )
	  ;; (if (let ( (x E) ) x) A B) -> (if E A B)
	  (predicate (cdar bindings) l) )
	 ((and (conditional? body)
	       (not (null? bindings))
	       (null? (cdr bindings))
	       (var? (conditional-test body))
	       (=fx 1 (local-occurrence (caar bindings)))
	       (eq? (caar bindings) (var-variable (conditional-test body))) )
	  ;; (if (let ( (x E) ) (if x AA BB)) A B) -> (if (if E AA BB) A B)
	  (conditional-test-set! body (cdar bindings))
	  (predicate body l) )
	 (else
	  (fork (call e (instantiate::rtl_if) e) l) ))))

