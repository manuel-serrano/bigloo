;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Cgen/cgen.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  2 13:17:04 1996                          */
;*    Last change :  Fri Nov  8 08:51:30 2024 (serrano)                */
;*    Copyright   :  1996-2024 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C production code.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_cgen
   
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   
   (import  tools_error
	    tools_shape
	    tools_speek
	    engine_param
	    type_type
	    type_tools
	    type_cache
	    type_typeof
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_local
	    ast_sexp
	    ast_app
	    ast_dump
	    sync_node
	    effect_effect
	    cgen_cop
	    cgen_emit-cop
	    cgen_capp
	    backend_c_emit
	    backend_c_prototype
	    backend_backend
	    backend_cplib)

   (static  (wide-class retblock/goto::retblock
	       local
	       label))
   
   (export  (cgen-function ::global)
	    (node-setq::setq variable::variable value::node)
	    (generic node->cop::cop ::node ::procedure ::bool)
	    (make-local-svar/name::local ::symbol ::type)
	    (bdb-let-var::cop ::cop loc)
	    *the-current-global*
	    *return-kont*
	    *id-kont*
	    (block-kont cop loc)
	    *stop-kont*))

;*---------------------------------------------------------------------*/
;*    cgen-function ...                                                */
;*---------------------------------------------------------------------*/
(define (cgen-function global::global)
   (when (require-prototype? global)
      (enter-function (global-id global))
      (trace cgen "cgen global: " (shape global) #\Newline)
      (trace (cgen 2) "*void-kont*: " *void-kont* #\Newline)
      (trace (cgen 2) "*return-kont*: " *return-kont* #\Newline)
      (let ((glob *the-current-global*))
	 (set! *the-current-global* global)
	 (let ((sh (shape global)))
	    (assert (sh) (string? (global-name global))))
	 (let* ((sfun (widen!::sfun/C (global-value global)
			 (label (instantiate::clabel
				   (type *unspec*)
				   (loc (sfun-loc (global-value global)))
				   (name (global-name global))))
			 (integrated #t)))
		(loc  (sfun-loc sfun))
		(cop  (node->cop (sfun-body sfun)
			 (if (eq? (global-type global) *void*)
			     *void-kont*
			     *return-kont*)
			 #f)))
	    (reset-bdb-loc!)
	    (newline *c-port*)
	    (newline *c-port*)
	    (display "/* " *c-port*)
	    (display
	       (pregexp-replace* "[*]/" (symbol->string! (shape global)) "*|")
	       *c-port*)
	    (display " */" *c-port*)
	    ;; we have to emit a dummy location otherwise gdb get confused
	    ;; with the function arguments! Thus all functions looks like
	    ;; starting at the module definition site instead of there correct
	    ;; definition site
	    (emit-bdb-loc #f)
	    ;; we have to reset the loc because we must not emit location
	    ;; before the first c expression otherwise gdb get confused
	    (reset-bdb-loc!)
	    (clabel-body-set! (sfun/C-label sfun) cop)
	    (global->c global)
	    (let ((cop (block-kont (sfun/C-label sfun) loc)))
	       ;; we define a local variable that acts as a temporary variable
	       (display "{" *c-port*)
	       ;; when compiling for debugging, we have to insert a dummy
	       ;; statement otherwise gdb get confused
	       (if (and (> *bdb-debug* 0) (location? loc))
		   (begin
		      (emit-bdb-loc loc)
		      (display "{ obj_t ___ = BUNSPEC; } /* bdb dummy init stmt */"
			 *c-port*)))
	       ;; we now may emit the body
	       (emit-cop cop)
	       ;; emit the current location before the closing bracket
	       (emit-bdb-loc (get-current-bdb-loc))
	       (fprint *c-port* "\n}"))
	    (no-bdb-newline)
	    (set! *the-current-global* glob)
	    (leave-function)))))
  
;*---------------------------------------------------------------------*/
;*    *the-current-global* ...                                         */
;*    -------------------------------------------------------------    */
;*    This variable is use to implement global tail calls.             */
;*---------------------------------------------------------------------*/
(define *the-current-global* #unspecified)

;*---------------------------------------------------------------------*/
;*    global->c ...                                                    */
;*---------------------------------------------------------------------*/
(define (global->c g::global)
   (with-access::global g (type id name import value)
      (when (eq? import 'export) (display "BGL_EXPORTED_DEF " *c-port*))
      (display (make-typed-declaration
		type
		(string-append
		 name
		 (if (null? (sfun-args value))
		     "(void)"
		     (string-append
		      "("
		      (let loop ((args (sfun-args value)))
			 (if (null? (cdr args))
			     (with-access::local (car args) (name id type)
				(string-append
				 (make-typed-declaration type name) ")"))
			     (with-access::local (car args) (name id type)
				(string-append
				 (make-typed-declaration type name)
				 ", "
				 (loop (cdr args))))))))))
	       *c-port*))
   (no-bdb-newline))

;*---------------------------------------------------------------------*/
;*    arg-type ...                                                     */
;*---------------------------------------------------------------------*/
(define (arg-type a)
   (cond
      ((type? a) a)
      ((variable? a) (variable-type a))
      (else "arg-type@cgen" "wrong argument" (shape a))))

;*---------------------------------------------------------------------*/
;*    capply-tailcallable? ...                                         */
;*    -------------------------------------------------------------    */
;*    Generalized C tail calls requires the caller and the callee      */
;*    to have the same number of arguments.                            */
;*---------------------------------------------------------------------*/
(define (capply-tailcallable? cop::capply)
   (let* ((caller (global-value *the-current-global*))
	  (formals (sfun-args caller)))
      (when (=fx (length formals) 2)
	 (and (eq? (arg-type (car formals)) *obj*)
	      (eq? (arg-type (cadr formals)) *obj*)))))

;*---------------------------------------------------------------------*/
;*    cfuncall-tailcallable? ...                                       */
;*---------------------------------------------------------------------*/
(define (cfuncall-tailcallable? cop::cfuncall)
   
   (define (fun-args callee actuals)
      (cond
	 ((sfun? callee) (sfun-args callee))
	 ((cfun? callee) (cfun-args-type callee))
	 (else actuals)))
   
   (with-access::cfuncall cop (fun args)
      (let ((caller (global-value *the-current-global*))
	    (callee (variable-value (varc-variable fun))))
	 (let loop ((actuals (fun-args callee args))
		    (formals (sfun-args caller)))
	    (cond
	       ((null? actuals)
		(null? formals))
	       ((null? formals)
		#f)
	       ((eq? (arg-type (car actuals)) (arg-type (car formals)))
		(loop (cdr actuals) (cdr formals)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    capp-tailcallable? ...                                           */
;*    -------------------------------------------------------------    */
;*    A tail call is tail callable is the caller and the callee        */
;*    have the same arguments (number and types).                      */
;*---------------------------------------------------------------------*/
(define (capp-tailcallable? cop::capp)

   (define (fun-args callee)
      (cond
	 ((sfun? callee) (sfun-args callee))
	 ((cfun? callee) (cfun-args-type callee))
	 (else '())))
   
   (with-access::capp cop (fun)
      (let ((caller (global-value *the-current-global*))
	    (callee (variable-value (varc-variable fun))))
	 (let loop ((actuals (fun-args callee))
		    (formals (sfun-args caller)))
	    (cond
	       ((null? actuals)
		(null? formals))
	       ((null? formals)
		#f)
	       ((eq? (arg-type (car actuals)) (arg-type (car formals)))
		(loop (cdr actuals) (cdr formals)))
	       (else
		#f))))))
   
;*---------------------------------------------------------------------*/
;*    *return-kont* ...                                                */
;*---------------------------------------------------------------------*/
(define *return-kont*
   (lambda (cop)
      (instantiate::creturn
	 (type *unspec*)
	 (loc (cop-loc cop))
	 (tail (when *c-tail-call*
		  (or (and (isa? cop capply) (capply-tailcallable? cop))
		      (and (isa? cop cfuncall) (cfuncall-tailcallable? cop))
		      (when (and (isa? cop capp) (capp-tailcallable? cop))
			 (let ((fun (varc-variable (capp-fun cop))))
			    (cond
			       ((not (cfun? (global-value fun))) #t)
			       ((cfun-infix? (global-value fun)) #f)
			       ((cfun-macro? (global-value fun)) #f)
			       (else #t)))))))
	 (value (cond
		   ((csetq? cop)
		    (instantiate::csequence
		       (type *unspec*) 
		       (loc (cop-loc cop))
		       (c-exp? #t)
		       (cops (list cop
				(instantiate::catom
				   (type *unspec*)
				   (value #unspecified))))))
		   (else
		    cop))))))

;*---------------------------------------------------------------------*/
;*    *id-kont* ...                                                    */
;*---------------------------------------------------------------------*/
(define *id-kont* (lambda (cop) cop))
      
;*---------------------------------------------------------------------*/
;*    *void-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *void-kont*
   (lambda (cop)
      (instantiate::cvoid
	 (type *void*)
	 (value cop))))

;*---------------------------------------------------------------------*/
;*    *stop-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *stop-kont*
   (lambda (cop)
      (instantiate::stop
	 (type *_*)
	 (value cop))))

;*---------------------------------------------------------------------*/
;*    block-kont ...                                                   */
;*---------------------------------------------------------------------*/
(define (block-kont cop loc)
   (cond
      ((isa? cop cblock)
       cop)
      (else
       (instantiate::cblock
	  (type (cop-type cop))
	  (body cop)
	  (loc  loc)))))

;*---------------------------------------------------------------------*/
;*    *fail-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *fail-kont* (lambda (cop) cop))

;*---------------------------------------------------------------------*/
;*    *exit-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *exit-kont* (lambda (cop) cop))

;*---------------------------------------------------------------------*/
;*    make-setq-kont ...                                               */
;*---------------------------------------------------------------------*/
(define (make-setq-kont var loc kont)
   (lambda (cop)
      (if (cfail? cop)
	  cop
	  (kont (instantiate::csetq
		   (type (variable-type var))
		   (var (instantiate::varc
			   (type (variable-type var))
			   (variable var)))
		   (value (cond
			     ((csetq? cop)
			      (instantiate::csequence
				 (type *unspec*)
				 (loc (cop-loc cop))
				 (c-exp? #t)
				 (cops (list cop
					  (instantiate::catom
					     (type *unspec*)
					     (loc (cop-loc cop))
					     (value #unspecified))))))
			     (else
			      cop)))
		   (loc loc))))))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (node->cop::cop node::node kont::procedure inpushexit::bool))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::literal kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::literal kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::atom node (value loc type)
      (kont (instantiate::catom
	       (type type)
	       (value value)
	       (loc   loc)))))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*    -------------------------------------------------------------    */
;*    No longer used.                                                  */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::patch kont inpushexit)
   (trace (cgen 3)
      "(node->cop node::patch kont): " (shape node) #\Newline
      "  kont: " kont #\Newline)
   (with-access::patch node (index loc type patchid value)
      (with-access::genpatchid patchid ((gindex index))
	 (with-access::var value (variable)
	    (instantiate::csequence
	       (type *obj*)
	       (loc loc)
	       (cops (list (instantiate::cpragma
			      (type *obj*)
			      (loc loc)
			      (format (format "BGL_PATCHABLE_CONSTANT_~a($1, $2, $3)"
					 (if (eq? type *obj*)
					     (bigloo-config 'elong-size)
					     32)))
			      (args (list (instantiate::catom
					     (type *long*)
					     (value index)
					     (loc loc))
				       (instantiate::catom
					  (type *long*)
					  (value gindex)
					  (loc loc))
				       (instantiate::varc
					  (type (variable-type variable))
					  (variable variable)
					  (loc loc)))))
			(node->cop value kont inpushexit))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::genpatchid kont inpushexit)
   (trace (cgen 3)
      "(node->cop node::getnpatchid kont): " (shape node) #\Newline
      "  kont: " kont #\Newline)
   (with-access::genpatchid node (index loc)
      (kont
	 (instantiate::catom
	    (type *long*)
	    (value index)
	    (loc loc)))))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::kwote kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::kwote kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (internal-error "node->cop" "Unexpected `kwote' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::var kont inpushexit)
   (trace (cgen 3)
      "(node->cop node::var kont): " (shape node) #\Newline
      "  kont: " kont #\Newline)
   (trace (cgen 4) "(node->cop node::var kont): " (shape node) #\Newline
      "  var-variable-name: " (variable-name (var-variable node))
      #\Newline)
   (with-access::var node (variable loc)
      (kont (instantiate::varc
	       (type (variable-type variable))
	       (variable variable)
	       (loc      loc)))))

;*---------------------------------------------------------------------*/
;*    node->cop ::closure ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::closure kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::closure kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (internal-error "node->cop" "Unexpected `closure' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    node->cop ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::sequence kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::sequence kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::sequence node (nodes loc)
      (let ((exp nodes))
	 (cond
	    ((null? exp)
	     (kont (instantiate::nop
		      (type *void*)
		      (loc loc))))
	    ((null? (cdr exp))
	     (let ((cop (node->cop (car exp) kont inpushexit)))
		(instantiate::stop
		   (type *_*)
		   (value cop))))
	    (else
	     (let ((inpushexit (or inpushexit (is-push-exit? (car exp)))))
		(let loop ((exp exp)
			   (new '()))
		   (if (null? (cdr exp))
		       (let ((cop (node->cop (car exp) kont inpushexit)))
			  (instantiate::csequence
			     (type (cop-type cop))
			     (loc  (cop-loc cop))
			     (cops (reverse! (cons cop new)))))
		       (if (not (side-effect? (car exp)))
			   (loop (cdr exp) new)
			   (loop (cdr exp)
			      (cons (node->cop (car exp) *stop-kont* inpushexit)
				 new)))))))))))

;*---------------------------------------------------------------------*/
;*    is-push-exit? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-push-exit? node::node)
   (when (isa? node app)
      (with-access::app node (fun)
	 (let ((var (var-variable fun)))
	    (when (global? var)
	       (eq? (variable-id var) 'push-exit!))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::sync ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::sync kont inpushexit)
   (node->cop (sync->sequence node) kont inpushexit))

;*---------------------------------------------------------------------*/
;*    extern->cop ...                                                  */
;*---------------------------------------------------------------------*/
(define (extern->cop format::bstring args-safe node::extern kont inpushexit)
   (trace (cgen 3)
	  "(extern->cop node::extern kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::extern node (expr* loc type)
      (node-args->cop type expr*
	 args-safe
	 loc
	 (lambda (new-args)
	    (kont (instantiate::cpragma
		     (type type)
		     (loc loc)
		     (format format)
		     (args new-args))))
	 inpushexit)))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::pragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::pragma kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::pragma kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::pragma node (format type effect expr* srfi0 loc)
      (if (eq? srfi0 'bigloo-c)
	  (if (and (string-null? format)
		   (pair? expr*)
		   (null? (cdr expr*))
		   (isa? (car expr*) var))
	      (with-access::var (car expr*) (variable)
		 (with-access::variable variable (name)
		    (extern->cop name #f node kont inpushexit)))
	      (extern->cop format #f node kont inpushexit))
	  (begin
	     (tprint "############## IGNORING PRAGMA " srfi0 " " format)
	     (instantiate::catom
		(type type)
		(value 0)
		(loc loc))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::private ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::private kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::private kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::private node (c-format)
      (extern->cop c-format #t node kont inpushexit)))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::cast kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::cast kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::cast node (arg type loc type)
      (node-args->cop type (list arg)
	 #t
	 loc
	 (lambda (new-args)
	    (kont (instantiate::ccast
		     (type type)
		     (loc loc)
		     (arg (car new-args)))))
	 inpushexit)))

;*---------------------------------------------------------------------*/
;*    node->cop ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::setq kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::setq kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::setq node (value loc)
      (let ((var (var-variable (setq-var node))))
	 (if (and (var? value) (eq? var (var-variable value)))
	     (kont (*void-kont* (instantiate::catom
				   (type *unspec*)
				   (loc loc)
				   (value #unspecified))))
	     (node->cop value (make-setq-kont var loc kont) inpushexit)))))

;*---------------------------------------------------------------------*/
;*    node->cop ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::conditional kont inpushexit)
   (trace (cgen 3)
      "(node->cop node::conditional kont): " (shape node) #\Newline
      "  loc: " (node-loc node) #\Newline
      "  kont: " kont #\Newline)
   (with-access::conditional node (test true false loc type)
      (let* ((aux   (make-local-svar/name (gensym 'test) *bool*))
	     (ctest (node->cop (node-setq aux test) *id-kont* inpushexit)))
	 (if (and (csetq? ctest) (eq? (varc-variable (csetq-var ctest)) aux))
	     (instantiate::cif
		(type type)
		(test (csetq-value ctest))
		(true (block-kont (node->cop true kont inpushexit) loc))
		(false (block-kont (node->cop false kont inpushexit) loc))
		(loc   loc))
	     (instantiate::cblock
		(type type)
		(loc loc)
		(body
		   (instantiate::csequence
		      (type type)
		      (loc loc)
		      (cops
			 (list
			    (instantiate::local-var
			       (type *obj*)
			       (vars (list aux))
			       (loc  loc))
			    ctest
			    (instantiate::cif
			       (type type)
			       (test (instantiate::varc
					(type (variable-type aux))
					(variable aux)
					(loc loc)))
			       (false (block-kont
					 (node->cop false kont inpushexit)
					 loc))
			       (true (block-kont
					(node->cop true kont inpushexit)
					loc))
			       (loc  loc)))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::fail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::fail kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::fail kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::fail node (proc msg obj loc type)
      (node-args->cop type (list proc msg obj)
	 #f
	 loc
	 (lambda (new-args)
	    (*fail-kont*
	       (instantiate::cfail
		  (type *obj*)
		  (loc loc)
		  (proc (car new-args))
		  (msg (cadr new-args))
		  (obj (caddr new-args)))))
	 inpushexit)))

;*---------------------------------------------------------------------*/
;*    node->cop ::switch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::switch kont inpushexit)
   (trace (cgen 3)
      "(node->cop node::switch kont): " (shape node) #\Newline
      "  kont: " kont #\Newline)
   (with-access::switch node (clauses test item-type loc type)
      (let ((cclauses (map (lambda (clause)
			     (cons (car clause)
				(node->cop (cdr clause) kont inpushexit)))
			clauses)))
	 (let* ((aux  (make-local-svar/name 'aux item-type))
		(cop (node->cop (node-setq aux test) *id-kont* inpushexit)))
	    (if (and (csetq? cop) (eq? (varc-variable (csetq-var cop)) aux))
		(instantiate::cswitch
		   (type type)
		   (loc  loc)
		   (test (csetq-value cop))
		   (clauses cclauses))
		(instantiate::cblock
		   (type type)
		   (loc loc)
		   (body (instantiate::csequence
			    (type type)
			    (loc loc)
			    (cops (list (instantiate::local-var
					   (type *obj*)
					   (loc  loc)
					   (vars (list aux)))
				     cop
				     (instantiate::cswitch
					(type type)
					(loc  loc)
					(test (instantiate::varc
						 (type (variable-type aux))
						 (loc loc)
						 (variable aux)))
					(clauses cclauses))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::let-fun kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::let-fun kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::let-fun node (body locals loc type)
      ;; local function are open-coded on their first call site.
      ;; So, the compilation of `let-fun' construction is just
      ;; the declaration of all local functions' formals (and
      ;; a initialization mark in local function to express the
      ;; need of integration of the first call site).
      (let loop ((locals locals)
		 (all-formals '()))
	 (if (null? locals)
	     (block-kont
		(bdb-let-var
		   (instantiate::csequence
		      (type type)
		      (loc loc)
		      (cops (list (instantiate::local-var
				     (type *obj*)
				     (loc loc)
				     (vars all-formals))
			       (node->cop body kont inpushexit))))
		   loc)
		#f)
	     (let ((local (car locals)))
		(set-variable-name! local)
		(let* ((fun (widen!::sfun/C (local-value local)
			       (label (instantiate::clabel
					 (type *void*)
					 (loc  (sfun-loc (local-value local)))
					 (name (local-name local))))
			       (integrated #f)))
		       (formals (sfun-args fun)))
		   (for-each set-variable-name! formals)
		   (loop (cdr locals) (append formals all-formals))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::let-var kont inpushexit)
   (trace (cgen 3)
      "(node->cop node::let-var kont): " (shape node) #\Newline
      "  kont: " kont #\Newline)

   (define (stackable? node::app)
      (with-access::app node (stackable fun)
	 (when stackable
	    (let ((v (var-variable fun)))
	       (when (isa? v global)
		  (pair? (fun-stack-allocator (global-value v))))))))
	    
   (define (alloca-let-var n::node)
      (and (isa? n let-var)
	   (isa? (let-var-body n) app)
	   (stackable? (let-var-body n))))
   
   (define (alloca x)
      ;; check wether the "x" binding can be tranformed into
      ;; a stack allocation
      (cond
	 ((and (isa? (cdr x) app)
	       (stackable? (cdr x))
	       (bigloo-config 'have-c99-stack-alloc))
	  ;; (let-var (... (var <app-alloc>) ...) ...)
	  (with-access::app (cdr x) (fun args loc)
	     (let* ((v (var-variable fun))
		    (sa (fun-stack-allocator (global-value v))))
		(verbose 3 "      stack allocation \"" (global-name v)
		   " " loc "\n")
		;; declare the variable for the stack allocation
		(let* ((id (gensym (variable-id v)))
		       (decl (let ((d (duplicate::local (car x)
					 (id id)
					 (name #f)
					 (type *obj*))))
				(set-variable-name! d)
				d))
		       (alloc (instantiate::cpragma
				 (type *_*)
				 (loc loc)
				 (format (format (car sa) (variable-name decl)))
				 (args (map (lambda (a)
					       (node->cop a *id-kont* inpushexit))
					  args)))))
		   ;; adjust the original function call
		   (set! fun (duplicate::ref fun
				(variable (duplicate::global v
					     (name (cadr sa))))))
		   (set! args (cons (instantiate::ref
				       (loc loc)
				       (type *obj*)
				       (variable decl))
				 args))
		   (list alloc)))))
	 ((and (alloca-let-var (cdr x))
	       (bigloo-config 'have-c99-stack-alloc))
	  ;; (let-var (... (var (let-var (....) <app-alloc>)) ...) ...)
	  (with-access::let-var (cdr x) (body)
	     (with-access::app body (fun args loc)
		(let* ((v (var-variable fun))
		       (sa (fun-stack-allocator (global-value v))))
		   (verbose 3 "      stack allocation \"" (global-name v)
		      " " loc "\n")
		   ;; declare the variable for the stack allocation
		   (let* ((id (gensym (variable-id v)))
			  (decl (let ((d (duplicate::local (car x)
					    (id id)
					    (name #f)
					    (type *obj*))))
				   (set-variable-name! d)
				   d))
			  (alloc (instantiate::cpragma
				    (type *_*)
				    (loc loc)
				    (format (format (car sa) (variable-name decl)))
				    (args (map (lambda (a)
						  (node->cop a *id-kont* inpushexit))
					     args)))))
		      ;; adjust the orignal function call
		      (set! fun (duplicate::ref fun
				   (variable (duplicate::global v
						(name (cadr sa))))))
		      (set! args (cons (instantiate::ref
					  (loc loc)
					  (type *obj*)
					  (variable decl))
				    args))
		      (list alloc))))))
	 ((and (isa? (cdr x) make-box)
	       (make-box-stackable (cdr x)))
	  ;; (let-var (... (var make-stack-box) ...) ...)
	  (with-access::make-box (cdr x) (loc stackable)
	     (verbose 3 "      stack allocation \"make-cell\" " loc "\n")
	     (let* ((decl (let ((d (duplicate::local (car x)
				      (id (gensym 'box))
				      (name #f)
				      (type *cell*))))
			     (set-variable-name! d)
			     d))
		    (alloc (instantiate::cpragma
			      (type *_*)
			      (loc loc)
			      (format (format "~a ~a"
					 (if *optim-unsafe-cell?*
					     "struct bgl_unsafe_cell"
					     "struct bgl_cell")
					 (variable-name decl)))
			      (args '()))))
		(set! stackable decl)
		(list alloc))))
	 (else
	  '())))
   
   (with-access::let-var node (body bindings loc)
      (for-each (lambda (x) (set-variable-name! (car x))) bindings)
      (let ((alloca (append-map alloca bindings))
	    (decls (instantiate::local-var
		      (type *obj*)
		      (loc loc)
		      (vars (map car bindings))))
	    (sets  (map (lambda (x)
			   (node->cop (node-setq (car x) (cdr x))
			      *stop-kont* inpushexit))
		      bindings))
	    (body  (let ((cop (node->cop body kont inpushexit)))
		      (instantiate::stop
			 (type *_*)
			 (value cop)))))
	 (block-kont
	    (bdb-let-var
	       (instantiate::csequence
		  (type (cop-type body))
		  (loc loc)
		  (cops (append alloca (cons decls (append sets (list body))))))
	       loc)
	    loc))))

;*---------------------------------------------------------------------*/
;*    bdb-let-var ...                                                  */
;*---------------------------------------------------------------------*/
(define (bdb-let-var cop loc)
   (if (and (>fx *bdb-debug* 0) (location? loc))
       (instantiate::bdb-block
	  (type (cop-type cop))
	  (body cop)
	  (loc loc))
       cop))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::set-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::set-ex-it kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::set-ex-it kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::set-ex-it node (var body onexit loc type)
      (let ((exit (var-variable var)))
	 (set-variable-name! exit)
	 (instantiate::csequence
	    (type type)
	    (loc loc)
	    (cops (list
		     (instantiate::cpragma
			(type *_*)
			(loc loc)
			(format "jmp_buf_t jmpbuf")
			(args '()))
		   (instantiate::local-var
		      (type *obj*)
		      (loc loc)
		      (vars (list (var-variable var))))
		   (instantiate::cset-ex-it
		      (type type)
		      (loc loc)
		      (exit (instantiate::varc
			       (type (variable-type exit))
			       (loc loc)
			       (variable exit)))
		      (jump-value (node->cop onexit kont inpushexit))
		      (body (instantiate::csequence
			       (type (node-type body))
			       (loc loc)
			       (cops
				(list
				 (node->cop
				  (node-setq
				   exit
				   (instantiate::pragma
				      (loc loc)
				      (type *_*)
				      (format
				       (string-append
					"("
					(string-sans-$
					 (type-name (local-type exit)))
					")jmpbuf"))
				      (expr* '())))
				  *id-kont* inpushexit)
				 (node->cop body kont inpushexit))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::jump-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::jump-ex-it kont inpushexit)
   (trace (cgen 3)
      "(node->cop node::jump-ex-it kont): " (shape node) #\Newline
      "  kont: " kont #\Newline)
   (with-access::jump-ex-it node (exit value loc type)
      (let* ((vaux  (make-local-svar/name 'aux *obj*))
	     (vcop  (node->cop (node-setq vaux value) *id-kont* inpushexit))
	     (exit  exit)
	     (eaux  (make-local-svar/name 'exit *procedure*))
	     (ecop  (node->cop (node-setq eaux exit) *id-kont* inpushexit)))
	 (cond
	    ((and (csetq? vcop) (eq? (varc-variable (csetq-var vcop)) vaux)
		  (csetq? ecop) (eq? (varc-variable (csetq-var ecop)) eaux))
	     (*exit-kont*
		(instantiate::cjump-ex-it
		   (type type)
		   (loc loc)
		   (exit (csetq-value ecop))
		   (value (csetq-value vcop)))))
	    ((and (csetq? vcop) (eq? (varc-variable (csetq-var vcop)) vaux))
	     (instantiate::cblock
		(type type)
		(loc loc)
		(body (instantiate::csequence
			 (type type)
			 (loc loc)
			 (cops (list (instantiate::local-var
					(type *obj*)
					(loc loc)
					(vars (list eaux)))
				  (instantiate::csequence
				     (type *obj*)
				     (loc loc)
				     (cops (list ecop)))
				  (*exit-kont*
				     (instantiate::cjump-ex-it
					(type type)
					(loc loc)
					(exit (instantiate::varc
						 (type (variable-type eaux))
						 (loc loc)
						 (variable eaux)))
					(value (csetq-value vcop))))))))))
	    ((and (csetq? ecop) (eq? (varc-variable (csetq-var ecop)) eaux))
	     (instantiate::cblock
		(type type)
		(loc loc)
		(body (instantiate::csequence
			 (type type)
			 (loc loc)
			 (cops (list (instantiate::local-var
					(type *obj*)
					(loc loc)
					(vars (list vaux)))
				  (instantiate::csequence
				     (type *obj*)
				     (loc loc)
				     (cops (list vcop)))
				  (*exit-kont*
				     (instantiate::cjump-ex-it
					(type type)
					(loc loc)
					(exit (csetq-value ecop))
					(value (instantiate::varc
						  (type (variable-type vaux))
						  (loc loc)
						  (variable vaux)))))))))))
	    (else
	     (instantiate::cblock
		(type type)
		(loc loc)
		(body (instantiate::csequence
			 (type type)
			 (loc loc)
			 (cops
			    (list
			       (instantiate::local-var
				  (type *obj*)
				  (loc loc)
				  (vars (list eaux vaux)))
			       (instantiate::csequence
				  (type *obj*)
				  (loc loc)
				  (cops (list ecop vcop)))
			       (*exit-kont*
				  (instantiate::cjump-ex-it
				     (type type)
				     (loc loc)
				     (exit (instantiate::varc
					      (type (variable-type eaux))
					      (variable eaux)))
				     (value (instantiate::varc
					       (type (variable-type vaux))
					       (variable vaux)))))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::retblock ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::retblock kont inpushexit)
   (with-access::retblock node (body loc)
      (let* ((local (make-local-svar (gensym '__retval) (node-type node)))
	     (label (instantiate::clabel
		       (type *void*)
		       (loc loc)
		       (used? #t)
		       (name (symbol->string (gensym '__return)))
		       (body (kont (instantiate::varc
				      (type (variable-type local))
				      (loc loc)
				      (variable local))))))
	     (retkont (make-setq-kont local loc (lambda (c) c))))
	 (widen!::retblock/goto node
	    (local local)
	    (label label))
	 (instantiate::cblock
	    (type *void*)
	    (loc loc)
	    (body (instantiate::csequence
		     (type *void*)
		     (loc loc)
		     (cops (list
			      (instantiate::local-var
				 (type *obj*)
				 (loc loc)
				 (vars (list local)))
			      (node->cop body retkont inpushexit)
			      label))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::return ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::return kont inpushexit)
   (with-access::return node (block loc value)
      (let ((kont (if (isa? block retblock/goto)
		      (with-access::retblock/goto block (label local)
			 (make-setq-kont local loc 
			    (lambda (c)
			       (instantiate::csequence
				  (type *void*)
				  (loc loc)
				  (cops (list c
					   (instantiate::cgoto
					      (type *void*)
					      (loc loc)
					      (label label))))))))
		      kont)))
	 (node->cop value kont inpushexit))))

;*---------------------------------------------------------------------*/
;*    node->cop ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::make-box kont inpushexit)
   (trace (cgen 3)
      "(node->cop node::make-box kont): " (shape node) #\Newline
      "  kont: " kont #\Newline)
   
   (define (simple-app? value)
      (when (app? value)
	 (with-access::app value (fun args)
	    (when (every simple-expr? args)
	       (with-access::variable (var-variable fun) (value)
		  (with-access::fun value (side-effect)
		     (or (not side-effect))))))))
   
   (define (simple-expr? value)
      (or (var? value) (atom? value) (kwote? value) (simple-app? value)))
   
   (with-access::make-box node (value loc stackable)
      (if (simple-expr? value)
	  (node->cop value
	     (lambda (v)
		(kont
		   (instantiate::cmake-box
		      (type *cell*)
		      (loc loc)
		      (value v)
		      (stackable stackable))))
	     inpushexit)
	  (let* ((aux  (make-local-svar/name 'cellval *obj*))
		 (cval (node->cop (node-setq aux value) *id-kont* inpushexit)))
	     (instantiate::cblock
		(type *cell*)
		(loc loc)
		(body (instantiate::csequence
			 (type *cell*)
			 (loc loc)
			 (cops (list
				  (instantiate::local-var
				     (type *obj*)
				     (loc loc)
				     (vars (list aux)))
				  cval
				  (kont
				     (instantiate::cmake-box
					(type *cell*)
					(loc loc)
					(value (instantiate::varc
						  (type (variable-type aux))
						  (loc loc)
						  (variable aux)))
					(stackable stackable))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::box-ref kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::box-ref kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::box-ref node (var loc type)
      (kont (node->cop var
	       (lambda (v) (instantiate::cbox-ref
			      (type type)
			      (loc loc)
			      (var v)))
	        inpushexit))))

;*---------------------------------------------------------------------*/
;*    node->cop ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::box-set! kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::box-set! kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::box-set! node (var value loc type)
      (let ((v (var-variable var)))
	 (node->cop value
	    (lambda (vl) (kont (instantiate::cbox-set!
				  (type type)
				  (loc loc)
				  (var (instantiate::varc
					  (type (variable-type v))
					  (loc loc)
					  (variable v)))
				  (value vl))))
	     inpushexit))))

;*---------------------------------------------------------------------*/
;*    node-setq ...                                                    */
;*---------------------------------------------------------------------*/
(define (node-setq::setq variable::variable value::node)
   (instantiate::setq
      (type *unspec*)
      (loc (node-loc value))
      (var (instantiate::ref
	      (loc #f)
	      (type (variable-type variable))
	      (variable variable)))
      (value value)))

;*---------------------------------------------------------------------*/
;*    make-local-svar/name ...                                         */
;*---------------------------------------------------------------------*/
(define (make-local-svar/name::local id::symbol type::type)
   (let ((local (make-local-svar id type)))
      (if (not (string? (local-name local)))
	  (error "make-local-svar/name" "Illegal local name" local))
      local))

;*---------------------------------------------------------------------*/
;*    no-bdb-newline ...                                               */
;*    -------------------------------------------------------------    */
;*    Emit a newline only if not compiling for bdb.                    */
;*---------------------------------------------------------------------*/
(define (no-bdb-newline)
   (if (eq? *bdb-debug* 0)
       (newline *c-port*)))

;*---------------------------------------------------------------------*/
;*    node-args->cop ...                                               */
;*---------------------------------------------------------------------*/
(define (node-args->cop type args args-safe loc kont inpushexit)
   (let loop ((old-actuals args)
	      (new-actuals '())
	      (aux (make-local-svar/name 'aux *obj*))
	      (auxs '())
	      (exps '()))
      (if (null? old-actuals)
	  (if (null? auxs)
	      (kont (reverse! new-actuals))
	      (instantiate::cblock
		 (type type)
		 (body (instantiate::csequence
			  (type type)
			  (loc loc)
			  (cops (list
				   (instantiate::local-var
				      (type *obj*)
				      (vars auxs)
				      (loc  loc))
				   (instantiate::csequence
				      (type type)
				      (cops exps))
				   (kont (reverse! new-actuals))))))))
	  (let ((cop (node->cop (node-setq aux (car old-actuals))
			*id-kont* inpushexit)))
	     (if (and (csetq? cop)
		      (eq? (varc-variable (csetq-var cop)) aux)
		      (or args-safe
			  (catom? (csetq-value cop))
			  (varc? (csetq-value cop))
			  (cpragma? (csetq-value cop))))
		 (loop (cdr old-actuals)
		    (cons (csetq-value cop) new-actuals)
		    aux
		    auxs
		    exps)
		 (begin
		    (local-type-set! aux (get-type (car old-actuals) #f))
		    (loop (cdr old-actuals)
		       (cons (instantiate::varc
				(type (variable-type aux))
				(variable aux)
				(loc loc))
			  new-actuals)
		       (make-local-svar/name 'aux *obj*)
		       (cons aux auxs)
		       (cons cop exps))))))))
   
   
