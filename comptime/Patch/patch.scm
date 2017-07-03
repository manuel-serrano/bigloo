;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Patch/patch.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 31 10:22:17 2017                          */
;*    Last change :  Tue Jun 27 15:40:50 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Patch management                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module patch_patch
   (include "Ast/unit.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_error
	    tools_misc
	    type_cache
	    type_typeof
	    type_type
	    type_env
	    object_class
	    object_slots
	    module_module
	    engine_param
	    ast_sexp
	    ast_glo-def
	    ast_local
	    ast_apply
	    ast_app
	    ast_dump
	    ast_env
	    ast_walk
	    backend_cplib)
   (export  (make-patch-unit)
	    (emit-patch-header ::output-port)
	    (patch->sexp exp stack loc site)
	    (patch-index->sexp exp stack loc site)
	    (patch-initialization! ast backend)))

;*---------------------------------------------------------------------*/
;*    make-patch-unit ...                                              */
;*---------------------------------------------------------------------*/
(define (make-patch-unit)
   (unit 'patch 0 '((begin #t)) #t #f))

;*---------------------------------------------------------------------*/
;*    emit-patch-header ...                                            */
;*---------------------------------------------------------------------*/
(define (emit-patch-header port)
   (fprint port "/* runtime code modification */\n"
      "#define BGL_SELF_MODIFYING_CODE "
      (if *optim-patch?* 1 0)
      "\n"))

;*---------------------------------------------------------------------*/
;*    *patch-index* ...                                                */
;*---------------------------------------------------------------------*/
(define *patch-index* 0)
(define *patch-armed* #f)

;*---------------------------------------------------------------------*/
;*    get-patch-index ...                                              */
;*---------------------------------------------------------------------*/
(define (get-patch-index)
   (let ((idx *patch-index*))
      (set! *patch-index* (+fx 1 *patch-index*))
      idx))

;*---------------------------------------------------------------------*/
;*    patch->sexp ...                                                  */
;*---------------------------------------------------------------------*/
(define (patch->sexp exp stack loc site)
   (match-case exp
      ((?- ?ref ?init)
       (set! *patch-armed* #t)
       (let ((ref (sexp->node ref stack loc 'value)))
	  (if (not (var? ref))
	      (error-sexp->node "Illegal patch" exp loc)
	      (let* ((typ (get-patch-type init exp loc))
		     (aux (make-local-svar (gensym 'patch) typ))
		     (init (sexp->node init stack loc 'value)))
		 (variable-access-set! aux 'write)
		 (instantiate::let-var
		    (type typ)
		    (bindings (list (cons aux init)))
		    (body (instantiate::patch
			     (loc loc)
			     (ref ref)
			     (type typ)
			     (value (instantiate::var
				       (type typ)
				       (variable aux))))))))))
      (else
       (error-sexp->node "Illegal patch" exp loc))))

;*---------------------------------------------------------------------*/
;*    patch-index->sexp ...                                            */
;*---------------------------------------------------------------------*/
(define (patch-index->sexp exp stack loc site)
   (match-case exp
      ((?-)
       (instantiate::genpatchid
	  (loc loc)
	  (type *long*)))
      (else
       (error-sexp->node "Illegal patch-index" exp loc))))

;*---------------------------------------------------------------------*/
;*    get-patch-type ...                                               */
;*---------------------------------------------------------------------*/
(define (get-patch-type value exp loc)
   (cond
      ((fixnum? value) *long*)
      (else *obj*)))

;*---------------------------------------------------------------------*/
;*    patch-initialization! ...                                        */
;*---------------------------------------------------------------------*/
(define (patch-initialization! ast backend)
   (let ((glo (find-global 'patch-init)))
      (when (global? glo)
	 '(let ((fun (variable-value glo)))
	   (sfun-side-effect-set! fun #t)
	   (sfun-body-set! fun
	      (instantiate::sequence
		 (type *obj*)
		 (nodes (if *main*
			    (list *main*
			       (instantiate::pragma
				  (type *void*)
				  (format "bgl_init_patch( bgl_patch_descrs )"))
			       (instantiate::literal
				  (type *obj*)
				  (value #unspecified)))
			    (list
			       (instantiate::literal
				  (type *obj*)
				  (value #unspecified))))))))
   
	 (let ((pvector (def-global-svar! 'bgl_patch_descrs
			   *module*
			   'bgl_patch_descr_t
			   'never))
	       (typ (declare-type! '__bgl_patch_descr_t "bgl_patch_descr_t" 'C)))
	    (global-user?-set! pvector #f)
	    (global-import-set! pvector 'static)
	    (global-type-set! pvector typ)
	    (global-name-set! pvector
	       "* bgl_patch_descrs[ 2 ] = { NULL, NULL }")
	    '()))))

(define (patch-initialization!to-be-removed-2017-06-20 ast backend)
   
   (define global-patch-index 0)
   
   (define patch-init-nodes '())
   
   (define (link-patches! g patches patchtypes sizeof)
      (let ((tidx 0)
	    (idx global-patch-index))
	 (for-each (lambda (p)
		      (with-access::patch p (patchid type index)
			 (when (memq type patchtypes)
			    (with-access::genpatchid patchid (index)
			       (set! index global-patch-index)
			       (set! global-patch-index
				  (+fx 1 global-patch-index)))
			    (set! index tidx)
			    (set! tidx (+fx 1 tidx)))))
	    patches)
	 (when (>fx tidx 0)
	    (let ((init (instantiate::pragma
			   (type *void*)
			   (format (format "bgl_init_patch_~a( &~a, ~a, &(__bgl_patches[ ~a ]) )"
				      sizeof
				      (set-variable-name! g) tidx idx)))))
	       (set! patch-init-nodes (cons init patch-init-nodes))))))

   (when *patch-armed*
      '(let ((env0 (patch-global-env)))
	(for-each (lambda (g)
		     (let ((fun (variable-value g)))
			(with-access::sfun fun (body)
			   (let ((patches (make-cell '())))
			      (bind-patch! body env0 patches)
			      (when (pair? (cell-ref patches))
				 (if (eq? (bigloo-config 'elong-size) 32)
				     (link-patches! g (cell-ref patches)
					(list *long* *obj*) 32)
				     (begin
					(link-patches! g (cell-ref patches)
					   (list *long*) 32)
					(link-patches! g (cell-ref patches)
					   (list *obj*) 64))))))))
	   ast))
      
      '(let* ((glo (find-global 'patch-init))
	     (fun (variable-value glo)))
	 (sfun-side-effect-set! fun #t)
	 (sfun-body-set! fun
	    (instantiate::sequence
	       (type *obj*)
	       (nodes (list
			 (when *main*
			    (instantiate::pragma
			       (type *void*)
			       (format "bgl_init_self_mod()")))
			 (instantiate::sequence
			    (type *void*)
			    (nodes (reverse! patch-init-nodes)))
			 (instantiate::literal
			    (type *obj*)
			    (value #unspecified)))))))
      
      (let ((pvector (def-global-svar! '__bgl_patches
			*module*
			'patches
			'never))
	    (typ (if *optim-patch?*
		     (declare-type! '__bgl_patch_descr "__bgl_patch_descr" 'C)
		     (declare-type! '__bgl_patch_descr "long" 'C))))
	 (global-user?-set! pvector #f)
	 (global-import-set! pvector 'static)
	 (global-type-set! pvector typ)
	 (global-name-set! pvector
	    (format "__bgl_patches[ ~a ]" global-patch-index))
	 '())))

;*---------------------------------------------------------------------*/
;*    collect-patches ...                                              */
;*---------------------------------------------------------------------*/
(define (collect-patches var::global)
   (let* ((fun (variable-value var))
	  (body (sfun-body fun))
	  (patches (collect-patches* body)))
      (when (pair? patches)
	 (cons var patches))))

;*---------------------------------------------------------------------*/
;*    collect-pcaches* ::node ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-patches* this::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-patches* ::patch ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-patches* this::patch)
   (list this))
		  
;*---------------------------------------------------------------------*/
;*    bind-patch! ::node ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-patch! this::node env patches)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    bind-patch! ::patch ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-patch! this::patch env patches)
   (with-access::patch this (ref index patchid loc)
      (cond
	 ((not (isa? ref var))
	  (user-error/location loc "patch" "illegal patch" this))
	 ((assq (var-variable ref) env)
	  =>
	  (lambda (b)
	     (if (eq? patchid #unspecified)
		 (begin
		    (set! patchid (cddr b))
		    (cell-set! patches (cons this (cell-ref patches)))
		    this)
		 (user-error/location loc "patch" "duplicated index" index))))
	 (else
	  (user-error/location loc "patch" "unbound patch" this)))))

;*---------------------------------------------------------------------*/
;*    bind-patch! ::var ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-patch! this::var env patches)
   (let ((pid (assq (var-variable this) env)))
      (if pid (cadr pid) this)))

;*---------------------------------------------------------------------*/
;*    bind-patch! ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-patch! this::setq env patches)
   (with-access::setq this (value)
      (set! value (bind-patch! value env patches)))
   this)

;*---------------------------------------------------------------------*/
;*    bind-patch! ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-patch! this::let-var env patches)
   
   (define (resolve-binding! b)
      (cond
	 ((not (isa? (cdr b) genpatchid))
	  (set-cdr! b (bind-patch! (cdr b) env patches))
	  #f)
	 ((>fx (variable-occurrencew (car b)) 0)
	  (with-access::node (cdr b) (loc)
	     (user-error/location loc "patch-index"
		"index must be bound to immutable variable"
		(car b))))
	 (else
	  (cons (car b) (cons (cdr b) (cdr b))))))
   
   (with-access::let-var this (body bindings loc)
      (let ((nenv (append (filter-map resolve-binding! bindings) env)))
	 (bind-patch! body nenv patches)
	 this)))

;*---------------------------------------------------------------------*/
;*    patch-global-env ...                                             */
;*    -------------------------------------------------------------    */
;*    Collect the patch-index call of the global variables             */
;*    initialization section.                                          */
;*---------------------------------------------------------------------*/
(define (patch-global-env)
   (let* ((glo (find-global 'toplevel-init))
	  (fun (variable-value glo)))
      (collect-init-indexes* (sfun-body fun))))

;*---------------------------------------------------------------------*/
;*    collect-init-indexes* ::node ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-init-indexes* this::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-init-indexes* ::setq ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-init-indexes* this::setq)
   
   (define (genpatch-value node)
      (cond
	 ((isa? node genpatchid)
	  (values node node))
	 ((isa? node app)
	  (with-access::app node (fun args)
	     (when (and (pair? args) (null? (cdr args)))
		(when (isa? (car args) genpatchid)
		   (let ((v (var-variable fun)))
		      (when (and (global? v) (cfun? (global-value v)))
			 (when (eq? (global-id v) '$long->bint)
			    (values node (car args)))))))))
	 (else
	  #f)))
   
   (with-access::setq this (var value)
      (multiple-value-bind (node genpatchid)
	 (genpatch-value value)
	 (if node
	     (with-access::var var (variable loc)
		(if (>fx (variable-occurrencew variable) 1)
		    (user-error/location loc "patch-index"
		       "index must be bound to immutable variable"
		       variable)
		    (list (cons variable (cons node genpatchid)))))
	     (call-default-walker)))))
	 
      

