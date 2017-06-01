;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Patch/patch.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 31 10:22:17 2017                          */
;*    Last change :  Thu Jun  1 09:06:22 2017 (serrano)                */
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
	    (patch->sexp exp stack loc site)
	    (bind-patch->sexp exp stack loc site)
	    (patch-initialization! ast backend)))

;*---------------------------------------------------------------------*/
;*    make-patch-unit ...                                              */
;*---------------------------------------------------------------------*/
(define (make-patch-unit)
   (unit 'patch
      0
      '(begin #t)
      #t
      #f))

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
      ((?- ?ref ?val)
       (set! *patch-armed* #t)
       (let ((ref (sexp->node ref stack loc 'value)))
	  (if (not (and (var? ref) (local? (var-variable ref))))
	      (error-sexp->node "Illegal patch" exp loc)
	      (instantiate::patch
		 (loc loc)
		 (ref ref)
		 (type (get-patch-type val exp loc))
		 (value val)))))
      (else
       (error-sexp->node "Illegal patch" exp loc))))

;*---------------------------------------------------------------------*/
;*    bind-patch ...                                                   */
;*---------------------------------------------------------------------*/
(define (bind-patch->sexp exp stack loc site)
   (match-case exp
      ((bind-patch (and ?vars (? (lambda (l) (every symbol? l)))) . ?exps)
       (sexp->node
	  (epairify-rec
	     `(let ,(map (lambda (var)
			    (list var
			       (instantiate::genpatchid
				  (loc loc)
				  (type *long*))))
		       vars)
		 ,@exps) exp)
	  stack loc site))
      (else
       (error-sexp->node "Illegal bind-patch" exp loc))))

;*---------------------------------------------------------------------*/
;*    get-patch-type ...                                               */
;*---------------------------------------------------------------------*/
(define (get-patch-type value exp loc)
   (cond
      ((fixnum? value) *long*)
      ((or (int32? value) (uint32? value)) *int32*)
      ((or (int64? value) (uint64? value)) *int64*)
      ((and (pair? value) (eq? (car value) 'quote)) (get-type-kwote (cadr value)))
      (else (error-sexp->node "Illegal patch value" exp loc))))

;*---------------------------------------------------------------------*/
;*    patch-initialization! ...                                        */
;*---------------------------------------------------------------------*/
(define (patch-initialization! ast backend)
   
   (define global-patch-index 0)
   
   (define (patch-init! g::global patches::pair)
      (let* ((fun (variable-value g))
	     (idx global-patch-index)
	     (len (length patches)))
	 ;; re-number the patches per function
	 (set! global-patch-index (+fx global-patch-index len))
	 (instantiate::pragma
	    (type *void*)
	    (format (format "bgl_init_patch_64( &~a, ~a, &(__bgl_patches[ ~a ]) )"
		       (set-variable-name! g) len idx)))))
   
   (when *patch-armed*
      ;; first replace all the genpatchid by their actual value
      (for-each (lambda (g)
		   (let ((fun (variable-value g)))
		      (with-access::sfun fun (body)
			 (set! body
			    (resolve-genpatchid! body '() *patch-index*)))))
	 ast)
      
      (let ((patches (filter-map collect-patches ast)))
	 (when (pair? patches)
	    (let* ((glo (find-global 'patch-init))
		   (fun (variable-value glo)))
	       (sfun-body-set! fun
		  (instantiate::sequence
		     (type *obj*)
		     (nodes (list
			       (instantiate::pragma
				  (type *void*)
				  (format "bgl_init_self_mod()"))
			       (instantiate::sequence
				  (type *void*)
				  (nodes (append
					    (map (lambda (e)
						    (patch-init! (car e) (cdr e)))
					       patches))))
			       (instantiate::literal
				  (type *obj*)
				  (value #unspecified)))))))))
      
      (let ((pvector (def-global-svar! '__bgl_patches
			*module*
			'patches
			'never))
	    (typ (declare-type! '__bgl_patch_descr "__bgl_patch_descr" 'C)))
	 (global-user?-set! pvector #f)
	 (global-import-set! pvector 'static)
	 (global-type-set! pvector typ)
	 (global-name-set! pvector
	    (format "__bgl_patches[ ~a ]" global-patch-index)))))

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
;*    collect-pcaches* ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-patches* this::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-patches* ::patch ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-patches* this::patch)
   (list this))
		  
;*---------------------------------------------------------------------*/
;*    resolve-genpatchid! ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve-genpatchid! this::node env ibase)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve-genpatchid! ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve-genpatchid! this::patch env ibase)
   (with-access::patch this (ref genpatchid)
      (cond
	 ((not (isa? ref var))
	  (user-error "resolve-genpatchid!" "Illegal patch" this))
	 ((assq (var-variable ref) env)
	  =>
	  (lambda (b)
	     (set! genpatchid (cdr b))
	     this))
	 (else
	  (user-error "resolve-genpatchid!" "Illegal patch" this)))))

;*---------------------------------------------------------------------*/
;*    resolve-genpatchid! ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve-genpatchid! this::genpatchid env ibase)
   (user-error "resolve-genpatchid!" "Illegal genpatchid" this))

;*---------------------------------------------------------------------*/
;*    resolve-genpatchid! ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve-genpatchid! this::var env ibase)
   (let ((index (assq (var-variable this) env)))
      (if index
	  (cdr index)
	  this)))

;*---------------------------------------------------------------------*/
;*    resolve-genpatchid! ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve-genpatchid! this::let-var env ibase)
   
   (define (resolve-binding! b)
      (cond
	 ((not (isa? (cdr b) genpatchid))
	  (set-cdr! b (resolve-genpatchid! (cdr b) env ibase))
	  #f)
	 ((>fx (variable-occurrencew (car b)) 0)
	  (user-error "resolve-genpatchid!"
	     "Variable should be immutable"
	     (car b)))
	 (else
	  (with-access::genpatchid (cdr b) (index rindex)
	     (let ((idx (get-patch-index)))
		(set! index idx)
		(set! rindex (-fx idx ibase)))
	     #t))))
   
   (with-access::let-var this (body bindings loc)
      (let ((nenv (append (filter resolve-binding! bindings) env)))
	 (set! body (resolve-genpatchid! body nenv ibase))
	 this)))
   


