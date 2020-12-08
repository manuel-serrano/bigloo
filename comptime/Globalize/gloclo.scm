;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/gloclo.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb  3 09:56:11 1995                          */
;*    Last change :  Tue Aug  9 08:57:35 2016 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The global closure creation                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_global-closure
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_args
	    tools_error
	    engine_param
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_sexp
	    ast_local
	    ast_glo-def
	    ast_ident
	    module_module
	    globalize_ginfo
	    globalize_node
	    globalize_free
	    ast_env)
    (export (global-closure::global ::global <loc>)
	    (make-global-closure::global ::global)
	    (foreign-closures)))

;*---------------------------------------------------------------------*/
;*    global-closure ...                                               */
;*---------------------------------------------------------------------*/
(define (global-closure global loc)
   (the-global-closure global loc)
   (make-global-closure global))
      
;*---------------------------------------------------------------------*/
;*    make-global-closure ...                                          */
;*---------------------------------------------------------------------*/
(define (make-global-closure global)
   (let ((glo (global/Ginfo-global-closure global)))
      (if (global? glo)
	  glo
	  (let ((old-fun (global-value global)))
	     (if (and (sfun? old-fun)
		      (or (pair? (sfun-optionals old-fun))
			  (pair? (sfun-keys old-fun)))
		      (eq? (global-module global) *module*))
		 (make-opt/key-global-closure global)
		 (make-noopt-global-closure global))))))

;*---------------------------------------------------------------------*/
;*    make-opt/key-global-closure ...                                  */
;*---------------------------------------------------------------------*/
(define (make-opt/key-global-closure global)
   (let ((gloclo (find-global (symbol-append '_ (global-id global))
			      (global-module global))))
      (fill-gloclo! global gloclo)
      (sfun-the-closure-global-set! (global-value gloclo) global)
      gloclo))

;*---------------------------------------------------------------------*/
;*    default-type ...                                                 */
;*---------------------------------------------------------------------*/
(define (default-type)
   (if (and *optim-cfa-unbox-closure-args* (>=fx *optim* 2)) *_* *obj*))

;*---------------------------------------------------------------------*/
;*    make-noopt-global-closure ...                                    */
;*---------------------------------------------------------------------*/
(define (make-noopt-global-closure global)
   (let* ((old-fun  (global-value global))
	  (env      (let ((var (make-local-svar 'env *procedure*)))
		       (widen!::local/Ginfo var)
		       var))
	  (new-args (map (lambda (old)
			    (let ((new (make-local-svar
					  (if (local? old)
					      (local-id old)
					      (gensym))
					  (default-type))))
			       (when (local? old)
				  (local-user?-set! new (local-user? old)))
			       (widen!::svar/Ginfo (local-value new))
			       (widen!::local/Ginfo new)
			       new))
		       (if (sfun? old-fun)
			   (sfun-args old-fun)
			   (if (cfun? old-fun)
			       ;; cfun-args-type is a list of type
			       ;; not a list of local. It doesn't
			       ;; matter. What is important here
			       ;; is just the list.
			       (cfun-args-type old-fun)
			       (internal-error "make-global-closure"
				  "Unexpected value"
				  old-fun)))))
	  (loc      (if (sfun? old-fun)
			(if (and (node? (sfun-body old-fun))
				 (node-loc (sfun-body old-fun)))
			    (node-loc (sfun-body old-fun))
			    (sfun-loc old-fun))))
	  (gloclo   (gloclo global env new-args))
	  (new-fun  (global-value gloclo)))
      ;; associate the closure entry point and the function
      (sfun-the-closure-global-set! (global-value gloclo) global)
      ;; we must set now the info slot of env
      (widen!::svar/Ginfo (local-value env))
      ;; we ajust the function definition
      (widen!::global/Ginfo gloclo (escape? #t))
      (with-access::sfun new-fun (body)
	 (set! body (make-noopt-body loc global new-args)))
      (trace (globalize 2) "=======> J'ai cree le corps:"
	 (shape (sfun-body new-fun))
	 " " (shape gloclo)
	 #\Newline)
      gloclo))

;*---------------------------------------------------------------------*/
;*    make-noopt-body ...                                              */
;*---------------------------------------------------------------------*/
(define (make-noopt-body loc global new-args)
   (instantiate::app
      (loc loc)
      (type (strict-node-type (global-type global) *obj*))
      (fun (instantiate::var
	      (loc loc)
	      (type (strict-node-type (global-type global) *_*))
	      (variable global)))
      ;; we have to ignore the addition environment
      ;; parameters, so we just take the cdr of the
      ;; formals list.
      (args (map (lambda (v)
		    (instantiate::var
		       (loc loc)
		       (type (strict-node-type (variable-type v) *obj*))
		       (variable v)))
		 new-args))))

;*---------------------------------------------------------------------*/
;*    *foreign-closures* ...                                           */
;*---------------------------------------------------------------------*/
(define *foreign-closures* '())

;*---------------------------------------------------------------------*/
;*    foreign-closures ...                                             */
;*---------------------------------------------------------------------*/
(define (foreign-closures)
   (let ((res *foreign-closures*))
      (set! *foreign-closures* '())
      res))
   
;*---------------------------------------------------------------------*/
;*    globalized-type-id ...                                           */
;*    -------------------------------------------------------------    */
;*    The type of the procedure entry point.                           */
;*    -------------------------------------------------------------    */
;*    See (@ globalized-type globalize_local->global) for a similar    */
;*    function.                                                        */
;*---------------------------------------------------------------------*/
(define (globalized-type-id global)
   (let ((type (global-type global)))
      (cond
	 ((not *optim-cfa-unbox-closure-args*)
	  'obj)
	 ((bigloo-type? type)
	  (type-id type))
	 ((eq? (global-import global) 'static)
	  ;; if the global is static, let the CFA finds the most
	  ;; appropriate type of this closure
	  '_)
	 (else
	  (type-id (get-bigloo-type type))))))

;*---------------------------------------------------------------------*/
;*    gloclo ...                                                       */
;*---------------------------------------------------------------------*/
(define (gloclo global env::local args)   
   (let* ((arity  (fun-optional-arity (global-value global)))
	  (id     (let ((str (string-append
			      "&"
			      (symbol->string (global-id global)))))
		     (if (symbol-exists? str)
			 (gensym (symbol-append '& (global-id global)))
			 (symbol-append '& (global-id global)))))
	  (tyid   (globalized-type-id global))
	  (gloclo (def-global-sfun-no-warning! (make-typed-ident id tyid)
		     (make-n-proto (+-arity arity 1))
		     (cons env args)
		     (if (eq? (global-import global) 'foreign)
			 *module*
			 (global-module global))
		     (global-import global)
		     'sfun
		     'now
		     #unspecified)))
      (fun-args-noescape-set! (variable-value gloclo)
	 (fun-args-noescape (variable-value global)))
      (fill-gloclo! global gloclo)))

;*---------------------------------------------------------------------*/
;*    fill-gloclo! ...                                                 */
;*---------------------------------------------------------------------*/
(define (fill-gloclo! global::global gclo::global)
   (global/Ginfo-global-closure-set! global gclo)
   ;; we have to propagate the location definition
   ;; of the local variable
   (let ((sfun (global-value global)))
      (when (sfun? sfun)
	 (sfun-loc-set! (global-value gclo) (sfun-loc sfun)))
      ;; Closure function are now always static. I don't know I did
      ;; it different before. If this turn out to be wrong, I will
      ;; be necessary to restore the old code that was:
      ;;     (global-import-set! gclo (global-import global))
      ;; For that modification I have added a new kind of removable
      ;; property: NEVER.
      ;; The Cgen prototyper (Cgen/proto.scm) checks for value prior
      ;; to decide not to emit a C function prototype.
      (if (eq? (global-import global) 'foreign)
	  (begin
	     (set! *foreign-closures* (cons gclo *foreign-closures*))
	     (global-removable-set! gclo 'never)
	     (global-import-set! gclo 'static))
	  (begin
	     (if (not (eq? (global-import global) 'static))
		 (global-removable-set! gclo 'never))
	     (global-import-set! gclo 'static)))
      (fun-side-effect-set! (global-value gclo) (fun-side-effect sfun))
      (if (not (global? gclo))
	  (internal-error "global-closure" "Can't allocate global closure" gclo)
	  gclo)))
