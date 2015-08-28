;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/init.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 17:59:38 1995                          */
;*    Last change :  Fri Aug 28 08:41:36 2015 (serrano)                */
;*    Copyright   :  1995-2015 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We produce a Bigloo's `main' function.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_init
   (import  engine_param
	    module_module
	    tools_shape
	    type_type
	    type_cache
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_env
	    ast_sexp
	    ast_local
	    ast_glo-def
	    ast_unit
	    ast_occur
	    ast_lvtype
	    coerce_coerce
	    backend_backend
	    backend_cplib)
   (export  (get-module-init)))

;*---------------------------------------------------------------------*/
;*    *module-init* ...                                                */
;*---------------------------------------------------------------------*/
(define *module-init* #unspecified)

;*---------------------------------------------------------------------*/
;*    get-module-init ...                                              */
;*---------------------------------------------------------------------*/
(define (get-module-init)
   (if (eq? *module-init* #unspecified)
       (set! *module-init* (make-module-init)))
   *module-init*)

;*---------------------------------------------------------------------*/
;*    make-module-init ...                                             */
;*---------------------------------------------------------------------*/
(define (make-module-init)

   (define (ubody-sans-debug)
      `(if require-initialization
	   (begin
	      (set! require-initialization #f)
	      ,(when *dlopen-init-gc* (backend-gc-init (the-backend)))
	      ,@(unit-init-calls))
	   #unspecified))

   (define (ubody-debug)
      (let ((tmp (gensym 'tmp)))
	 `(if require-initialization
	      (begin
		 (set! require-initialization #f)
		 ,(when *dlopen-init-gc* (backend-gc-init (the-backend)))
		 (pragma
		    ,(string-append "bgl_init_module_debug_start(\""
			(symbol->string *module*)
			"\")"))
		 (let ((,tmp (begin ,@(unit-init-calls))))
		    (pragma
		       ,(string-append "bgl_init_module_debug_end(\""
			   (symbol->string *module*)
			   "\")"))
		    ,tmp))
	      #unspecified)))
   
   (let* ((req  (def-global-svar! 'require-initialization::obj
		   *module*
		   'module-initalization
		   'now))
	  (bc    (the-backend))
	  (dbg   (and (>fx *debug-module* 0)
		      (memq 'module (backend-debug-support bc))
		      (backend-pragma-support bc)))
	  (ubody (if dbg (ubody-debug) (ubody-sans-debug)))
	  (body (if *unsafe-version*
		    ubody
		    `(if (=fx (bit-and checksum ,*module-checksum*) checksum)
			 ,ubody
			 ,(if (backend-pragma-support (the-backend))
			      `(let ((s::string
				      (pragma::string
				       ,(format "~s" (symbol->string *module*)))))
				  (module-init-error s from))
			      `(module-init-error ,(symbol->string *module*)
						  from)))))
	  (cvar (make-local-svar 'checksum *long*))
	  (nvar (make-local-svar 'from *string*))
	  (node (let ((node (sexp->node body (list cvar nvar) '() 'value)))
		   (lvtype-node! node)
		   (coerce!  node req *obj* #f)))
	  (init (def-global-sfun-no-warning!
		   (module-initialization-id *module*)
		   '(checksum from)
		   (list cvar nvar)
		   *module*
		   'sfun
		   'module-initialization
		   'now
		   node)))
      (set-variable-name! req)
      (global-import-set! init 'export)
      (global-type-set! init *obj*)
      (occur-node-in! node init)
      init))

