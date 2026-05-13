;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/comptime/Module/library.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 16:05:09 1996                          */
;*    Last change :  Wed May 13 09:34:22 2026 (serrano)                */
;*    Copyright   :  1996-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Library finalizer                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_library
   (include "Ast/unit.sch")
   (import  type_type
	    backend_backend
	    ast_var
	    ast_env
	    ast_glo-decl
	    tools_shape
	    module_module
	    module_impuse
	    engine_param
	    bdb_setting)
   (export  (library-finalizer)
	    (with-library-module! module::symbol)))

;*---------------------------------------------------------------------*/
;*    library-finalizer ...                                            */
;*---------------------------------------------------------------------*/
(define (library-finalizer)
   ;; we set the key
   (set! *key* (gensym))
   ;; we mark the already imported modules
   (for-each (lambda (module) (putprop! module *key* #t))
      (get-imported-modules))
   ;; we also mark the dummy `foreign' module
   (putprop! 'foreign *key* #t)
   ;; and, of course, the current module
   (putprop! *module* *key* #t)
   ;; first, we collect all the needed library modules
   (for-each-global! (get-genv)
      (lambda (global)
	 (with-access::global global (occurrence module library value)
	    (when (and (>fx occurrence 0)
		       library
		       (not (or (cfun? value) (cvar? value))))
	       (need-library-module! module)))))
   ;; when compiling for bdb we must initialize the bdb module
   (if (and (>fx *bdb-debug* 0)
	    (memq 'bdb (backend-debug-support (the-backend))))
       (for-each need-library-module! *bdb-module*))
   (if (and *jvm-debug*
	    (not *lib-mode*)
	    (memq 'jvm (backend-debug-support (the-backend))))
       (for-each need-library-module! *jvm-debug-module*))
   ;; we mark all the WITH modules (i.e. modules mentionned in a with module
   ;; clause or in a -with command line option)
   (for-each need-library-module! *with-library-modules*)
   ;; then we declare a special unit
   (let ((modules *needed-modules*))
      (if (null? modules)
	  '()
	  (let loop ((modules modules)
		     (init-call* '(#unspecified)))
	     (if (null? modules)
		 (let ((body (if (and (>fx *debug-module* 0)
				      (memq 'module
					 (backend-debug-support
					    (the-backend))))
				 `((begin
				      (pragma::void
					 ,(string-append
					     "bgl_init_module_debug_library(\""
					     (symbol->string *module*)
					     "\")"))
				      ,@init-call*))
				 init-call*)))
		    (unit 'library-modules 2 body #t #f))
		 (let* ((mid (car modules))
			(id (module-initialization-id mid)))
		    (module-declare-init! (get-genv) id mid)
		    (loop (cdr modules)
		       (cons `((@ ,id ,mid)
			       0
			       ;; 0 means here not to perform version
			       ;; checking about library
			       ,(symbol->string *module*))
			  init-call*))))))))

;*---------------------------------------------------------------------*/
;*    module-declare-init! ...                                         */
;*---------------------------------------------------------------------*/
(define (module-declare-init! env id::symbol mid::symbol)
   (unless (find-global/module env id mid)
      (declare-global-sfun! env id id
	 '(checksum::long path::string) mid 'import 'sfun #f #f)))

;*---------------------------------------------------------------------*/
;*    *needed-modules* ...                                             */
;*---------------------------------------------------------------------*/
(define *needed-modules* '())

;*---------------------------------------------------------------------*/
;*    *key* ...                                                        */
;*---------------------------------------------------------------------*/
(define *key* #unspecified)

;*---------------------------------------------------------------------*/
;*    need-library-module! ...                                         */
;*---------------------------------------------------------------------*/
(define (need-library-module! module::symbol)
   (if (not (getprop module *key*))
       (begin
	  (putprop! module *key* #t)
	  (set! *needed-modules* (cons module *needed-modules*)))))

;*---------------------------------------------------------------------*/
;*    *with-key* ...                                                   */
;*---------------------------------------------------------------------*/
(define *with-key* (gensym 'with-key))

;*---------------------------------------------------------------------*/
;*    *with-library-modules* ...                                       */
;*---------------------------------------------------------------------*/
(define *with-library-modules* '())

;*---------------------------------------------------------------------*/
;*    with-library-module! ...                                         */
;*---------------------------------------------------------------------*/
(define (with-library-module! module::symbol)
   (unless (getprop module *with-key*)
      (putprop! module *with-key* #t)
      (set! *with-library-modules* (cons module *with-library-modules*))))

