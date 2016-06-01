;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cc/roots.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 28 09:37:13 2016                          */
;*    Last change :  Wed Jun  1 11:51:37 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Explicit GC roots registration                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cc_roots
   
   (include "Ast/unit.sch")
   
   (import backend_backend
	   module_module
	   tools_shape
	   type_type
	   ast_var
	   ast_env)
   
   (export (make-gc-roots-unit)
	   (gc-roots-emit ::output-port)))

;*---------------------------------------------------------------------*/
;*    make-gc-roots-unit ...                                           */
;*---------------------------------------------------------------------*/
(define (make-gc-roots-unit)
   (unit 'gc-roots
      -1
      (if (backend-pragma-support (the-backend))
	  '(begin (pragma::obj "bgl_gc_roots_register()"))
	  `(begin #unspecified))
      #t
      #f)) 

;*---------------------------------------------------------------------*/
;*    gc-roots-emit ...                                                */
;*---------------------------------------------------------------------*/
(define (gc-roots-emit port)
   (display "/* GC roots registration */\n" port)
   (display "static obj_t bgl_gc_roots_register() {\n" port)
   (display "#if defined( BGL_GC_ROOTS )\n" port)
   (display "#define ADD_ROOT( addr ) (addr > roots_max ? roots_max = addr : (addr < roots_min ? roots_min = addr : 0))\n" port)
   (display "void *roots_min = (void*)ULONG_MAX, *roots_max = 0;\n" port)
   (for-each-global!
      (lambda (global)
	 (when (is-local-gc-root? global)
	    (emit-gc-registration global port))))
   (display "#undef ADD_ROOT\n" port)
   (display "if( roots_max > 0 ) GC_add_roots( roots_min, ((void **)roots_max) + 1 );\n" port)
   (display "#endif\n" port)
   (display "return BUNSPEC;\n" port)
   (display "}\n\n" port))

;*---------------------------------------------------------------------*/
;*    is-local-gc-root? ...                                            */
;*---------------------------------------------------------------------*/
(define (is-local-gc-root? global)
   (when (svar? (global-value global))
      (with-access::variable global (name type user?)
	 (when (and user? name (bigloo-type? type))
	    (eq? (global-module global) *module*)))))

;*---------------------------------------------------------------------*/
;*    emit-gc-registration ...                                         */
;*---------------------------------------------------------------------*/
(define (emit-gc-registration global::variable port)
   (with-access::variable global (name)
      (display "ADD_ROOT( (void *)(&" port)
      (display name port)
      (display ") );\n" port)))
