;*=====================================================================*/
;*    .../project/bigloo/5.0.x/comptime/SawMill/woodcutter.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri May 22 08:29:57 2026                          */
;*    Last change :                                                    */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Generate an intermediate SawMill representation of a function    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_woodcutter
   (import type_type
	   type_cache
	   ast_env
	   ast_var
	   ast_node
	   engine_pass
	   write_ast
	   object_class
	   engine_param
	   module_module
	   tools_shape
	   backend_backend
	   saw_defs
	   saw_node2rtl
	   saw_collapse
	   saw_remove
	   saw_inline_return
	   saw_blockorder
	   saw_gotos
	   saw_cast)
   (export (global->blocks::pair-nil b::backend v::global)))

;*---------------------------------------------------------------------*/
;*    Debugging configuration                                          */
;*---------------------------------------------------------------------*/
(define *collapse* #t)
(define *remove* #t)
(define *saw_inline_returns* #t)
(define *cast* #t)

;*---------------------------------------------------------------------*/
;*    woodcutter ...                                                   */
;*---------------------------------------------------------------------*/
(define (woodcutter::pair-nil back::backend v::global)
   (let ((b (global->rtl v))
	 (args (map local->reg (sfun-args (global-value v)))))
      (when *collapse* (collapse b))
      (when *remove* (set! b (remove b)))
      (when *saw_inline_returns* (inline-returns b))
      (let ((l (block-ordering b)))
	 (let mark ((i 0) (l l))
	    (when (pair? l)
	       (block-label-set! (car l) i)
	       (mark (+fx i 1) (cdr l))))
	 (add-gotos l)
	 (when *cast* (add-casts back l))
	 l)))
      
;*---------------------------------------------------------------------*/
;*    call-with-ast-port ...                                           */
;*---------------------------------------------------------------------*/
(define (call-with-ast-port proc)
   (let ((f (ast-filename)))
      (if (string? f)
	  (call-with-append-file f proc)
	  (proc (current-output-port)))))

;*---------------------------------------------------------------------*/
;*    global->blocks ...                                               */
;*---------------------------------------------------------------------*/
(define (global->blocks b::backend v::global)
   (let ((l (woodcutter b v)))
      (when (eq? *pass* 'sawmill)
	 (call-with-ast-port
	    (lambda (port)
	       (fprintf port ";; ~a\n" (global-id v))
	       (fprintf port "(~a ~a\n" (typeof (global-value v)) (global-id v))
	       (dump l port 0)
	       (display ")\n" port))))
      l))
