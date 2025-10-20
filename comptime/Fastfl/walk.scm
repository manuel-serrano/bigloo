;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Fastfl/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Mon Oct 20 09:15:02 2025 (serrano)                */
;*    Copyright   :  2010-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Optimize fast flonum operations by removing useless conversions  */
;*    and type tests.                                                  */
;*    -------------------------------------------------------------    */
;*    This optimization must execute before the coercion. It           */
;*    implements the two following transformations:                    */
;*      ($fast-flonum? x::double) -> #t                                */
;*      ($fast-real->double x::double) -> x                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module fastfl_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_location
	    type_cache
	    ast_ident
	    ast_local
	    ast_env
	    ast_sexp
	    ast_private
	    ast_lvtype
	    ast_dump
	    ast_walk
	    effect_effect
	    engine_param
	    backend_backend)
   (export  (fastfl-walk! globals)))

;*---------------------------------------------------------------------*/
;*    fastfl-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (fastfl-walk! globals)
   (pass-prelude "fastfl" init-fastfl-cache!) 
   (for-each fastfl-fun! globals)
   (pass-postlude globals clear-fastfl-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *$fast-flonum?* #f)
(define *$fast-real->double* #f)

;*---------------------------------------------------------------------*/
;*    init-fastfl-cache! ...                                           */
;*---------------------------------------------------------------------*/
(define (init-fastfl-cache!)
   (set! *$fast-flonum?* (find-global (get-genv) '$fast-flonum? 'foreign))
   (set! *$fast-real->double* (find-global (get-genv) '$fast-real->double 'foreign))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-fastfl-cache! ...                                          */
;*---------------------------------------------------------------------*/
(define (clear-fastfl-cache!)
   (set! *$fast-flonum?* #f)
   (set! *$fast-real->double* #f))

;*---------------------------------------------------------------------*/
;*    fastfl-fun! ...                                                  */
;*---------------------------------------------------------------------*/
(define (fastfl-fun! var)
   (enter-function (variable-id var))
   (let ((fun (variable-value var)))
      (sfun-body-set! fun (fastfl! (sfun-body fun)))
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    fastfl! ...                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (fastfl! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    fastfl! ::app ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (fastfl! node::app)

   (define (is-fast-flonum? fun)
      (eq? (var-variable fun) *$fast-flonum?*))
   
   (define (is-fast-real->double? fun)
      (eq? (var-variable fun) *$fast-real->double*))
   
   (with-access::app node (fun args type)
      (cond
	 ((and (is-fast-flonum? fun) (eq? (node-type (car args)) *real*))
	  (instantiate::literal
	     (type *bool*)
	     (value #t)))
	 ((and (is-fast-real->double? fun) (eq? (node-type (car args)) *real*))
	  (car args))
	 (else
	  node))))

