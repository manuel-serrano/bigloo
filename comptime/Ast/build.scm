;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/build.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 14:00:21 1996                          */
;*    Last change :  Tue Apr 19 14:18:41 2016 (serrano)                */
;*    -------------------------------------------------------------    */
;*    From the code definition, we build the Ast                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_build
   (include "Engine/pass.sch"
	    "Ast/node.sch")
   (import  ast_unit
	    ast_sexp
	    ast_env
	    ast_find-gdefs
	    ast_remove
	    ast_local
	    tools_args
	    tools_progn
	    tools_location
	    tools_error
	    tools_shape)
   (export  (append-ast::pair-nil ::pair-nil ::pair-nil)
	    (build-ast ::obj)
	    (build-ast-sans-remove ::obj)))

;*---------------------------------------------------------------------*/
;*    append-ast ...                                                   */
;*---------------------------------------------------------------------*/
(define (append-ast a1 a2)
   (append a1 a2))

;*---------------------------------------------------------------------*/
;*    build-ast ...                                                    */
;*    -------------------------------------------------------------    */
;*    All global variables are now bound, we can now, build the ast.   */
;*---------------------------------------------------------------------*/
(define (build-ast units)
   (remove-var 'ast (build-ast-sans-remove units)))

;*---------------------------------------------------------------------*/
;*    build-ast-sans-remove ...                                        */
;*    -------------------------------------------------------------    */
;*    All global variables are now bound, we can now, build the ast.   */
;*---------------------------------------------------------------------*/
(define (build-ast-sans-remove units)
   (pass-prelude "Ast")
   ;; there are two separate `map' because we can't build
   ;; node of the ast _until_ all the units have been processed
   ;; (otherwise some global variables could be unbound).
   (let* ((nberr *nb-error-on-pass*)
	  (defs (apply append (map unit->defs units))))
      (if (=fx nberr *nb-error-on-pass*)
	  (begin
	     ;; we can now check if all declared global variables are defined.
	     (check-to-be-define)
	     ;; and build the regular ast
	     (let ((ast (map sfun-def->ast defs)))
		;; and we return the constructed ast
		(pass-postlude ast)))
	  (pass-postlude '()))))

;*---------------------------------------------------------------------*/
;*    sfun-def->ast ...                                                */
;*---------------------------------------------------------------------*/
(define (sfun-def->ast::global def::global)
   (enter-function (global-id def))
   (unwind-protect
      (let* ((sfun (global-value def))
	     (sfun-args (sfun-args sfun))
	     (sfun-body-exp (sfun-body sfun))
	     (def-loc (find-location (global-src def)))
	     (loc (find-location/loc sfun-body-exp def-loc))
	     (body (sexp->node sfun-body-exp sfun-args loc 'value)))
	 (sfun-body-set! sfun body))
      (leave-function))
   def)

