;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/comptime/Coerce/postcnst.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 28 17:44:11 2024                          */
;*    Last change :  Sat Dec 28 17:55:49 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Patch simple constants inserted after the CNST pass.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_postcnst
   (include "Engine/pass.sch"
            "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_remove
	    ast_walk
	    coerce_convert)
   (export  (post-cnst node::node)))

;*---------------------------------------------------------------------*/
;*    post-cnst ...                                                    */
;*    -------------------------------------------------------------    */
;*    Replace badly typed atom introduced after the CNST pass          */
;*---------------------------------------------------------------------*/
(define (post-cnst node::node)
   (post-cnst! node)
   node)

;*---------------------------------------------------------------------*/
;*    post-cnst! ...                                                   */
;*    -------------------------------------------------------------    */
;*    Replace badly typed atom introduced after the CNST pass          */
;*---------------------------------------------------------------------*/
(define-walk-method (post-cnst! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    post-cnst! ::literal ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (post-cnst! node::literal)
   (with-access::literal node (value type)
      (cond
	 ((and (string? value) (eq? type *bstring*))
	  (convert! node *string* *bstring* #f))
	 ((and (integer? value) (eq? type *bint*))
	  (convert! node *long* *bint* #f))
	 (else
	  node))))
   
   
