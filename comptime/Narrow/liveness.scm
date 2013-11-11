;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/liveness.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov  9 08:10:38 2013                          */
;*    Last change :  Sun Nov 10 09:51:26 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Straightforward liveness analysis (no loop).                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module narow_livenes
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    ast_walk
	    module_module
	    engine_param)
;* 	    narrow_types)                                              */
   (export  (liveness-variable ::variable)))

;*---------------------------------------------------------------------*/
;*    liveness-variable ...                                            */
;*    -------------------------------------------------------------    */
;*    Compute a liveness analysis for local variables.                 */
;*---------------------------------------------------------------------*/
(define (liveness-variable v::variable)
   (with-access::variable v (value)
      (when (isa? value sfun)
	 (with-access::sfun value (body)
	    (liveness! body)))))

;*---------------------------------------------------------------------*/
;*    liveness! ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (liveness! n::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    liveness! ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (liveness! n::let-var)
   (tprint "liveness let-var: " (shape n))
   n)

;* {*---------------------------------------------------------------------*} */
;* {*    liveness-node! ::local ...                                       *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (liveness-node! n::local in out)                     */
;*    (with-access::local node (variable)                              */
;*       (let ((nin (cons variable in)))                               */
;* 	 (widen!::local/narrow n                                       */
;* 	    (in nin)                                                   */
;* 	    (out in)))))                                               */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    liveness-node! ::sequence ...                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (liveness-node! n::sequence in out)                  */
;*    (with-access::sequence node (nodes)                              */
;*       (let loop ((sedon (reverse nodes))                            */
;* 		 (nin in)                                              */
;* 		 (def '())                                             */
;* 		 (use '()))                                            */
;* 	 (if (null? nin)                                               */
;* 	     (widen!::sequence/narrow n                                */
;* 		(def def)                                              */
;* 		(use use)                                              */
;* 		(in nin)                                               */
;* 		(out in))                                              */
;* 	                                                               */
;*       (let ((nin (cons variable in)))                               */
;* 	 (widen!::sequence/narrow node                                 */
;* 	    (in nin)                                                   */
;* 	    (out in)))))                                               */
;*                                                                     */
;*                                                                     */
	    
