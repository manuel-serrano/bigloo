;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/walk.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  6 18:02:26 2013                          */
;*    Last change :  Wed Nov  6 18:03:32 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Narrow the variable scopes. This optimization looks like a       */
;*    register allocation. It rewrites code as follows:                */
;*                                                                     */
;*       (let ((x #unspecified))                                       */
;*           expr1                                                     */
;*           ...                                                       */
;*           exprN                                                     */
;*           (set! x 3) ;; x not used before that assignment           */
;*           ...                                                       */
;*           exprM)                                                    */
;*    =>                                                               */
;*       (begin                                                        */
;*           expr1                                                     */
;*           ...                                                       */
;*           exprN                                                     */
;*           (let ((x 3))                                              */
;*             ...                                                     */
;*             exprM))                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module narrow_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    module_module
	    engine_param)
   (export  (narrow-walk! ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    narrow-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (narrow-walk! globals)
   (pass-prelude "Narrow")
   (for-each (lambda (g)
		#f)
      globals)
   (pass-postlude globals))

   
