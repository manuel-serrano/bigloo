;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/comptime/Foreign/calias.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  6 12:25:42 1996                          */
;*    Last change :  Thu Feb 12 16:31:10 2026 (serrano)                */
;*    Copyright   :  1996-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C alias accessors creations.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_calias
   (include "Tools/trace.sch")
   (import  type_tools
	    type_type
	    tools_shape
	    foreign_ctype
	    foreign_access))
   
;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ::calias ...                                */
;*---------------------------------------------------------------------*/
(define-method (make-ctype-accesses! what::calias who::type loc mod::symbol)
   (trace (expand 3)
	  "make-ctype-accesses(calias): " (shape what) " " (shape who)
	  #\Newline)
   (make-ctype-accesses! (type-alias what) who loc mod))

 
