;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/comptime/Foreign/access.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Thu Feb 12 16:33:19 2026 (serrano)                */
;*    Copyright   :  1996-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the ctype accessors                                      */
;*    -------------------------------------------------------------    */
;*    This function simply defines and exports the generic             */
;*    accessors builder function. It is imported by all the specific   */
;*    accessors builders.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_access
   
   (import type_type
	   foreign_calias
	   foreign_cenum
	   foreign_copaque
	   foreign_cfunction
	   foreign_cpointer
	   foreign_cstruct)
   
   (export (generic make-ctype-accesses! ::type ::type loc mod::symbol)))

;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (make-ctype-accesses! what::type who::type loc mod::symbol)
   '())
