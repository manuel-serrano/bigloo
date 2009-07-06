;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Foreign/access.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Mon May 15 07:47:43 2000 (serrano)                */
;*    Copyright   :  1996-2000 Manuel Serrano, see LICENSE file        */
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
   
   (export (generic make-ctype-accesses! ::type ::type loc)))

;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (make-ctype-accesses! what::type who::type loc)
   '())
