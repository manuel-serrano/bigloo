;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/cvm.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Mon Feb 28 14:03:05 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The C backend class definition                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_cvm
   (import type_type
	   backend_backend)
   (export (abstract-class cvm::backend)
	   (class sawc::cvm)
	   (class cgen::cvm)))
