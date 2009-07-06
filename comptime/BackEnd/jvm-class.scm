;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/jvm-class.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Mon Feb 28 20:36:59 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The C backend class definition                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_jvm_class
   (import type_type
	   backend_backend
	   backend_bvm)
   (export (class jvm::bvm
	      (qname (default 'unamed))
	      (classes (default '()))
	      (current-class (default #f))
	      (declarations (default '()))
	      (fields (default '()))
	      (methods (default '()))
	      (current-method (default #f))
	      (code (default '())))))


