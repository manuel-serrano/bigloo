;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/jvm_class.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Wed Apr  6 13:15:37 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
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
	      (code (default '()))
	      (light-funcalls (default '()))
	      (inline (default #t)))))


