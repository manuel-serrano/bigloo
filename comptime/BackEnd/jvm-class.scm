;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/jvm-class.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Mon Mar 29 17:54:06 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
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
	      (inline (default #t)))))


