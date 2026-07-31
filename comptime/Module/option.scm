;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Module/option.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 28 10:20:55 1998                          */
;*    Last change :  Thu Jul 30 09:52:15 2026 (serrano)                */
;*    Copyright   :  1998-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compiler option clause compilation                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_option
   (include "Ast/unit.sch")
   (import  module_module
	    tools_error
	    engine_param)
   (export  (make-option-compiler)))

;*---------------------------------------------------------------------*/
;*    make-option-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-option-compiler)
   (instantiate::ccomp
      (id 'option)
      (producer option-producer)))

;*---------------------------------------------------------------------*/
;*    option-producer ...                                              */
;*---------------------------------------------------------------------*/
(define (option-producer clause)
   (match-case clause
      ((?- . ?protos)
       (for-each (lambda (e)
		    (with-handler
		       (lambda (exn)
			  (fprintf (current-error-port)
			     "*** WARNING: evaluting module option \"s\" raises an error")
			  (error-notify exn)
			  #unspecified)
		       (eval e)))
	  protos)
       '())
      (else
       (user-error "Parse error" "Illegal `option' clause" clause '()))))
