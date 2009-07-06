;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/option.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 28 10:20:55 1998                          */
;*    Last change :  Mon May 15 07:57:20 2000 (serrano)                */
;*    Copyright   :  1998-2000 Manuel Serrano, see LICENSE file        */
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
       (for-each eval protos)
       '())
      (else
       (user-error "Parse error" "Illegal `option' clause" clause '()))))
