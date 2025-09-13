;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/User/user.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 29 14:52:11 1994                          */
;*    Last change :  Sat Sep 13 06:56:22 2025 (serrano)                */
;*    Copyright   :  1994-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The entry user point                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module user_user
   (include "Engine/pass.sch"
	    "Ast/unit.sch")
   (import  engine_param
	    module_include
	    tools_speek
	    tools_error)
   (export  (user-walk unit)))

;*---------------------------------------------------------------------*/
;*    user-walk ...                                                    */
;*---------------------------------------------------------------------*/
(define (user-walk unit)
   (when (procedure? *user-pass*)
      (pass-prelude *user-pass-name*)
      (unless (procedure? (unit-sexp* unit))
	 ;; a freezed unit (such as the eval unit) cannot be walked.
	 (unit-sexp*-set! unit (*user-pass* (unit-sexp* unit))))
      (pass-postlude 'dummy)))


