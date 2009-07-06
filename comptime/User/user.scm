;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/User/user.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 29 14:52:11 1994                          */
;*    Last change :  Mon May 15 08:08:22 2000 (serrano)                */
;*    Copyright   :  1994-2000 Manuel Serrano, see LICENSE file        */
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
   (export  (user-walk units)))

;*---------------------------------------------------------------------*/
;*    user-walk ...                                                    */
;*---------------------------------------------------------------------*/
(define (user-walk units)
   (if (procedure? *user-pass*)
       (let ((unit (get-toplevel-unit)))
	  (pass-prelude *user-pass-name*)
	  (if (procedure? (unit-sexp* unit))
	      ;; a freezed unit (such as the eval
	      ;; unit) cannot be walked.
	      'nothing
	      (unit-sexp*-set! unit (*user-pass* (unit-sexp* unit))))
	  (pass-postlude 'dummy))
       'done))


