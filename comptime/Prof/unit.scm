;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Prof/unit.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  8 17:21:15 1998                          */
;*    Last change :  Thu Sep 18 12:00:46 2008 (serrano)                */
;*    Copyright   :  1998-2008 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The debugging information code production.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module prof_walk
   (include "Engine/pass.sch"
	    "Ast/unit.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    type_type
	    module_include
	    ast_var
	    ast_node
	    backend_backend)
   (export  (make-prof-unit)))

;*---------------------------------------------------------------------*/
;*    make-prof-unit ...                                               */
;*---------------------------------------------------------------------*/
(define (make-prof-unit)
   (pass-prelude "Prof")
   (unit 'prof
	 (+ 100 (get-toplevel-unit-weight))
	 (if (backend-pragma-support (the-backend))
	     '(begin (pragma "write_bprof_table()"))
	     `(begin #unspecified))
	 #t
	 #f))


