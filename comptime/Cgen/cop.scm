;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime.case1.3/Cgen/cop.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  2 13:53:24 1996                          */
;*    Last change :  Thu Jul  5 18:23:13 2001 (serrano)                */
;*    Copyright   :  1996-2001 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The intermediate structure to emit c code.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_cop
   
   (import type_type
	   ast_var
	   ast_node)
   
   (export (class cop
	      ;; the source line number associated to this instruction
	      (loc::obj (default #f)))

	   (class clabel::cop
	      (name::bstring read-only)
	      (used?::bool (default #f))
	      (body (default #unspecified)))

	   (class cgoto::cop
	      (label::clabel read-only))

	   (class block::cop
	      (body::cop read-only))

	   (class creturn::cop
	      (value::cop read-only))

	   (class cvoid::cop
	      (value::cop read-only))

	   (class catom::cop
	      (value read-only))
	   
	   (class varc::cop
	      (variable::variable read-only))
	   
	   (class cpragma::cop
	      (format::bstring read-only)
	      (args read-only))

	   (class ccast::cop
	      (type::type read-only)
	      (arg::cop read-only))
	   
	   (class csequence::cop
	      (c-exp?::bool read-only (default #f))
	      (cops read-only))

	   (class nop::cop)

	   (class stop::cop
	      (value::cop read-only))

	   (class csetq::cop
	      (var::varc read-only)
	      (value::cop read-only))

	   (class cif::cop
	      (test::cop read-only)
	      (true::cop read-only)
	      (false::cop read-only))
	   
	   (class local-var::cop
	      (vars read-only))
	   
	   (class cfuncall::cop
	      (fun::cop read-only)
	      (args read-only)
	      (strength::symbol read-only))
	   
	   (class capply::cop
	      (fun::cop read-only)
	      (arg::cop read-only))

	   (class capp::cop
	      (fun::cop read-only)
	      (args read-only))

	   (class cfail::cop
	      (proc::cop read-only)
	      (msg::cop read-only)
	      (obj::cop read-only))

	   (class cswitch::cop
	      (test::cop read-only)
	      (clauses read-only))

	   (class cmake-box::cop
	      (value::cop read-only))

	   (class cbox-ref::cop
	      (var::cop read-only))

	   (class cbox-set!::cop
	      (var::cop read-only)
	      (value::cop read-only))
	   
	   (class cset-ex-it::cop
	      (exit::cop read-only)
	      (jump-value::cop read-only)
	      (body::cop read-only))

	   (class cjump-ex-it::cop
	      (exit::cop read-only)
	      (value::cop read-only))

	   (wide-class sfun/C::sfun
	      (label::clabel read-only)
	      integrated::bool)

	   (class bdb-block::cop
	      (body::cop read-only))))
	      

	   


