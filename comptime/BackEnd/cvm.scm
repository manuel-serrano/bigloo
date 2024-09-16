;*=====================================================================*/
;*    /priv/serrano2/bigloo/wasm/comptime/BackEnd/cvm.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Mon Sep 16 08:33:32 2024 (serrano)                */
;*    Copyright   :  2005-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    C-like backend class definitions                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_cvm
   (import type_type
	   backend_backend)
   (include "BackEnd/cvm.sch")
   (export (abstract-class cvm::backend)
	   (class sawc::cvm)
	   (class cgen::cvm)
	   (class wasm::cvm)))
