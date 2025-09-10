;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Module/libinfo.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct  3 13:29:11 2010                          */
;*    Last change :  Wed Sep 10 09:38:30 2025 (serrano)                */
;*    Copyright   :  2010-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Lib info structure (see runtime/Eval/library.scm)                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    libinfo ...                                                      */
;*    -------------------------------------------------------------    */
;*    This structure must be identically defined in                    */
;*    runtime/library.scm                                              */
;*---------------------------------------------------------------------*/
(define-struct libinfo
   id basename version
   init_s init_e
   module_s module_e
   class_s class_e
   init eval srfi
   wasm-js wasm_s wasm_e)
