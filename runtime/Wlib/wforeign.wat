;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wforeign.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 10:14:39 2024                          */
;*    Last change :  Thu Jul 10 07:43:36 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM foreign objects                                             */
;*=====================================================================*/

(module $__bigloo_foreign
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (rec
      (type $cobj
	 (struct))
      (type $foreign
	 (struct
	    (field $id (ref $symbol))
	    (field $ptr i32))))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref $bunspecified)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref $bbool)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref $bbool)))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))

   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $foreign-default-value
      (export "BGL_FOREIGN_DEFAULT_VALUE") (ref $foreign)
      (struct.new $foreign
	 (global.get $symbol-default-value)
	 (i32.const 0)))
   
   )
