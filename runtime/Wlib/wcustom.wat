;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wcustom.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 10:14:39 2024                          */
;*    Last change :  Tue Jul 22 12:29:55 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM custom objects                                              */
;*=====================================================================*/

(module $__bigloo_custom
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $custom (struct (field $ident (mut (ref $bstring)))))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref eq)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref eq)))
   (import "__bigloo" "BNIL" (global $BNIL (ref eq)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))

   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $custom-default-value
      (export "BGL_CUSTOM_DEFAULT_VALUE") (ref $custom)
      (struct.new $custom
	 (global.get $bstring-default-value)))
   
   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------
   
   (func $CUSTOM_IDENTIFIER_SET (export "CUSTOM_IDENTIFIER_SET")
      (param $custom (ref $custom))
      (param $ident (ref $bstring))
      (result (ref eq))
      (struct.set $custom $ident (local.get $custom) (local.get $ident))
      (global.get $BUNSPEC))
   
   
   )
