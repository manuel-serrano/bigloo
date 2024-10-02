;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wcustom.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 10:14:39 2024                          */
;*    Last change :  Wed Oct  2 11:02:46 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM custom objects                                              */
;*=====================================================================*/

(module $__bigloo_custom
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $custom (struct (field $ident (mut (ref $bstring)))))
   
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
