;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wsymbols.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 09:56:44 2024                          */
;*    Last change :  Wed Oct  2 09:59:07 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM Symbols and keywords                                        */
;*=====================================================================*/

(module $__bigloo_symbol
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $symbol
      (struct (field $str (ref $bstring)) (field $cval (mut (ref eq)))))
   (type $keyword
      (struct (field $str (ref $bstring)) (field $cval (mut (ref eq)))))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $symbol-default-value
      (export "BGL_SYMBOL_DEFAULT_VALUE") (ref $symbol)
      (struct.new $symbol (global.get $bstring-default-value) (global.get $BNIL)))
   (global $keyword-default-value
      (export "BGL_KEYWORD_DEFAULT_VALUE") (ref $keyword)
      (struct.new $keyword (global.get $bstring-default-value) (global.get $BNIL)))
   
   ;; --------------------------------------------------------
   ;; Library functions 
   ;; --------------------------------------------------------
   (func $set-symbol-plist (export "SET_SYMBOL_PLIST")
      (param $sym (ref eq))
      (param $val (ref eq))
      (result (ref eq))
      (struct.set $symbol $cval (ref.cast (ref $symbol) (local.get $sym))
	 (local.get $val))
      (local.get $val))
   
   (func $set-keyword-plist (export "SET_KEYWORD_PLIST")
      (param $sym (ref eq))
      (param $val (ref eq))
      (result (ref eq))
      (struct.set $keyword $cval (ref.cast (ref $keyword) (local.get $sym))
	 (local.get $val))
      (local.get $val))
   
   )
   
   
