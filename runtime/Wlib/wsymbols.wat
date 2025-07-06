;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wsymbols.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 09:56:44 2024                          */
;*    Last change :  Sun Jul  6 10:27:33 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
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
      (struct (field $dummy i8) (field $str (ref $bstring)) (field $cval (mut (ref eq))) ))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref $bunspecified)))

   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $symbol-default-value
      (export "BGL_SYMBOL_DEFAULT_VALUE") (ref $symbol)
      (struct.new $symbol (global.get $bstring-default-value) (global.get $BNIL)))
   (global $keyword-default-value
      (export "BGL_KEYWORD_DEFAULT_VALUE") (ref $keyword)
      (struct.new $keyword (i32.const 0) (global.get $bstring-default-value) (global.get $BNIL)))
   
   ;; --------------------------------------------------------
   ;; Constructors 
   ;; --------------------------------------------------------
   
   (func $make-symbol (export "bgl_make_symbol")
      (param $str (ref $bstring))
      (param $cval (ref eq))
      (result (ref $symbol))
      (return (struct.new $symbol (local.get $str) (local.get $cval))))
   
   (func $make-keyword (export "bgl_make_keyword")
      (param $str (ref $bstring))
      (param $cval (ref eq))
      (result (ref $keyword))
      (return (struct.new $keyword (i32.const 0) (local.get $str) (local.get $cval))))
   
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
   
   
