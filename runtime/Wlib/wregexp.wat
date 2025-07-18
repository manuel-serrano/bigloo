;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wregexp.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 19 09:18:29 2024                          */
;*    Last change :  Fri Jul 18 16:01:47 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM regexp                                                      */
;*=====================================================================*/

(module $__bigloo_regexp
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
 
   (type $regexp
      (struct
	 (field $pat (ref $bstring))
	 (field $preg (mut (ref eq)))
	 (field $capturecount (mut i32))))
 
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
   
   (global $regexp-default-value
      (export "BGL_REGEXP_DEFAULT_VALUE") (ref $regexp)
      (struct.new $regexp
	 (global.get $bstring-default-value)
	 (global.get $BUNSPEC)
	 (i32.const 0)))

   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   (func $bgl_make_regexp (export "bgl_make_regexp")
      (param $p (ref $bstring))
      (result (ref $regexp))
      (struct.new $regexp (local.get $p) (global.get $BUNSPEC) (i32.const 0)))

   (func $BGL_REGEXP_PAT (export "BGL_REGEXP_PAT")
      (param $rx (ref $regexp))
      (result (ref $bstring))
      (struct.get $regexp $pat (local.get $rx)))

   (func $BGL_REGEXP_PREG (export "BGL_REGEXP_PREG")
      (param $rx (ref $regexp))
      (result (ref eq))
      (struct.get $regexp $preg (local.get $rx)))

   (func $BGL_REGEXP_PREG_SET (export "BGL_REGEXP_PREG_SET")
      (param $rx (ref $regexp))
      (param $preg (ref eq))
      (result (ref eq))
      (struct.set $regexp $preg (local.get $rx) (local.get $preg))
      (local.get $rx))

   (func $BGL_REGEXP_CAPTURE_COUNT (export "BGL_REGEXP_CAPTURE_COUNT")
      (param $rx (ref $regexp))
      (result i64)
      (i64.extend_i32_u (struct.get $regexp $capturecount (local.get $rx)))))











   
  
