;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wreal.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  1 09:40:49 2024                          */
;*    Last change :  Thu Jul 24 14:28:54 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM reals                                                       */
;*=====================================================================*/

(module $__bigloo_real
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $real (struct (field $v f64)))

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_store_substring" (func $store_substring (param (ref $bstring)) (param i64) (param i64) (param i32)))
   (import "__js_math" "strtod" (func $js_strtod (param i32) (param i32) (result f64)))


   ;; -----------------------------------------------------------------
   ;; Macros 
   ;; -----------------------------------------------------------------
   
   (func $REALP (export "REALP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $real) (local.get $o)))
  
   (func $BGL_FAST_REALP (export "BGL_FAST_REALP")
      (param $o (ref eq))
      (result i32)
      (return_call $REALP (local.get $o)))
  
   (func $BGL_REALSP (export "BGL_REALSP")
      (param $x (ref eq))
      (param $y (ref eq))
      (result i32)
      (return
	 (if (result i32) (ref.test (ref $real) (local.get $x))
	     (then (ref.test (ref $real) (local.get $y)))
	     (else (i32.const 0)))))

   (func $BGL_FAST_REALSP (export "BGL_FAST_REALSP")
      (param $x (ref eq))
      (param $y (ref eq))
      (result i32)
      (return_call $BGL_REALSP (local.get $x) (local.get $y)))
  
   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------
   (func $STRTOD (export "STRTOD")
      (param $str (ref $bstring))
      (result f64)
      
      (call $store_substring (local.get $str)
	 (i64.const 0)
	 (i64.extend_i32_u (array.len (local.get $str)))
	 (i32.const 128))
      
      (return_call $js_strtod (i32.const 128)
	 (array.len (local.get $str))))

   )
