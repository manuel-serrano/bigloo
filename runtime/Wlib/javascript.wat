;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/javascript.wat     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 25 11:07:11 2024                          */
;*    Last change :  Wed Feb  5 11:15:23 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript imports                                               */
;*    -------------------------------------------------------------    */
;*    This file is concatanated to runtime.wat to build bigloo_x.wat   */
;*=====================================================================*/

(module $__bigloo_javascript
   
   (import "__js" "not_implemented" (func $not_implemented (param i32)))
   (import "__js" "trace" (func $js_trace (param i32)))
   (import "__js" "internalError" (func $js_internal_error (param i32) (param i32)))
   
   (import "__js" "argc" (global $js_argc i32))
   (import "__js" "get_arg" (func $js_get_arg (param i32 i32) (result i32)))
   (import "__js" "getcwd" (func $js_getcwd (param i32) (result i32)))
   (import "__js" "getenv" (func $js_getenv (param i32) (param i32) (result i32)))
   (import "__js_math" "fmod" (func $fmod (param f64 f64) (result f64)))
   (import "__js_math" "exp" (func $exp (param f64) (result f64)))
   (import "__js_math" "log" (func $log (param f64) (result f64)))
   (import "__js_math" "log2" (func $log2 (param f64) (result f64)))
   (import "__js_math" "log10" (func $log10 (param f64) (result f64)))
   (import "__js_math" "sin" (func $sin (param f64) (result f64)))
   (import "__js_math" "cos" (func $cos (param f64) (result f64)))
   (import "__js_math" "tan" (func $tan (param f64) (result f64)))
   (import "__js_math" "asin" (func $asin (param f64) (result f64)))
   (import "__js_math" "acos" (func $acos (param f64) (result f64)))
   (import "__js_math" "atan" (func $atan (param f64) (result f64)))
   (import "__js_math" "atan2" (func $atan2 (param f64 f64) (result f64)))
   (import "__js_math" "pow" (func $pow (param f64 f64) (result f64)))
   (import "__js_math" "randomf" (func $RANDOMFL (result f64)))
   (import "__js_math" "strtod" (func $js_strtod (param i32) (param i32) (result f64)))
   
   (import "__js" "exit" (func $js_exit (param i32)))
   (import "__js" "signal" (func $js_signal (param i32) (param (ref eq))))
   
   (import "$__object" "BGl_classzd2nilzd2initz12z12zz__objectz00" (func $BGl_z62classzd2nilzd2initz12z70zz__objectz00 (param (ref $class)) (result (ref eq))))

   ;; load_string_in_buffer
   (func $load_string_in_buffer
      (param $addr i32)
      (param $length i32)
      (param $buffer (ref $bstring))
      (param $offset i32)
      
      (local $str (ref $bstring))
      (local $i i32)
      (local.set $i (i32.const 0))
      
      (loop $loop
	 (if (i32.lt_u (local.get $i) (local.get $length))
	     (then 
		(array.set $bstring 
		   (local.get $buffer)
		   (i32.add (local.get $offset) (local.get $i))
		   (i32.load8_u (i32.add (local.get $addr) (local.get $i))))
		(local.set $i (i32.add (local.get $i) (i32.const 1)))
		(br $loop)))))

   ;; load_string
   (func $load_string
      (param $addr i32)
      (param $length i32)
      (result (ref $bstring))
      
      (local $buffer (ref $bstring))
      (local.set $buffer (array.new_default $bstring (local.get $length)))
      
      (call $load_string_in_buffer (local.get $addr) (local.get $length)
	 (local.get $buffer) (i32.const 0))
      
      (local.get $buffer))

   ;; store_substring
   (func $store_substring
      (param $text (ref $bstring))
      (param $start i64)
      (param $end i64)
      (param $addr i32)
      
      (local $i i32)
      (local.set $i (i32.wrap_i64 (local.get $start)))
      
      (loop $loop
	 (if (i32.lt_u (local.get $i) (i32.wrap_i64 (local.get $end)))
	     (then
		(i32.store8 (local.get $addr)
		   (array.get $bstring (local.get $text) (local.get $i)))
		(local.set $i (i32.add (local.get $i) (i32.const 1)))
		(local.set $addr (i32.add (local.get $addr) (i32.const 1)))
		(br $loop)))))

   ;; store_string
   (func $store_string
      (param $text (ref $bstring))
      (param $addr i32)
      
      (call $store_substring
	 (local.get $text)
	 (i64.const 0)
	 (i64.extend_i32_u (array.len (local.get $text)))
	 (local.get $addr)))

   ;; store_ucs2string
   (func $store_ucs2string
      (param $text (ref $ucs2string))
      (param $addr i32)

      (local $i i32)
      (local $len i32)
      (local.set $len (array.len (local.get $text)))
      
      (loop $loop
	 (if (i32.lt_u (local.get $i) (local.get $len))
	     (then
		(i32.store8 (local.get $addr) 
		   (i32.and (array.get $ucs2string (local.get $text) (local.get $i)) (i32.const 255)))
		(i32.store8 (i32.add (local.get $addr) (i32.const 1))
		   (i32.shr_u (array.get $ucs2string (local.get $text) (local.get $i)) (i32.const 8)))
		(local.set $addr (i32.add (local.get $addr) (i32.const 2)))
		(local.set $i (i32.add (local.get $i) (i32.const 1)))
		(br $loop)))))

   )


