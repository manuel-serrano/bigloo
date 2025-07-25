;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/javascript.wat     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 25 11:07:11 2024                          */
;*    Last change :  Fri Jul 25 10:27:27 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript imports                                               */
;*    -------------------------------------------------------------    */
;*    This file is concatanated to runtime.wat to build bigloo_x.wat   */
;*=====================================================================*/

(module $__bigloo_javascript
   
   (memory 1)
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------

   (import "__js" "not_implemented" (func $not_implemented (param i32)))
   (import "__js" "unsupported" (func $js_unsupported (param i32)))
   (import "__js" "trace" (func $js_trace (param i32)))
   (import "__js" "internalError" (func $js_internal_error (param i32) (param i32)))
   
   ;; load_string_in_buffer
   (func $load_string_in_buffer (export "bgl_load_string_in_buffer")
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
   (func $load_string (export "bgl_load_string")
      (param $addr i32)
      (param $length i32)
      (result (ref $bstring))
      (local $buffer (ref $bstring))

      (local.set $buffer (array.new_default $bstring (local.get $length)))
      
      (call $load_string_in_buffer (local.get $addr) (local.get $length)
	 (local.get $buffer) (i32.const 0))

      (local.get $buffer))

   ;; store_substring
   (func $store_substring (export "bgl_store_substring")
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
		   (array.get_u $bstring (local.get $text) (local.get $i)))
		(call $js_trace (local.get $addr))
		(local.set $i (i32.add (local.get $i) (i32.const 1)))
		(local.set $addr (i32.add (local.get $addr) (i32.const 1)))
		(br $loop)))))

   ;; store_string
   (func $store_string (export "bgl_store_string")
      (param $text (ref $bstring))
      (param $addr i32)
      
      (call $store_substring
	 (local.get $text)
	 (i64.const 0)
	 (i64.extend_i32_u (array.len (local.get $text)))
	 (local.get $addr)))

   ;; store_string_len2 (as store_string but also stores the string-length
   ;; endoed as a two bytes integer).
   (func $store_string_len2 (export "bgl_store_string_len2")
      (param $text (ref $bstring))
      (param $addr i32)
      (result i32)
      (local $len i32)
      
      (local.set $len (array.len (local.get $text)))
      (i32.store8 (local.get $addr) (i32.div_s (local.get $len) (i32.const 256)))
      (i32.store8 (i32.add (local.get $addr) (i32.const 1)) (i32.rem_s (local.get $len) (i32.const 256)))
      (call $store_string (local.get $text)
	 (i32.add (local.get $addr) (i32.const 2)))
      (return (i32.add (local.get $addr)
		 (i32.add (i32.const 2) (local.get $len)))))

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
		   (i32.and (array.get_u $ucs2string (local.get $text) (local.get $i)) (i32.const 255)))
		(i32.store8 (i32.add (local.get $addr) (i32.const 1))
		   (i32.shr_u (array.get_u $ucs2string (local.get $text) (local.get $i)) (i32.const 8)))
		(local.set $addr (i32.add (local.get $addr) (i32.const 2)))
		(local.set $i (i32.add (local.get $i) (i32.const 1)))
		(br $loop)))))

   )


