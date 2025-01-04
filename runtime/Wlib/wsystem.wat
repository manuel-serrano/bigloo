;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wsystem.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Sat Jan  4 09:06:26 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM system ops                                                  */
;*=====================================================================*/

(module $__bigloo_system

   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------

   (func $bgl_getcwd (export "bgl_getcwd")
      (result (ref $bstring))
      (return_call $load_string
	 (i32.const 128)
	 (call $js_getcwd (i32.const 128))))

   (func $bgl_getenvp (export "bgl_getenvp")
      (param $var (ref $bstring)) 
      (result i32)
      
      (local $sz i32)
      (call $store_string (local.get $var) (i32.const 128))
      (local.set $sz
	 (call $js_getenv (i32.const 128) (array.len (local.get $var))))
      (if (i32.ge_s (local.get $sz) (i32.const 0))
	  (then (return (i32.const 1)))
	  (else (return (i32.const 0)))))

   (func $bgl_getenv (export "bgl_getenv")
      (param $var (ref $bstring)) 
      (result (ref $bstring))
      
      (local $sz i32)
      (call $store_string (local.get $var) (i32.const 128))
      (local.set $sz
	 (call $js_getenv (i32.const 128) (array.len (local.get $var))))
      (if (i32.ge_s (local.get $sz) (i32.const 0))
	  (then (return (call $load_string (i32.const 128) (local.get $sz))))
	  (else (return (array.new_fixed $bstring 0)))))

   (func $bgl_get_trace_stack (export "bgl_get_trace_stack")
      (param $depth i32)
      (result (ref eq))

      (return (global.get $BNIL)))
   
   )
