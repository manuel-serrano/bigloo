;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wsystem.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Fri Jul 18 07:51:56 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM system ops                                                  */
;*=====================================================================*/

(module $__bigloo_system

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------

   (import "__js_system" "command_line_size" (func $js_command_line_size (result i32)))
   (import "__js_system" "command_line_entry" (func $js_command_line_entry (param i32) (param i32) (result i32)))
   (import "__js_system" "executable_name" (func $js_executable_name (param i32) (result i32)))
   (import "__js_system" "getcwd" (func $js_getcwd (param i32) (result i32)))
   (import "__js_system" "getenv" (func $js_getenv (param i32) (param i32) (result i32)))
   (import "__js_system" "date" (func $js_date (param i32) (result i32)))
   (import "__js_system" "umask" (func $js_umask (param i32) (result i32)))

   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref $bunspecified)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref $bbool)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref $bbool)))
   (import "__bigloo" "BEOF" (global $BEOF (ref $bcnst)))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_store_substring" (func $store_substring (param (ref $bstring)) (param i64) (param i64) (param i32)))

   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------

   (global $the_command_line (mut (ref eq)) (global.get $BUNSPEC))
   
   (func $bgl_command_line (export "command_line")
      (result (ref eq))
      (local $idx i32)
      (local $res (ref eq))
      
      (if (ref.eq (global.get $the_command_line) (global.get $BUNSPEC))
	  (then
	     (local.set $idx (call $js_command_line_size))
	     (local.set $res (global.get $BNIL))
	     (loop $while
		(if (i32.eqz (local.get $idx))
		    (then
		       (global.set $the_command_line (local.get $res)))
		    (else
		     (local.set $idx (i32.sub (local.get $idx) (i32.const 1)))
		     (local.set $res
			(struct.new $pair
			   (call $load_string
			      (i32.const 128)
			      (call $js_command_line_entry (local.get $idx)
				 (i32.const 128)))
			   (local.get $res)))
		     (br $while))))))
      
      (return (global.get $the_command_line)))
      
   (global $the_executable_name (mut (ref null $bstring)) (ref.null none))
   
   (func $bgl_executable_name (export "executable_name")
      (result (ref $bstring))
      (if (ref.is_null (global.get $the_executable_name))
	  (then
	     (global.set $the_executable_name
		(call $load_string
		   (i32.const 128)
		   (call $js_executable_name (i32.const 128))))))
      (return (ref.cast (ref $bstring) (global.get $the_executable_name))))
      
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
      (return
	 (if (result i32) (i32.ge_s (local.get $sz) (i32.const 0))
	     (then (i32.const 1))
	     (else (i32.const 0)))))

   (func $bgl_getenv (export "bgl_getenv")
      (param $var (ref $bstring)) 
      (result (ref $bstring))
      
      (local $sz i32)
      (call $store_string (local.get $var) (i32.const 128))
      (local.set $sz
	 (call $js_getenv (i32.const 128) (array.len (local.get $var))))
      (if (i32.ge_s (local.get $sz) (i32.const 0))
	  (then (return_call $load_string (i32.const 128) (local.get $sz)))
	  (else (return (array.new_fixed $bstring 0))))
      (unreachable))

   (func $bgl_get_trace_stack (export "bgl_get_trace_stack")
      (param $depth i32)
      (result (ref eq))

      (return (global.get $BNIL)))

   (func $c_date (export "c_date")
      (result (ref $bstring))
      (return_call $load_string
	 (i32.const 128)
	 (call $js_date (i32.const 128))))

   (func $umask (export "umask")
      (param $mask i64)
      (result i64)

      (return
	 (i64.extend_i32_u
	    (call $js_umask (i32.wrap_i64 (local.get $mask))))))

   )
