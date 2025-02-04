;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wsystem.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Tue Feb  4 16:41:52 2025 (serrano)                */
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
