;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wsystem.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Fri Jul 25 14:36:08 2025 (serrano)                */
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
   (import "__js_system" "getenv_len" (func $js_getenv_len (result i32)))
   (import "__js_system" "getenv_var" (func $js_getenv_var (param i32 i32) (result i32)))
   (import "__js_system" "setenv" (func $js_setenv (param i32 i32 i32 i32) (result i32)))
   (import "__js_system" "date" (func $js_date (param i32) (result i32)))
   (import "__js_system" "umask" (func $js_umask (param i32) (result i32)))
   (import "__js_system" "chdir" (func $js_chdir (param i32 i32) (result i32)))
   (import "__js_system" "system" (func $js_system (param i32 i32) (result i32)))
   (import "__js_system" "sleep" (func $js_sleep (param i32)))

   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref eq)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref eq)))
   (import "__bigloo" "BEOF" (global $BEOF (ref eq)))
   (import "__bigloo" "BNIL" (global $BNIL (ref eq)))
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_store_substring" (func $store_substring (param (ref $bstring)) (param i64) (param i64) (param i32)))
   (import "__bigloo" "MAKE_YOUNG_PAIR" (func $MAKE_YOUNG_PAIR (param (ref eq) (ref eq)) (result (ref $pair))))

   
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

   (func $bgl_getenv_all (export "bgl_getenv_all")
      (result (ref eq))
      (local $sz i32)
      (local $res (ref eq))
      (local $var (ref $bstring))
      (local $val (ref eq))

      (local.set $sz (call $js_getenv_len))
      (local.set $res (global.get $BNIL))

      (loop $while
	 (local.set $sz (i32.sub (local.get $sz) (i32.const 1)))
	 (if (i32.ge_s (local.get $sz) (i32.const 0))
	     (then
		(local.set $var
		   (call $load_string (i32.const 128)
		      (call $js_getenv_var (local.get $sz) (i32.const 128))))
		(local.set $val (call $bgl_getenv (local.get $var)))
		(local.set $res (call $MAKE_YOUNG_PAIR
				   (call $MAKE_YOUNG_PAIR
				      (local.get $var)
				      (local.get $val))
				   (local.get $res))))))

      (return (local.get $res)))

   (func $bgl_setenv (export "bgl_setenv")
      (param $id (ref $bstring))
      (param $val (ref $bstring))
      (result i32)
      (local $len i32)
      (local $len2 i32)

      (local.set $len (array.len (local.get $id)))
      (local.set $len2 (array.len (local.get $val)))
      (call $store_string (local.get $id) (i32.const 128))
      (call $store_string (local.get $val) (i32.add (local.get $len) (i32.const 128)))
      (return_call $js_setenv (i32.const 128) (local.get $len)
	 (i32.add (local.get $len) (i32.const 128)) (local.get $len2)))

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

   (func $chdir (export "chdir")
      (param $dir (ref $bstring)) 
      (result i32)
      
      (local $sz i32)
      (call $store_string (local.get $dir) (i32.const 128))
      (return_call $js_chdir (i32.const 128) (array.len (local.get $dir))))

   (func $system (export "system")
      (param $cmd (ref $bstring))
      (result i32)

      (local $sz i32)
      (call $store_string (local.get $cmd) (i32.const 128))
      (return_call $js_system (i32.const 128) (array.len (local.get $cmd))))

   (func $bgl_sleep (export "bgl_sleep")
      (param $tmt i64)
      (call $js_sleep (i32.wrap_i64 (local.get $tmt))))

   )
