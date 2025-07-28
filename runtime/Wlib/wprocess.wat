;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wprocess.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Mon Jul 28 07:44:22 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM processes                                                   */
;*=====================================================================*/

(module $__bigloo_process
   
   ;; -----------------------------------------------------------------
   ;; Data 
   ;; -----------------------------------------------------------------

   (data $PIPE "pipe")
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $process
      (sub
	 (struct
	    (field $proc (mut externref))
	    (field $input-port (mut (ref eq)))
	    (field $output-port (mut (ref eq)))
	    (field $error-port (mut (ref eq))))))

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__js" "trace" (func $js_trace (param i32)))
   
   (import "__js" "unsupported" (func $js_unsupported (param i32)))
   
   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref eq)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref eq)))
   (import "__bigloo" "BNIL" (global $BNIL (ref eq)))
   (import "__bigloo" "BEOF" (global $BEOF (ref eq)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))
   (import "__bigloo" "STRING_LENGTH" (func $STRING_LENGTH (param (ref $bstring)) (result i64)))
   (import "__bigloo" "bgl_load_string_in_buffer" (func $load_string_in_buffer (param i32) (param i32) (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_store_string_len2" (func $store_string_len2 (param (ref $bstring) i32) (result i32)))
   (import "__bigloo" "BINT" (func $BINT (param i64) (result (ref eq))))
   (import "__bigloo" "the_failure" (func $the_failure (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "BGL_INPUT_PORT_DEFAULT_VALUE" (global $input-port-default-value (ref $input-port)))
   (import "__bigloo" "BGL_OUTPUT_PORT_DEFAULT_VALUE" (global $output-port-default-value (ref $output-port)))
   (import "__bigloo" "$$bstring->keyword@__r4_symbols_6_4" (func $$$bstring->keyword@__r4_symbols_6_4 (param (ref $bstring)) (result (ref $keyword))))
   (import "__bigloo" "bgl_open_input_string" (func $bgl_open_input_string (param (ref $bstring) i64) (result (ref $string-input-port))))
   (import "__bigloo" "bgl_open_output_socket" (func $bgl_open_output_socket (param externref (ref $bstring)) (result (ref $output-port))))
   (import "__bigloo" "bgl_open_input_socket" (func $bgl_open_input_socket (param externref (ref $bstring)) (result (ref $input-port))))

   (import "__js_process" "nullprocess" (global $nullprocess externref))
   (import "__js_process" "run" (func $run (param i32 i32 i32 i32 i32 i32 i32 i32 i32 i32) (result externref)))
   (import "__js_process" "xstatus" (func $xstatus (param externref) (result i32)))
   (import "__js_process" "alive" (func $alive (param externref) (result i32)))
   (import "__js_process" "getinport" (func $getinport (param externref i32) (result i32)))
   (import "__js_process" "getportsock" (func $getportsock (param externref i32) (result externref)))
   (import "__js_process" "getoutport" (func $getoutport (param externref i32 i32) (result i32)))
   (import "__js_process" "pid" (func $pid (param externref) (result i32)))
   (import "__js_process" "kill" (func $kill (param externref) (result i32)))
   
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $process-default-value
      (export "BGL_PROCESS_DEFAULT_VALUE") (ref $process)
      (struct.new $process
	 ;; proc
	 (global.get $nullprocess)
	 ;; input-port
	 (global.get $BUNSPEC)
	 ;; output-port
	 (global.get $BUNSPEC)
	 ;; error-port
	 (global.get $BUNSPEC)))

   (global $pipe (mut (ref null $keyword)) (ref.null $keyword))
      
   ;; --------------------------------------------------------
   ;; Macros
   ;; --------------------------------------------------------

   (func $PROCESSP (export "PROCESSP")
      (param $o (ref eq))
      (result i32)
      (return (ref.test (ref $process) (local.get $o))))

   (func $bgl_process_nil (export "bgl_process_nil")
      (result (ref $process))
      (return (global.get $process-default-value)))
   
   (func $PROCESS_PID (export "PROCESS_PID")
      (param $process (ref $process))
      (result i32)
      (return_call $pid (struct.get $process $proc (local.get $process))))

   (func $PROCESS_INPUT_PORT (export "PROCESS_INPUT_PORT")
      (param $process (ref $process))
      (result (ref eq))
      (return (struct.get $process $input-port (local.get $process))))

   (func $PROCESS_OUTPUT_PORT (export "PROCESS_OUTPUT_PORT")
      (param $process (ref $process))
      (result (ref eq))
      (return (struct.get $process $output-port (local.get $process))))

   (func $PROCESS_ERROR_PORT (export "PROCESS_ERROR_PORT")
      (param $process (ref $process))
      (result (ref eq))
      (return (struct.get $process $error-port (local.get $process))))

   ;; --------------------------------------------------------
   ;; Library functions
   ;; --------------------------------------------------------
   
   (func $pipe-keyword 
      (result (ref $keyword))
      (if (ref.is_null (global.get $pipe))
	  (then
	     (global.set $pipe
		(call $$$bstring->keyword@__r4_symbols_6_4
		   (array.new_data $bstring $PIPE (i32.const 0) (i32.const 4))))))
      (return (ref.cast (ref $keyword) (global.get $pipe))))
	    
   (func $c_run_process (export "c_run_process")
      (param $host (ref eq))
      (param $fork (ref eq))
      (param $wait (ref eq))
      (param $input (ref eq))
      (param $output (ref eq))
      (param $error (ref eq))
      (param $command (ref eq))
      (param $args (ref eq))
      (param $env (ref eq))
      (result (ref eq))
      (local $idx i32)
      (local $input-idx i32)
      (local $input-len i32)
      (local $output-idx i32)
      (local $output-len i32)
      (local $error-idx i32)
      (local $error-len i32)
      (local $proc externref)
      (local $res (ref $process))
      (local $len i32)
      (local $porttmp i32)
      (local.set $idx (i32.const 128))
      (local.set $len (i32.const 0))

      ;; store the command
      (local.set $idx
	 (call $store_string_len2 (ref.cast (ref $bstring) (local.get $command))
	    (local.get $idx)))

      ;; store the arguments
      (loop $loop
	 (if (ref.test (ref $pair) (local.get $args))
	     (then
		(local.set $idx
		   (call $store_string_len2
		      (ref.cast (ref $bstring)
			 (struct.get $pair $car
			    (ref.cast (ref $pair) (local.get $args))))
		      (local.get $idx)))
		(local.set $args
		   (struct.get $pair $cdr
		      (ref.cast (ref $pair) (local.get $args))))
		(local.set $len (i32.add (i32.const 1) (local.get $len)))
		(br $loop))))

      ;; store the input name
      (if (ref.test (ref $bstring) (local.get $input))
	  (then
	     (local.set $input-idx (local.get $idx))
	     (local.set $input-len
		(array.len (ref.cast (ref $bstring) (local.get $input))))
	     (call $store_string
		(ref.cast (ref $bstring) (local.get $input))
		(local.get $input-idx))
	     (local.set $idx
		(i32.add (local.get $idx) (local.get $input-len))))
	  (else
	   (if (ref.eq (local.get $input) (call $pipe-keyword))
	       (then (local.set $input-idx (i32.const -1)))
	       (else (local.set $input-idx (i32.const -2))))))

      ;; store the output name
      (if (ref.test (ref $bstring) (local.get $output))
	  (then
	     (local.set $output-idx (local.get $idx))
	     (local.set $output-len
		(array.len (ref.cast (ref $bstring) (local.get $output))))
	     (call $store_string
		(ref.cast (ref $bstring) (local.get $output))
		(local.get $output-idx))
	     (local.set $idx
		(i32.add (local.get $idx) (local.get $output-len))))
	  (else
	   (if (ref.eq (local.get $output) (call $pipe-keyword))
	       (then (local.set $output-idx (i32.const -1)))
	       (else (local.set $output-idx (i32.const -2))))))

      ;; store the error name
      (if (ref.test (ref $bstring) (local.get $error))
	  (then
	     (local.set $error-idx (local.get $idx))
	     (local.set $error-len
		(array.len (ref.cast (ref $bstring) (local.get $error))))
	     (call $store_string
		(ref.cast (ref $bstring) (local.get $error))
		(local.get $error-idx))
	     (local.set $idx
		(i32.add (local.get $idx) (local.get $error-len))))
	  (else
	   (if (ref.eq (local.get $error) (call $pipe-keyword))
	       (then (local.set $error-idx (i32.const -1)))
	       (else (local.set $error-idx (i32.const -2))))))

      (local.set $proc
	 (call $run
	    ;; command and arguments
	    (local.get $len) (i32.const 128)
	    ;; fork
	    (ref.eq (local.get $fork) (global.get $BTRUE))
	    ;; wait
	    (ref.eq (local.get $wait) (global.get $BTRUE))
	    ;; input
	    (local.get $input-idx)
	    (local.get $input-len)
	    ;; output
	    (local.get $output-idx)
	    (local.get $output-len)
	    ;; error
	    (local.get $error-idx)
	    (local.get $error-len)))
      
      (local.set $res
	 (struct.new $process (local.get $proc)
	    (global.get $BUNSPEC)
	    (global.get $BUNSPEC)
	    (global.get $BUNSPEC)))

      ;; input-port
      (local.set $porttmp
	 (call $getinport (local.get $proc) (i32.const 128)))

      (if (i32.ge_s (local.get $porttmp) (i32.const 0))
	  (then
	     (struct.set $process $input-port (local.get $res)
		(call $bgl_open_input_string
		   (call $load_string (i32.const 128) (local.get $porttmp))
		   (i64.const 0))))
	  (else
	   (if (i32.ge_s (local.get $porttmp) (i32.const -1))
	       (then
		  (struct.set $process $input-port (local.get $res)
		     (call $bgl_open_output_socket
			(call $getportsock (local.get $proc) (i32.const 0))
			(array.new_default $bstring (i32.const 1024))))))))
      
      ;; output-port
      (local.set $porttmp
	 (call $getoutport (local.get $proc) (i32.const 1) (i32.const 128)))

      (if (i32.ge_s (local.get $porttmp) (i32.const 0))
	  (then
	     (struct.set $process $output-port (local.get $res)
		(call $bgl_open_input_string
		   (call $load_string (i32.const 128) (local.get $porttmp))
		   (i64.const 0))))
	  (else
	   (if (i32.ge_s (local.get $porttmp) (i32.const -1))
	       (then
		  (struct.set $process $output-port (local.get $res)
		     (call $bgl_open_input_socket
			(call $getportsock (local.get $proc) (i32.const 1))
			(array.new_default $bstring (i32.const 1024))))))))
      
      ;; error-port
      (local.set $porttmp
	 (call $getoutport (local.get $proc) (i32.const 2) (i32.const 128)))

      (if (i32.ge_s (local.get $porttmp) (i32.const 0))
	  (then
	     (struct.set $process $error-port (local.get $res)
		(call $bgl_open_input_string
		   (call $load_string (i32.const 128) (local.get $porttmp))
		   (i64.const 0))))
	  (else
	   (if (i32.ge_s (local.get $porttmp) (i32.const -1))
	       (then (call $js_trace (i32.const 9999))))))
      
      (return (local.get $res)))
		    
   
   (func $c_process_alivep (export "c_process_alivep")
      (param $process (ref $process))
      (result i32)
      (call $alive (struct.get $process $proc (local.get $process))))

   (func $c_process_wait (export "c_process_wait")
      (param $process (ref $process))
      (result (ref eq))
      (return (local.get $process)))

   (func $c_process_xstatus (export "c_process_xstatus")
      (param $process (ref $process))
      (result (ref eq))
      (local $status i32)
      (local.set $status
	 (call $xstatus (struct.get $process $proc (local.get $process))))

      (if (result (ref eq)) (i32.lt_s (local.get $status) (i32.const 0))
	  (then (return (global.get $BFALSE)))
	  (else (return (call $BINT (i64.extend_i32_u (local.get $status)))))))
	     

   (func $c_process_send_signal (export "c_process_send_signal")
      (param $process (ref $process))
      (param $sig i32)
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   (func $c_process_kill (export "c_process_kill")
      (param $process (ref $process))
      (result (ref eq))
      (call $kill (struct.get $process $proc (local.get $process)))
      (return (global.get $BUNSPEC)))

   (func $c_process_stop (export "c_process_stop")
      (param $process (ref $process))
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   (func $c_process_continue (export "c_process_continue")
      (param $process (ref $process))
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   (func $c_unregister_process (export "c_unregister_process")
      (param $process (ref $process))
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   (func $c_process_list (export "c_process_list")
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   )
