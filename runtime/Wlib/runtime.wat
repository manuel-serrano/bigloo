;; -*- eval: (bee-mode) -*-
;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/runtime.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 10:34:00 2024                          */
;*    Last change :  Tue Jul 22 12:29:17 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bigloo WASM builtin runtime                                      */
;*=====================================================================*/

(module $__bigloo
   
   ;; -----------------------------------------------------------------
   ;; Memory
   ;; -----------------------------------------------------------------

   (memory 1)
   (export "memory" (memory 0))
   
   ;; -----------------------------------------------------------------
   ;; Imports
   ;; -----------------------------------------------------------------

   (import "__js" "trace" (func $js_trace (param i32)))
   (import "__js" "internalError" (func $js_internal_error (param i32) (param i32)))
   (import "__js" "$__main" (func $__main (param (ref $pair)) (result (ref eq))))
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
   
   (import "__js_system" "signal" (func $js_signal (param i32) (param (ref eq))))   
   (import "__js_system" "argc" (global $js_argc i32))
   (import "__js_system" "get_arg" (func $js_get_arg (param i32 i32) (result i32)))
   (import "__js_system" "exit" (func $js_exit (param i32)))
   
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
   (import "__bigloo" "BOOLEANP" (func $BOOLEANP (param (ref eq)) (result i32)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))
   (import "__bigloo" "BGL_MUTEX_DEFAULT_VALUE" (global $mutex-default-value (ref $mutex)))
   (import "__bigloo" "BGL_CONDVAR_DEFAULT_VALUE" (global $condvar-default-value (ref $condvar)))
   (import "__bigloo" "bgl_obj_hash_number" (func $bgl_obj_hash_number (param (ref eq)) (result i64)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_INPUT_PORT_DEFAULT_VALUE" (global $input-port-default-value (ref $input-port)))
   (import "__bigloo" "BGL_OUTPUT_PORT_DEFAULT_VALUE" (global $output-port-default-value (ref $output-port)))
   (import "__bigloo" "bgl_flush_output_port" (func $bgl_flush_output_port (param (ref $output-port)) (result (ref eq))))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   (import "__bigloo" "bgl_init_io" (func $bgl_init_io))
   (import "__bigloo" "bgl_init_trace" (func $bgl_init_trace (param (ref $dynamic-env))))
   (import "__bigloo" "BGL_CURRENT_DYNAMIC_ENV" (func $BGL_CURRENT_DYNAMIC_ENV (result (ref $dynamic-env))))
   (import "__bigloo" "BGL_ENV_CURRENT_OUTPUT_PORT" (func $BGL_ENV_CURRENT_OUTPUT_PORT (param (ref $dynamic-env)) (result (ref $output-port))))
   (import "__bigloo" "BGL_ENV_CURRENT_ERROR_PORT" (func $BGL_ENV_CURRENT_ERROR_PORT (param (ref $dynamic-env)) (result (ref $output-port))))
   (import "__bigloo" "BGL_ENV_CURRENT_INPUT_PORT" (func $BGL_ENV_CURRENT_INPUT_PORT (param (ref $dynamic-env)) (result (ref $input-port))))
   (import "__bigloo" "OBJ_TO_INT" (func $OBJ_TO_INT (param (ref eq)) (result i64)))
   (import "__bigloo" "INTEGERP" (func $INTEGERP (param (ref eq)) (result i32)))
   (import "__bigloo" "bigloo_exit_apply" (func $bigloo_exit_apply (param (ref eq)) (result (ref eq))))
   
   (func $bgl_internal_error (export "bgl_internal_error")
      (param $errno i32)
      (param $val i32)
      (result (ref eq))
      (call $js_internal_error (local.get $errno) (local.get $val))
      (return (global.get $BUNSPEC)))

   (func $CELLP (export "CELLP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $cell) (local.get $o)))

   (func $OPAQUEP (export "OPAQUEP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $opaque) (local.get $o)))
      
   (func $CNST_TABLE_SET (export "CNST_TABLE_SET")
      (param $offset i32)
      (param $val (ref eq))
      (result (ref eq))
      (global.get $BUNSPEC))
   
   (func $CNST_TABLE_REF (export "CNST_TABLE_REF")
      (param $offset i32)
      (result (ref eq))
      (global.get $BUNSPEC))
   
   ;; Bigloo types default value
   (global $funptr-default-value
      (export "BGL_FUNPTR_DEFAULT_VALUE") (ref func)
      (ref.func $BOOLEANP))
   
   (global $real-default-value
      (export "BGL_REAL_DEFAULT_VALUE") (ref $real)
      (struct.new $real (f64.const 0)))
   
   (global $bucs2-default-value
      (export "BGL_BUCS2_DEFAULT_VALUE") (ref $bucs2)
      (struct.new $bucs2 (i32.const 0)))
   
   (global $cell-default-value
      (export "BGL_CELL_DEFAULT_VALUE") (ref $cell)
      (struct.new $cell (global.get $BUNSPEC)))
   
   (global $struct-default-value
      (export "BGL_STRUCT_DEFAULT_VALUE") (ref $struct)
      (struct.new $struct (global.get $BUNSPEC) (global.get $vector-default-value)))
   
   (global $socket-default-value
      (export "BGL_SOCKET_DEFAULT_VALUE") (ref $socket)
      (struct.new $socket))
   (global $datagram-socket-default-value
      (export "BGL_DATAGRAM_SOCKET_DEFAULT_VALUE") (ref $datagram-socket)
      (struct.new $datagram-socket))
   
   (global $semaphore-default-value
      (export "BGL_SEMAPHOE_DEFAULT_VALUE") (ref $semaphore)
      (struct.new $semaphore))
   
   (global $process-default-value
      (export "BGL_PROCESS_DEFAULT_VALUE") (ref $process)
      (struct.new $process))
   
   (global $object-default-value
      (export "BGL_OBJECT_DEFAULT_VALUE") (ref $BgL_objectz00_bglt)
      (struct.new $BgL_objectz00_bglt
	 (i64.const 0)
	 (global.get $BUNSPEC)))
   
   (global $opaque-default-value
      (export "BGL_OPAQUE_DEFAULT_VALUE") (ref $opaque)
      (struct.new $opaque))
   
   (global $tvector-default-value
      (export "BGL_TVECTOR_DEFAULT_VALUE") (ref array)
      (array.new_fixed $bstring 0))
   
   (global $BGL_AUTO_FINALIZER
      (export "BGL_AUTO_FINALIZER") i32
      (i32.const 0))
   
   ;; -----------------------------------------------------------------
   ;; Utilities
   ;; -----------------------------------------------------------------
   
   ;; memcpy
   (func $memcpy (export "bgl_memcpy")
      (param $dest i32)
      (param $src (ref $bstring))
      (param $i i32)
      (param $len i32)
      (loop $loop
	 (if (i32.lt_u (local.get $i) (local.get $len))
	     (then
		(i32.store8 (i32.add (local.get $dest) (local.get $i))
		   (array.get_u $bstring (local.get $src) (local.get $i)))
		(local.set $i (i32.add (local.get $i) (i32.const 1)))
		(br $loop)))))
   
   ;; --------------------------------------------------------
   ;; Struct functions
   ;; --------------------------------------------------------

   (func $STRUCTP (export "STRUCTP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $struct) (local.get $o)))

   (func $STRUCT_KEY (export "STRUCT_KEY")
      (param $struct (ref $struct))
      (result (ref eq))
      (struct.get $struct $key (local.get $struct)))

   (func $STRUCT_KEY_SET (export "STRUCT_KEY_SET")
      (param $struct (ref $struct))
      (param $key (ref eq))
      (result (ref eq))
      (struct.set $struct $key (local.get $struct) (local.get $key))
      (return (local.get $struct)))

   (func $STRUCT_LENGTH (export "STRUCT_LENGTH")
      (param $struct (ref $struct))
      (result i32)
      (array.len (struct.get $struct $values (local.get $struct))))

   (func $STRUCT_REF (export "STRUCT_REF")
      (param $struct (ref $struct))
      (param $index i32)
      (result (ref eq))
      (array.get $vector
	 (struct.get $struct $values (local.get $struct))
	 (local.get $index)))

   (func $STRUCT_SET (export "STRUCT_SET")
      (param $struct (ref $struct))
      (param $index i32)
      (param $value (ref eq))
      (result (ref eq))
      (array.set $vector 
	 (struct.get $struct $values (local.get $struct)) 
	 (local.get $index) 
	 (local.get $value))
      (global.get $BUNSPEC))
   
   ;; --------------------------------------------------------
   ;; Multiple values functions
   ;; --------------------------------------------------------
   (global $mvalues_count (export "mvalues_count")
      (mut i32) (i32.const 0))
   
   (global $mvalues (export "mvalues")
      (mut (ref $vector))
      (array.new $vector (global.get $BUNSPEC) (i32.const 16)))
   
   (func $BGL_MVALUES_VAL (export "BGL_MVALUES_VAL")
      (param $i i32)
      (result (ref eq))
      (array.get $vector (global.get $mvalues) (local.get $i)))
   
   (func $BGL_MVALUES_VAL_SET (export "BGL_MVALUES_VAL_SET")
      (param $i i32)
      (param $val (ref eq))
      (result (ref eq))
      (array.set $vector (global.get $mvalues) (local.get $i) (local.get $val))
      (local.get $val))
   
   (func $BGL_MVALUES_NUMBER (export "BGL_MVALUES_NUMBER")
      (result i32)
      (global.get $mvalues_count))
   
   (func $BGL_MVALUES_NUMBER_SET (export "BGL_MVALUES_NUMBER_SET")
      (param $n i32)
      (result i32)
      (global.set $mvalues_count (local.get $n))
      ;; Reallocate mvalues if not large enough.
      (if (i32.gt_u (local.get $n) (array.len (global.get $mvalues)))
	  (then
	     (if (i32.gt_s (local.get $n) (i32.const 0))
		 (then
		    (global.set $mvalues
		       (array.new $vector (global.get $BUNSPEC) (local.get $n)))))))
      (local.get $n))
   
   ;; --------------------------------------------------------
   ;; Cell functions
   ;; --------------------------------------------------------
   
   (func $CELL_SET (export "CELL_SET") (param $c (ref $cell)) (param $v (ref eq)) (result (ref eq))
      (struct.set $cell $val (local.get $c) (local.get $v))
      (global.get $BUNSPEC))
   
   ;; --------------------------------------------------------
   ;; Pair functions
   ;; --------------------------------------------------------
   
   (func $SET_CAR (export "SET_CAR") (param $p (ref $pair)) (param $v (ref eq)) (result (ref eq))
      (struct.set $pair $car (local.get $p) (local.get $v))
      (global.get $BUNSPEC))
   
   (func $SET_CDR (export "SET_CDR") (param $p (ref $pair)) (param $v (ref eq)) (result (ref eq))
      (struct.set $pair $cdr (local.get $p) (local.get $v))
      (global.get $BUNSPEC))
   
   (func $SET_CER (export "SET_CER") (param $p (ref $epair)) (param $v (ref eq)) (result (ref eq))
      (struct.set $epair $cer (local.get $p) (local.get $v))
      (global.get $BUNSPEC))
   
   ;; --------------------------------------------------------
   ;; Flonum builtin functions
   ;; --------------------------------------------------------
   
   (func $BGL_SIGNBIT (export "BGL_SIGNBIT") (param $v f64) (result i64)
      (i64.shr_u (i64.reinterpret_f64 (local.get $v)) (i64.const 63)))
   
   (func $BGL_IS_FINITE (export "BGL_IS_FINITE") (param $v f64) (result i32)
      ;; This is the code generated by Clang for __builtin_isfinite().
      ;; See https://godbolt.org/z/WPoW3djYK
      (i64.lt_s
	 (i64.and
	    (i64.reinterpret_f64 (local.get $v))
	    (i64.const 0x7FFFFFFFFFFFFFFF #;NaN))
	 (i64.const 0x7FF0000000000000 #;Inf)))
   
   (func $BGL_IS_INF (export "BGL_IS_INF") (param $v f64) (result i32)
      ;; The abs is required to take care of -INF and +INF.
      (f64.eq (f64.abs (local.get $v)) (f64.const inf)))
   
   (func $BGL_IS_NAN (export "BGL_IS_NAN") (param $v f64) (result i32)
      ;; NaN are the only floating point values that are never
      ;; equal to themself (this is the trick used by Clang).
      (f64.ne (local.get $v) (local.get $v)))
   
   ;; --------------------------------------------------------
   ;; OS functions
   ;; --------------------------------------------------------
   
   (global $OS_CLASS (export "OS_CLASS") (ref $bstring)
      ;; ASCII for 'wasm'
      (array.new_fixed $bstring 4 (i32.const 0x77) (i32.const 0x61) (i32.const 0x73) (i32.const 0x6D)))
   
   (global $OS_NAME (export "OS_NAME") (ref $bstring)
      ;; ASCII for 'wasm'
      (array.new_fixed $bstring 4 (i32.const 0x77) (i32.const 0x61) (i32.const 0x73) (i32.const 0x6D)))
   
   (global $OS_ARCH (export "OS_ARCH") (ref $bstring)
      ;; ASCII for 'wasm'
      (array.new_fixed $bstring 4 (i32.const 0x77) (i32.const 0x61) (i32.const 0x73) (i32.const 0x6D)))
   
   (global $OS_TMP (export "OS_TMP") (ref $bstring)
      ;; ASCII for '/tmp'
      (array.new_fixed $bstring 4 (i32.const 0x2F) (i32.const 0x74) (i32.const 0x6D) (i32.const 0x70)))
   
   ;; --------------------------------------------------------
   ;; Object functions
   ;; --------------------------------------------------------
   
   (func $BGL_OBJECT_CLASS_NUM_SET (export "BGL_OBJECT_CLASS_NUM_SET")
      (param $o (ref $BgL_objectz00_bglt))
      (param $num i64)
      (result (ref eq))
      (struct.set $BgL_objectz00_bglt $header (local.get $o) (local.get $num))
      (global.get $BUNSPEC))
   
   ;; --------------------------------------------------------
   ;; Mutext functions
   ;; --------------------------------------------------------
   
   (func $bgl_make_nil_mutex (export "bgl_make_nil_mutex")
      (result (ref $mutex))
      (global.get $mutex-default-value))
   
   (func $bgl_make_nil_condvar (export "bgl_make_nil_condvar")
      (result (ref $condvar))
      (global.get $condvar-default-value))
   
;*   ;; close_file_input_port                                          */
;*   (func $close_file_input_port                                      */
;*     (param $port (ref $file-input-port))                            */
;*     (result (ref eq))                                               */
;*                                                                     */
;*     (call $js_close_file (struct.get $file-input-port $fd (local.get $port))) */
;*     (local.get $port))                                              */
;*                                                                     */
   ;; --------------------------------------------------------
   ;; Hash functions
   ;; --------------------------------------------------------
   
   (func $bgl_string_hash (export "bgl_string_hash")
      (param $str (ref $bstring))
      (param $start i32)
      (param $len i32)
      (result i64)
      (local $r i64)
      (local $i i32)
      ;; We use the same algorithm as for the C and Java implementation of Bigloo runtime.
      (local.set $r (i64.const 5381))
      (local.set $i (local.get $start))
      (loop $for-loop
	 (if (i32.lt_u (local.get $i) (local.get $len))
	     (then
		;; r <- r + (r << 5) + s[i]
		(local.set $r
		   (i64.add
		      (local.get $r)
		      (i64.add
			 (i64.shl
			    (local.get $r)
			    (i64.const 5))
			 (i64.extend_i32_u
			    (array.get_u $bstring (local.get $str) (local.get $i))))))
		(local.set $i (i32.add (local.get $i) (i32.const 1)))
		(br $for-loop))))
      (i64.and
	 (local.get $r)
	 (i64.const 536870911 #;((1 << 29) - 1))))
   
   (func $bgl_string_hash_persistent (export "bgl_string_hash_persistent")
      (param $str (ref $bstring))
      (param $start i32)
      (param $len i32)
      (result i64)
      (call $bgl_string_hash (local.get $str) (local.get $start) (local.get $len)))
   
   (func $bgl_symbol_hash_number (export "bgl_symbol_hash_number")
      (param $sym (ref $symbol))
      (result i64)
      (i64.add
	 (call $bgl_string_hash 
	    (struct.get $symbol $str (local.get $sym)) 
	    (i32.const 0) 
	    (array.len (struct.get $symbol $str (local.get $sym))))
	 (i64.const 1)))
   
   (func $bgl_symbol_hash_number_persistent (export "bgl_symbol_hash_number_persistent")
      (param $sym (ref $symbol))
      (result i64)
      (call $bgl_symbol_hash_number (local.get $sym)))
   
   (func $bgl_keyword_hash_number (export "bgl_keyword_hash_number")
      (param $key (ref $keyword))
      (result i64)
      (i64.add
	 (call $bgl_string_hash 
	    (struct.get $keyword $str (local.get $key)) 
	    (i32.const 0) 
	    (array.len (struct.get $keyword $str (local.get $key))))
	 (i64.const 2)))
   
   (func $bgl_keyword_hash_number_persistent (export "bgl_keyword_hash_number_persistent")
      (param $key (ref $keyword))
      (result i64)
      (call $bgl_keyword_hash_number (local.get $key)))
   
   (func $bgl_pointer_hash_number (export "bgl_pointer_hash_number")
      (param $obj (ref eq))
      (param $power i64)
      (result i64)
      (i64.rem_u
	 (call $bgl_obj_hash_number (local.get $obj))
	 (local.get $power)))

   (func $bgl_pointer_hashnumber (export "bgl_pointer_hashnumber")
      (param $obj (ref eq))
      (param $power i64)
      (result i64)
      (return (i64.rem_u (i64.const 933204) (local.get $power))))
   
   (func $bgl_foreign_hash_number (export "bgl_foreign_hash_number")
      (param $obj (ref $foreign))
      (result i64)
      (return_call $bgl_pointer_hash_number (local.get $obj) (i64.const 1)))

   (func $FOREIGN_TYPE_NAME (export "FOREIGN_TYPE_NAME")
      (param (ref eq))
      (result (ref $bstring))
      (return (array.new_fixed $bstring 1 (i32.const 0x5f))))
   
   ;; --------------------------------------------------------
   ;; lockf function and constants
   ;; --------------------------------------------------------
   
   (global $F_LOCK (export "F_LOCK") i32 (i32.const 0))
   (global $F_TLOCK (export "F_TLOCK") i32 (i32.const 0))
   (global $F_ULOCK (export "F_ULOCK") i32 (i32.const 0))
   (global $F_TEST (export "F_TEST") i32 (i32.const 0))
   (func $bgl_lockf (export "bgl_lockf")
      (param $port (ref $output-port))
      (param i32)
      (param i64)
      (result i32)
      ;; Not implemented (not supported by NodeJS fs API).
      (i32.const 0 #;FALSE))
   
   ;; --------------------------------------------------------
   ;; Math functions
   ;; --------------------------------------------------------
   
   (export "fmod" (func $fmod))
   (export "exp" (func $exp))
   (export "log" (func $log))
   (export "log2" (func $log2))
   (export "log10" (func $log10))
   (export "sin" (func $sin))
   (export "cos" (func $cos))
   (export "tan" (func $tan))
   (export "asin" (func $asin))
   (export "acos" (func $acos))
   (export "atan" (func $atan))
   (export "atan2" (func $atan2))
   (export "pow" (func $pow))
   (export "RANDOMFL" (func $RANDOMFL))
   
   ;; flonums
   (export "bgl_double_to_ieee_string" (func $bgl_double_to_ieee_string))
   (export "bgl_float_to_ieee_string" (func $bgl_float_to_ieee_string))
   
   (func $bgl_double_to_ieee_string
      (param $x f64)
      (result (ref $bstring))
      ;; CARE MS 16sep2024: TODO
      (global.get $bstring-default-value))
   
   (func $bgl_float_to_ieee_string
      (param $x f32)
      (result (ref $bstring))
      ;; CARE MS 16sep2024: TODO
      (global.get $bstring-default-value))
   
   ;; --------------------------------------------------------
   ;; Sockets
   ;; --------------------------------------------------------
   (func $SOCKET_INPUT (export "SOCKET_INPUT")
      (param $socket (ref $socket))
      (result (ref $input-port))
      (global.get $input-port-default-value))
   
   (func $SOCKET_OUTPUT (export "SOCKET_OUTPUT")
      (param $socket (ref $socket))
      (result (ref $output-port))
      (global.get $output-port-default-value))
   
   ;; --------------------------------------------------------
   ;; OS
   ;; --------------------------------------------------------
   (func $bgl_signal (export "bgl_signal")
      (param $sig i32)
      (param $hdl (ref eq))
      (result (ref eq))
      (call $js_signal (local.get $sig) (local.get $hdl))
      (return (global.get $BUNSPEC)))
   
   (func $BGL_SIGSETMASK (export "BGL_SIGSETMASK")
      (param $i i32)
      (result i32)
      (return (i32.const 0)))
   
   ;; --------------------------------------------------------
   ;; Eval
   ;; --------------------------------------------------------
   
   (func $__EVMEANING_ADDRESS (export "__EVMEANING_ADDRESS")
      (param $get (ref $bgl_evmeaning_getter))
      (param $set (ref $bgl_evmeaning_setter))
      (result (ref eq))
      (return (struct.new $bgl_evmeaning_addr
		 (local.get $get)
		 (local.get $set))))
   
   (func $__EVMEANING_ADDRESS_REF (export "__EVMEANING_ADDRESS_REF")
      (param $o (ref eq))
      (result (ref eq))
      (return_call_ref $bgl_evmeaning_getter
	 (ref.cast (ref $bgl_evmeaning_getter)
	    (struct.get $bgl_evmeaning_addr $get
	       (ref.cast (ref $bgl_evmeaning_addr) (local.get $o))))))
   
   (func $__EVMEANING_ADDRESS_SET (export "__EVMEANING_ADDRESS_SET")
      (param $o (ref eq))
      (param $v (ref eq))
      (result (ref eq))
      (return_call_ref $bgl_evmeaning_setter
	 (local.get $v)
	 (ref.cast (ref $bgl_evmeaning_setter)
	    (struct.get $bgl_evmeaning_addr $set
	       (ref.cast (ref $bgl_evmeaning_addr) (local.get $o))))))
   
   ;; --------------------------------------------------------
   ;; Main function
   ;; --------------------------------------------------------
   
   (func $__bigloo_main (export "__bigloo_main")
      (local $i i32)
      (local $argv (ref eq))
      (local.set $i (i32.sub (global.get $js_argc) (i32.const 1)))
      (local.set $argv (global.get $BNIL))
      (loop $loop
	 (if (i32.ge_s (local.get $i) (i32.const 0))
	     (then
		(local.set $argv
		   (struct.new $pair
		      (call $load_string (i32.const 128)
			 (call $js_get_arg (local.get $i) (i32.const 128)))
		      (local.get $argv)))
		(local.set $i (i32.sub (local.get $i) (i32.const 1)))
		(br $loop))))

      (call $bgl_init_io)
      (call $bgl_init_trace (call $BGL_CURRENT_DYNAMIC_ENV))
      
      (drop
	 (call $__main (ref.cast (ref $pair) (local.get $argv)))))
   
   (func $BIGLOO_EXIT (export "BIGLOO_EXIT")
      (param $val (ref eq))
      (result (ref eq))

      (local.set $val (call $bigloo_exit_apply (local.get $val)))
      
      (drop
	 (call $bgl_flush_output_port
	    (call $BGL_ENV_CURRENT_OUTPUT_PORT
	       (call $BGL_CURRENT_DYNAMIC_ENV))))
      (drop
	 (call $bgl_flush_output_port
	    (call $BGL_ENV_CURRENT_ERROR_PORT
	       (call $BGL_CURRENT_DYNAMIC_ENV))))
      (call $js_exit
	 (if (result i32)
	     (call $INTEGERP (local.get 0))
	     (then (i32.wrap_i64 (call $OBJ_TO_INT (local.get 0))))
	     (else (i32.const 0))))
      
      (return (local.get $val)))
   )
