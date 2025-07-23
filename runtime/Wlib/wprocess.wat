;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wprocess.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Wed Jul 23 13:49:39 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM processes                                                   */
;*=====================================================================*/

(module $__bigloo_process
   
   ;; -----------------------------------------------------------------
   ;; Data 
   ;; -----------------------------------------------------------------

   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $process
      (sub
	 (struct
	    (field $proc (mut externref)))))

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__js" "trace" (func $js_trace (param i32)))
   
   (import "__js" "unsupported" (func $js_unsupported (param i32)))
   (import "__js_process" "nullprocess" (global $nullprocess externref))
   
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
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "BINT" (func $BINT (param i64) (result (ref eq))))
   (import "__bigloo" "the_failure" (func $the_failure (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "BGL_INPUT_PORT_DEFAULT_VALUE" (global $input-port-default-value (ref $input-port)))
   (import "__bigloo" "BGL_OUTPUT_PORT_DEFAULT_VALUE" (global $output-port-default-value (ref $output-port)))

   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $process-default-value
      (export "BGL_PROCESS_DEFAULT_VALUE") (ref $process)
      (struct.new $process
	 ;; proc
	 (global.get $nullprocess)))
   
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
      (return (i32.const 0)))

   (func $PROCESS_INPUT_PORT (export "PROCESS_INPUT_PORT")
      (param $process (ref $process))
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   (func $PROCESS_OUTPUT_PORT (export "PROCESS_OUTPUT_PORT")
      (param $process (ref $process))
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   (func $PROCESS_ERROR_PORT (export "PROCESS_ERROR_PORT")
      (param $process (ref $process))
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   ;; --------------------------------------------------------
   ;; Library functions
   ;; --------------------------------------------------------
   (func $c_run_process (export "c_run_process")
      (param $host (ref eq))
      (param $fork (ref eq))
      (param $waiting (ref eq))
      (param $input (ref eq))
      (param $output (ref eq))
      (param $error (ref eq))
      (param $command (ref eq))
      (param $args (ref eq))
      (param $env (ref eq))
      (result (ref eq))
      (return (global.get $process-default-value)))
   
   (func $c_process_alivep (export "c_process_alivep")
      (param $process (ref $process))
      (result i32)
      (return (i32.const 0)))

   (func $c_process_wait (export "c_process_wait")
      (param $process (ref $process))
      (result (ref eq))
      (return (local.get $process)))

   (func $c_process_xstatus (export "c_process_xstatus")
      (param $process (ref $process))
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   (func $c_process_send_signal (export "c_process_send_signal")
      (param $process (ref $process))
      (param $sig i32)
      (result (ref eq))
      (return (global.get $BUNSPEC)))

   (func $c_process_kill (export "c_process_kill")
      (param $process (ref $process))
      (result (ref eq))
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
