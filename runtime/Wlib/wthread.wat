;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wthread.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Mon Jul 21 08:11:14 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM threads                                                     */
;*=====================================================================*/

(module $__bigloo_thread
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $mutex
      (struct
	 (field $name (ref eq))
	 (field $backend (ref eq))
	 (field $state (ref eq))))
   
   (type $condvar
      (struct
	 (field $name (ref eq))))
   
   (type $semaphore (struct))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------

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
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "BINT" (func $BINT (param i64) (result (ref eq))))
   (import "__bigloo" "the_failure" (func $the_failure (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))

;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $mutex-default-value
      (export "BGL_MUTEX_DEFAULT_VALUE") (ref $mutex)
      (struct.new $mutex
	 ;; name
	 (global.get $BUNSPEC)
	 ;; backend
	 (global.get $BUNSPEC)
	 ;; state
	 (global.get $BUNSPEC)))
   
   (global $condvar-default-value
      (export "BGL_CONDVAR_DEFAULT_VALUE") (ref $condvar)
      (struct.new $condvar
	 ;; name
	 (global.get $BUNSPEC)))
   
   ;; -----------------------------------------------------------------
   ;; Macros
   ;; -----------------------------------------------------------------
   
   (func $BGL_MUTEXP (export "BGL_MUTEXP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $mutex) (local.get $o)))

   (func $BGL_MUTEX_NAME (export "BGL_MUTEX_NAME")
      (param $o (ref $mutex))
      (result (ref eq))
      (return (struct.get $mutex $name (local.get $o))))
   
   (func $BGL_MUTEX_BACKEND (export "BGL_MUTEX_BACKEND")
      (param $o (ref $mutex))
      (result (ref eq))
      (return (struct.get $mutex $backend (local.get $o))))
   
   (func $BGL_MUTEX_STATE (export "BGL_MUTEX_STATE")
      (param $o (ref $mutex))
      (result (ref eq))
      (return (struct.get $mutex $state (local.get $o))))
   
   (func $BGL_CONDVARP (export "BGL_CONDVARP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $condvar) (local.get $o)))

   (func $BGL_CONDVAR_NAME
      (export "BGL_CONDVAR_NAME")
      (param $o (ref $condvar))
      (result (ref eq))
      (return (struct.get $condvar $name (local.get $o))))

   (func $BGL_CONDVAR_BROADCAST
      (export "BGL_CONDVAR_BROADCAST")
      (param (ref $condvar))
      (result i32)
      (call $js_unsupported (i32.const 1))
      (i32.const 0))
   
   (func $BGL_CONDVAR_TIMED_WAIT
      (export "BGL_CONDVAR_TIMED_WAIT")
      (param (ref $condvar))
      (param (ref $mutex))
      (param i64)
      (result i32)
      (call $js_unsupported (i32.const 2))
      (i32.const 0))
   
   (func $BGL_CONDVAR_WAIT
      (export "BGL_CONDVAR_WAIT")
      (param (ref $condvar))
      (param (ref $mutex))
      (result i32)
      (call $js_unsupported (i32.const 3))
      (i32.const 0))
   
   (func $BGL_CONDVAR_SIGNAL
      (export "BGL_CONDVAR_SIGNAL")
      (param (ref $condvar))
      (result i32)
      (call $js_unsupported (i32.const 4))
      (i32.const 0))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   (func $bgl_make_mutex
      (export "bgl_make_mutex")
      (param $name (ref eq))
      (result (ref $mutex))
      (struct.new $mutex
	 ;; name
	 (local.get $name)
	 ;; backend
	 (global.get $BUNSPEC)
	 ;; state
	 (global.get $BUNSPEC)))

   (func $bgl_make_spinlock
      (export "bgl_make_spinlock")
      (param $name (ref eq))
      (result (ref $mutex))
      (return_call $bgl_make_mutex (local.get $name)))
   
   (func $bgl_make_condvar
      (export "bgl_make_condvar")
      (param $name (ref eq))
      (result (ref $condvar))
      (struct.new $condvar
	 ;; name
	 (local.get $name)))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   )
  

   
