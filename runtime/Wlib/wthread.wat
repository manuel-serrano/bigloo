;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wthread.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Wed Feb  5 07:20:21 2025 (serrano)                */
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
   )
  

   
