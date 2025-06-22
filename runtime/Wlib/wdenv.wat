;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wdenv.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jan  4 06:08:48 2025                          */
;*    Last change :  Sun Jun 22 09:32:07 2025 (serrano)                */
;*    Copyright   :  2025 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM dynamic env                                                 */
;*=====================================================================*/

(module $__bigloo_denv
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $exit
      (struct 
	 (field $userp (mut i32))
	 (field $stamp (mut i32))
	 (field $protect (mut (ref eq)))
	 (field $prev (mut (ref null $exit)))))
   
   (rec
      (type $bgl_dframe
	 (struct
	    (field $name (mut (ref eq)))
	    (field $location (mut (ref eq)))
	    (field $link (mut (ref null $bgl_dframe)))))
      
      (type $bgl_exit_trace_list
	 (struct
	    (field $head (ref $bgl_dframe))
	    (field $next (ref null $bgl_exit_trace_list))))
      
      (type $dynamic-env
	 (struct
	    (field $id i32)
	    (field $exitd_top (mut (ref $exit)))
	    (field $exitd_val (mut (ref eq)))
	    (field $uncaught-exception-handler (mut (ref eq)))
	    (field $error-handler (mut (ref eq)))
	    (field $error-notifiers (mut (ref eq)))
	    (field $interrupt-notifier (mut (ref eq)))
	    
	    (field $current-output-port (mut (ref null $output-port)))
	    (field $current-error-port (mut (ref null $output-port)))
	    (field $current-input-port (mut (ref null $input-port)))
	    
	    (field $evstate (mut (ref eq)))
	    (field $module (mut (ref eq)))
	    (field $abase (mut (ref eq)))
	    
	    (field $lexical-stack (mut (ref eq)))
	    (field $top-of-frame (mut (ref $bgl_dframe)))
	    (field $exit-traces (mut (ref null $bgl_exit_trace_list)))

	    (field $thread-backend (mut (ref eq))))))
   
   (rec
      (type $bexception
	 (struct
	    (field $exit (ref $exit))
	    (field $val (ref eq)))))
   
   ;; -----------------------------------------------------------------
   ;; Data 
   ;; -----------------------------------------------------------------
   (data $default_internal_error "wasm internal error")
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   (global $bgl_internal_error (mut (ref null eq))
      (ref.null none))
   
   (global $exit-default-value
      (export "BGL_EXIT_DEFAULT_VALUE") (ref $exit)
      (struct.new $exit
	 ;; userp
	 (i32.const 0)
	 ;; stamp
	 (i32.const 0)
	 ;; protect
	 (global.get $BUNSPEC)
	 ;; prev
	 (ref.null none)))
   
   (global $dynamic-env-default-value
      (export "BGL_DYNAMIC_ENV_DEFAULT_VALUE") (ref $dynamic-env)
      (struct.new $dynamic-env
	 ;; id
	 (i32.const 0)
	 ;; exitd_top
	 (global.get $exit-default-value)
	 ;; exitd_val
	 (global.get $BUNSPEC)
	 ;; uncaught-exception-handler
	 (global.get $BUNSPEC)
	 ;; error-handler
	 (global.get $BUNSPEC)
	 ;; error-notifiers
	 (global.get $BNIL)
	 ;; interrupt-notifier
	 (global.get $BNIL)
	 ;; current-output-port
	 (ref.null none)
	 ;; current-error-port
	 (ref.null none)
	 ;; current-input-port
	 (ref.null none)
	 ;; evstate
	 (global.get $BUNSPEC)
	 ;; module
	 (global.get $BUNSPEC)
	 ;; abase
	 (global.get $BUNSPEC)
	 ;; lexical-stack
	 (global.get $BNIL)
	 ;; top-of-frame
	 (struct.new $bgl_dframe
	    (global.get $BUNSPEC)
	    (global.get $BUNSPEC)
	    (ref.null none))
	 ;; exit-traces
	 (ref.null none)
	 ; thread-backend
	 (global.get $BUNSPEC)
	 ))
   
   ;; --------------------------------------------------------
   ;; Dynamic env functions
   ;; --------------------------------------------------------
   
   (global $current-dynamic-env (ref $dynamic-env) 
      (struct.new $dynamic-env
	 ;; id
	 (i32.const 3)
	 ;; exitd_top
	 (struct.new $exit
	    (i32.const 0)
	    (i32.const 0)
	    (global.get $BNIL)
	    (ref.null none))
	 ;; exitd_val
	 (struct.new $pair
	    (struct.new $pair
	       (global.get $BUNSPEC)
	       (global.get $BUNSPEC))
	    (global.get $BUNSPEC))
	 ;; uncaught-exception-handler
	 (global.get $BNIL)
	 ;; error-handler
	 (struct.new $pair
	    (global.get $BUNSPEC)
	    (global.get $BFALSE))
	 ;; error-notifiers
	 (global.get $BNIL)
	 ;; interrupt-notifier
	 (global.get $BNIL)
	 ;; current-output-port
	 (ref.null none)
	 ;; current-error-port
	 (ref.null none)
	 ;; current-input-port
	 (ref.null none)
	 ;; evstate
	 (global.get $BUNSPEC)
	 ;; module
	 (global.get $BUNSPEC)
	 ;; abase
	 (global.get $BUNSPEC)
	 ;; lexical-stack
	 (global.get $BNIL)
	 ;; top-of-frame
	 (struct.new $bgl_dframe
	    (global.get $BUNSPEC)
	    (global.get $BUNSPEC)
	    (ref.null none))
	 ;; exit-traces
	 (ref.null none)
	 ; thread-backend
	 (global.get $BUNSPEC)
	 ))
   
   (func $BGL_CURRENT_DYNAMIC_ENV
      (export "BGL_CURRENT_DYNAMIC_ENV")
      (result (ref $dynamic-env))
      (global.get $current-dynamic-env))
   
   (func $BGL_ENV_CURRENT_OUTPUT_PORT
      (export "BGL_ENV_CURRENT_OUTPUT_PORT")
      (param $env (ref $dynamic-env))
      (result (ref $output-port))
      (ref.cast (ref $output-port)
	 (struct.get $dynamic-env $current-output-port (local.get $env))))
   
   (func $BGL_ENV_CURRENT_ERROR_PORT
      (export "BGL_ENV_CURRENT_ERROR_PORT")
      (param $env (ref $dynamic-env))
      (result (ref $output-port))
      (ref.cast (ref $output-port)
	 (struct.get $dynamic-env $current-error-port (local.get $env))))
   
   (func $BGL_ENV_CURRENT_INPUT_PORT
      (export "BGL_ENV_CURRENT_INPUT_PORT")
      (param $env (ref $dynamic-env))
      (result (ref $input-port))
      (ref.cast (ref $input-port)
	 (struct.get $dynamic-env $current-input-port (local.get $env))))
   
   (func $BGL_ENV_CURRENT_OUTPUT_PORT_SET
      (export "BGL_ENV_CURRENT_OUTPUT_PORT_SET")
      (param $env (ref $dynamic-env))
      (param $port (ref $output-port))
      (struct.set $dynamic-env $current-output-port (local.get $env) (local.get $port)))
   
   (func $BGL_ENV_CURRENT_ERROR_PORT_SET
      (export "BGL_ENV_CURRENT_ERROR_PORT_SET")
      (param $env (ref $dynamic-env))
      (param $port (ref $output-port))
      (struct.set $dynamic-env $current-error-port (local.get $env) (local.get $port)))
   
   (func $BGL_ENV_CURRENT_INPUT_PORT_SET
      (export "BGL_ENV_CURRENT_INPUT_PORT_SET")
      (param $env (ref $dynamic-env))
      (param $port (ref $input-port))
      (struct.set $dynamic-env $current-input-port (local.get $env) (local.get $port)))
   
   (func $BGL_MODULE
      (export "BGL_MODULE")
      (result (ref eq))
      (struct.get $dynamic-env $module (global.get $current-dynamic-env)))
   
   (func $BGL_MODULE_SET
      (export "BGL_MODULE_SET")
      (param $mod (ref eq))
      (result (ref eq))
      (struct.set $dynamic-env $module (global.get $current-dynamic-env) (local.get $mod))
      (local.get $mod))
   
   (func $BGL_ABASE
      (export "BGL_ABASE")
      (result (ref eq))
      (struct.get $dynamic-env $abase (global.get $current-dynamic-env)))
   
   (func $BGL_ABASE_SET
      (export "BGL_ABASE_SET")
      (param $abase (ref eq))
      (result (ref eq))
      (struct.set $dynamic-env $abase (global.get $current-dynamic-env) (local.get $abase))
      (local.get $abase))
   
   (func $bgl_init_trace
      (export "bgl_init_trace")
      (param $env (ref $dynamic-env))
      (local $top (ref $bgl_dframe))

      (local.set $top
	 (struct.new $bgl_dframe
	    (global.get $BUNSPEC)
	    (global.get $BUNSPEC)
	    (ref.null none)))
      
      (call $BGL_ENV_SET_TOP_OF_FRAME (local.get $env) (local.get $top)))
   
   (func $BGL_ENV_GET_TOP_OF_FRAME
      (param $env (ref $dynamic-env))
      (result (ref $bgl_dframe))
      (struct.get $dynamic-env $top-of-frame (local.get $env)))

   (func $bgl_dframe_length
      ;; debug
      (param $l (ref null $bgl_dframe))
      (result i32)
      (if (result i32) (ref.is_null (local.get $l))
	  (then
	     (i32.const 0))
	  (else
	   (i32.add (i32.const 1)
	      (call $bgl_dframe_length
		 (struct.get $bgl_dframe $link (local.get $l)))))))
		
   (func $BGL_ENV_SET_TOP_OF_FRAME
      (param $env (ref $dynamic-env))
      (param $tof (ref $bgl_dframe))
;*       (call $js_trace (i32.const 11110))                            */
;*       (call $js_trace (call $bgl_dframe_length (local.get $tof)))   */
      (struct.set $dynamic-env $top-of-frame
	 (local.get $env) (local.get $tof)))
   
   (func $BGL_ENV_EXIT_TRACES
      (param $env (ref $dynamic-env))
      (result (ref null $bgl_exit_trace_list))
      (struct.get $dynamic-env $exit-traces (local.get $env)))
   
   (func $BGL_ENV_EXIT_TRACES_SET
      (param $env (ref $dynamic-env))
      (param $traces (ref null $bgl_exit_trace_list))
      (struct.set $dynamic-env $exit-traces (local.get $env)
	 (local.get $traces)))
   
   (func $BGL_ENV_PUSH_TRACE
      (export "BGL_ENV_PUSH_TRACE")
      (param $env (ref $dynamic-env))
      (param $name (ref eq))
      (param $loc (ref eq))
      (result (ref eq))
      (local $tmp (ref $bgl_dframe))

;*       (call $js_trace (i32.const 11111))                            */
      (local.set $tmp
	 (struct.new $bgl_dframe
	    (local.get $name)
	    (local.get $loc)
	    (call $BGL_ENV_GET_TOP_OF_FRAME (local.get $env))))
      (call $BGL_ENV_SET_TOP_OF_FRAME (local.get $env) (local.get $tmp))
      
      (return (global.get $BUNSPEC)))
   
   (func $BGL_ENV_POP_TRACE
      (export "BGL_ENV_POP_TRACE")
      (param $env (ref $dynamic-env))
      (result (ref eq))

;*       (call $js_trace (i32.const 11112))                            */
;*       (call $js_trace                                               */
;* 	 (call $bgl_dframe_length                                      */
;* 	    (struct.get $bgl_dframe $link                              */
;* 	       (struct.get $dynamic-env $top-of-frame (local.get $env))))) */
			 
      (call $BGL_ENV_SET_TOP_OF_FRAME
	 (local.get $env)
	 (ref.as_non_null
	    (struct.get $bgl_dframe $link
	       (struct.get $dynamic-env $top-of-frame (local.get $env)))))
      
      (return (global.get $BUNSPEC)))
   
   (func $BGL_ENV_RESTORE_TRACE
      (export "BGL_ENV_RESTORE_TRACE")
      (param $env (ref $dynamic-env))
      (call $BGL_ENV_SET_TOP_OF_FRAME
	 (local.get $env)
	 (struct.get $bgl_exit_trace_list $head
	    (call $BGL_ENV_EXIT_TRACES (local.get $env))))
      (call $BGL_ENV_EXIT_TRACES_SET (local.get $env)
	 (struct.get $bgl_exit_trace_list $next
	    (call $BGL_ENV_EXIT_TRACES (local.get $env)))))
   
   (func $BGL_ENV_RESTORE_TRACE_WITH_VALUE
      (param $env (ref $dynamic-env))
      (param $value (ref eq))
      (result (ref eq))
      (call $BGL_ENV_RESTORE_TRACE (local.get $env))
      (return (local.get $value)))

   (func $BGL_ENV_STORE_TRACE
      (param $env (ref $dynamic-env))
      (call $BGL_ENV_EXIT_TRACES_SET
	 (local.get $env)
	 (struct.new $bgl_exit_trace_list
	    (call $BGL_ENV_GET_TOP_OF_FRAME (local.get $env))
	    (call $BGL_ENV_EXIT_TRACES (local.get $env)))))

   (func $BGL_STORE_TRACE
      (export "BGL_STORE_TRACE")
      (return_call $BGL_ENV_STORE_TRACE
	 (global.get $current-dynamic-env)))
   
   (func $BGL_RESTORE_TRACE_WITH_VALUE
      (export "BGL_RESTORE_TRACE_WITH_VALUE")
      ;; used to simplify bind-exit code generation
      ;; (see comptime/SawWasm/code.scm)
      (param $value (ref eq))
      (result (ref eq))
      (return_call $BGL_ENV_RESTORE_TRACE_WITH_VALUE
	 (call $BGL_CURRENT_DYNAMIC_ENV)
	 (local.get $value)))
   
   (func $BGL_ENV_LEXICAL_STACK
      (export "BGL_ENV_LEXICAL_STACK")
      (param $env (ref $dynamic-env))
      (result (ref eq))
      (struct.get $dynamic-env $lexical-stack (local.get $env)))
   
   (func $BGL_ENV_LEXICAL_STACK_SET
      (export "BGL_ENV_LEXICAL_STACK_SET")
      (param $env (ref $dynamic-env))
      (param $stk (ref eq))
      (result (ref eq))
      
      (struct.set $dynamic-env $lexical-stack (local.get $env)  (local.get $stk))
      (return (global.get $BUNSPEC)))
   
   (func $BGL_LEXICAL_STACK
      (export "BGL_LEXICAL_STACK")
      (result (ref eq))
      
      (return_call $BGL_ENV_LEXICAL_STACK
	 (global.get $current-dynamic-env)))
   
   (func $BGL_LEXICAL_STACK_SET
      (export "BGL_LEXICAL_STACK_SET")
      (param $stk (ref eq))
      (result (ref eq))
      
      (return_call $BGL_ENV_LEXICAL_STACK_SET
	 (global.get $current-dynamic-env)
	 (local.get $stk)))
   
   (func $BGL_ENV_THREAD_BACKEND
      (export "BGL_ENV_THREAD_BACKEND")
      (param $env (ref $dynamic-env))
      (result (ref eq))
      (struct.get $dynamic-env $thread-backend (local.get $env)))
   
   (func $BGL_ENV_THREAD_BACKEND_SET
      (export "BGL_ENV_THREAD_BACKEND_SET")
      (param $env (ref $dynamic-env))
      (param $be (ref eq))
      (result (ref eq))
      
      (struct.set $dynamic-env $thread-backend (local.get $env)  (local.get $be))
      (return (global.get $BUNSPEC)))
   
   (func $BGL_THREAD_BACKEND
      (export "BGL_THREAD_BACKEND")
      (result (ref eq))
      
      (return_call $BGL_ENV_THREAD_BACKEND
	 (global.get $current-dynamic-env)))
   
   (func $BGL_THREAD_BACKEND_SET
      (export "BGL_THREAD_BACKEND_SET")
      (param $be (ref eq))
      (result (ref eq))
      
      (return_call $BGL_ENV_THREAD_BACKEND_SET
	 (global.get $current-dynamic-env)
	 (local.get $be)))
   
   (func $BGL_ENV_EVSTATE
      (export "BGL_ENV_EVSTATE")
      (param $env (ref $dynamic-env))
      (result (ref eq))
      
      (struct.get $dynamic-env $evstate (local.get $env)))
   
   (func $BGL_ENV_EVSTATE_SET
      (export "BGL_ENV_EVSTATE_SET")
      (param $env (ref $dynamic-env))
      (param $state (ref eq))
      (result (ref eq))
      
      (struct.set $dynamic-env $evstate (local.get $env)  (local.get $state))
      (return (global.get $BUNSPEC)))
   
   (func $BGL_EVSTATE
      (export "BGL_EVSTATE")
      (result (ref eq))
      
      (return_call $BGL_ENV_EVSTATE
	 (global.get $current-dynamic-env)))
   
   (func $BGL_EVSTATE_SET
      (export "BGL_EVSTATE_SET")
      (param $stk (ref eq))
      (result (ref eq))
      
      (return_call $BGL_ENV_EVSTATE_SET
	 (global.get $current-dynamic-env)
	 (local.get $stk)))
   
   (func $BGL_ENV_SET_TRACE_LOCATION
      (export "BGL_ENV_SET_TRACE_LOCATION")
      (param $env (ref $dynamic-env))
      (param $o (ref eq))
      (result (ref eq))
      (struct.set $bgl_dframe $location
	 (struct.get $dynamic-env $top-of-frame (local.get $env))
	 (local.get $o))
      (local.get $o))
   
   (func $BGL_ENV_SET_TRACE_NAME
      (export "BGL_ENV_SET_TRACE_NAME")
      (param $env (ref $dynamic-env))
      (param $o (ref eq))
      (result (ref eq))
      (struct.set $bgl_dframe $name
	 (struct.get $dynamic-env $top-of-frame (local.get $env))
	 (local.get $o))
      (local.get $o))
   
   
   ;; -----------------------------------------------------------------
   ;; Exit and Exception 
   ;; -----------------------------------------------------------------
   
   (func $bgl_make_bexception
      (export "bgl_make_bexception")
      (param $exit (ref $exit))
      (param $val (ref eq))
      (result (ref $bexception))
      (return (struct.new $bexception (local.get $exit) (local.get $val))))
   
   (func $bgl_make_exit
      (export "bgl_make_exit")
      (result (ref $exit))
      (struct.new $exit
	 (i32.const 0)
	 (i32.const 0)
	 (global.get $BNIL)
	 (ref.null none)))

   (func $bgl_internal_error_get
      (result (ref eq))
      (local $tmp (ref eq))
      (if (ref.is_null (global.get $bgl_internal_error))
	  (then
	     (return
		(array.new_data $bstring $default_internal_error
		   (i32.const 0)
		   (i32.const 19))))
	  (else
	   (local.set $tmp
	      (ref.cast (ref eq) (global.get $bgl_internal_error)))
	   (global.set $bgl_internal_error (ref.null none))
	   (return (local.get $tmp)))))

   (func $bgl_internal_error_set
      (param $e (ref eq))
      (global.set $bgl_internal_error (local.get $e)))

   (func $bgl_internal_handler
      (export "bgl_internal_handler")
      (param $exn (ref null exn))
      (param $exit (ref $exit))
      (result (ref eq))
      (local $cell (ref $cell))
      (if (i32.eqz (struct.get $exit $userp (local.get $exit)))
	  (then
	     (return (global.get $BUNSPEC)))
	  (else
	   (if (struct.get $exit $stamp (local.get $exit))
	       (then
		  (throw_ref (local.get $exn)))
	       (else
		(local.set $cell
		   (ref.cast (ref $cell)
		      (struct.get $pair $cdr
			 (ref.cast (ref $pair)
			    (call $BGL_ERROR_HANDLER_GET)))))
		(struct.set $cell $val (local.get $cell)
		   (call $bgl_internal_error_get))
		(return (local.get $cell)))))))
   
   (func $bgl_exception_handler
      (export "bgl_exception_handler")
      (param $exn (ref $bexception))
      (param $exit (ref $exit))
      (result (ref eq))
      (struct.set $exit $stamp (local.get $exit) (i32.const 1))
      (if (i32.eqz (struct.get $exit $userp (local.get $exit)))
	  (then
	     (return (struct.get $bexception $val (local.get $exn))))
	  (else
	   (if (ref.eq (struct.get $bexception $exit (local.get $exn))
		  (local.get $exit))
	       (then
		  (return (struct.get $bexception $val (local.get $exn))))
	       (else
		(throw $BEXCEPTION (local.get $exn)))))))
   
   (func $BGL_ENV_ERROR_HANDLER_GET
      (export "BGL_ENV_ERROR_HANDLER_GET")
      (param $env (ref $dynamic-env))
      (result (ref eq))
      (struct.get $dynamic-env $error-handler (local.get $env)))
   
   (func $BGL_ERROR_HANDLER_GET (export "BGL_ERROR_HANDLER_GET")
      (result (ref eq))
      (struct.get $dynamic-env $error-handler (global.get $current-dynamic-env)))
   
   (func $BGL_ENV_ERROR_HANDLER_SET
      (export "BGL_ENV_ERROR_HANDLER_SET") 
      (param $env (ref $dynamic-env))
      (param $hdl (ref eq)) 
      (struct.set $dynamic-env $error-handler (local.get $env) (local.get $hdl)))
   
   (func $BGL_ERROR_HANDLER_SET
      (export "BGL_ERROR_HANDLER_SET") 
      (param $hdl (ref eq))
      (return_call $BGL_ENV_ERROR_HANDLER_SET
	 (global.get $current-dynamic-env) (local.get $hdl)))
   
   (func $BGL_ENV_ERROR_HANDLER_PUSH
      (export "BGL_ENV_ERROR_HANDLER_PUSH")
      (param $env (ref $dynamic-env))
      (param $h (ref eq))
      (param $hdl (ref eq))
      (return_call $BGL_ENV_ERROR_HANDLER_SET
	 (local.get $env)
	 (struct.new $pair (local.get $h) (local.get $hdl))))
   
   (func $BGL_ERROR_HANDLER_PUSH (export "BGL_ERROR_HANDLER_PUSH") 
      (param $h (ref eq))
      (param $hdl (ref eq))
      (return_call $BGL_ENV_ERROR_HANDLER_PUSH
	 (global.get $current-dynamic-env) (local.get $h) (local.get $hdl)))
   
   (func $BGL_UNCAUGHT_EXCEPTION_HANDLER_GET
      (export "BGL_UNCAUGHT_EXCEPTION_HANDLER_GET") (result (ref eq))
      (struct.get $dynamic-env $uncaught-exception-handler (global.get $current-dynamic-env)))
   
   (func $BGL_UNCAUGHT_EXCEPTION_HANDLER_SET
      (export "BGL_UNCAUGHT_EXCEPTION_HANDLER_SET") (param $hdl (ref eq))
      (struct.set $dynamic-env $uncaught-exception-handler (global.get $current-dynamic-env) (local.get $hdl)))
   
   (func $BGL_ENV_ERROR_NOTIFIERS_GET
      (export "BGL_ENV_ERROR_NOTIFIERS_GET")
      (param $env (ref $dynamic-env))
      (result (ref eq))
      (struct.get $dynamic-env $error-notifiers (local.get $env)))
   
   (func $BGL_ERROR_NOTIFIERS_GET (export "BGL_ERROR_NOTIFIERS_GET")
      (result (ref eq))
      (struct.get $dynamic-env $error-notifiers (global.get $current-dynamic-env)))
   
   (func $BGL_ENV_ERROR_NOTIFIERS_SET
      (export "BGL_ENV_ERROR_NOTIFIERS_SET") 
      (param $env (ref $dynamic-env))
      (param $hdl (ref eq))
      (result (ref eq))
      (struct.set $dynamic-env $error-notifiers (local.get $env) (local.get $hdl))
      (return (local.get $hdl)))
   
   (func $BGL_ERROR_NOTIFIERS_SET
      (export "BGL_ERROR_NOTIFIERS_SET") 
      (param $hdl (ref eq))
      (result (ref eq))
      (return_call $BGL_ENV_ERROR_NOTIFIERS_SET
	 (global.get $current-dynamic-env) (local.get $hdl)))
   
   (func $BGL_ENV_INTERRUPT_NOTIFIER_GET
      (export "BGL_ENV_INTERRUPT_NOTIFIER_GET")
      (param $env (ref $dynamic-env))
      (result (ref eq))
      (struct.get $dynamic-env $interrupt-notifier (local.get $env)))
   
   (func $BGL_INTERRUPT_NOTIFIER_GET (export "BGL_INTERRUPT_NOTIFIER_GET")
      (result (ref eq))
      (struct.get $dynamic-env $interrupt-notifier (global.get $current-dynamic-env)))
   
   (func $BGL_ENV_INTERRUPT_NOTIFIER_SET
      (export "BGL_ENV_INTERRUPT_NOTIFIER_SET") 
      (param $env (ref $dynamic-env))
      (param $hdl (ref eq)) 
      (struct.set $dynamic-env $interrupt-notifier (local.get $env) (local.get $hdl)))
   
   (func $BGL_INTERRUPT_NOTIFIER_SET
      (export "BGL_INTERRUPT_NOTIFIER_SET") 
      (param $hdl (ref eq))
      (return_call $BGL_ENV_INTERRUPT_NOTIFIER_SET
	 (global.get $current-dynamic-env) (local.get $hdl)))
   
   ;; -----------------------------------------------------------------
   ;; Exit
   ;; -----------------------------------------------------------------
   
  (func $PUSH_ENV_EXIT (export "PUSH_ENV_EXIT") 
    (param $env (ref $dynamic-env)) 
    (param $v (ref $exit)) 
    (param $protect i64) 
    (result (ref eq))
    (struct.set $exit $userp (local.get $v) (i32.wrap_i64 (local.get $protect)))
    (struct.set $exit $prev (local.get $v) (struct.get $dynamic-env $exitd_top (local.get $env)))
    (struct.set $dynamic-env $exitd_top (local.get $env) (local.get $v))
    (global.get $BUNSPEC))

  (func $PUSH_EXIT (export "PUSH_EXIT") 
    (param $v (ref $exit)) 
    (param $protect i64) 
    (result (ref eq))
    (call $PUSH_ENV_EXIT 
        (global.get $current-dynamic-env)
        (local.get $v) 
        (local.get $protect)))

  (func $POP_ENV_EXIT (export "POP_ENV_EXIT")
    (param $env (ref $dynamic-env)) 
    (result (ref eq))
    (struct.set $dynamic-env $exitd_top
        (local.get $env)
	(ref.cast (ref $exit)
	   (struct.get $exit $prev
	      (struct.get $dynamic-env $exitd_top (local.get $env)))))
    (global.get $BUNSPEC))

  (func $POP_EXIT (export "POP_EXIT") (result (ref eq))
    (call $POP_ENV_EXIT (global.get $current-dynamic-env)))

  (func $EXITD_STAMP (export "EXITD_STAMP") (param $o (ref eq))
     (result (ref eq))
     (call $BINT
	(i64.extend_i32_s
	   (struct.get $exit $stamp (ref.cast (ref $exit) (local.get $o))))))

  (func $EXITD_CALLCCP (export "EXITD_CALLCCP") (param $o (ref eq)) (result i32)
    (i32.const 0))

  (func $EXITD_TO_EXIT (export "EXITD_TO_EXIT") (param $o (ref eq)) (result (ref $exit))
    (ref.cast (ref $exit) (local.get $o)))

  (func $BGL_EXITD_PROTECT (export "BGL_EXITD_PROTECT") 
    (param (ref $exit)) 
    (result (ref eq))
    (struct.get $exit $protect (local.get 0)))

  (func $BGL_EXITD_PROTECT_SET (export "BGL_EXITD_PROTECT_SET") 
    (param $e (ref $exit)) 
    (param $p (ref eq)) 
    (struct.set $exit $protect (local.get $e) (local.get $p)))

  (func $BGL_EXITD_PUSH_PROTECT (export "BGL_EXITD_PUSH_PROTECT") 
    (param $e (ref $exit)) 
    (param $p (ref eq))
    (call $BGL_EXITD_PROTECT_SET (local.get $e)
      (struct.new $pair 
        (local.get $p)
        (struct.get $exit $protect (local.get $e)))))

  (func $BGL_ENV_EXITD_TOP_AS_OBJ (export "BGL_ENV_EXITD_TOP_AS_OBJ") 
      (param $env (ref $dynamic-env))
      (result (ref eq))
      (struct.get $dynamic-env $exitd_top (local.get $env)))
   
   (func $BGL_EXITD_TOP_AS_OBJ (export "BGL_EXITD_TOP_AS_OBJ")
      (result (ref eq))
      (call $BGL_ENV_EXITD_TOP_AS_OBJ (global.get $current-dynamic-env)))
   
   (func $BGL_EXITD_BOTTOMP (export "BGL_EXITD_BOTTOMP") (param $o (ref eq)) (result i32)
      (ref.is_null
	 (struct.get $exit $prev (ref.cast (ref $exit) (local.get $o)))))
   
   (func $BGL_ENV_EXITD_VAL_SET (export "BGL_ENV_EXITD_VAL_SET") 
      (param $env (ref $dynamic-env)) 
      (param $v (ref eq)) 
      (result (ref eq))
      (struct.set $dynamic-env $exitd_val (local.get $env) (local.get $v))
      (global.get $BUNSPEC))
   
   (func $BGL_EXITD_VAL_SET (export "BGL_EXITD_VAL_SET") (param $v (ref eq)) (result (ref eq))
      (call $BGL_ENV_EXITD_VAL_SET (global.get $current-dynamic-env) (local.get $v)))
   
   )
   

