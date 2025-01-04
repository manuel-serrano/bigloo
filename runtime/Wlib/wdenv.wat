;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wdenv.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jan  4 06:08:48 2025                          */
;*    Last change :  Sat Jan  4 07:02:52 2025 (serrano)                */
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
	 (field $userp (mut i64))
	 (field $stamp (mut i64))
	 (field $protect (mut (ref eq)))
	 (field $prev (mut (ref null $exit)))))
   
   (rec
      (type $bgl_dframe
	 (struct
	    (field $name (mut (ref eq)))
	    (field $location (mut (ref eq)))
	    (field $link (mut (ref null $bgl_dframe)))))
      
      (type $bgl_dframe_list
	 (struct
	    (field $head (ref $bgl_dframe))
	    (field $next (ref null $bgl_dframe_list))))
      
      (type $dynamic-env
	 (struct 
	    (field $exitd_top (mut (ref $exit)))
	    (field $exitd_val (mut (ref eq)))
	    (field $uncaught-exception-handler (mut (ref eq)))
	    (field $error-handler (mut (ref eq)))
	    
	    (field $current-output-port (mut (ref null $output-port)))
	    (field $current-error-port (mut (ref null $output-port)))
	    (field $current-input-port (mut (ref null $input-port)))
	    
	    (field $evstate (mut (ref eq)))
	    (field $module (mut (ref eq)))
	    (field $abase (mut (ref eq)))
	    
	    (field $lexical-stack (mut (ref eq)))
	    (field $top (mut (ref null $bgl_dframe)))
	    (field $top-of-frame (mut (ref null $bgl_dframe)))
	    (field $exit-traces (mut (ref null $bgl_dframe_list))))))
   
   (rec
      (type $bexception
	 (struct
	    (field $exit (ref $exit))
	    (field $val (ref eq)))))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $exit-default-value
      (export "BGL_EXIT_DEFAULT_VALUE") (ref $exit)
      (struct.new $exit
	 ;; userp
	 (i64.const 0)
	 ;; stamp
	 (i64.const 0)
	 ;; protect
	 (global.get $BUNSPEC)
	 ;; prev
	 (ref.null none)))

   (global $dynamic-env-default-value
      (export "BGL_DYNAMIC_ENV_DEFAULT_VALUE") (ref $dynamic-env)
      (struct.new $dynamic-env
	 ;; exitd_top
	 (global.get $exit-default-value)
	 ;; exitd_val
	 (global.get $BUNSPEC)
	 ;; uncaught-exception-handler
	 (global.get $BUNSPEC)
	 ;; error-handler
	 (global.get $BUNSPEC)
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
	 ;; top
	 (struct.new $bgl_dframe
	    (global.get $BUNSPEC)
	    (global.get $BUNSPEC)
	    (ref.null none))
	 ;; top-of-frame
	 (ref.null none)
	 ;; exit-traces
	 (ref.null none)
	 ))
   
   ;; --------------------------------------------------------
   ;; Dynamic env functions
   ;; --------------------------------------------------------
   
   (global $current-dynamic-env (ref $dynamic-env) 
      (struct.new $dynamic-env
	 ;; exitd_top
	 (struct.new $exit
	    (i64.const 0)
	    (i64.const 0)
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
	 ;; top
	 (struct.new $bgl_dframe
	    (global.get $BUNSPEC)
	    (global.get $BUNSPEC)
	    (ref.null none))
	 ;; top-of-frame
	 (ref.null none)
	 ;; exit-traces
	 (ref.null none)
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
      (local $top (ref null $bgl_dframe))
      (local.set $top (struct.get $dynamic-env $top (local.get $env)))
      
      (struct.set $bgl_dframe $name (local.get $top) (global.get $BUNSPEC))
      (struct.set $bgl_dframe $location (local.get $top) (global.get $BUNSPEC))
      (struct.set $bgl_dframe $link (local.get $top) (ref.null none))
      (call $BGL_ENV_SET_TOP_OF_FRAME (local.get $env) (local.get $top)))
   
   (func $BGL_ENV_GET_TOP_OF_FRAME
      (param $env (ref $dynamic-env))
      (result (ref null $bgl_dframe))
      (struct.get $dynamic-env $top-of-frame (local.get $env)))
   
   (func $BGL_ENV_SET_TOP_OF_FRAME
      (param $env (ref $dynamic-env))
      (param $tof (ref null $bgl_dframe))
      (struct.set $dynamic-env $top-of-frame (local.get $env) (local.get $tof)))
   
   (func $BGL_ENV_EXIT_TRACES
      (param $env (ref $dynamic-env))
      (result (ref $bgl_dframe_list))
      (ref.as_non_null (struct.get $dynamic-env $exit-traces (local.get $env))))
   
   (func $BGL_ENV_EXIT_TRACES_SET
      (param $env (ref $dynamic-env))
      (param $traces (ref null $bgl_dframe_list))
      (struct.set $dynamic-env $exit-traces (local.get $env)
	 (local.get $traces)))
   
   (func $BGL_ENV_PUSH_TRACE
      (export "BGL_ENV_PUSH_TRACE")
      (param $env (ref $dynamic-env))
      (param $name (ref eq))
      (param $loc (ref eq))
      (result (ref eq))
      
      (local $tmp (ref $bgl_dframe))
      (local.set $tmp
	 (struct.new $bgl_dframe (local.get $name) (local.get $loc)
	    (struct.get $dynamic-env $top (local.get $env))))
      (struct.set $dynamic-env $top (local.get $env) (local.get $tmp))
      (return (global.get $BUNSPEC)))
   
   (func $BGL_ENV_POP_TRACE
      (export "BGL_ENV_POP_TRACE")
      (param $env (ref $dynamic-env))
      (result (ref eq))
      
      (struct.set $dynamic-env $top (local.get $env)
	 (struct.get $bgl_dframe $link
	    (struct.get $dynamic-env $top (local.get $env))))
      (return (global.get $BUNSPEC)))
   
   (func $BGL_ENV_STORE_TRACE
      (export "BGL_ENV_STORE_TRACE")
      (param $env (ref $dynamic-env))
      (call $BGL_ENV_EXIT_TRACES_SET (local.get $env)
	 (struct.new $bgl_dframe_list
	    (ref.as_non_null (call $BGL_ENV_GET_TOP_OF_FRAME (local.get $env)))
	    (call $BGL_ENV_EXIT_TRACES (local.get $env)))))
   
   (func $BGL_ENV_RESTORE_TRACE
      (export "BGL_ENV_RESTORE_TRACE")
      (param $env (ref $dynamic-env))
      (call $BGL_ENV_SET_TOP_OF_FRAME
	 (local.get $env)
	 (struct.get $bgl_dframe_list $head
	    (call $BGL_ENV_EXIT_TRACES (local.get $env))))
      (call $BGL_ENV_EXIT_TRACES_SET (local.get $env)
	 (struct.get $bgl_dframe_list $next
	    (call $BGL_ENV_EXIT_TRACES (local.get $env)))))
   
   (func $BGL_ENV_RESTORE_TRACE_WITH_VALUE
      (param $env (ref $dynamic-env))
      (param $value (ref eq))
      (result (ref eq))
      (call $BGL_ENV_RESTORE_TRACE (local.get $env))
      (return (local.get $value)))
   
   (func $BGL_RESTORE_TRACE_WITH_VALUE
      (export "BGL_RESTORE_TRACE_WITH_VALUE")
      ;; used to simplify bind-exit code generation
      ;; (see comptime/SawWasm/code.scm)
      (param $value (ref eq))
      (result (ref eq))
;*      (return_call $BGL_ENV_RESTORE_TRACE_WITH_VALUE                 */
;* 	(call $BGL_CURRENT_DYNAMIC_ENV)                                */
      (local.get $value))
   
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
	 (struct.get $dynamic-env $top (local.get $env))
	 (local.get $o))
      (local.get $o))
   
   (func $BGL_ENV_SET_TRACE_NAME
      (export "BGL_ENV_SET_TRACE_NAME")
      (param $env (ref $dynamic-env))
      (param $o (ref eq))
      (result (ref eq))
      (struct.set $bgl_dframe $name
	 (struct.get $dynamic-env $top (local.get $env))
	 (local.get $o))
      (local.get $o))
   
   
   ;; -----------------------------------------------------------------
   ;; Exit and Exception 
   ;; -----------------------------------------------------------------
   
   (func $bgl_make_bexception (export "bgl_make_bexception")
      (param $exit (ref $exit))
      (param $val (ref eq))
      (result (ref $bexception))
      (return (struct.new $bexception (local.get $exit) (local.get $val))))
   
   (func $bgl_make_exit (export "bgl_make_exit")
      (result (ref $exit))
      (struct.new $exit
	 (i64.const 0)
	 (i64.const 0)
	 (global.get $BNIL)
	 (ref.null none)))
   
   (func $bgl_exception_handler (export "bgl_exception_handler")
      (param $exn (ref $bexception))
      (param $exit (ref $exit))
      (result (ref eq))
      (call $js_trace (i32.const 66666))
      (if (i64.eqz (struct.get $exit $userp (local.get $exit)))
	  (then
	     (call $js_trace (i32.const 666662))
	     (return (struct.get $bexception $val (local.get $exn))))
	  (else
	   (if (ref.eq (struct.get $bexception $exit (local.get $exn))
		  (local.get $exit))
	       (then
		  (call $js_trace (i32.const 666663))
		  (return (struct.get $bexception $val (local.get $exn))))
	       (else
		(call $js_trace (i32.const 666664))
		(throw $BEXCEPTION (local.get $exn)))))))
   
   )
   

