;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Llib/error.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 08:19:23 1995                          */
;*    Last change :  Sat Jan  4 08:23:01 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The error machinery                                              */
;*    -------------------------------------------------------------    */
;*    The error functionnality can be used even if this module is      */
;*    not initialized. This feature allows a correct printing for      */
;*    the errors occuring during the initialization time.              */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __error
   
   (option  (bigloo-compiler-debug-set! 0)
            (set! *compiler-debug* 0)
            (set! *optim-O-macro?* #f)
            (set! *unsafe-type*    #t)
            (set! *unsafe-arity*   #t)
            (set! *unsafe-range*   #t))

   (extern  (export the_failure "the_failure")
	    (export error/errno "bgl_system_failure")
	    (export find-runtime-type "bgl_find_runtime_type")
	    (export typeof "bgl_typeof")
	    (export c-debugging-show-type "bgl_show_type")
	    (export stack-overflow-error "bgl_stack_overflow_error")
	    
	    (macro $get-trace-stack::pair-nil (::int) "bgl_get_trace_stack")
	    (macro $init-trace-stacksp::void (::dynamic-env) "bgl_init_trace")
	    (macro $get-trace-stacksp::obj () "BGL_GET_TRACE_STACKSP")
	    (macro $set-trace-stacksp::obj (::obj) "BGL_SET_TRACE_STACKSP")
	    (macro $push-trace::obj (::obj ::obj) "BGL_PUSH_TRACE")
	    (macro $env-push-trace::obj (::dynamic-env ::obj ::obj) "BGL_ENV_PUSH_TRACE")
	    (macro $env-set-trace-name::obj (::dynamic-env ::obj) "BGL_ENV_SET_TRACE_NAME")
	    (macro $env-set-trace-location::obj (::dynamic-env ::obj) "BGL_ENV_SET_TRACE_LOCATION")
	    (macro $env-pop-trace::obj (::dynamic-env) "BGL_ENV_POP_TRACE")
	    (macro $pop-trace::obj () "BGL_POP_TRACE")
	    (macro $get-error-handler::obj () "BGL_ERROR_HANDLER_GET")
	    (macro $env-get-error-handler::obj (::dynamic-env) "BGL_ENV_ERROR_HANDLER_GET")
	    (macro $set-error-handler!::void (::obj) "BGL_ERROR_HANDLER_SET")
	    (macro $env-set-error-handler!::void (::dynamic-env ::obj) "BGL_ENV_ERROR_HANDLER_SET")
	    (macro $push-error-handler!::void (::obj ::obj) "BGL_ERROR_HANDLER_PUSH")
	    (macro $get-uncaught-exception-handler::obj () "BGL_UNCAUGHT_EXCEPTION_HANDLER_GET")
	    (macro $set-uncaught-exception-handler!::void (::obj) "BGL_UNCAUGHT_EXCEPTION_HANDLER_SET")
	    (macro $get-error-notifiers::obj () "BGL_ERROR_NOTIFIERS_GET")
	    (macro $set-error-notifiers!::obj (::obj) "BGL_ERROR_NOTIFIERS_SET")
	    (macro $get-interrupt-notifier::obj () "BGL_INTERRUPT_NOTIFIER_GET")
	    (macro $set-interrupt-notifier!::void (::obj) "BGL_INTERRUPT_NOTIFIER_SET")
	    (macro sighup::int "SIGHUP")
	    (macro sigquit::int "SIGQUIT")
	    (macro sigill::int "SIGILL")
	    (macro sigabrt::int "SIGABRT")
	    (macro sigfpe::int "SIGFPE")
	    (macro sigkill::int "SIGKILL")
	    (macro sigbus::int "SIGBUS")
	    (macro sigsegv::int "SIGSEGV")
	    (macro sigpipe::int "SIGPIPE")
	    (macro sigalrm::int "SIGALRM")
	    (macro sigterm::int "SIGTERM")
	    (macro sigint::int "SIGINT")
	    (macro sigusr1::int "SIGUSR1")
	    (macro sigusr2::int "SIGUSR2")
	    (macro sigwinch::int "SIGWINCH")
	    (macro sigtrap::int "SIGTRAP")
	    
	    (macro $foreign-typeof::string (::obj) "FOREIGN_TYPE_NAME")
	    
	    (macro $errno-type-error::int "BGL_TYPE_ERROR")
	    (macro $errno-typename-error::int "BGL_TYPENAME_ERROR")
	    (macro $errno-index-out-of-bound-error::int "BGL_INDEX_OUT_OF_BOUND_ERROR")
	    (macro $errno-io-error::int "BGL_IO_ERROR")
	    (macro $errno-io-port-error::int "BGL_IO_PORT_ERROR")
	    (macro $errno-io-parse-error::int "BGL_IO_PARSE_ERROR")
	    (macro $errno-io-read-error::int "BGL_IO_READ_ERROR")
	    (macro $errno-io-write-error::int "BGL_IO_WRITE_ERROR")
	    
	    (macro $errno-io-file-not-found-error::int "BGL_IO_FILE_NOT_FOUND_ERROR")
	    (macro $errno-io-unknown-host-error::int "BGL_IO_UNKNOWN_HOST_ERROR")
	    (macro $errno-io-parse-error::int "BGL_IO_PARSE_ERROR")
	    (macro $errno-io-malformed-url-error::int "BGL_IO_MALFORMED_URL_ERROR")
	    (macro $errno-io-sigpipe-error::int "BGL_IO_SIGPIPE_ERROR")
	    (macro $errno-io-timeout-error::int "BGL_IO_TIMEOUT_ERROR")
	    (macro $errno-io-connection-error::int "BGL_IO_CONNECTION_ERROR")
	    (macro $errno-process-exception::int "BGL_PROCESS_EXCEPTION"))

   (wasm    (sighup "(i32.const 1)")
            (sigint "(i32.const 2)")
	    (sigquit "(i32.const 3)")
	    (sigill "(i32.const 4)")
	    (sigtrap "(i32.const 5)")
	    (sigabrt "(i32.const 6)")
	    (sigbus "(i32.const 7)")
	    (sigfpe "(i32.const 8)")
	    (sigkill "(i32.const 9)")
	    (sigusr1 "(i32.const 10)")
	    (sigsegv "(i32.const 11)")
	    (sigusr2 "(i32.const 12)")
	    (sigpipe "(i32.const 13)")
	    (sigalrm "(i32.const 14)")
	    (sigterm "(i32.const 15)")
	    (sigwinch "(i32.const 28)"))

   (java    (export the_failure "the_failure")
	    (export error/errno "bgl_system_failure")
	    (class foreign
	       (method static $get-trace-stack::pair-nil (::int)
		       "get_trace_stack")
	       (method static $init-trace-stacksp::void (::dynamic-env)
		       "init_trace_stacksp")
	       (method static $get-trace-stacksp::obj ()
		       "BGL_GET_TRACE_STACKSP")
	       (method static $set-trace-stacksp::obj (::obj)
		       "BGL_SET_TRACE_STACKSP")
	       (method static $push-trace::obj (::obj ::obj)
		       "PUSH_TRACE")
	       (method static $set-trace-name::obj (::obj)
		       "SET_TRACE_NAME")
	       (method static $pop-trace::obj ()
		       "POP_TRACE")
	       (method static $env-push-trace::obj (::dynamic-env ::obj ::obj)
		       "BGL_ENV_PUSH_TRACE")
	       (method static $env-set-trace-name::obj (::dynamic-env ::obj)
		       "BGL_ENV_SET_TRACE_NAME")
	       (method static $env-set-trace-location::obj (::dynamic-env ::obj)
		       "BGL_ENV_SET_TRACE_LOCATION")
	       (method static $env-pop-trace::obj (::dynamic-env)
		       "BGL_ENV_POP_TRACE")
 	       (method static $foreign-typeof::string (::obj)
		       "FOREIGN_TYPE_NAME")
	       
	       (method static $get-error-handler::obj ()
		       "BGL_ERROR_HANDLER_GET")
	       (method static $env-get-error-handler::obj (::dynamic-env)
		       "BGL_ENV_ERROR_HANDLER_GET")
	       (method static $set-error-handler!::void (::obj)
		       "BGL_ERROR_HANDLER_SET")
	       (method static $env-set-error-handler!::void (::dynamic-env ::obj)
		       "BGL_ENV_ERROR_HANDLER_SET")
	       (method static $push-error-handler!::void (::obj ::obj)
		       "BGL_ERROR_HANDLER_PUSH")
	       (method static $get-uncaught-exception-handler::obj ()
		       "BGL_UNCAUGHT_EXCEPTION_HANDLER_GET")
	       (method static $set-uncaught-exception-handler!::void (::obj)
		       "BGL_UNCAUGHT_EXCEPTION_HANDLER_SET")
	       (method static $get-error-notifiers::obj ()
		       "BGL_ERROR_NOTIFIERS_GET")
	       (method static $set-error-notifiers!::obj (::obj)
		       "BGL_ERROR_NOTIFIERS_SET")
	       (method static $get-interrupt-notifier::obj ()
		       "BGL_INTERRUPT_NOTIFIER_GET")
	       (method static $set-interrupt-notifier!::void (::obj)
		       "BGL_INTERRUPT_NOTIFIER_SET")
	       
	       (field static sighup::int "SIGHUP")
	       (field static sigint::int "SIGINT")
	       (field static sigquit::int "SIGQUIT")
	       (field static sigill::int "SIGILL")
	       (field static sigabrt::int "SIGABRT")
	       (field static sigfpe::int "SIGFPE")
	       (field static sigsegv::int "SIGSEGV")
	       (field static sigalrm::int "SIGALRM")
	       (field static sigbus::int "SIGBUS")
	       (field static sigpipe::int "SIGPIPE")
	       (field static sigterm::int "SIGTERM")
	       (field static sigkill::int "SIGKILL")
	       (field static sigusr1::int "SIGUSR1")
	       (field static sigusr2::int "SIGUSR2")
	       (field static sigwinch::int "SIGWINCH")
	       (field static sigtrap::int "SIGTRAP")
	       
	       (field static $errno-type-error::int
		      "BGL_TYPE_ERROR")
	       (field static $errno-typename-error::int
		      "BGL_TYPENAME_ERROR")
	       (field static $errno-index-out-of-bound-error::int
		      "BGL_INDEX_OUT_OF_BOUND_ERROR")
	       
	       (field static $errno-io-error::int
		      "BGL_IO_ERROR")
	       (field static $errno-io-port-error::int
		      "BGL_IO_PORT_ERROR")
	       (field static $errno-io-parse-error::int
		      "BGL_IO_PARSE_ERROR")
	       (field static $errno-io-read-error::int
		      "BGL_IO_READ_ERROR")
	       (field static $errno-io-write-error::int
		      "BGL_IO_WRITE_ERROR")
	       
	       (field static $errno-io-file-not-found-error::int
		      "BGL_IO_FILE_NOT_FOUND_ERROR")
	       (field static $errno-io-unknown-host-error::int
		      "BGL_IO_UNKNOWN_HOST_ERROR")
	       (field static $errno-io-parse-error::int
		      "BGL_IO_PARSE_ERROR")
	       
	       (field static $errno-io-malformed-url-error::int
		      "BGL_IO_MALFORMED_URL_ERROR")
	       (field static $errno-io-sigpipe-error::int
		      "BGL_IO_SIGPIPE_ERROR")
	       (field static $errno-io-timeout-error::int
		      "BGL_IO_TIMEOUT_ERROR")
	       (field static $errno-io-connection-error::int
		      "BGL_IO_CONNECTION_ERROR")
	       (field static $errno-process-exception::int
		      "BGL_PROCESS_EXCEPTION")))
   
   (import  __r4_input_6_10_2
	    __object)

   (use     __type
	    __bigloo
	    __param
	    __os
	    __foreign
	    __binary
 	    __structure
	    __dsssl
	    __tvector
	    __socket
	    __process
	    __custom
	    __unicode
	    __ucs2
	    __thread
	    __bexit
	    __bignum
	    __date
	    __srfi4
	    __regexp
	    __bit
	    
	    __pp_circle
	    
	    __reader
	    
	    __rgc
	    (mmap? __mmap)
	    
	    __r4_vectors_6_8
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_control_features_6_9
	    __r4_output_6_10_3
	    __r5_control_features_6_4
	    
	    __evenv)

   (export  (the_failure::magic ::obj ::obj ::obj)
	    (exit::magic . obj)
	    
	    (with-exception-handler ::procedure ::procedure)
	    (current-exception-handler::procedure)
	    (raise ::obj)
	    
	    (error ::obj ::obj ::obj)
	    (error/errno::magic ::int ::obj ::obj ::obj)
	    (error/location::obj ::obj ::obj ::obj ::obj ::obj)
	    (error/source-location::obj ::obj ::obj ::obj ::obj)
	    (error/source::obj ::obj ::obj ::obj ::obj)
	    (error/c-location::obj ::obj ::obj ::obj ::string ::long)
	    (bigloo-type-error::obj ::obj ::obj ::obj)
	    (bigloo-type-error/location::obj ::obj ::obj ::obj ::obj ::obj)

	    (type-error fname loc proc type obj)
	    (index-out-of-bounds-error fname loc proc obj i::int len::int)
	    (stack-overflow-error)

	    (module-init-error ::string ::string)

	    (error-notify ::obj)
	    (error-notify/location ::obj ::bstring ::int)
	    (warning-notify ::obj)
	    (warning-notify/location ::obj ::bstring ::int)
	    
	    (get-trace-stack #!optional depth)
	    (display-trace-stack ::obj ::output-port #!optional (offset 1))
	    (display-trace-stack-source ::obj ::output-port)
	    (dump-trace-stack port depth)
	    
	    (find-runtime-type::bstring  ::obj)
	    (typeof::bstring  ::obj)
	    (c-debugging-show-type::string ::obj)
	    (bigloo-type-error-msg::bstring  ::bstring ::bstring ::bstring)
	    
	    (warning . args)
	    (warning/location::obj ::obj ::obj . obj)
	    (warning/loc::obj ::obj . obj)
	    (warning/c-location::obj ::string ::long . obj)

	    (&try ::procedure ::procedure)
	    
	    (notify-interrupt ::int)
	    
	    (inline push-error-handler! ::procedure ::obj)
	    (inline get-error-handler::obj)
	    (inline env-get-error-handler ::dynamic-env)
	    (inline set-error-handler! ::obj)
	    (inline env-set-error-handler! ::dynamic-env ::obj))
	    
   (pragma  (typeof no-cfa-top args-safe)
	    ($push-error-handler! (args-noescape))
            (push-error-handler! (args-noescape))
	    ($set-error-handler! (args-noescape))
            (set-error-handler! (args-noescape))
	    ($env-set-error-handler! (args-noescape))
            (env-set-error-handler! (args-noescape))))

;*---------------------------------------------------------------------*/
;*    get-trace-stack ...                                              */
;*---------------------------------------------------------------------*/
(define (get-trace-stack #!optional depth)
   (let ((d (cond
	       ((fixnum? depth) depth)
	       ((getenv "BIGLOOSTACKDEPTH") => string->integer)
	       ((fixnum? (bigloo-trace-stack-depth)) (bigloo-trace-stack-depth))
	       (else (*fx (bigloo-debug) 10)))))
      ($get-trace-stack d)))

;*---------------------------------------------------------------------*/
;*    the_failure ...                                                  */
;*---------------------------------------------------------------------*/
(define (the_failure proc msg obj)
   (if (isa? proc &exception)
       (raise proc)
       (error proc msg obj)))

;*---------------------------------------------------------------------*/
;*    error/errno ...                                                  */
;*---------------------------------------------------------------------*/
(define (error/errno sysno proc msg obj)
   (cond
      ((=fx sysno $errno-io-error)
       (raise
	  (instantiate::&io-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-port-error)
       (raise
	  (instantiate::&io-port-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-read-error)
       (raise
	  (instantiate::&io-read-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-write-error)
       (raise
	  (instantiate::&io-write-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-unknown-host-error)
       (raise
	  (instantiate::&io-unknown-host-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-file-not-found-error)
       (raise
	  (instantiate::&io-file-not-found-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-parse-error)
       (raise
	  (instantiate::&io-parse-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-malformed-url-error)
       (raise
	  (instantiate::&io-malformed-url-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-sigpipe-error)
       (raise
	  (instantiate::&io-sigpipe-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-timeout-error)
       (raise
	  (instantiate::&io-timeout-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-io-connection-error)
       (raise
	  (instantiate::&io-connection-error (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-process-exception)
       (raise
	  (instantiate::&process-exception (proc proc) (msg msg) (obj obj))))
      ((=fx sysno $errno-type-error)
       (raise (type-error #f #f proc msg obj)))
      ((=fx sysno $errno-typename-error)
       (raise (typename-error #f #f proc msg obj)))
      ((=fx sysno $errno-index-out-of-bound-error)
       (raise (instantiate::&index-out-of-bounds-error (fname #f) (location #f) (proc proc)
                 (msg msg) (obj obj) (index -1))))
      (else
       (error proc msg obj))))

;*---------------------------------------------------------------------*/
;*    stack-overflow-error ...                                         */
;*---------------------------------------------------------------------*/
(define (stack-overflow-error)
   (let ((stk (get-trace-stack)))
      (match-case stk
	 (((?proc (at ?fname ?loc)) . ?-)
	  (raise
	     (instantiate::&stack-overflow-error
		(fname fname)
		(location loc)
		(stack stk)
		(proc proc)
		(msg "stack overflow")
		(obj (current-dynamic-env)))))
	 ((?proc . ?-)
	  (raise
	     (instantiate::&stack-overflow-error
		(stack stk)
		(proc proc)
		(msg "stack overflow")
		(obj (current-dynamic-env)))))
	 (else
	  (raise
	     (instantiate::&stack-overflow-error
		(stack stk)
		(proc #f)
		(msg "stack overflow")
		(obj (current-dynamic-env))))))))

;*---------------------------------------------------------------------*/
;*    exit ...                                                         */
;*---------------------------------------------------------------------*/
(define (exit . n)
   (let ((val (cond
		 ((null? n) 0)
		 ((not (fixnum? (car n))) 0)
		 (else (car n)))))
      (%exit val)
      val))

;*---------------------------------------------------------------------*/
;*    with-exception-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (with-exception-handler handler thunk)
   (if (correct-arity? handler 1)
       (let ((old-handlers ($get-error-handler)))
	  ($set-error-handler!
	     ($acons (lambda (c)
			($set-error-handler! old-handlers)
			(handler c))
		#unspecified))
	  (unwind-protect
	     (if (correct-arity? thunk 0)
		 (thunk)
		 (error "with-exception-handler" "Incorrect thunk arity" thunk))
	     ($set-error-handler! old-handlers)))
       (error "with-exception-handler" "Incorrect handler arity" handler)))

;*---------------------------------------------------------------------*/
;*    current-exception-handler ...                                    */
;*---------------------------------------------------------------------*/
(define (current-exception-handler)
   (lambda (val)
      (raise val)))
   
;*    (define (wrong-handler handler)                                  */
;*       (tprint "wrong handler..." (current-thread))                  */
;*       default-exception-handler)                                    */
;* {*       (error "current-exception-handler"                            *} */
;* {* 	 "wrong error handler" (typeof handler)))                      *} */
;*                                                                     */
;*    (let ((handler ($get-error-handler)))                            */
;*       (cond                                                         */
;* 	 ((pair? handler)                                              */
;* 	  (cond                                                        */
;* 	     ((dynamic-env? (cdr handler))                             */
;* 	                                                               */
;* 	      ((eq? (cdr handler) #unspecified)                        */
;* 	      (car handler))                                           */
;* 	     ((eq? (cdr handler) #f)                                   */
;* 	      default-exception-handler)                               */
;* 	     (else                                                     */
;* 	      (wrong-handler handler))))                               */
;* 	 (else                                                         */
;* 	  (wrong-handler handler)))))                                  */

;*---------------------------------------------------------------------*/
;*    raise ...                                                        */
;*---------------------------------------------------------------------*/
(define (raise val)
   
   (define (raise/denv hdl)
      ;; since 19 jun 2021, error handlers are pushed
      ;; along a cell where to store the exception value
      ;; this removes one closure allocation of 
      ;; with-handler forms (see comptime/Expand/exit.scm)
      (let ((denv (cdr hdl))
	    (h (car hdl)))
	 (set-cdr! hdl val)
	 (unwind-until! h denv)))
   
   (define (raise/cell hdl)
      ;; since 14 jun 2021, error handlers are pushed
      ;; along a cell where to store the exception value
      ;; this removes one closure allocation of 
      ;; with-handler forms (see comptime/Expand/exit.scm)
      (let ((cell (cdr hdl))
	    (h (car hdl)))
	 (cell-set! cell val)
	 (unwind-until! h cell)))

   (define (raise/cell hdl)
      ;; since 14 jun 2021, error handlers are pushed
      ;; along a cell where to store the exception value
      ;; this removes one closure allocation of 
      ;; with-handler forms (see comptime/Expand/exit.scm)
      (let ((cell (cdr hdl))
	    (h (car hdl)))
	 (cell-set! cell val)
	 (unwind-until! h cell)))
   
   (define (wrong-handler handler val)
      (tprint "*** INTERNAL ERROR: wrong error handler " handler)
      (tprint "when raising error: " val)
      (exit 0))
   
   (let ((handler ($get-error-handler)))
      (cond
	 ((pair? handler)
	  (cond
	     ((dynamic-env? (cdr handler))
	      (raise/denv handler))
	     ((cell? (cdr handler))
	      (raise/cell handler))
	     ((not (cdr handler))
	      (default-exception-handler val)
	      (the_failure "raise" "uncaught exception" val))
	     ((eq? (cdr handler) #unspecified)
	      ((car handler) val))
	     (else
	      (wrong-handler handler val))))
	 (else
	  (wrong-handler handler val)))))

;*---------------------------------------------------------------------*/
;*    default-exception-handler ...                                    */
;*---------------------------------------------------------------------*/
(define (default-exception-handler val)
   (exception-notify val)
   (unless (isa? val &warning)
      (let ((retval (if (isa? val &error) 1 2)))
	 (unwind-stack-until! #f #f retval (lambda (x) (%exit retval)) #f)))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    module-init-error ...                                            */
;*---------------------------------------------------------------------*/
(define (module-init-error current::string from::string)
   (fprint (current-error-port)
	   #"*** ERROR:" current ":Inconsistent module initialization\n"
	   "Module `" current "' is inconsistently initialized by module `"
	   from "'.\n"
	   "At least `" from "' must be recompiled (see also -unsafev option).")
   (%exit 1))

;*---------------------------------------------------------------------*/
;*    error ...                                                        */
;*---------------------------------------------------------------------*/
(define (error proc msg obj)
   (raise
      (instantiate::&error
	 (proc proc)
	 (msg msg)
	 (obj obj))))

;*---------------------------------------------------------------------*/
;*    error/location ...                                               */
;*---------------------------------------------------------------------*/
(define (error/location proc msg obj fname loc)
   (raise
      (instantiate::&error
	 (fname fname)
	 (location loc)
	 (proc proc)
	 (msg msg)
	 (obj obj))))

;*---------------------------------------------------------------------*/
;*    error/source-location ...                                        */
;*---------------------------------------------------------------------*/
(define (error/source-location proc msg obj loc)
   (match-case loc
      ((at ?fname ?loc)
       (error/location proc msg obj fname loc))
      (else
       (error proc msg obj))))

;*---------------------------------------------------------------------*/
;*    error/source ...                                                 */
;*---------------------------------------------------------------------*/
(define (error/source proc msg obj source)
   (if (not (epair? source))
       (error proc msg obj)
       (error/source-location proc msg obj (cer source))))

;*---------------------------------------------------------------------*/
;*    error/c-location ...                                             */
;*    -------------------------------------------------------------    */
;*    This function implements a cast for its last two arguments.      */
;*    It impliclty casts the fname and location from C values to       */
;*    Bigloo values.                                                   */
;*---------------------------------------------------------------------*/
(define (error/c-location proc message object fname loc)
   (error/location proc message object fname loc))

;*---------------------------------------------------------------------*/
;*    bigloo-type-error-msg ...                                        */
;*---------------------------------------------------------------------*/
(define (bigloo-type-error-msg prefix from to)
   (string-append prefix " \"" from "\" expected, \"" to "\" provided"))

;*---------------------------------------------------------------------*/
;*    bigloo-type-error ...                                            */
;*---------------------------------------------------------------------*/
(define (bigloo-type-error proc type obj)
   (let* ((ty (cond
		 ((string? type)
		  type)
		 ((symbol? type)
		  (symbol->string type))
		 (else
		  "???")))
	  (msg (bigloo-type-error-msg "Type" ty (typeof obj))))
      (raise
	 (instantiate::&type-error
	    (proc proc)
	    (msg msg)
	    (obj obj)
	    (type type)))))

;*---------------------------------------------------------------------*/
;*    bigloo-type-error/location ...                                   */
;*---------------------------------------------------------------------*/
(define (bigloo-type-error/location proc type obj fname loc)
   (raise (type-error fname loc proc type obj)))

;*---------------------------------------------------------------------*/
;*    type-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (type-error fname loc proc type obj)
   (let* ((ty (cond
		 ((string? type) type)
		 ((symbol? type) (symbol->string type))
		 (else "???")))
	  (msg (bigloo-type-error-msg "Type" ty (typeof obj))))
      (instantiate::&type-error
	 (fname fname)
	 (location loc)
	 (proc proc)
	 (msg msg)
	 (obj obj)
	 (type type))))

;*---------------------------------------------------------------------*/
;*    typename-error ...                                               */
;*---------------------------------------------------------------------*/
(define (typename-error fname loc proc type obj)
   (let* ((ty (cond
		 ((string? type) type)
		 ((symbol? type) (symbol->string type))
		 (else "???")))
	  (msg (bigloo-type-error-msg "Type" ty obj)))
      (instantiate::&type-error
	 (fname fname)
	 (location loc)
	 (proc proc)
	 (msg msg)
	 (obj #unspecified)
	 (type type))))

;*---------------------------------------------------------------------*/
;*    index-out-of-bounds-error ...                                    */
;*---------------------------------------------------------------------*/
(define (index-out-of-bounds-error fname loc proc obj len i)
   (let* ((len (cond
		  ((fixnum? len) len)
		  ((string? len) (string->integer len))
		  (else 0)))
	  (msg (string-append
		  "index " (integer->string i) " out of range [0.."
		  (integer->string (-fx len 1))
		  "]")))
      (instantiate::&index-out-of-bounds-error
	 (fname fname)
	 (location loc)
	 (proc proc)
	 (msg msg)
	 (obj obj)
	 (index len))))

;*---------------------------------------------------------------------*/
;*    warning ...                                                      */
;*---------------------------------------------------------------------*/
(define (warning . args)
   (warning-notify
      (instantiate::&warning (args args))))

;*---------------------------------------------------------------------*/
;*    warning/location ...                                             */
;*---------------------------------------------------------------------*/
(define (warning/location fname loc . args)
   (warning-notify
      (instantiate::&warning (fname fname) (location loc) (args args))))

;*---------------------------------------------------------------------*/
;*    warning/loc ...                                                  */
;*---------------------------------------------------------------------*/
(define (warning/loc loc . args)
   (match-case loc
      ((at ?fname ?loc)
       (apply warning/location fname loc args))
      (else
       (apply warning args))))
   
;*---------------------------------------------------------------------*/
;*    warning/c-location ...                                           */
;*    -------------------------------------------------------------    */
;*    This function implements a cast for its first two arguments.     */
;*    It impliclty casts the fname and location from C values to       */
;*    Bigloo values.                                                   */
;*---------------------------------------------------------------------*/
(define (warning/c-location fname loc . args)
   (apply warning/location fname loc args))
			    
;*---------------------------------------------------------------------*/
;*    notify-&error ...                                                */
;*---------------------------------------------------------------------*/
(define (notify-&error err)
   (let ((port (current-error-port)))
      (with-access::&error err (proc msg obj stack)
	 (flush-output-port port)
	 (display "*** ERROR:" port)
	 (display-circle proc port)
	 (display #":\n" port)
	 (display-circle msg port)
	 (unless (eq? obj '%no-error-obj)
	    (display " -- " port)
	    (display-circle obj port))
	 (newline port)
	 (display-trace-stack (or stack (get-trace-stack)) port)
	 (flush-output-port port))))

;*---------------------------------------------------------------------*/
;*    notify-&error/location-no-loc ...                                */
;*---------------------------------------------------------------------*/
(define (notify-&error/location-no-loc err)
   (let ((port (current-error-port)))
      (with-access::&error err (fname location)
	 (flush-output-port port)
	 (newline port)
	 (fprint port "File \"" fname "\", character " location #\:)
	 (notify-&error err))))

;*---------------------------------------------------------------------*/
;*    notify-&error/location-loc ...                                   */
;*---------------------------------------------------------------------*/
(define (notify-&error/location-loc err fname line loc string col)
   (let ((len (string-length string)))
      (if (>fx len 256)
	  (let* ((ncol 60)
		 (nstring (substring string (maxfx 0 (-fx col ncol)) (+fx col 10))))
	     (notify-&error/location-loc err fname line loc
		(string-append "..." nstring "...")
		(+fx ncol 3)))
	  (with-access::&error err (proc msg obj stack)
	     (let ((port (current-error-port)))
		;; we flush error-port
		(flush-output-port port)
		(newline port)
		(let* ((space-string (if (>fx col 0) (make-string col #\space) ""))
		       (l (string-length string))
		       (n-col (if (>=fx col l) l col)))
		   ;; we ajust tabulation in space string.
		   (fix-tabulation! n-col string space-string)
		   ;; we now print the error message
		   (print-cursor fname line loc string space-string)
		   ;; we display the error message
		   (display "*** ERROR:" port)
		   (display-circle proc port)
		   (newline port)
		   (display-circle msg port)
		   (unless (eq? obj '%no-error-obj)
		      (display " -- " port)
		      (display-circle obj port))
		   (newline port)
		   (display-trace-stack (or stack (get-trace-stack)) port)
		   ;; we are now done, we flush
		   (flush-output-port port)))))))

;*---------------------------------------------------------------------*/
;*    notify-&error/loc ...                                            */
;*---------------------------------------------------------------------*/
(define (notify-&error/loc err fname loc)
   (if (or (not (string? fname)) (not (fixnum? loc)))
       (notify-&error err)
       (multiple-value-bind (file lnum lpoint lstring)
	  (location-line-num `(at ,fname ,loc))
	  (if (not lnum)
	      (notify-&error/location-no-loc err)
	      (notify-&error/location-loc err fname lnum loc lstring lpoint)))))

;*---------------------------------------------------------------------*/
;*    open-for-error ...                                               */
;*---------------------------------------------------------------------*/
(define (open-for-error fname)
   (cond
      ((file-exists? fname)
       (unless (directory? fname)
	  (with-handler
	     (lambda (e) #f)
	     (open-input-file fname))))
      ((string=? fname "stdin")
       (open-input-string (input-port-buffer (current-input-port))))
      ((string-prefix? "string://" fname)
       (open-input-string (substring fname 9)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    filename-for-error ...                                           */
;*---------------------------------------------------------------------*/
(define (filename-for-error file #!optional (sz 255))
   (cond
      ((<fx sz 0)
       "...")
      ((file-exists? file)
       (relative-file-name file))
      ((string-prefix? "string://" file)
       (if (<=fx (string-length file) (+fx 9 sz))
	   (substring file 9)
	   (string-append (substring file 9 (+fx sz 6)) "...")))
      ((<=fx (string-length file) sz)
       file)
      ((<fx sz 4)
       "...")
      (else
       (string-append (substring file 0 (-fx sz 3)) "..."))))

;*---------------------------------------------------------------------*/
;*    location-line-num ...                                            */
;*---------------------------------------------------------------------*/
(define (location-line-num loc)

   (define (relative file)
      (let ((rel (relative-file-name file)))
	 (if (<fx (string-length rel) (string-length file))
	     rel
	     file)))
	 
   (define (location-at file point)
      (when (and (string? file) (integer? point))
	 (let* ((fname (if (string=? (os-class) "win32")
			   (string-replace (uncygdrive file) #\/ #\\)
			   file))
		(port (open-for-error fname)))
	    (if (input-port? port)
		(unwind-protect
		   (let loop ((ostring #f)
			      (lnum 1)
			      (opos 0))
		      (let ((lstring (read-line port)))
			 (if (eof-object? lstring)
			     (values (relative fname) lnum (+fx 1 (-fx point opos))
				(if (string? ostring)
				    (string-append ostring "<eof>")
				    "<eof>"))
			     (if (>fx (input-port-position port) point)
				 (values (relative file) lnum (-fx point opos) lstring)
				 (let ((opos (input-port-position port)))
				    (loop lstring (+fx lnum 1) opos))))))
		   (close-input-port port))
		(values (relative file) #f point #f)))))
   
   (define (location-line-col file line col)
      (if (and (>=fx line 0) (>=fx col 0))
	  (let* ((fname (if (string=? (os-class) "win32")
			    (string-replace (uncygdrive file) #\/ #\\)
			    file))
		 (port (open-for-error fname)))
	     (if (input-port? port)
		 (unwind-protect
		    (let loop ((ostring #f)
			       (lnum line))
		       (let ((lstring (read-line port)))
			  (if (eof-object? lstring)
			      (values (relative file) line (+fx col 1)
				 (string-append ostring "<eof>"))
			      (if (=fx lnum 0)
				  (values (relative file) line col lstring)
				  (let ((opos (input-port-position port)))
				     (loop lstring (-fx lnum 1)))))))
		    (close-input-port port))
		 (values (relative file) line col #f)))
	  (values (relative file) line col #f)))

   (match-case loc
      ((at ?file ?point)
       ;; at schema: (at file line)
       (location-at file point))
      ((line-col ?file ?line ?col)
       ;; line-col schema: (line-col file "line:col")
       (location-line-col file line col))
      ((line ?file ?point)
       ;; line schema: (line file line)
       (location-line-col file point 0))
      (else
       (values #f #f #f #f))))

;*---------------------------------------------------------------------*/
;*    exception-location? ...                                          */
;*---------------------------------------------------------------------*/
(define (exception-location? e)
   (with-access::&exception e (fname location)
      (and (string? fname) (integer? location))))
   
;*---------------------------------------------------------------------*/
;*    error-notify ...                                                 */
;*---------------------------------------------------------------------*/
(define (error-notify e)
   (cond
      ((isa? e &error)
       (if (exception-location? e)
	   (with-access::&exception e (fname location)
	      (notify-&error/loc e fname location))
	   (notify-&error e)))
      ((isa? e &condition)
       (fprint (current-error-port) "*** CONDITION: " e))))

;*---------------------------------------------------------------------*/
;*    error-notify/location ...                                        */
;*---------------------------------------------------------------------*/
(define (error-notify/location e fname location)
   (when (isa? e &error)
      (notify-&error/loc e fname location)))

;*---------------------------------------------------------------------*/
;*    warning-notify ...                                               */
;*---------------------------------------------------------------------*/
(define (warning-notify e)
   (define (simple-warning e)
      (flush-output-port (current-output-port))
      (display "*** WARNING: " (current-error-port))
      (with-access::&warning e (args)
	 (if (not (null? args))
	     (begin
		(display-circle (car args) (current-error-port))
		(newline (current-error-port))
		(for-each (lambda (a)
			     (display-circle a (current-error-port)))
			  (cdr args)))))
      (newline (current-error-port))
      (flush-output-port (current-error-port)))
   (when (>fx (bigloo-warning) 0)
      (if (exception-location? e)
	  (with-access::&warning e (fname location args)
	     (cond
		((string=? fname "[string]")
		 (simple-warning e))
		((string=? fname "[stdin]")
		 (simple-warning e))
		(else
		 (warning/location-file fname location args))))
	  (simple-warning e))
      ;; stack
      (with-access::&warning e (stack)
	 (when stack
	    (display-trace-stack stack (current-error-port)))))
   #f)

;*---------------------------------------------------------------------*/
;*    warning-notify ...                                               */
;*---------------------------------------------------------------------*/
(define (warning-notify/location e fname location)
   (when (>fx (bigloo-warning) 0)
      (with-access::&warning e (args)
	 (warning/location-file fname location args))))

;*---------------------------------------------------------------------*/
;*    warning/location-file ...                                        */
;*---------------------------------------------------------------------*/
(define (warning/location-file fname loc args)
   ;; we compute the message to print the location
   (let ((port (open-for-error fname)))
      (if (not (input-port? port))
	  ;; we are enable to re-open the file, we just print a
	  ;; standard warning
	  (apply warning args)
	  ;; we readlines until we reach location
	  (multiple-value-bind (file lnum lpoint lstring)
	     (location-line-num `(at ,fname ,loc))
	     (if (not lnum)
		 (apply warning args)
		 (do-warn/location fname lnum loc lstring lpoint args))))))

;*---------------------------------------------------------------------*/
;*    do-warn/location ...                                             */
;*---------------------------------------------------------------------*/
(define (do-warn/location fname line char string marker args)
   (flush-output-port (current-output-port))
   (newline (current-error-port))
   (let* ((space-string (if (>fx marker 0)
			    (make-string marker #\space)
			    ""))
	  (l (string-length string))
	  (n-marker (if (>=fx marker l) l marker)))
      ;; we ajust tabulation in space string.
      (fix-tabulation! n-marker string space-string)
      ;; we now print the warning message
      (print-cursor fname line char string space-string)
      ;; we display the warning message
      (display "*** WARNING: " (current-error-port))
      (if (not (null? args))
	  (let ((port (current-error-port)))
	     (display-circle (car args) port)
	     (newline port)
	     (for-each (lambda (a)
			  (display-circle a (current-error-port)))
		       (cdr args))))
      (newline (current-error-port))
      (flush-output-port (current-error-port))))

;*---------------------------------------------------------------------*/
;*    alist? ...                                                       */
;*---------------------------------------------------------------------*/
(define (alist? l)
   ;; it is crucial that this function never fails (otherwise, it falls
   ;; into a infinite loop), hence it implement drastric type checks
   (and (list? l) (every pair? l)))

;*---------------------------------------------------------------------*/
;*    display-trace-stack ...                                          */
;*---------------------------------------------------------------------*/
(define (display-trace-stack stack port #!optional (offset 1))

   (define (filename file num sz)
      (cond
	 ((=fx num 1)
	  (filename-for-error file sz))
	 ((file-exists? file)
	  file)
	 (else
	  "")))
	  
   (define (display-trace-stack-frame frame level num)
      (match-case frame
	 ((?name ?loc . (and (? alist?) ?rest))
	  (let* ((name (if (symbol? name) (symbol->string name) name))
		 (margin (assq 'margin rest))
		 (fmt (assq 'format rest))
		 (nm (if (and (pair? fmt) (string? (cdr fmt)))
			 (format (cdr fmt) name)
			 name)))
	     ;; margin
	     (if (and (pair? margin) (char? (cdr margin)))
		 (display (cdr margin) port)
		 (display " " port))
	     (cond
		((<fx level 10) (display "   " port))
		((<fx level 100) (display "  " port))
		((<fx level 1000) (display " " port)))
	     ;; level
	     (display level port)
	     (display ". " port)
	     ;; frame name
	     (let ((i (string-index name #\@)))
		(if (and i (>fx i 0) loc)
		    (display (substring name 0 i) port)
		    (display name port)))
	     (cond
		((>fx num 1)
		 (display " (* " port)
		 (display num port)
		 (display ")" port))
		(loc
		 (multiple-value-bind (file lnum lpoint lstring)
		    (location-line-num loc)
		    ;; file name
		    (when (and (string? file) (not (string=? file ".")))
		       (display ", " port)
		       (display file port))
		    ;; line num
		    (cond
		       ((and (fixnum? lpoint) (=fx lpoint 0))
			#unspecified)
		       (lnum
			(display ":" port)
			(display lnum port))
		       (lpoint
			(display "@" port)
			(display lpoint port))))))
	     (newline port))
	  (+fx level 1))
	 ((?name)
	  (cond
	     ((<fx level 10) (display "    " port))
	     ((<fx level 100) (display "   " port))
	     ((<fx level 1000) (display "  " port)))
	  ;; level
	  (display level port)
	  (display (if (or (symbol? name) (string? name)) ". " "! ") port)
	  (display name port)
	  (newline port)
	  (+fx level 1))
	 ((? string?)
	  ;; plain string (e.g., for separators)
	  (display frame port)
	  (newline port)
	  level)
	 (else
	  ;; bad stack frame
	  (display "! " port)
	  (display frame port)
	  (newline port)
	  (+fx level 1))))

   (when (pair? stack)
      (let loop ((i offset)
		 (stk (cdr stack))
		 (hds (car stack))
		 (hdn 1))
	 (cond
	    ((null? stk)
	     (display-trace-stack-frame hds i hdn)
	     (flush-output-port port))
	    ((not (pair? stk))
	     (fprintf (current-error-port)
		"\n*** INTERNAL ERROR: corrupted stack -- ~s\n" stack)
	     (flush-output-port port))
	    ((eq? (car stk) hds)
	     (loop (+fx i 1) (cdr stk) hds (+fx hdn 1)))
	    (else
	     (let ((ni (display-trace-stack-frame hds i hdn)))
		(loop ni (cdr stk) (car stk) 1)))))))

;*---------------------------------------------------------------------*/
;*    display-trace-stack-source ...                                   */
;*    -------------------------------------------------------------    */
;*    Display an excerpt of the source file corresponding to the       */
;*    top of the stack trace.                                          */
;*---------------------------------------------------------------------*/
(define (display-trace-stack-source stack port)

   (define (display-source file lnum lpoint lstring)
      (let* ((tabs (if (>fx lpoint 0) (make-string lpoint #\space) ""))
	     (l (string-length lstring))
	     (ncol (if (>=fx lpoint l) l lpoint)))
	 ;; we ajust tabulation in space string.
	 (fix-tabulation! ncol lstring tabs)
	 ;; we now print the error message
	 (print-cursor file lnum lpoint lstring tabs)))

   (let loop ((stack stack))
      (when (pair? stack)
	 (match-case (car stack)
	    ((?name ?loc . (and (? alist?) ?rest))
	     ;; got a localized stack frame
	     (multiple-value-bind (file lnum lpoint lstring)
		(location-line-num loc)
		(cond
		   ((and (string? file) (string? lstring))
		    (display-source file lnum lpoint lstring))
		   ((and (string? file) (integer? lpoint))
		    (if (integer? lnum)
			(fprintf (current-error-port)
			   "File ~s, line ~d, character ~d\n"
			   (filename-for-error file)
			   lnum
			   lpoint)
			(fprintf (current-error-port)
			   "File ~s, character ~d\n"
			   (filename-for-error file)
			   lpoint)))
		   (else
		    (loop (cdr stack))))))
	    (else
	     (loop (cdr stack)))))))

;*---------------------------------------------------------------------*/
;*    dump-trace-stack ...                                             */
;*---------------------------------------------------------------------*/
(define (dump-trace-stack port depth)
   (display-trace-stack (get-trace-stack depth) port))
	     
;*---------------------------------------------------------------------*/
;*    fix-tabulation! ...                                              */
;*---------------------------------------------------------------------*/
(define (fix-tabulation! marker src dst)
   (let loop ((read (-fx marker 1)))
      (cond
	 ((=fx read -1)
	  'done)
	 ((char=? (string-ref src read) #\tab)
	  (string-set! dst read #\tab)
	  (loop (-fx read 1)))
	 (else
	  (loop (-fx read 1))))))

;*---------------------------------------------------------------------*/
;*    print-cursor ...                                                 */
;*---------------------------------------------------------------------*/
(define (print-cursor fname line char string space-string)
   (fprint (current-error-port)
	   "File \"" (filename-for-error fname) "\", line " line ", character "
	   char ":"
	   #\Newline
	   "#" string #\Newline
	   "#"
	   space-string
	   "^"))

;*---------------------------------------------------------------------*/
;*    relative-file-name ...                                           */
;*    -------------------------------------------------------------    */
;*    We remove the current path to fname                              */
;*---------------------------------------------------------------------*/
(define (relative-file-name fname)
   (let ((pwd   (pwd))
	 (dname (dirname fname)))
      (if (or (not (string? pwd))
	      (string=? dname ".")
	      (not (char=? (string-ref fname 0) #\/)))
	  fname
	  ;; we compute the two path lists
	  (let ((original-cmp-path (dirname->list dname)))
	     (let loop ((cmp-path original-cmp-path)
			(cur-path (dirname->list pwd)))
		(cond
		   ((null? cmp-path)
		    (if (null? cur-path)
			(basename fname)
			;; we have to complete with ../
			(let loop ((len (length cur-path))
				   (res (basename fname)))
			   (if (=fx len 0)
			       res
			       (loop (-fx len 1) (string-append "../" res))))))
		   ((null? cur-path)
		    (let loop ((path (reverse! cmp-path))
			       (res  (basename fname)))
		       (if (null? path)
			   res
			   (loop (cdr path)
				 (string-append (car path) "/" res)))))
		   ((string=? (car cur-path) (car cmp-path))
		    (loop (cdr cmp-path) (cdr cur-path)))
		   (else
		    (let loop ((path (reverse cmp-path))
			       (res  (basename fname)))
		       (if (null? path)
			   (if (eq? cmp-path original-cmp-path)
			       (string-append "/" res)
			       (let loop ((len (length cur-path))
					  (res res))
				  (if (=fx len 0)
				      res
				      (loop (-fx len 1)
					    (string-append "../" res)))))
			   (loop (cdr path)
				 (string-append (car path) "/" res)))))))))))

;*---------------------------------------------------------------------*/
;*    uncygdrive ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is used on the Win32 port to get rid of the fake   */
;*    directory introduced by Cygwin.                                  */
;*---------------------------------------------------------------------*/
(define (uncygdrive::bstring str::bstring)
   (if (substring=? "/cygdrive/" str 10)
       (if (and (>fx (string-length str) 12)
                (char-alphabetic? (string-ref str 10))
                (char=? (string-ref str 11) #\/))
           (string-append (string (string-ref str 10) #\: #\/)
                          (substring str 12 (string-length str)))
           str)
       str))

;*---------------------------------------------------------------------*/
;*    dirname->list ...                                                */
;*---------------------------------------------------------------------*/
(define (dirname->list path)
   (let ((len  (let ((len (string-length path)))
		  (if (char=? (string-ref path (-fx len 1)) (file-separator))
		      (-fx len 1)
		      len)))
	 (init (if (char=? (string-ref path 0) (file-separator)) 1 0)))
      (if (string=? path "/")
	  '()
	  (let loop ((read init)
		     (prev init)
		     (list '()))
	     (cond
		((=fx read len)
		 (reverse! (cons (substring path prev read) list)))
		((char=? (string-ref path read) (file-separator))
		 (loop (+fx read 1)
		       (+fx read 1)
		       (cons (substring path prev read) list)))
		(else
		 (loop (+fx read 1) prev list)))))))

;*---------------------------------------------------------------------*/
;*    typeof ...                                                       */
;*    -------------------------------------------------------------    */
;*    This function tries to determine the type of an object in        */
;*    order to produce better type error messages.                     */
;*---------------------------------------------------------------------*/
(define (typeof obj)
   (cond
      ((fixnum? obj)
       "bint")
      ((flonum? obj)
       "real")
      ((string? obj)
       "bstring")
      ((symbol? obj)
       "symbol")
      ((keyword? obj)
       "keyword")
      ((char? obj)
       "bchar")
      ((boolean? obj)
       "bbool")
      ((null? obj)
       "bnil")
      ((eq? obj #unspecified)
       "unspecified")
      ((epair? obj)
       "epair")
      ((pair? obj)
       "pair")
      ((class? obj)
       "class")
      ((vector? obj)
       "vector")
      ((tvector? obj)
       "tvector")
      ((struct? obj)
       (string-append "struct:" (symbol->string (struct-key obj))))
      ((procedure? obj)
       "procedure")
      ((input-port? obj)
       "input-port")
      ((output-port? obj)
       "output-port")
      ((binary-port? obj)
       "binary-port")
      ((cell? obj)
       "cell")
      ((foreign? obj)
       (string-append "foreign:" (symbol->string (foreign-id obj))))
      ((socket? obj)
       "socket")
      ((datagram-socket? obj)
       "datagram-socket")
      ((process? obj)
       "process")
      ((custom? obj)
       "custom")
      ((opaque? obj)
       "opaque")
      ((object? obj)
       (let ((class  (object-class obj)))
	  ;; here we are very chicken here because want to ensure that
	  ;; this function is always correct, even if the system is not
	  ;; totally (or not correctly) intialized.
	  (if (class? class)
	      (let ((sym (class-name class)))
		 (if (symbol? sym)
		     (symbol->string sym)
		     "_"))
	      "_")))
      ((ucs2-string? obj)
       "ucs2string")
      ((ucs2? obj)
       "ucs2")
      ((elong? obj)
       "elong")
      ((llong? obj)
       "llong")
      ((mutex? obj)
       "mutex")
      ((condition-variable? obj)
       "condvar")
      ((date? obj)
       "date")
      (($hvector? obj)
       (multiple-value-bind (tag _ _ _)
	  (homogeneous-vector-info obj)
	  (string-append (symbol->string tag) "vector")))
      ((bignum? obj)
       "bignum")
      (($mmap? obj)
       "mmap")
      ((regexp? obj)
       "regexp")
      ((int8? obj)
       "int8")
      ((uint8? obj)
       "uint8")
      ((int16? obj)
       "int16")
      ((uint16? obj)
       "uint16")
      ((int32? obj)
       "int32")
      ((uint32? obj)
       "uint32")
      ((int64? obj)
       "int64")
      ((uint64? obj)
       "uint64")
      ((cnst? obj)
       "bcnst")
      (else
       ($foreign-typeof obj))))

;*---------------------------------------------------------------------*/
;*    find-runtime-type ...                                            */
;*---------------------------------------------------------------------*/
(define (find-runtime-type o)
   (typeof o))

;*---------------------------------------------------------------------*/
;*    c-debugging-show-type ...                                        */
;*    -------------------------------------------------------------    */
;*    See also bgl_typeof (runtime/Clib/cerror.c).                     */
;*---------------------------------------------------------------------*/
(define (c-debugging-show-type obj)
   (let ((t (typeof obj)))
      (fprint (current-error-port) t)
      t))

;*---------------------------------------------------------------------*/
;*    &try ...                                                         */
;*---------------------------------------------------------------------*/
(define (&try thunk handler)
   (bind-exit (esc)
      (with-exception-handler
	 (lambda (e)
	    (if (isa? e &error)
		(with-access::&error e (proc msg obj)
		   (handler esc proc msg obj)
		   (exit 4))
		(raise e)))
	 thunk)))

;*---------------------------------------------------------------------*/
;*    push-error-handler! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (push-error-handler! hdl exit)
   ($push-error-handler! hdl exit))

;*---------------------------------------------------------------------*/
;*    set-error-handler! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (set-error-handler! ehdl)
   ($set-error-handler! ehdl))

;*---------------------------------------------------------------------*/
;*    env-set-error-handler! ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (env-set-error-handler! env ehdl)
   ($env-set-error-handler! env ehdl))

;*---------------------------------------------------------------------*/
;*    get-error-handler ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (get-error-handler)
   ($get-error-handler))

;*---------------------------------------------------------------------*/
;*    env-get-error-handler ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (env-get-error-handler env)
   ($env-get-error-handler env))

;*---------------------------------------------------------------------*/
;*    notify-interrupt ...                                             */
;*---------------------------------------------------------------------*/
(define (notify-interrupt sig)
   (let ((notify ($get-interrupt-notifier)))
      ((if (procedure? notify)
	   notify
	   default-interrupt-notifier)
       sig)))

;*---------------------------------------------------------------------*/
;*    default-interrupt-notifier ...                                   */
;*---------------------------------------------------------------------*/
(define (default-interrupt-notifier sig)
   (let ((port (current-error-port)))
      (newline port)
      (fprint port "*** INTERRUPT:bigloo:")
      (flush-output-port port)))

;*---------------------------------------------------------------------*/
;*    sigfpe-error-handler ...                                         */
;*---------------------------------------------------------------------*/
(define (sigfpe-error-handler n)
   (error "arithmetic procedure" "`floating point' exception" "raised"))

;*---------------------------------------------------------------------*/
;*    sigill-error-handler ...                                         */
;*---------------------------------------------------------------------*/
(define (sigill-error-handler n)
   (error "bigloo" "`illegal instruction' exception" "raised"))

;*---------------------------------------------------------------------*/
;*    sigbus-error-handler ...                                         */
;*---------------------------------------------------------------------*/
(define (sigbus-error-handler n)
   (error "bigloo" "`bus error' exception" "raised"))

;*---------------------------------------------------------------------*/
;*    sigsegv-error-handler ...                                        */
;*---------------------------------------------------------------------*/
(define (sigsegv-error-handler n)
   (error "bigloo" "`segmentation violation' exception" "raised"))

;*---------------------------------------------------------------------*/
;*    On installe le ratrappage des exceptions                         */
;*---------------------------------------------------------------------*/
(signal sigfpe sigfpe-error-handler)
;; sigfpe is not always enough as some C compilers are so
;; smart that they can replace the sigfpe with a sigtrap
(signal sigtrap sigfpe-error-handler)
(signal sigill sigill-error-handler)
(signal sigbus sigbus-error-handler)
(signal sigsegv sigsegv-error-handler)
