;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/error.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 08:19:23 1995                          */
;*    Last change :  Sat Dec 25 11:02:12 2010 (serrano)                */
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
   
   (extern  (export the_failure "the_failure")
	    (export error/errno "bgl_system_failure")
	    (export find-runtime-type "bgl_find_runtime_type")
	    (export typeof "bgl_typeof")
	    (export c-debugging-show-type "bgl_show_type")
	    
	    ($get-trace-stack::pair-nil (::int) "get_trace_stack")
	    (macro $push-trace::obj (::obj ::obj) "BGL_PUSH_TRACE")
	    (macro $env-push-trace::obj (::dynamic-env ::obj ::obj) "BGL_ENV_PUSH_TRACE")
	    (macro $env-set-trace-name::obj (::dynamic-env ::obj) "BGL_ENV_SET_TRACE_NAME")
	    (macro $env-set-trace-location::obj (::dynamic-env ::obj) "BGL_ENV_SET_TRACE_LOCATION")
	    (macro $env-pop-trace::obj (::dynamic-env) "BGL_ENV_POP_TRACE")
	    (macro $pop-trace::obj () "BGL_POP_TRACE")
	    (macro $get-error-handler::obj () "BGL_ERROR_HANDLER_GET")
	    (macro $set-error-handler!::void (::obj) "BGL_ERROR_HANDLER_SET")
	    (macro $get-uncaught-exception-handler::obj () "BGL_UNCAUGHT_EXCEPTION_HANDLER_GET")
	    (macro $set-uncaught-exception-handler!::void (::obj) "BGL_UNCAUGHT_EXCEPTION_HANDLER_SET")
	    (macro $get-error-notifiers::obj () "BGL_ERROR_NOTIFIERS_GET")
	    (macro $set-error-notifiers!::obj (::obj) "BGL_ERROR_NOTIFIERS_SET")
	    (macro $get-interrupt-notifier::obj () "BGL_INTERRUPT_NOTIFIER_GET")
	    (macro $set-interrupt-notifier!::void (::obj) "BGL_INTERRUPT_NOTIFIER_SET")
	    (macro sigfpe::int "SIGFPE")
	    (macro sigill::int "SIGILL")
	    (macro sigbus::int "SIGBUS")
	    (macro sigsegv::int "SIGSEGV")
	    (macro sigpipe::int "SIGPIPE")
	    
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
	    (macro $errno-process-exception::int "BGL_PROCESS_EXCEPTION"))

   (java    (export the_failure "the_failure")
	    (export error/errno "bgl_system_failure")
	    (class foreign
	       (method static $get-trace-stack::pair-nil (::int)
		       "get_trace_stack")
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
	       (method static $set-error-handler!::void (::obj)
		       "BGL_ERROR_HANDLER_SET")
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
	       
	       (field static sigfpe::int "SIGFPE")
	       (field static sigill::int "SIGILL")
	       (field static sigbus::int "SIGBUS")
	       (field static sigsegv::int "SIGSEGV")
	       (field static sigpipe::int "SIGPIPE")
	       
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
	    
	    __pp_circle
	    
	    __reader
	    
	    __rgc
	    
	    __r4_vectors_6_8
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
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
	    (index-out-of-bounds-error fname loc proc len obj)

	    (module-init-error ::string ::string)

	    (error-notify ::obj)
	    (error-notify/location ::obj ::bstring ::int)
	    (warning-notify ::obj)
	    (warning-notify/location ::obj ::bstring ::int)
	    
	    (get-trace-stack #!optional depth)
	    (display-trace-stack ::obj ::output-port)
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
	    
	    (notify-interrupt ::int))
	    
   (option  (bigloo-compiler-debug-set! 0)
	    (set! *compiler-debug* 0)
	    (set! *optim-O-macro?* #f)
	    (set! *unsafe-type*    #t)
	    (set! *unsafe-arity*   #t)
	    (set! *unsafe-range*   #t)))

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
   (if (&exception? proc)
       (raise proc)
       (error proc msg obj)))

;*---------------------------------------------------------------------*/
;*    error/errno ...                                                  */
;*---------------------------------------------------------------------*/
(define (error/errno sysno proc msg obj)
   (cond
      ((=fx sysno $errno-io-error)
       (raise (make-&io-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-port-error)
       (raise (make-&io-port-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-read-error)
       (raise (make-&io-read-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-write-error)
       (raise (make-&io-write-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-unknown-host-error)
       (raise (make-&io-unknown-host-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-file-not-found-error)
       (raise (make-&io-file-not-found-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-parse-error)
       (raise (make-&io-parse-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-malformed-url-error)
       (raise (make-&io-malformed-url-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-sigpipe-error)
       (raise (make-&io-sigpipe-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-io-timeout-error)
       (raise (make-&io-timeout-error #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-process-exception)
       (raise (make-&process-exception #f #f (get-trace-stack) proc msg obj)))
      ((=fx sysno $errno-type-error)
       (raise (type-error #f #f proc msg obj)))
      ((=fx sysno $errno-typename-error)
       (raise (typename-error #f #f proc msg obj)))
      ((=fx sysno $errno-index-out-of-bound-error)
       (raise (index-out-of-bounds-error #f #f proc msg obj)))
      (else
       (error proc msg obj))))

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
	  ($set-error-handler! (cons handler old-handlers))
	  (unwind-protect
	     (if (correct-arity? thunk 0)
		 (thunk)
		 (error 'with-exception-handler "Incorrect thunk arity" thunk))
	     ($set-error-handler! old-handlers)))
       (error 'with-exception-handler "Incorrect handler arity" handler)))

;*---------------------------------------------------------------------*/
;*    current-exception-handler ...                                    */
;*---------------------------------------------------------------------*/
(define (current-exception-handler)
   (if (pair? ($get-error-handler))
       (car ($get-error-handler))
       default-exception-handler))

;*---------------------------------------------------------------------*/
;*    raise ...                                                        */
;*---------------------------------------------------------------------*/
(define (raise val)
   (let ((handlers ($get-error-handler)))
      (if (pair? handlers)
	  (let ((hdls (cdr handlers)))
	     ($set-error-handler! hdls)
	     (let ((r ((car handlers) val)))
		($set-error-handler! hdls)
		(when (&error? val)
		   (error/location 'raise
				   "Handler return from error"
				   val
				   (&error-fname val)
				   (&error-location val)))
		r))
	  (begin
	     (default-exception-handler val)
	     (the_failure "raise" "uncaught execption" val)))))

;*---------------------------------------------------------------------*/
;*    default-exception-handler ...                                    */
;*---------------------------------------------------------------------*/
(define (default-exception-handler val)
   (exception-notify val)
   (unless (&warning? val)
      (let ((retval (if (&error? val) 1 2)))
	 (unwind-stack-until! #f #f retval (lambda (x) (%exit retval)))))
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
   (raise (make-&error #f #f (get-trace-stack) proc msg obj)))

;*---------------------------------------------------------------------*/
;*    error/location ...                                               */
;*---------------------------------------------------------------------*/
(define (error/location proc msg obj fname loc)
   (raise (make-&error fname loc (get-trace-stack) proc msg obj)))

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
   (string-append prefix " `" from "' expected, `" to "' provided"))

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
      (raise (make-&type-error #f #f (get-trace-stack) proc msg obj type))))

;*---------------------------------------------------------------------*/
;*    bigloo-type-error/location ...                                   */
;*---------------------------------------------------------------------*/
(define (bigloo-type-error/location proc type obj fname loc)
   (let* ((ty (cond
		 ((string? type)
		  type)
		 ((symbol? type)
		  (symbol->string type))
		 (else
		  "???")))
	  (msg (bigloo-type-error-msg "Type" ty (typeof obj))))
      (raise
       (make-&type-error fname loc (get-trace-stack) proc msg obj type))))

;*---------------------------------------------------------------------*/
;*    type-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (type-error fname loc proc type obj)
   (let* ((ty (cond
		 ((string? type) type)
		 ((symbol? type) (symbol->string type))
		 (else "???")))
	  (msg (bigloo-type-error-msg "Type" ty (typeof obj))))
      (make-&type-error fname loc (get-trace-stack) proc msg obj type)))

;*---------------------------------------------------------------------*/
;*    typename-error ...                                               */
;*---------------------------------------------------------------------*/
(define (typename-error fname loc proc type obj)
   (let* ((ty (cond
		 ((string? type) type)
		 ((symbol? type) (symbol->string type))
		 (else "???")))
	  (msg (bigloo-type-error-msg "Type" ty obj)))
      (make-&type-error fname loc (get-trace-stack) proc msg #unspecified type)))

;*---------------------------------------------------------------------*/
;*    index-out-of-bounds-error ...                                    */
;*---------------------------------------------------------------------*/
(define (index-out-of-bounds-error fname loc proc len obj)
   (let* ((len (cond
		  ((fixnum? len) len)
		  ((string? len) (string->integer len))
		  (else 0)))
	  (msg (string-append "index out of range [0.."
			      (integer->string (-fx len 1))
			      "]")))
      (make-&index-out-of-bounds-error fname loc
				       (get-trace-stack)
				       proc msg obj len)))

;*---------------------------------------------------------------------*/
;*    warning ...                                                      */
;*---------------------------------------------------------------------*/
(define (warning . args)
   (warning-notify (make-&warning #f #f (get-trace-stack) args)))

;*---------------------------------------------------------------------*/
;*    warning/location ...                                             */
;*---------------------------------------------------------------------*/
(define (warning/location fname loc . args)
   (warning-notify (make-&warning fname loc (get-trace-stack) args)))

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
      (flush-output-port port)
      (display "*** ERROR:" port)
      (display-circle (&error-proc err) port)
      (display #":\n" port)
      (display-circle (&error-msg err) port)
      (display " -- " port)
      (display-circle (&error-obj err) port)
      (newline port)
      (display-trace-stack (or (&error-stack err) (get-trace-stack)) port)
      (flush-output-port port)))

;*---------------------------------------------------------------------*/
;*    notify-&error/location-no-loc ...                                */
;*---------------------------------------------------------------------*/
(define (notify-&error/location-no-loc err)
   (let ((port (current-error-port)))
      (flush-output-port port)
      (newline port)
      (fprint port "File \"" (&error-fname err)
	      "\", character " (&error-location err) #\:)
      (notify-&error err)))

;*---------------------------------------------------------------------*/
;*    notify-&error/location-loc ...                                   */
;*---------------------------------------------------------------------*/
(define (notify-&error/location-loc err fname line loc string column)
   (let ((port (current-error-port))
	 (proc (&error-proc err))
	 (msg (&error-msg err))
	 (obj (&error-obj err)))
      ;; we flush error-port
      (flush-output-port port)
      (newline port)
      (let* ((space-string (if (>fx column 0)
			       (make-string column #\space)
			       ""))
	     (l (string-length string))
	     (n-column (if (>=fx column l)
			   l
			   column)))
	 ;; we ajust tabulation in space string.
	 (fix-tabulation! n-column string space-string)
	 ;; we now print the error message
	 (print-cursor fname line loc string space-string)
	 ;; we display the error message
	 (display "*** ERROR:" port)
	 (display-circle proc port)
	 (newline port)
	 (display-circle msg port)
	 (display " -- " port)
	 (display-circle obj port)
	 (newline port)
	 (display-trace-stack (or (&error-stack err) (get-trace-stack)) port)
	 ;; we are now done, we flush
	 (flush-output-port port))))

;*---------------------------------------------------------------------*/
;*    notify-&error/loc ...                                            */
;*---------------------------------------------------------------------*/
(define (notify-&error/loc err fname loc)
   (if (or (not (string? fname)) (not (fixnum? loc)))
       (notify-&error err)
       (multiple-value-bind (lnum lpoint lstring)
	  (location-line-num fname loc)
	  (if (not lnum)
	      (notify-&error/location-no-loc err)
	      (notify-&error/location-loc err fname lnum loc lstring lpoint)))))

;*---------------------------------------------------------------------*/
;*    location-line-num ...                                            */
;*---------------------------------------------------------------------*/
(define (location-line-num file point)
   (if (and (string? file) (fixnum? point))
       (let* ((fname (if (string=? (os-class) "win32")
			 (string-replace (uncygdrive file) #\/ #\\)
			 file))
	      (port (open-input-file fname)))
	  (if (input-port? port)
	      (let loop ((lstring (read-line port))
			 (lnum 1)
			 (opos 0))
		 (if (eof-object? lstring)
		     (begin
			(close-input-port port)
			(values #f #f #f))
		     (if (>fx (input-port-position port) point)
			 (begin
			    (close-input-port port)
			    (values lnum (-fx point opos) lstring))
			 (let ((opos (input-port-position port)))
			    (loop (read-line port) 
				  (+fx lnum 1)
				  opos)))))))
       (values #f #f #f)))

;*---------------------------------------------------------------------*/
;*    exception-location? ...                                          */
;*---------------------------------------------------------------------*/
(define (exception-location? e)
   (and (string? (&exception-fname e)) (integer? (&exception-location e))))
   
;*---------------------------------------------------------------------*/
;*    error-notify ...                                                 */
;*---------------------------------------------------------------------*/
(define (error-notify e)
   (cond
      ((&error? e)
       (if (exception-location? e)
	   (notify-&error/loc e (&exception-fname e) (&exception-location e))
	   (notify-&error e)))
      ((&condition? e)
       (fprint (current-error-port) "*** CONDITION: " e))))

;*---------------------------------------------------------------------*/
;*    error-notify/location ...                                        */
;*---------------------------------------------------------------------*/
(define (error-notify/location e fname location)
   (when (&error? e)
      (notify-&error/loc e fname location)))

;*---------------------------------------------------------------------*/
;*    warning-notify ...                                               */
;*---------------------------------------------------------------------*/
(define (warning-notify e)
   (define (simple-warning e)
      (flush-output-port (current-output-port))
      (display "*** WARNING:bigloo:" (current-error-port))
      (let ((args (&warning-args e)))
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
	  (let ((fname (&warning-fname e)))
	     (cond
		((string=? fname "[string]")
		 (simple-warning e))
		((string=? fname "[stdin]")
		 (simple-warning e))
		(else
		 (warning/location-file fname
					(&warning-location e)
					(&warning-args e)))))
	  (simple-warning e)))
   ;; stack
   (when (&warning-stack e)
      (display-trace-stack (&warning-stack e) (current-error-port)))
   #f)

;*---------------------------------------------------------------------*/
;*    warning-notify ...                                               */
;*---------------------------------------------------------------------*/
(define (warning-notify/location e fname location)
   (when (>fx (bigloo-warning) 0)
      (warning/location-file fname location (&warning-args e))))

;*---------------------------------------------------------------------*/
;*    warning/location-file ...                                        */
;*---------------------------------------------------------------------*/
(define (warning/location-file fname loc args)
   ;; we compute the message to print the location
   (let ((port (open-input-file fname)))
      (if (not (input-port? port))
	  ;; we are enable to re-open the file, we just print a
	  ;; standard warning
	  (apply warning args)
	  ;; we readlines until we reach location
	  (multiple-value-bind (lnum lpoint lstring)
	     (location-line-num fname loc)
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
      (display "*** WARNING:bigloo:" (current-error-port))
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
;*    display-trace-stack ...                                          */
;*---------------------------------------------------------------------*/
(define (display-trace-stack stack port)
   
   (define (display-trace-stack-frame level frame num)
      (cond
	 ((<fx level 10) (display "    " port))
	 ((<fx level 100) (display "   " port))
	 ((<fx level 1000) (display "  " port)))
      (display level port)
      (display ". " port)
      (display (car frame) port)
      (cond
	 ((>fx num 1)
	  (display " (* " port)
	  (display num port)
	  (display ")" port))
	 ((pair? (cdr frame))
	  (display ", " port)
	  (multiple-value-bind (lnum lpoint lstring)
	     (location-line-num (cadr (cdr frame)) (caddr (cdr frame)))
	     (if lnum
		 (begin
		    (display (relative-file-name (cadr (cdr frame))) port)
		    (display ":" port)
		    (display lnum port))
		 (begin
		    (display (cadr (cdr frame)) port)
		    (display "@" port)
		    (display (caddr (cdr frame)) port))))))
      (newline port))

   (when (pair? stack)
      (let loop ((i 1)
		 (stack (cdr stack))
		 (hds (car stack))
		 (hdn 1))
	 (cond
	    ((null? stack)
	     (display-trace-stack-frame i hds hdn)
	     (flush-output-port port))
	    ((eq? (car stack) hds)
	     (loop (+fx i 1) (cdr stack) hds (+fx hdn 1)))
	    (else
	     (display-trace-stack-frame i hds hdn)
	     (loop (+fx i 1) (cdr stack) (car stack) 1))))))

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
	   "File \"" (relative-file-name fname) "\", line " line ", character "
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
       "struct")
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
      ((cnst? obj)
       "bcnst")
      ((socket? obj)
       "socket")
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
	    (if (&error? e)
		(begin
		   (handler esc (&error-proc e) (&error-msg e) (&error-obj e))
		   (exit 4))
		(raise e)))
	 thunk)))
       
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
(signal sigill sigill-error-handler)
(signal sigbus sigbus-error-handler)
(signal sigsegv sigsegv-error-handler)
