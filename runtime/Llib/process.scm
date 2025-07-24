;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Llib/process.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Mon Jan 19 17:35:12 1998                          */
;*    Last change :  Thu Jul 24 13:32:52 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Process handling. This part is mostly compatible with            */
;*    STk. This code is extracted from STk by Erick Gallesio.          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __process

   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __ucs2
	    __dsssl
	    __bexit
	    __bignum
	    __object
	    __thread
	    __bit
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_control_features_6_9

	    __evenv)

   (extern  (macro $process?::bool (::obj) "PROCESSP")
	    (macro $process-pid::int (::process) "PROCESS_PID")
	    (macro $process-input-port::obj (::process) "PROCESS_INPUT_PORT")
	    (macro $process-output-port::obj (::process) "PROCESS_OUTPUT_PORT")
	    (macro $process-error-port::obj (::process) "PROCESS_ERROR_PORT")
	    ($process-alive?::bool (::process) "c_process_alivep")
	    ($process-wait::obj (::process) "c_process_wait")
	    ($process-exit-status::obj (::process) "c_process_xstatus")
	    ($process-send-signal::obj (::process ::int) "c_process_send_signal")
	    ($process-kill::obj (::process) "c_process_kill")
	    ($process-stop::obj (::process) "c_process_stop")
	    ($process-continue::obj (::process) "c_process_continue")
	    ($run-process::process (::obj ::obj ::obj ::obj ::obj ::obj ::bstring ::obj ::obj)
				    "c_run_process")
	    ($unregister-process::obj (::process) "c_unregister_process")
	    ($process-list::obj () "c_process_list")
	    ($process-nil::process () "bgl_process_nil"))

   (java    (class foreign
	       (method static $process?::bool (::obj)
		       "PROCESSP")
	       (method static $process-nil::process ()
		       "bgl_process_nil")
	       (method static $process-pid::int (::process)
		       "PROCESS_PID")
	       (method static $process-input-port::obj (::process)
		       "PROCESS_INPUT_PORT")
	       (method static $process-output-port::obj (::process)
		       "PROCESS_OUTPUT_PORT")
	       (method static $process-error-port::obj (::process)
		       "PROCESS_ERROR_PORT")
	       (method static $process-alive?::bool (::process)
		       "c_process_alivep")
	       (method static $process-wait::obj (::process)
		       "c_process_wait")
	       (method static $process-exit-status::obj (::process)
		       "c_process_xstatus")
	       (method static $process-send-signal::obj (::process ::int)
		       "c_process_send_signal")
	       (method static $process-kill::obj (::process)
		       "c_process_kill")
	       (method static $process-stop::obj (::process)
		       "c_process_stop")
	       (method static $process-continue::obj (::process)
		       "c_process_continue")
	       (method static $run-process::process (::obj ::obj ::obj ::obj ::obj ::obj ::bstring ::obj ::obj)
		       "c_run_process")
	       (method static $unregister-process::obj (::process)
		       "c_unregister_process")
	       (method static $process-list::obj ()
		       "c_process_list")))
   
   (export  (inline process?::bool ::obj)
	    (inline process-nil::process)
	    (inline process-pid::int ::process)
	    (inline process-output-port::obj ::process)
	    (inline process-input-port::obj ::process)
	    (inline process-error-port::obj  ::process)
	    (inline process-alive?::bool ::process)
	    (inline process-wait::bool ::process)
	    (inline process-exit-status::obj ::process)
	    (inline process-send-signal::obj ::process ::int)
	    (inline process-kill::obj ::process)
	    (inline process-stop::obj ::process)
	    (inline process-continue::obj ::process)
	    (inline process-list::obj)
	    (run-process::process ::bstring . rest)
	    (close-process-ports ::process)
	    (inline unregister-process ::process))

   (pragma  ($process? nesting)
	    ($process-pid nesting args-safe)
	    ($process-input-port nesting args-safe)
	    ($process-output-port nesting args-safe)
	    ($process-error-port nesting args-safe)))

;*---------------------------------------------------------------------*/
;*    process? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (process? obj)
   ($process? obj))

;*---------------------------------------------------------------------*/
;*    process-nil ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (process-nil)
   ($process-nil))

;*---------------------------------------------------------------------*/
;*    process-pid ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (process-pid proc)
   ($process-pid proc))

;*---------------------------------------------------------------------*/
;*    process-output-port ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (process-output-port proc)
   ($process-output-port proc))

;*---------------------------------------------------------------------*/
;*    process-input-port ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (process-input-port proc)
   ($process-input-port proc))

;*---------------------------------------------------------------------*/
;*    process-error-port ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (process-error-port proc)
   ($process-error-port proc))

;*---------------------------------------------------------------------*/
;*    process-alive? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (process-alive? proc)
   ($process-alive? proc))

;*---------------------------------------------------------------------*/
;*    process-wait ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (process-wait proc)
   (if (process-alive? proc)
       ($process-wait proc)))

;*---------------------------------------------------------------------*/
;*    process-exit-status ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (process-exit-status proc)
   ($process-exit-status proc))

;*---------------------------------------------------------------------*/
;*    process-send-signal ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (process-send-signal proc signal)
   ($process-send-signal proc signal))

;*---------------------------------------------------------------------*/
;*    process-kill ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (process-kill proc)
   ($process-kill proc)
   (close-process-ports proc))

;*---------------------------------------------------------------------*/
;*    process-stop ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (process-stop proc)
   ($process-stop proc))

;*---------------------------------------------------------------------*/
;*    process-continue ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (process-continue proc)
   ($process-continue proc))

;*---------------------------------------------------------------------*/
;*    process-list ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (process-list)
   ($process-list))

;*---------------------------------------------------------------------*/
;*    run-process ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function accepts keyworded arguments. E.g.                  */
;*      (run-process "ls" "-l" "/bin" output: "/tmp/X" wait: #f        */
;*---------------------------------------------------------------------*/
(define (run-process command . rest)
   (let ((fork   #t)
	 (wait   #f)
	 (input  #unspecified)
	 (output #unspecified)
	 (error  #unspecified)
	 (host   #unspecified)
	 (args   '())
	 (env    '())
	 (arg-error (lambda (rest)
		       (error "run-process" "Illegal argument" rest))))
      (let loop ((rest rest))
	 (cond
	    ((null? rest)
	     ($run-process host fork wait
			    input output error
			    command (reverse! args) env))
	    ((and (keyword? (car rest)) (pair? (cdr rest)))
	     (let ((val (cadr rest)))
		(case (car rest)
		   ((wait:)
		    (if (boolean? val)
			(set! wait val)
			(arg-error rest)))
		   ((fork:)
		    (if (boolean? val)
			(set! fork val)
			(arg-error rest)))
		   ((input:)
		    (if (or (string? val) (eq? val pipe:))
			(set! input val)
			(arg-error rest)))
		   ((output:)
		    (if (or (string? val) (eq? val pipe:) (eq? val null:))
			(set! output val)
			(arg-error rest)))
		   ((error:)
		    (if (or (string? val) (eq? val pipe:) (eq? val null:))
			(set! error val)
			(arg-error rest)))
		   ((host:)
		    (if (string? val)
			(set! host val)
			(arg-error rest)))
		   ((env:)
		    (if (string? val)
			(set! env (cons val env))
			(arg-error rest)))
		   (else
		    (arg-error rest)))
		(loop (cdr (cdr rest)))))
	    ((string? (car rest))
	     (set! args (cons (car rest) args))
	     (loop (cdr rest)))
	    (else
	     (arg-error rest))))))

;*---------------------------------------------------------------------*/
;*    close-process-ports ...                                          */
;*---------------------------------------------------------------------*/
(define (close-process-ports proc)
   (if (output-port? (process-input-port proc))
       (close-output-port (process-input-port proc)))
   (if (input-port? (process-error-port proc))
       (close-input-port (process-error-port proc)))
   (if (input-port? (process-output-port proc))
       (close-input-port (process-output-port proc))))

;*---------------------------------------------------------------------*/
;*    unregister-process ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (unregister-process proc)
   ($unregister-process proc))
