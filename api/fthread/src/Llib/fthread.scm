

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __fthread
   (library pthread)
   
   (import  __ft_types
	    __ft_%types
	    __ft_%thread
	    __ft_scheduler
	    __ft_%scheduler
            __ft_signal
	    __ft_backend)
   
   (export
    ($fscheduler-new ::procedure ::obj)
    ($fthread-new ::fthread)
    (inline $fthread-start ::$fthread ::obj)
    ($fthread-wait ::$fthread)
    ($fthread-switch ::obj ::$fthread)
    ($fthread-enter-scheduler ::$fthread)
    ($fthread-leave-scheduler ::$fthread))

   (export ;; asynchronous threads
    ($async-spawn ::$fthread ::procedure ::obj)
    ($async-synchronize ::$fthread)
    ($async-asynchronize ::$fthread)
    ($async-scheduler-wait ::$fthread)
    ($async-scheduler-notify ::$fthread)))


;*---------------------------------------------------------------------*/
;*    *$fthread-global-lock* ...                                       */
;*---------------------------------------------------------------------*/
(define *$fthread-global-lock* (make-mutex))

;*---------------------------------------------------------------------*/
;*    *$fthread-global-cv* ...                                         */
;*---------------------------------------------------------------------*/
(define *$fthread-global-cv* (make-condition-variable))


;*---------------------------------------------------------------------*/
;*    *token* ...                                                      */
;*---------------------------------------------------------------------*/
; The current $fthread, the value #f means the main() thread
; is currently running, as it is not a $thread object.
(define *token* #f)

;*---------------------------------------------------------------------*/
;*    current-$fthread ...                                             */
;*---------------------------------------------------------------------*/
; Get the $fthread object associated with the current native thread
(define-inline (current-$fthread)
   (thread-parameter 'fth))


;*---------------------------------------------------------------------*/
;*    $fscheduler-new ...                                              */
;*---------------------------------------------------------------------*/
(define ($fscheduler-new body name)
   (with-trace 4 "$fscheduler-new"
      (letrec ((fth (instantiate::$fthread
		       (thread pth)))
	       (pth (instantiate::pthread
		       (body (lambda()
				(thread-parameter-set! 'fth fth)
				($fthread-wait fth)
				(body)))
		       (name name))))
	 (trace-item "fth=" (trace-string fth))
	 (trace-item "pth=" (trace-string pth))
	 fth)))

;*---------------------------------------------------------------------*/
;*    $fthread-new ...                                                 */
;*---------------------------------------------------------------------*/
(define ($fthread-new fthread::fthread)
   (define (execute-thread t)
      (with-access::fthread t (%state %result cleanup scheduler body)
	 ;; terminate is used to abruptly terminate a thread
	 (bind-exit (terminate)
	    (fthread-%terminate-set! t terminate)
	    (with-exception-handler
	       (lambda (e)
		  (let ((u (instantiate::uncaught-exception
			      (reason e))))
		     (fthread-%exc-result-set! t u)
		     (exception-notify e)
		     (terminate #f)))
	       (lambda ()
		  ;; store the result of the thread
		  (set! %result (body)))))
	 ;; broadcast a signal for the thread termination
	 (broadcast! (instantiate::%sigjoin (thread t)) %result)
	 ;; invoke the thread cleanup
	 (if (procedure? cleanup)
	     (if (correct-arity? cleanup 1)
		 (cleanup t)
		 (error t "Illegal cleanup function" cleanup)))
	 ;; kill the thread is now dead and switch back to the scheduler
	 (%thread-kill! t)))
   
   (with-trace 4 "$fthread-new"
      (letrec ((fth (instantiate::$fthread
		       (thread pth)))
	       (pth (instantiate::pthread
		       (body (lambda ()
				(thread-parameter-set! 'fth fth)
				($fthread-wait fth)
				(execute-thread fthread)))
		       (name (fthread-name fthread)))))
	 (trace-item "fth=" (trace-string fth))
	 fth)))

; Starting a $fthread is starting the built-in pthread
;*---------------------------------------------------------------------*/
;*    $fthread-start ...                                               */
;*---------------------------------------------------------------------*/
(define-inline ($fthread-start ft::$fthread o::obj)
   (thread-start! ($fthread-thread ft)))


;*---------------------------------------------------------------------*/
;*    $fthread-wait ...                                                */
;*---------------------------------------------------------------------*/
(define ($fthread-wait ft::$fthread)
   (with-trace 4 "$fthread-wait"
      (with-access::$fthread ft (mutex condvar)
	 (trace-item "wait on thread " (trace-string ft))
	 (trace-item "cv=" (trace-string condvar))
	 (trace-item "mutex=" (trace-string mutex))

	 (mutex-lock! mutex)
	 (let loop ()
	    (unless (eq? *token* ft)
	       (condition-variable-wait! condvar mutex)
	       (loop)))
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    $fthread-switch ...                                              */
;*---------------------------------------------------------------------*/
(define ($fthread-switch ft nt::$fthread)
   (with-trace 4 "$fthread-switch"
      (trace-item "from=" (trace-string ft))
      (trace-item "to=" (trace-string nt))
      
      (with-access::$fthread nt (mutex condvar)
	 (mutex-lock! mutex)
	 (set! *token* nt)
	 (trace-item "signal! on " (trace-string condvar))
	 (condition-variable-signal! condvar)
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    $fthread-enter-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define ($fthread-enter-scheduler scdl::$fthread)
   (with-trace 4 "$fthread-enter-scheduler"
      (let ((this (current-$fthread)))
	 
	 ($fthread-parent-set! scdl this)
	 
	 (let ((mutex (if this
			  ($fthread-mutex this)
			  *$fthread-global-lock*))
	       (condvar (if this
			    ($fthread-condvar this)
			    *$fthread-global-cv*)))
	    
	    ($fthread-switch this scdl)
	    
	    (mutex-lock! mutex)
	    (let loop ()
	       (unless (eq? *token* this)
		  (trace-item "wait on " (trace-string condvar))
		  (condition-variable-wait! condvar mutex)
		  (loop)))
	    (mutex-unlock! mutex)))))

;*---------------------------------------------------------------------*/
;*    $fthread-leave-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define ($fthread-leave-scheduler scdl::$fthread)
   (with-trace 4 "$fthread-leave-scheduler"
      (let ((this ($fthread-parent scdl)))
	 
	 (let ((mutex (if this
			  ($fthread-mutex this)
			  *$fthread-global-lock*))
	       (condvar (if this
			    ($fthread-condvar this)
			    *$fthread-global-cv*)))
	    
	    (set! *token* #f)
	    
	    (mutex-lock! mutex)
	    (set! *token* this)
	    (condition-variable-signal! condvar)
	    (mutex-unlock! mutex))
	 
	 ($fthread-wait scdl))))


;;; ASYNCHRONOUS THREADS

;*---------------------------------------------------------------------*/
;*    $async-spawn ...                                                 */
;*---------------------------------------------------------------------*/
(define ($async-spawn scdl::$fthread body::procedure o::obj)
   (with-trace 4 "$async-spawn"
      (trace-item "scdl=" (trace-string scdl))
      (trace-item "body=" (trace-string body))
      (trace-item "o=" (trace-string o))
      (letrec ((fth (instantiate::$fthread
		       (thread pth)
		       (id o)))
	       (pth (instantiate::pthread
		       (body (lambda()
				(thread-parameter-set! 'fth fth)
				(body)))
		       (name (symbol-append (gensym 'async) o)))))
	 (thread-start! ($fthread-thread fth))
	 fth)))

;*---------------------------------------------------------------------*/
;*    $async-synchronize ...                                           */
;*---------------------------------------------------------------------*/
(define ($async-synchronize scdl::$fthread)
   (with-trace 4 "$async-synchronize"
      (trace-item "scdl=" (trace-string scdl))
      (mutex-lock! ($fthread-mutex scdl))))

;*---------------------------------------------------------------------*/
;*    $async-asynchronize ...                                          */
;*---------------------------------------------------------------------*/
(define ($async-asynchronize scdl::$fthread)
   (with-trace 4 "$async-asynchronize"
      (trace-item "scdl=" (trace-string scdl))
      (mutex-unlock! ($fthread-mutex scdl))))

;*---------------------------------------------------------------------*/
;*    $async-scheduler-wait ...                                        */
;*---------------------------------------------------------------------*/
(define ($async-scheduler-wait scdl::$fthread)
   (with-trace 4 "$async-scheduler-wait"
      (trace-item "scdl=" (trace-string scdl))
      (condition-variable-wait! ($fthread-condvar scdl)
				($fthread-mutex scdl))))

;*---------------------------------------------------------------------*/
;*    $async-scheduler-notify ...                                      */
;*---------------------------------------------------------------------*/
(define ($async-scheduler-notify scdl::$fthread)
   (with-trace 4 "$async-scheduler-notify"
      (trace-item "scdl=" (trace-string scdl))
      (condition-variable-signal! ($fthread-condvar scdl))))

;*---------------------------------------------------------------------*/
;*    object-write ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-write o::$fthread . port)
   (with-output-to-port (if (and (pair? port) (output-port? (car port)))
			    (car port)
			    (current-output-port))
      (lambda ()
	 (with-access::$fthread o (id parent thread)
	    ; Just to avoid the use of display-circle, in case of self reference
	    (let ((the-parent (if (eq? o parent)
				  "*self*"
				  parent)))
	       (display* "#<$fthread:" id
			 " parent:" the-parent
			 " thread:" thread
			 ">"))))))

;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-method (object-display o::$fthread . port)
   (object-write o port))
