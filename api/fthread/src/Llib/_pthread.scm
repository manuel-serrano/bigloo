;*=====================================================================*/
;*    .../prgm/project/bigloo/api/fthread/src/Llib/_pthread.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Cyprien Nicolas                                   */
;*    Creation    :  Fri Jul 17 16:42:27 2009                          */
;*    Last change :  Fri Jul 17 17:17:36 2009 (serrano)                */
;*    Copyright   :  2009 Cyprien Nicolas, Manuel Serrano              */
;*    -------------------------------------------------------------    */
;*    pthread backend for fthreads                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_%pthread
   
   (library pthread)
   
   (import  __ft_types
	    __ft_%types
	    __ft_%thread
	    __ft_scheduler
	    __ft_%scheduler
            __ft_signal)
   
   (export  (%fscheduler-new ::procedure ::obj)
	    (%pthread-new ::fthread)
	    (inline %pthread-start ::%pthread)
	    (%pthread-wait ::%pthread)
	    (%pthread-switch ::obj ::%pthread)
	    (%pthread-enter-scheduler ::%pthread)
	    (%pthread-leave-scheduler ::%pthread)
	    
	    ;; asynchronous threads
	    (%async-spawn ::%pthread ::procedure ::obj)
	    (%async-synchronize ::%pthread)
	    (%async-asynchronize ::%pthread)
	    (%async-scheduler-wait ::%pthread)
	    (%async-scheduler-notify ::%pthread)
	    
	    (inline current-fthread)))

;*---------------------------------------------------------------------*/
;*    *%pthread-global-lock* ...                                       */
;*---------------------------------------------------------------------*/
(define *%pthread-global-lock* (make-mutex))

;*---------------------------------------------------------------------*/
;*    *%pthread-global-cv* ...                                         */
;*---------------------------------------------------------------------*/
(define *%pthread-global-cv* (make-condition-variable))

;*---------------------------------------------------------------------*/
;*    *token* ...                                                      */
;*    -------------------------------------------------------------    */
;*    The current %pthread, the value #f means the main() thread       */
;*    is currently running, as it is not a $thread object.             */
;*---------------------------------------------------------------------*/
(define *token* #f)

;*---------------------------------------------------------------------*/
;*    $fscheduler-new ...                                              */
;*---------------------------------------------------------------------*/
(define (%fscheduler-new body name)
   (with-trace 4 "%fscheduler-new"
      (letrec ((%pth (instantiate::%pthread
			(body (lambda ()
				 (%pthread-wait %pth)
				 (body)))
			(name name))))
	 (trace-item "%pth=" (trace-string %pth))
	 %pth)))

;*---------------------------------------------------------------------*/
;*    %pthread-new ...                                                 */
;*---------------------------------------------------------------------*/
(define (%pthread-new ft::fthread)
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
   
   (with-trace 4 "%pthread-new"
      (letrec ((%pth (instantiate::%pthread
			(fthread ft)
			(body (lambda ()
				 (%pthread-wait %pth)
				 (execute-thread ft)))
			(name (fthread-name ft)))))
	 (trace-item "%pth=" (trace-string %pth))
	 %pth)))

;*---------------------------------------------------------------------*/
;*    %pthread-start ...                                               */
;*    -------------------------------------------------------------    */
;*    Starting a %pthread is starting the built-in pthread             */
;*---------------------------------------------------------------------*/
(define-inline (%pthread-start %pth::%pthread)
   (thread-start! %pth))

;*---------------------------------------------------------------------*/
;*    %pthread-wait ...                                                */
;*---------------------------------------------------------------------*/
(define (%pthread-wait ft::%pthread)
   (with-trace 4 "%pthread-wait"
      (with-access::%pthread ft (mutex condvar)
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
;*    %pthread-switch ...                                              */
;*---------------------------------------------------------------------*/
(define (%pthread-switch ft nt::%pthread)
   (with-trace 4 "%pthread-switch"
      (trace-item "from=" (trace-string ft))
      (trace-item "to=" (trace-string nt))
      
      (with-access::%pthread nt (mutex condvar)
	 (mutex-lock! mutex)
	 (set! *token* nt)
	 (trace-item "signal! on " (trace-string condvar))
	 (condition-variable-signal! condvar)
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    %pthread-enter-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define (%pthread-enter-scheduler scdl::%pthread)
   (with-trace 4 "%pthread-enter-scheduler"
      (let ((this (and (current-fthread)
		       (fthread-%builtin (current-fthread)))))
	 
	 (%pthread-parent-set! scdl this)
	 
	 (let ((mutex (if this
			  (%pthread-mutex this)
			  *%pthread-global-lock*))
	       (condvar (if this
			    (%pthread-condvar this)
			    *%pthread-global-cv*)))
	    
	    (%pthread-switch this scdl)
	    
	    (mutex-lock! mutex)
	    (let loop ()
	       (unless (eq? *token* this)
		  (trace-item "wait on " (trace-string condvar))
		  (condition-variable-wait! condvar mutex)
		  (loop)))
	    (mutex-unlock! mutex)))))

;*---------------------------------------------------------------------*/
;*    %pthread-leave-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define (%pthread-leave-scheduler scdl::%pthread)
   (with-trace 4 "%pthread-leave-scheduler"
      (let ((this (%pthread-parent scdl)))
	 
	 (let ((mutex (if this
			  (%pthread-mutex this)
			  *%pthread-global-lock*))
	       (condvar (if this
			    (%pthread-condvar this)
			    *%pthread-global-cv*)))
	    
	    (set! *token* #f)
	    
	    (mutex-lock! mutex)
	    (set! *token* this)
	    (condition-variable-signal! condvar)
	    (mutex-unlock! mutex))
	 
	 (%pthread-wait scdl))))

;*---------------------------------------------------------------------*/
;*    %async-spawn ...                                                 */
;*---------------------------------------------------------------------*/
(define (%async-spawn scdl::%pthread body::procedure o::obj)
   (with-trace 4 "%async-spawn"
      (trace-item "scdl=" (trace-string scdl))
      (trace-item "body=" (trace-string body))
      (trace-item "o=" (trace-string o))
      (letrec ((%pth (instantiate::%pthread
			(id o)
			(body body)
			(name (symbol-append (gensym 'async) o)))))
	 (thread-start! %pth)
	 %pth)))

;*---------------------------------------------------------------------*/
;*    %async-synchronize ...                                           */
;*---------------------------------------------------------------------*/
(define (%async-synchronize scdl::%pthread)
   (with-trace 4 "%async-synchronize"
      (trace-item "scdl=" (trace-string scdl))
      (mutex-lock! (%pthread-mutex scdl))))

;*---------------------------------------------------------------------*/
;*    %async-asynchronize ...                                          */
;*---------------------------------------------------------------------*/
(define (%async-asynchronize scdl::%pthread)
   (with-trace 4 "%async-asynchronize"
      (trace-item "scdl=" (trace-string scdl))
      (mutex-unlock! (%pthread-mutex scdl))))

;*---------------------------------------------------------------------*/
;*    %async-scheduler-wait ...                                        */
;*---------------------------------------------------------------------*/
(define (%async-scheduler-wait scdl::%pthread)
   (with-trace 4 "%async-scheduler-wait"
      (trace-item "scdl=" (trace-string scdl))
      (condition-variable-wait! (%pthread-condvar scdl)
				(%pthread-mutex scdl))))

;*---------------------------------------------------------------------*/
;*    %async-scheduler-notify ...                                      */
;*---------------------------------------------------------------------*/
(define (%async-scheduler-notify scdl::%pthread)
   (with-trace 4 "%async-scheduler-notify"
      (trace-item "scdl=" (trace-string scdl))
      (condition-variable-signal! (%pthread-condvar scdl))))

;*---------------------------------------------------------------------*/
;*    object-write ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-write o::%pthread . port)
   (with-output-to-port (if (and (pair? port) (output-port? (car port)))
			    (car port)
			    (current-output-port))
      (lambda ()
	 (with-access::%pthread o (id parent fthread)
	    ; Just to avoid the use of display-circle, in case of self reference
	    (let ((the-parent (if (eq? o parent)
				  "*self*"
				  parent)))
	       (display* "#<%pthread:" id
			 " fthread:" fthread
			 " parent:" the-parent)
	       (call-next-method)
	       (display ">"))))))

;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-method (object-display o::%pthread . port)
   (object-write o port))

;*---------------------------------------------------------------------*/
;*    current-fthread ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (current-fthread)
   (when (scheduler? (current-scheduler))
      (%scheduler-current-thread (current-scheduler))))

;*---------------------------------------------------------------------*/
;*    %user-current-thread ...                                         */
;*---------------------------------------------------------------------*/
(define-method (%user-current-thread o::%pthread)
   (or (%pthread-fthread o)
       (current-fthread)))

;*---------------------------------------------------------------------*/
;*    %user-thread-yield! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-yield! o::%pthread)
   (error '%user-thread-yield "should be here" o))

;*---------------------------------------------------------------------*/
;*    %user-thread-yield! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-yield! o::fthread)
   (%thread-yield! o))

;*---------------------------------------------------------------------*/
;*    %user-thread-sleep! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-sleep! o::%pthread timeout)
   (let ((fth (%pthread-fthread o)))
      (when (and fth
		 (>fx timeout 0))
	 (%thread-timeout! fth timeout))))

;*---------------------------------------------------------------------*/
;*    %user-thread-sleep! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-sleep! o::fthread timeout)
   (when (>fx timeout 0) (%thread-timeout! o timeout)))
