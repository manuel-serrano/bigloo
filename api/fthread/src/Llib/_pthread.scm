

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
   
   (export  (%fscheduler-new ::%scheduler)
	    (%pthread-new ::fthread)
	    (%pthread-wait ::%pthread)
	    (%pthread-switch ::obj ::%pthread)
	    (%pthread-enter-scheduler ::%pthread)
	    (%pthread-leave-scheduler ::%pthread)

	    ;; asynchronous threads
	    (%async-spawn ::%pthread ::procedure ::obj)
	    (%async-synchronize ::%pthread)
	    (%async-asynchronize ::%pthread)
	    (%async-scheduler-wait ::%pthread)
	    (%async-scheduler-notify ::%pthread)))

;*---------------------------------------------------------------------*/
;*    *scheduler-current-token & %get-scheduler-token ...              */
;*---------------------------------------------------------------------*/
(define *scheduler-current-token* #f)

(define (%get-scheduler-token)
   (let ((scdl (current-scheduler)))
      (if (%scheduler? scdl)
	  (%scheduler-current-token scdl)
	  *scheduler-current-token*)))

(define (%set-scheduler-token! token)
   (let ((scdl (current-scheduler)))
      (if (%scheduler? scdl)
	  (%scheduler-current-token-set! scdl token)
	  (set! *scheduler-current-token* token))))

;*---------------------------------------------------------------------*/
;*    $fscheduler-new ...                                              */
;*---------------------------------------------------------------------*/
(define (%fscheduler-new scdl::%scheduler)
   (with-trace 4 "%fscheduler-new"
      (with-access::%scheduler scdl (body name)
	 (letrec ((%pth (instantiate::%pthread
			   (body (lambda () #unspecified))
			   (name name)
			   (fthread scdl))))
	    (trace-item "%pth=" (trace-string %pth))
	    %pth))))

;*---------------------------------------------------------------------*/
;*    %pthread-new ...                                                 */
;*---------------------------------------------------------------------*/
(define (%pthread-new ft::fthread)
   (define (execute-thread t)
      (with-access::fthread t (%state %result %cleanup scheduler body)
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
	 (if (procedure? %cleanup)
	     (if (correct-arity? %cleanup 1)
		 (%cleanup t)
		 (error t "Illegal cleanup function" %cleanup)))
	 ;; kill the thread is now dead and switch back to the scheduler
	 (%thread-kill! t)))
   
   (with-trace 4 "%pthread-new"
      (letrec ((%pth (instantiate::%pthread
			(fthread ft)
			(body (lambda ()
				 (default-scheduler (fthread-scheduler ft))
				 (%pthread-wait %pth)
				 (execute-thread ft)))
			(name (fthread-name ft)))))
	 (trace-item "%pth=" (trace-string %pth))
	 %pth)))

;*---------------------------------------------------------------------*/
;*    %pthread-wait ...                                                */
;*---------------------------------------------------------------------*/
(define (%pthread-wait ft::%pthread)
   (with-trace 4 "%pthread-wait"
      (with-access::%pthread ft (mutex condvar)
	 (trace-item "wait on thread " (trace-string ft))
	 (trace-item "mutex=" (trace-string mutex))
	 (trace-item "cv=" (trace-string condvar))

	 (mutex-lock! mutex)
	 (let loop ()
	    (unless (eq? (%get-scheduler-token) ft)
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
	 (%set-scheduler-token! nt)
	 (trace-item "signal! on " (trace-string condvar))
	 (condition-variable-signal! condvar)
	 (mutex-unlock! mutex))))

;*---------------------------------------------------------------------*/
;*    %pthread-enter-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define (%pthread-enter-scheduler scdl::%pthread)
   (with-trace 4 "%pthread-enter-scheduler"
      (trace-item "scdl=" (trace-string scdl))
      
      (with-access::%pthread scdl (parent fthread)
	 
	 ; Find the parent thread in the creation hierarchy, is not set
	 (when (not parent)
	    (let ((th (current-thread)))
	       (cond
		  ; Only to catch %pthread objects, as they are also pthread
		  ((%pthread? th)
		   (error '%pthread-enter-scheduler
			  "Bogus (current-thread) procedure"
			  th))
		  ; A scheduler scheduling another one, ignore
		  ((scheduler? th)
		   #unspecified)
		  ; A fair thread calling the scheduler, ignore
		  ((fthread? th)
		   #unspecified)
		  ; A native posix thread
		  ((pthread? th)
		   (set! parent th))
		  ; #f, means main() entry point
		  ((and (boolean? th) (not th))
		   (set! parent #f))
		  ; don't know what to do here
		  (else
		   (error '%pthread-enter-scheduler
			  "Undefined current-thread"
			  (find-runtime-type th)))))
	    (trace-item "setting parent to: " (trace-string parent)))

	 ; As the scheduleding is done by the calling thread, set the parameter
	 (when (not (scheduler? (current-scheduler)))
	    (current-scheduler-set! fthread))

	 ; Runs the scheduler's body
	 ((scheduler-body fthread)))))

;*---------------------------------------------------------------------*/
;*    %pthread-leave-scheduler ...                                     */
;*---------------------------------------------------------------------*/
(define (%pthread-leave-scheduler scdl::%pthread)
   (with-trace 4 "%pthread-leave-scheduler"
      (trace-item "scdl=" (trace-string scdl))

      (when (not (fthread? (current-thread)))
	 (current-scheduler-set! #f)
	 (%pthread-parent-set! scdl #f))
      #f))


;;; ASYNCHRONOUS THREADS

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
	 (with-access::%pthread o (name parent)
	    ; Just to avoid the use of display-circle, in case of self reference
	    (let ((the-parent (if (eq? o parent) "*self*" parent)))
	       (display* "#<%pthread:" name " parent:" the-parent ">"))))))

;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-method (object-display o::%pthread . port)
   (apply object-write o port))


;*---------------------------------------------------------------------*/
;*    current-fthread ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (%current-fthread)
   (let ((cs (current-scheduler)))
      (if (scheduler? cs)
	  (%scheduler-current-thread cs)
	  (let ((ds (default-scheduler)))
	     (if (scheduler? ds)
		 (%scheduler-current-thread ds))))))

;*---------------------------------------------------------------------*/
;*    %user-current-thread ...                                         */
;*---------------------------------------------------------------------*/
(define-method (%user-current-thread o::%pthread)
   (if (fthread? (%pthread-fthread o))
       (%pthread-fthread o)
       (%current-fthread)))

;*---------------------------------------------------------------------*/
;*    %user-thread-yield! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-yield! o::%pthread)
   (let ((fth (%pthread-fthread o)))
      (when fth
	 (%thread-yield! fth))))

;*---------------------------------------------------------------------*/
;*    %user-thread-sleep! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-sleep! o::%pthread timeout)
   (let ((fth (%pthread-fthread o)))
      (when (and (fthread? fth)
		 (>fx timeout 0))
	 (%thread-timeout! fth timeout))))


;*---------------------------------------------------------------------*/
;*    %user-current-thread ...                                         */
;*---------------------------------------------------------------------*/
(define-method (%user-current-thread o::fthread)
   o)

;*---------------------------------------------------------------------*/
;*    %user-thread-yield! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-yield! o::fthread)
   (%thread-yield! o))

;*---------------------------------------------------------------------*/
;*    %user-thread-sleep! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-sleep! o::fthread timeout)
   (when (>fx timeout 0) (%thread-timeout! o timeout)))
