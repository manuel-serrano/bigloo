;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/src/Llib/thread.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 11:49:11 2002                          */
;*    Last change :  Fri Dec 13 12:03:39 2013 (serrano)                */
;*    Copyright   :  2002-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public FairThreads implementation.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_thread

   (option (set! *dlopen-init-gc* #t))

   (library pthread)
   
   (import  __ft_types
	    __ft_%types
	    __ft_%thread
	    __ft_scheduler
	    __ft_%scheduler
	    __ft_env
	    __ft_%env
	    __ft_signal
	    __ft_%pthread)
   
   (export  (class join-timeout-exception::&exception))
   
   (export  (thread-await! ::obj . timeout)
	    (thread-await*! ::pair . timeout)
	    (thread-await-values! ::obj . timeout)
	    (thread-await-values*! ::pair . timeout)
	    (thread-get-values! ::obj)
	    (thread-get-values*! ::pair)
	    (thread-suspend! ::fthread)
            (thread-resume! ::fthread)))

;*---------------------------------------------------------------------*/
;*    Initialization for dynamic library loading                       */
;*---------------------------------------------------------------------*/
(library-multithread-set! #t)

;*---------------------------------------------------------------------*/
;*    object-equal? ::%sigjoin ...                                     */
;*---------------------------------------------------------------------*/
(define-method (object-equal? o1::%sigjoin o2)
   (when (isa? o2 %sigjoin)
      (with-access::%sigjoin o1 ((thread1 thread))
	 (with-access::%sigjoin o2 ((thread2 thread))
	    (eq? thread1 thread2)))))

;*---------------------------------------------------------------------*/
;*    thread-join! ::fthread ...                                       */
;*---------------------------------------------------------------------*/
(define-method (thread-join! t::fthread . args)
   (with-access::fthread t (%state %result %exc-result %exc-raised)
      (if (or (eq? %state 'terminated) (eq? %state 'dead))
	  (if %exc-raised
	      (raise %exc-result)
	      %result)
	  (match-case args
	     ((?to ?to-val)
	      (if (not (number? to))
		  (bigloo-type-error "thread-join" "integer" to)
		  (let ((v (thread-await! (instantiate::%sigjoin
					     (thread t))
			      to)))
		     (with-access::fthread t (%exc-result)
			(cond
			   ((isa? %exc-result terminated-thread-exception)
			    (raise %exc-result))
			   (v
			      v)
			   (else
			    to-val))))))
	     ((?to)
	      (if (not (number? to))
		  (bigloo-type-error "thread-join" "integer" to)
		  (let ((v (thread-await! (instantiate::%sigjoin
					     (thread t))
			      to)))
		     (with-access::fthread t (%exc-result)
			(cond
			   ((isa? %exc-result terminated-thread-exception)
			    (raise %exc-result))
			   (v
			      v)
			   (else
			    (raise (instantiate::join-timeout-exception))))))))
	     (else
	      (let ((v (thread-await! (instantiate::%sigjoin
					 (thread t)))))
		 (with-access::fthread t (%exc-result)
		    (cond
		       ((isa? %exc-result terminated-thread-exception)
			(raise %exc-result))
		       (else
			v)))))))))

;*---------------------------------------------------------------------*/
;*    thread-await! ...                                                */
;*---------------------------------------------------------------------*/
(define (thread-await! sig . arg)
   (let* ((t (current-thread)))
      ;; wait until the signal is present
      (define (await scdl::scheduler sig::obj)
	 (with-access::scheduler scdl ((env env+))
	    (cond
	       ((signal-lookup sig env)
		;; the signal is present we return its value
		(signal-value sig env))
	       ((and (isa? sig %sigasync)
		     (with-access::%sigasync sig (spawned)
			(not spawned)))
		;; this is an asynchronous signal not already spawned 
		(with-access::%sigasync sig (spawned id thunk)
		   (set! spawned #t)
		   ;; detach the thread, i.e., make it asynchronous (unfair)
		   (%thread-asynchronize! t id)
		   (let ((res (unwind-protect
				 (thunk)
				 (%thread-synchronize! t))))
		      (%broadcast! scdl sig res)
		      res)))
	       (else
		;; the signal is absent, we block the thread on the signal
		(signal-register-thread! sig env t)
		(%thread-cooperate t)
		;; we reach this point when the signal has been emitted
		(signal-value sig env)))))
      ;; await at most n instants until the signal is present
      (define (await-ntimes scdl::scheduler sig::obj timeout)
	 (if (and (number? timeout) (> timeout 0))
	     (with-access::scheduler scdl ((env env+))
		(cond
		   ((signal-lookup sig env)
		    (signal-value sig env))
		   (else
		    ;; register the thread on the signal
		    (signal-register-thread! sig env t)
		    ;; if we are waiting for an asynchronous signal,
		    ;; it is time to spawn it
		    (if (isa? sig %sigasync)
			(%scheduler-spawn-async scdl sig))
		    ;; we now mark that the current thread is timeout on SIG
		    (%thread-timeout! t timeout)
		    ;; we reach this point when the thread is unblocked
		    (if (signal-lookup sig env)
			;; we have a value for the signal
			(signal-value sig env)
			;; we got unblock because of the timeout
			#f))))
	     (error "thread-await" "Illegal timeout" timeout)))
      (cond
	 ((not (isa? t thread))
	  (error "thread-await" "no such thread" t))
	 ((not (%thread-attached? t))
	  (error "thread-await" "unattached thread" t))
	 ((pair? arg)
	  (with-access::fthread t (scheduler)
	     (await-ntimes scheduler sig (car arg))))
	 (else
	  (with-access::fthread t (scheduler)
	     (await scheduler sig))))))

;*---------------------------------------------------------------------*/
;*    thread-await*! ...                                               */
;*---------------------------------------------------------------------*/
(define (thread-await*! s* . arg)
   (let* ((t (current-thread)))
      ;; await until one of the signals is present
      (define (await* e* s)
	 (with-access::scheduler s ((env env+))
	    (let loop ((es e*))
	       (cond
		  ((null? es)
		   (with-access::fthread t ((scdl scheduler))
		      (for-each (lambda (e)
				   (signal-register-thread! e env t))
				e*)
		      (%thread-cooperate t)
		      (with-access::fthread t (%awake-value %awake-signal)
			 (values %awake-value %awake-signal))))
		  ((signal-lookup (car es) env)
		   (values (signal-value (car es) env) (car es)))
		  (else
		   (loop (cdr es)))))))
      ;; await at most n instants until one of the signals is present
      (define (await*-ntimes e* s timeout)
	 (if (and (number? timeout) (>= timeout 0))
	     (with-access::scheduler s (env+)
		(let loop ((es e*)
			   (stage 'init))
		   (cond
		      ((null? es)
		       (if (eq? stage 'init)
			   (with-access::fthread t (scheduler)
			      (with-access::scheduler scheduler (env+)
				 (for-each (lambda (e)
					      (signal-register-thread! e env+ t))
				    e*))
			      (%thread-timeout! t timeout)
			      (loop e* 'end))
			   (values #f #f)))
		      ((signal-lookup (car es) env+)
		       (values (signal-value (car es) env+) (car es)))
		      (else
		       (loop (cdr es) stage)))))
	     (error "thread-await*" "Illegal timeout" timeout)))
      (cond
	 ((not (isa? t thread))
	  (error "thread-await" "no such thread" t))
	 ((not (%thread-attached? t))
	  (error "thread-await" "unattached thread" t))
	 ((pair? arg)
	  (with-access::fthread t (scheduler)
	     (await*-ntimes s* scheduler (car arg))))
	 (else
	  (with-access::fthread t (scheduler)
	     (await* s* scheduler))))))

;*---------------------------------------------------------------------*/
;*    thread-get-values! ...                                           */
;*---------------------------------------------------------------------*/
(define (thread-get-values! s)
   (let ((t (current-thread)))
      (when (isa? t thread)
	 (%thread-yield! t)
	 (with-access::fthread t (scheduler)
	    (with-access::scheduler scheduler (env+)
	       (signal-last-values s env+))))))

;*---------------------------------------------------------------------*/
;*    thread-get-values*! ...                                          */
;*---------------------------------------------------------------------*/
(define (thread-get-values*! s*)
   (let ((t (current-thread)))
      (when (isa? t thread)
	 (%thread-yield! t)
	 (map (lambda (s)
		 (with-access::fthread t (scheduler)
		    (with-access::scheduler scheduler (env+)
		       (cons s (signal-last-values s env+)))))
	    s*))))

;*---------------------------------------------------------------------*/
;*    thread-await-values! ...                                         */
;*---------------------------------------------------------------------*/
(define (thread-await-values! s . tmt)
   (apply thread-await! s tmt)
   (thread-get-values! s))
   
;*---------------------------------------------------------------------*/
;*    thread-await-values*! ...                                        */
;*---------------------------------------------------------------------*/
(define (thread-await-values*! s* . tmt)
   (apply thread-await*! s* tmt)
   (thread-get-values*! s*))
   
;*---------------------------------------------------------------------*/
;*    object-write ::fthread ...                                       */
;*---------------------------------------------------------------------*/
(define-method (object-write o::fthread . port)
   (with-output-to-port (if (pair? port) (car port) (current-output-port))
      (lambda ()
	 (with-access::fthread o (name %state %timeout %signals)
	    (display* "#<fthread:" name
		      " state=" %state
		      " timeout=" %timeout
		      " signals=" %signals
		      ">")))))

;*---------------------------------------------------------------------*/
;*    thread-start! ::fthread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (thread-start! t::fthread . o)
   (with-trace 2 "thread-start!"
      (trace-item "thread=" (trace-bold t))
      (trace-item "o=" (trace-string o))
      ;; check that the thread is not already attached to a scheduler
      ;; (i.e. the thread is not already running in another scheduler)
      (if (%thread-attached? t)
	  (error "thread-start!" "thread already running" t)
	  (let ((scdl (%get-optional-scheduler 'thread-start! o)))
	     ;; attach the thread to the scheduler
	     (with-access::fthread t (scheduler %state %builtin)
		(set! scheduler scdl)
		(set! %builtin (%pthread-new t))
		(set! %state 'started)
		;; start the builtin thread
		(thread-start! %builtin))
	     ;; adding a thread only appends it to the list of thread
	     ;; to be started at the next scheduler instant
	     (with-access::%scheduler scdl (tostart %live-thread-number)
		;; increment the number of live threads
		(set! %live-thread-number (+fx 1 %live-thread-number))
		;; thread are append in the inverse order
		(set! tostart (cons t tostart)))
	     ;; return the started thread
	     t))))
   
;*---------------------------------------------------------------------*/
;*    thread-terminate! ::fthread ...                                  */
;*---------------------------------------------------------------------*/
(define-method (thread-terminate! t::fthread)
   (with-access::fthread t (scheduler %exc-result)
      (with-trace 3 '%thread-terminate!
	 (trace-item "t=" (trace-string t))
	 (trace-item "attached?=" (%thread-attached? t))
	 (trace-item "dead?=" (%thread-is-dead t))
	 (trace-item "toterminate?=" (%thread-is-toterminate t))
	 (trace-item "terminated?=" (%thread-is-terminated t)))
      (cond
	 ((not (%thread-attached? t))
	  ;; it is an error to terminate an unattached thread
	  (error 'thread-terminate! "Unattached thread" t))
	 ((%thread-is-dead t)
	  ;; nothing to do, it is already dead!
	  #unspecified)
	 ((%thread-is-toterminate t)
	  ;; the thread has already been terminated in the instant, we skip
	  #unspecified)
	 (else
	  ;; mark that the thread is terminated (which is not dead)
	  (%thread-is-toterminate t #t)
	  ;; mark the termination result
	  (set! %exc-result (instantiate::terminated-thread-exception))
	  ;; adding a thread simply append it to the list of thread
	  ;; to be stoped at the next scheduler instant
	  (with-access::%scheduler scheduler (toterminate)
	     ;; thread are append in the inverse order.
	     (set! toterminate (cons t toterminate)))))))

;*---------------------------------------------------------------------*/
;*    thread-suspend/resume! ...                                       */
;*---------------------------------------------------------------------*/
(define (thread-suspend/resume! t who val)
   (cond
      ((not (%thread-attached? t))
       ;; it is an error to terminate an unattached thread
       (error who "Unattached thread" t))
      ((or (%thread-is-toterminate t)
	   (%thread-is-terminated t)
	   (%thread-is-dead t))
       ;; the thread is dead or terminated, nothing to do
       #unspecified)
      (else
       (with-access::fthread t (scheduler)
	  ;; adding a thread simply append it to the list of thread
	  ;; to be stoped at the next scheduler instant
	  (with-access::%scheduler scheduler (tosuspend/resume)
	     (set! tosuspend/resume (cons (cons t val) tosuspend/resume)))))))

;*---------------------------------------------------------------------*/
;*    thread-suspend! ...                                              */
;*---------------------------------------------------------------------*/
(define (thread-suspend! t)
   (thread-suspend/resume! t 'thread-suspend! #t))

;*---------------------------------------------------------------------*/
;*    thread-resume! ...                                               */
;*---------------------------------------------------------------------*/
(define (thread-resume! t)
   (thread-suspend/resume! t 'thread-resume! #f))

;*---------------------------------------------------------------------*/
;*    thread-specific ::fthread ...                                    */
;*---------------------------------------------------------------------*/
(define-method (thread-specific th::fthread)
   (with-access::fthread th (%specific)
      %specific))

;*---------------------------------------------------------------------*/
;*    thread-specific-set! ::fthread ...                               */
;*---------------------------------------------------------------------*/
(define-method (thread-specific-set! th::fthread v)
   (with-access::fthread th (%specific)
      (set! %specific v)))

;*---------------------------------------------------------------------*/
;*    thread-cleanup ::fthread ...                                     */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup th::fthread)
   (with-access::fthread th (%cleanup)
      %cleanup))

;*---------------------------------------------------------------------*/
;*    thread-cleanup-set! ::fthread ...                                */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup-set! th::fthread v)
   (with-access::fthread th (%cleanup)
      (set! %cleanup v)))
