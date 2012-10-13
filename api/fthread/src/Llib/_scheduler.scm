;*=====================================================================*/
;*    .../prgm/project/bigloo/api/fthread/src/Llib/_scheduler.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 29 07:51:57 2003                          */
;*    Last change :  Sat Oct 13 07:57:26 2012 (serrano)                */
;*    Copyright   :  2003-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The private scheduler implementation.                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_%scheduler
   
   (library pthread)
   
   (import  __ft_types
	    __ft_%types
	    __ft_scheduler
	    __ft_%thread
	    __ft_env
	    __ft_%env
	    __ft_signal
	    __ft_%pthread)
   
   (export  (%get-optional-scheduler::scheduler ::symbol ::pair-nil)
	    (%schedule-instant ::%scheduler)
	    (%broadcast! scdl::%scheduler ::obj ::obj)
	    (%scheduler-next-thread ::fthread ::scheduler)
	    (%scheduler-switch-to-next-thread ::fthread ::scheduler)
	    (%scheduler-time ::scheduler)
	    (%scheduler-add-async-runnable! ::%scheduler ::fthread)
	    (%scheduler-add-async! ::%scheduler ::%sigasync)
	    (%scheduler-spawn-async ::%scheduler ::%sigasync)
	    (%scheduler-add-broadcast! ::%scheduler ::obj ::obj)
	    (%scheduler-waiting-threads::pair-nil ::%scheduler)))

;*---------------------------------------------------------------------*/
;*    synchronize ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (synchronize scdl . body)
   (let ((res (gensym)))
      `(with-access::%scheduler ,scdl (%builtin)
	  (%async-synchronize %builtin)
	  (let ((,res (begin ,@body)))
	     (%async-asynchronize %builtin)
	     ,res))))

;*---------------------------------------------------------------------*/
;*    %get-optional-scheduler ...                                      */
;*---------------------------------------------------------------------*/
(define (%get-optional-scheduler who o)
   (cond
      ((null? o)
       (let ((s (default-scheduler)))
	  (if (isa? s scheduler)
	      s
	      (default-scheduler (make-scheduler)))))
      ((isa? (car o) scheduler)
       (car o))
      (else
       (error who "Illegal scheduler" (car o)))))

;*---------------------------------------------------------------------*/
;*    %scheduler-get-async-runnable ...                                */
;*    -------------------------------------------------------------    */
;*    Aquires the async lock and then returns the contents of async    */
;*    runnable. If the optional argument is provided, its value        */
;*    replaces the former value of the list.                           */
;*---------------------------------------------------------------------*/
(define (%scheduler-get-async-runnable scdl::%scheduler . nv)
   (synchronize scdl
      ;;; the synchronized body
      (let ((v (with-access::%scheduler scdl (async-runnable) async-runnable)))
	 (when (pair? nv)
	    (with-access::%scheduler scdl (async-runnable)
	       (set! async-runnable (car nv))))
	 v)))

;*---------------------------------------------------------------------*/
;*    %scheduler-add-async-runnable! ...                               */
;*    -------------------------------------------------------------    */
;*    Aquires the async lock and then adds a thread to the async       */
;*    runnable list                                                    */
;*---------------------------------------------------------------------*/
(define (%scheduler-add-async-runnable! scdl::%scheduler t::fthread)
   (with-trace 2 '%scheduler-add-async-runnable
      (trace-item "t=" t)
      (synchronize scdl
          ;;; the synchronized body
	  (with-access::%scheduler scdl (%builtin async-runnable)
	     (set! async-runnable (cons t async-runnable))
	     (%async-scheduler-notify %builtin)
	     #unspecified))))

;*---------------------------------------------------------------------*/
;*    %scheduler-add-async! ...                                        */
;*---------------------------------------------------------------------*/
(define (%scheduler-add-async! scdl::%scheduler sig::%sigasync)
   (with-access::%scheduler scdl (tospawn)
      (set! tospawn (cons sig tospawn))))

;*---------------------------------------------------------------------*/
;*    %scheduler-next-thread ...                                       */
;*---------------------------------------------------------------------*/
(define (%scheduler-next-thread t::fthread scdl::scheduler)
   (with-access::%scheduler scdl (threads-runnable
				  threads-runnable-last-pair
				  threads-yield
				  current-thread)
      (with-trace 2 '%scheduler-next-thread
	 (%scheduler-next-thread-debug t scdl)
	 (let loop ((runnable threads-runnable))
	    (if (pair? runnable)
		;; there is another thread to execute...
		(let ((nt (car runnable)))
		   (cond
		      ((with-access::fthread nt (%is-suspend) %is-suspend)
		       ;; it is suspended
		       (set! threads-yield (cons nt threads-yield))
		       (loop (cdr runnable)))
		      ((%thread-is-dead nt)
		       (loop (cdr runnable)))
		      (else
		       (set! threads-runnable (cdr runnable))
		       (when (null? threads-runnable)
			  (set! threads-runnable-last-pair '()))
		       (with-access::%scheduler scdl (current-thread)
			  (set! current-thread nt))
		       nt)))
		(let ((async (%scheduler-get-async-runnable scdl '())))
		   (if (pair? async)
		       (let ((nt (car async)))
			  (set! threads-runnable (cdr async))
			  (if (pair? threads-runnable)
			      (set! threads-runnable-last-pair
				    (last-pair threads-runnable))
			      (set! threads-runnable-last-pair '()))
			  (with-access::%scheduler scdl (current-thread)
			     (set! current-thread nt))
			  nt)
		       ;; there is no more threads, this is the end of the 
		       ;; (the micro instant), we switch back to the scheduler
		       scdl)))))))

;*---------------------------------------------------------------------*/
;*    %scheduler-next-thread-debug ...                                 */
;*---------------------------------------------------------------------*/
(define (%scheduler-next-thread-debug t scdl)
   (with-output-to-port (current-error-port)
      (lambda ()
	 (with-access::%scheduler scdl (threads-runnable
					threads-yield
					threads-timeout
					%live-thread-number)
	    (trace-item "%live-thread-number=" %live-thread-number)
	    (trace-item "runnable=" (trace-string threads-runnable))
	    (trace-item "yield=" (trace-string threads-yield))
	    (trace-item "timeout=" (trace-string threads-timeout))
	    (trace-item "async-runnable="
			(trace-string
			 (%scheduler-get-async-runnable scdl)))))))
   
;*---------------------------------------------------------------------*/
;*    %scheduler-switch-to-next-thread ...                             */
;*    -------------------------------------------------------------    */
;*    Switches to the next threads or ends the current instant.        */
;*---------------------------------------------------------------------*/
(define (%scheduler-switch-to-next-thread t::fthread scdl::scheduler)
   (with-access::fthread t (%builtin %state)
      (with-trace 3 '%scheduler-switch-to-next-thread
	 (let ((nt (%scheduler-next-thread t scdl)))
	    (%scheduler-switch-to-next-thread-debug t scdl nt)
	    (with-access::fthread nt ((fbuiltin %builtin))
	       (%pthread-switch %builtin fbuiltin))
	    (unless (eq? %state 'dead)
	       (%pthread-wait %builtin)
	       #unspecified)))))

;*---------------------------------------------------------------------*/
;*    %scheduler-switch-to-next-thread-debug ...                       */
;*---------------------------------------------------------------------*/
(define (%scheduler-switch-to-next-thread-debug t scdl nt)
   (trace-item "scdl=" (trace-string scdl))
   (trace-item "t=" (trace-string t))
   (trace-item "nt=" (trace-bold (trace-string nt))))

;*---------------------------------------------------------------------*/
;*    %schedule-instant ...                                            */
;*    -------------------------------------------------------------    */
;*    Executes one instant                                             */
;*---------------------------------------------------------------------*/
(define (%schedule-instant scdl::%scheduler)
   ;; debug message
   (with-trace 1 (format "%schedule-instant: ~a" (%scheduler-time scdl))
      ;; start the new environment instant
      (%env-init-instant scdl)
      ;; terminate the threads that are dying
      (%terminate-threads scdl)
      ;; suspend/resume threads
      (%suspend/resume-threads scdl)
      ;; select the runnable threads
      (%select-threads! scdl)
      ;; add all the new user threads
      (%start-new-threads scdl)
      ;; broadcast the external signals
      (%scheduler-broadcast*! scdl)
      ;; run until the end of the instant (switch to the first runnable thread)
      (%scheduler-switch-to-next-thread scdl scdl)
      ;; start asynchronous threads (only those that are not waited yet)
      (%spawn-async scdl)))

;*---------------------------------------------------------------------*/
;*    %env-init-instant ...                                            */
;*    -------------------------------------------------------------    */
;*    Prepares the environment for the new instant.                    */
;*---------------------------------------------------------------------*/
(define (%env-init-instant scdl::scheduler)
   (with-access::%scheduler scdl (env+)
      (for-each instant-env! env+)))

;*---------------------------------------------------------------------*/
;*    %sort-threads ...                                                */
;*---------------------------------------------------------------------*/
(define (%sort-threads threads)
   (sort threads
	 (lambda (t1 t2)
	    (with-access::fthread t1 ((ident1 %ident))
	       (with-access::fthread t2 ((ident2 %ident))
		  (<fx ident1 ident2))))))

;*---------------------------------------------------------------------*/
;*    %select-threads! ...                                             */
;*    -------------------------------------------------------------    */
;*    Amongst the previously idle (cooperating or waiting) threads     */
;*    selects the ones that are now ready.                             */
;*---------------------------------------------------------------------*/
(define (%select-threads! scdl::%scheduler)
   (with-access::%scheduler scdl (threads-runnable
				  threads-runnable-last-pair
				  threads-yield
				  threads-timeout
				  %threads-ready
				  %live-thread-number
				  strict-order?)
      (let ((runnable threads-runnable)
	    (yield threads-yield)
	    (timeout threads-timeout)
	    (tostart (with-access::%scheduler scdl (tostart) tostart))
	    (live %live-thread-number))
	 (assert (runnable)
             ;;; check the runnable threads	   
	     (every (lambda (t) (>= (with-access::fthread t (%timeout) %timeout) 0)) runnable))
	 (assert (yield)
             ;;; check the yield threads	   
	     (every (lambda (t) (>= (with-access::fthread t (%timeout) %timeout) 0)) yield))
	 (assert (timeout)
             ;;; check the timeout threads	   
	     (every (lambda (t) (>= (with-access::fthread t (%timeout) %timeout) 0)) timeout)))
      ;; mark that no threads have currently yield in the instant
      (set! %threads-ready #f)
      (let ((runnable threads-yield))
	 (set! threads-yield '())
	 (set! threads-timeout
	       (filter! (lambda (t)
			   (with-access::fthread t (%timeout)
			      (case %timeout
				 ((0)
				  ;; the thread has been awake by a signal
				  ;; or it is dead, we skip it
				  #f)
				 ((1)
				  ;; the thread is now ready
				  (%thread-unregister-signals! t)
				  (set! runnable (cons t runnable))
				  #f)
				 (else
				  ;; the thread is still idle
				  (set! %timeout (-fx %timeout 1))
				  (set! %threads-ready #t)
				  #t)))) 
			threads-timeout))
	 (if strict-order?
	     (set! threads-runnable (%sort-threads runnable))
	     (set! threads-runnable runnable))
	 (if (pair? threads-runnable)
	     (set! threads-runnable-last-pair (last-pair threads-runnable))
	     (set! threads-runnable-last-pair '())))))

;*---------------------------------------------------------------------*/
;*    %start-new-threads ...                                           */
;*    -------------------------------------------------------------    */
;*    Starts the newly started threads.                                */
;*---------------------------------------------------------------------*/
(define (%start-new-threads s::%scheduler)
   (with-access::%scheduler s (tostart
			       threads-runnable
			       threads-runnable-last-pair)
      (when (pair? tostart)
	 (cond
	    ((pair? threads-runnable-last-pair)
	     (append! threads-runnable-last-pair (reverse! tostart))
	     (set! threads-runnable-last-pair
		   (last-pair threads-runnable-last-pair)))
	    (else
	     (set! threads-runnable
		   (append! threads-runnable (reverse! tostart)))
	     (set! threads-runnable-last-pair
		   (last-pair threads-runnable))))
	 (set! tostart '()))))

;*---------------------------------------------------------------------*/
;*    %terminate-threads ...                                           */
;*    -------------------------------------------------------------    */
;*    Stops the terminated threads.                                    */
;*---------------------------------------------------------------------*/
(define (%terminate-threads scdl::%scheduler)
   (with-access::%scheduler scdl (toterminate
				  threads-runnable
				  threads-yield
				  threads-timeout)
      (if (pair? toterminate)
	  (with-trace 2 '%terminate-threads
	     ;; we have to remove multiple occurrences of thread to be
	     ;; terminated otherwise we will have a lock because we will run
	     ;; a terminated thread
	     (let ((thds (filter! (lambda (t)
				     (and (%thread-is-toterminate t)
					  (begin
					     (%thread-is-terminated t #t)
					     #t)))
				  toterminate)))
		(trace-item "toterminate=" (trace-string toterminate))
		(trace-item "runnable=" (trace-string toterminate))
		(trace-item "yield=" (trace-string threads-yield))
		(trace-item "timeout=" (trace-string threads-timeout))
		(set! toterminate '())
		(let ((runnable (filter! (lambda (t)
					    (not (%thread-is-toterminate t)))
					 threads-runnable))
		      (yield (filter! (lambda (t)
					 (not (%thread-is-toterminate t)))
				      threads-yield))
		      (timeout (filter! (lambda (t)
					   (not (%thread-is-toterminate t)))
					threads-timeout)))
		   (set! threads-runnable thds)
		   (trace-item "terminating=" (trace-string thds))
		   (trace-item "new-runnable=" (trace-string runnable))
		   (trace-item "new-yield=" (trace-string yield))
		   (trace-item "new-timeout" (trace-string timeout))
		   ;; run the terminated thread (i.e., kill them)
		   (%scheduler-switch-to-next-thread scdl scdl)
		   ;; restore the unterminated runnable and yield threads
		   (set! threads-runnable runnable)
		   (set! threads-yield yield)
		   (set! threads-timeout timeout)))))))

;*---------------------------------------------------------------------*/
;*    %suspend/resume-threads ...                                      */
;*---------------------------------------------------------------------*/
(define (%suspend/resume-threads scdl::%scheduler)
   (with-access::%scheduler scdl (tosuspend/resume)
      (if (pair? tosuspend/resume)
	  (begin
	     (for-each (lambda (v)
			  (with-access::fthread (car v) (%is-suspend)
			     (set! %is-suspend (cdr v))))
		       (reverse! tosuspend/resume))
	     (set! tosuspend/resume '())))))

;*---------------------------------------------------------------------*/
;*    %scheduler-spawn-async ...                                       */
;*---------------------------------------------------------------------*/
(define (%scheduler-spawn-async scdl::%scheduler sig::%sigasync)
   (with-access::%sigasync sig (spawned id thunk)
      (unless spawned
	  (let ((nt (lambda ()
		       (%scheduler-add-broadcast! scdl sig (thunk))
		       #unspecified)))
	     (set! spawned #t)
	     (with-access::%scheduler scdl (%builtin)
		(%async-spawn %builtin nt id))
	     #unspecified))))
	 
;*---------------------------------------------------------------------*/
;*    %spawn-async ...                                                 */
;*    -------------------------------------------------------------    */
;*    Spawns all the unwaited asynchronous threads. Since              */
;*    no one is waiting for the signals, we don't have to broadcast    */
;*    any signal when the computation is complete.                     */
;*---------------------------------------------------------------------*/
(define (%spawn-async scdl::%scheduler)
   (with-access::%scheduler scdl (tospawn)
      (when (pair? tospawn)
	 (for-each (lambda (s) (%scheduler-spawn-async scdl s)) tospawn)
	 (set! tospawn '()))))

;*---------------------------------------------------------------------*/
;*    %broadcast! ...                                                  */
;*---------------------------------------------------------------------*/
(define (%broadcast! scdl::%scheduler sig val)
   (with-trace 2 '%broadcast
      (trace-item "sig=" sig)
      (with-access::%scheduler scdl (env+)
	 (signal-emit sig val env+))))

;*---------------------------------------------------------------------*/
;*    %scheduler-broadcast*! ...                                       */
;*---------------------------------------------------------------------*/
(define (%scheduler-broadcast*! scdl::%scheduler)
   (define (broadcast-external sig)
      (%broadcast! scdl (car sig) (cdr sig)))
   (synchronize scdl
      ;;; the synchronized body
      (with-trace 2 '%scheduler-broadcast*
	 (with-access::%scheduler scdl (tobroadcast)
	    (trace-item "tobroadcast=" tobroadcast)
	    (if (pair? tobroadcast)
		(begin
		   (for-each (cond-expand
				;; debug for bugloo
				(bigloo-jvm (car (list broadcast-external)))
				(else broadcast-external))
		      tobroadcast)
		   (set! tobroadcast '())))))))

;*---------------------------------------------------------------------*/
;*    %scheduler-add-broadcast! ...                                    */
;*---------------------------------------------------------------------*/
(define (%scheduler-add-broadcast! scdl::%scheduler sig::obj val::obj)
   (synchronize scdl
      ;;; the synchronized body
      (with-access::%scheduler scdl (%builtin tobroadcast)
	 (let ((sv (cons sig val)))
	    (set! tobroadcast (cons sv tobroadcast)))
	 (%async-scheduler-notify %builtin)
	 #unspecified)))

;*---------------------------------------------------------------------*/
;*    %scheduler-time ...                                              */
;*---------------------------------------------------------------------*/
(define (%scheduler-time scdl::scheduler)
   (with-access::scheduler scdl (env+)
      (with-access::ftenv (car env+) (instant)
	 instant)))

;*---------------------------------------------------------------------*/
;*    %scheduler-waiting-threads ...                                   */
;*---------------------------------------------------------------------*/
(define (%scheduler-waiting-threads scdl)
   (with-access::scheduler scdl (env+)
      (let loop ((envs env+)
		 (ths '()))
	 (if (pair? envs)
	     (loop (cdr envs) (append (ftenv-threads (car env+)) ths))
	     ths))))
   

