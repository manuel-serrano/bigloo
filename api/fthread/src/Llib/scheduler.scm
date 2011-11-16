;*=====================================================================*/
;*    .../prgm/project/bigloo/api/fthread/src/Llib/scheduler.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 29 06:40:08 2003                          */
;*    Last change :  Tue Nov 15 11:01:24 2011 (serrano)                */
;*    Copyright   :  2003-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The FairThreads scheduler                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_scheduler
   
   (library pthread)

   (import  __ft_types
	    __ft_%types
	    __ft_%scheduler
	    __ft_%env
	    __ft_%thread
	    __ft_thread
	    __ft_signal
	    __ft_%pthread)
   
   (export  (current-scheduler)
	    (current-scheduler-set! scdl)
	    (default-scheduler . scdl)
	    (make-scheduler::scheduler . args)
	    (with-scheduler ::scheduler ::procedure)
	    (scheduler-react! . scdl)
	    (scheduler-start! . args)
	    (scheduler-terminate! . ::obj)
	    (scheduler-instant::int . ::obj)
	    
	    (broadcast! ::obj . val)
	    (scheduler-broadcast! ::scheduler ::obj . val)))
	    
	   
;*---------------------------------------------------------------------*/
;*    *current-scheduler* & *default-scheduler* ...                    */
;*---------------------------------------------------------------------*/
(define *current-scheduler* #f)
(define *default-scheduler* #f)

;*---------------------------------------------------------------------*/
;*    current-scheduler & current-scheduler-set! ...                   */
;*---------------------------------------------------------------------*/
(define *current-scdl-param-id* '%current@__ft_scheduler)

(define (current-scheduler)
   (let ((th (current-thread)))
      (cond
	 ((isa? th scheduler)
	  th)
	 ((isa? th fthread)
	  (with-access::fthread th (scheduler) scheduler))
	 ((isa? th thread)
	  (thread-parameter *current-scdl-param-id*))
	 (else
	  *current-scheduler*))))

(define (current-scheduler-set! scdl)
   (let ((th (current-thread)))
      (cond
	 ((isa? th fthread)
	  (error 'current-scheduler-set! "Read-only value" th))
	 ((isa? th thread)
	  (thread-parameter-set! *current-scdl-param-id* scdl))
	 (else
	  (set! *current-scheduler* scdl)))))

;*---------------------------------------------------------------------*/
;*    default-scheduler ...                                            */
;*---------------------------------------------------------------------*/
(define (default-scheduler . scdl)
   (let ((th (current-thread)))

      ; The first thread in hierarchy which is not a scheduler, nor a fthread
      (when (isa? th fthread)
	 (let ((s (if (isa? th scheduler)
		      th
		      (with-access::fthread th (scheduler) scheduler))))
	    (with-access::fthread s (%builtin)
	       (with-access::%pthread %builtin (parent)
		  (set! th parent)))))

      (cond
	 ((null? scdl)
	  (if (isa? th thread)
	      (thread-parameter 'fthread*default-scheduler*)
	      *default-scheduler*))
	 ((isa? (car scdl) scheduler)
	  (if (isa? th thread)
	      (thread-parameter-set! 'fthread*default-scheduler* (car scdl))
	      (set! *default-scheduler* (car scdl)))
	  (car scdl))
	 (else
	  (error "default-scheduler" "Illegal scheduler" (car scdl))))))

;*---------------------------------------------------------------------*/
;*    with-scheduler ...                                               */
;*---------------------------------------------------------------------*/
(define (with-scheduler s thunk)
   (let ((old (default-scheduler)))
      (unwind-protect
	 (begin
	    (default-scheduler s)
	    (thunk))
	 (default-scheduler old))))
   
;*---------------------------------------------------------------------*/
;*    make-scheduler ...                                               */
;*---------------------------------------------------------------------*/
(define (make-scheduler . args)
   (with-trace 2 "make-scheduler"
      (let* ((id (gensym 'scheduler))
	     (order-set? (and (pair? args) (boolean? (car args))))
	     (order (if order-set? (car args) #f))
	     (envs (if order-set? (cdr args) args)))
	 (letrec ((s (instantiate::%scheduler
			(body (lambda () (schedule s)))
			(name id)
			(env+ (append envs (list (instantiate::%env)))))))
	    (trace-item "created scdl=" (trace-string s))
	    (trace-item "order=" order ", envs=" (trace-string envs))
	    ;; there is no native thread... %builtin won't be started
	    (with-access::%scheduler s (%builtin strict-order?)
	       (set! %builtin (%fscheduler-new s))
	       (set! strict-order? order))
	    ;; if there is no default scheduler, store that one
	    (if (not (default-scheduler)) (default-scheduler s))
	    ;; return the newly allocated scheduler
	 s))))

;*---------------------------------------------------------------------*/
;*    scheduler-state ...                                              */
;*---------------------------------------------------------------------*/
(define (scheduler-state s::scheduler)
   (with-access::%scheduler s (%state
			       %live-thread-number
			       %threads-ready
			       tostart
			       toterminate
			       tosuspend/resume)
      (cond
	 ((=fx %live-thread-number 0)
	  'done)
	 ((or %threads-ready
	      (pair? tostart)
	      (pair? toterminate)
	      (pair? tosuspend/resume))
	  ;; some threads have to be started or stopped, we are ready to
	  'ready)
	 ((%thread-is-dead s)
	  'dead)
	 (else
	  'await))))

;*---------------------------------------------------------------------*/
;*    schedule ...                                                     */
;*    -------------------------------------------------------------    */
;*    The body of the scheduler which consists in a infinite loop      */
;*    executing instants.                                              */
;*---------------------------------------------------------------------*/
(define (schedule scdl::%scheduler)
   (with-trace 1 "schedule"
      (trace-item "scdl=" (trace-string scdl))
      (with-access::%scheduler scdl (%builtin next-instant)
	 (let loop ((i (%scheduler-time scdl)))
	    (%schedule-instant scdl)
	    (when (next-instant scdl i)
	       (loop (+fx i 1))))
	 #unspecified)))

;*---------------------------------------------------------------------*/
;*    scheduler-react! ...                                             */
;*    -------------------------------------------------------------    */
;*    Blocks the current thread (or program), executes one instant     */
;*    of the specified scheduler and returns its state.                */
;*---------------------------------------------------------------------*/
(define (scheduler-react! . o)
   (let ((scdl (%get-optional-scheduler 'scheduler-react! o)))
      (with-access::%scheduler scdl (next-instant)
	 (set! next-instant
	       (lambda (scdl i)
		  (with-access::scheduler scdl (%builtin) 
		     (%pthread-leave-scheduler %builtin))))
	 ;; acquire the global cpu lock
	 (with-access::scheduler scdl (%builtin) 
	    (%pthread-enter-scheduler %builtin))
	 ;; return a description of new state
	 (scheduler-state scdl))))

;*---------------------------------------------------------------------*/
;*    scheduler-start! ...                                             */
;*---------------------------------------------------------------------*/
(define (scheduler-start! . args)
   (let* ((iterp (and (pair? args) (number? (car args))))
	  (scdl (if (null? args)
		    (default-scheduler)
		    (%get-optional-scheduler 'scheduler-start! (cdr args))))
	  (stop (cond
		   ((null? args)
		    (lambda (i)
		       #f))
		   ((number? (car args))
		    (let ((st (+ (car args) (%scheduler-time scdl) -1)))
		       (lambda (i)
			  (>= i st))))
		   ((procedure? (car args))
		    (lambda (i)
		       ((car args) i)))
		   (else
		    (error "scheduler-start!"
			   "Illegal optional parameter"
			   args)))))
      (define (busy-waiting-next-instant scdl i)
	 (if (stop i)
	     (with-access::scheduler scdl (%builtin) 
		(%pthread-leave-scheduler %builtin))
	     (let ((state (scheduler-state scdl)))
		(with-trace 2 'busy-waiting-next-instant
		   (trace-item "state=" state))
		(case state
		   ((ready)
		    #t)
		   ((await)
		    ;; busy waiting mode
		    #t)
		   (else
		    (with-access::scheduler scdl (%builtin) 
		       (%pthread-leave-scheduler %builtin)))))))
      (define (no-busy-waiting-next-instant scdl i)
	 (if (stop i)
	     (with-access::scheduler scdl (%builtin) 
		(%pthread-leave-scheduler %builtin))
	     (let ((state (scheduler-state scdl)))
		(with-trace 2 'no-busy-waiting-next-instant
		   (trace-item "state=" state))
		(case state
		   ((ready)
		    #t)
		   ((await)
		    ;;; the synchronized body
		    (with-access::%scheduler scdl (%builtin
						   tobroadcast
						   async-runnable)
		       (%async-synchronize %builtin)
		       ;; it might be possible that an asynchronous
		       ;; event has been broadcast since we have
		       ;; computed the scheduler state
		       (if (and (null? tobroadcast) (null? async-runnable))
			   (begin
			      (%async-scheduler-wait %builtin)
			      #unspecified))
		       (%async-asynchronize %builtin)
		       #t))
		   (else
		    (with-access::scheduler scdl (%builtin)
		       (%pthread-leave-scheduler %builtin)))))))

      (with-access::%scheduler scdl (%builtin next-instant)
	 (set! next-instant
	       (if iterp
		   busy-waiting-next-instant
		   no-busy-waiting-next-instant))
	 ;; acquires the global cpu lock
	 (with-access::scheduler scdl (%builtin) 
	    (%pthread-enter-scheduler %builtin))
	 #unspecified)))

;*---------------------------------------------------------------------*/
;*    broadcast! ...                                                   */
;*---------------------------------------------------------------------*/
(define (broadcast! sig . val)
   (let ((t (current-thread))
	 (v (if (pair? val) (car val) #unspecified)))
      (if (isa? t thread)
	  (if (%thread-attached? t)
	      (with-access::fthread t (scheduler)
		 (%broadcast! scheduler sig v))
	      (error "broadcast!" "Unattached thread" t)))))

;*---------------------------------------------------------------------*/
;*    scheduler-broadcast! ...                                         */
;*---------------------------------------------------------------------*/
(define (scheduler-broadcast! s sig . val)
   (%scheduler-add-broadcast! s sig (if (pair? val) (car val) #unspecified)))

;*---------------------------------------------------------------------*/
;*    scheduler-terminate! ...                                         */
;*---------------------------------------------------------------------*/
(define (scheduler-terminate! . s)
   (let ((s (cond
	       ((null? s)
		(default-scheduler))
	       ((isa? (car s) scheduler)
		(car s))
	       (else
		(error "scheduler-react!"
		       "Illegal scheduler"
		       (car s))))))
      (with-access::%scheduler s (tosuspend/resume
				  tostart
				  threads-runnable
				  threads-yield
				  threads-timeout
				  current-thread
				  env+)
	 (with-trace 3 'scheduler-terminate!
	    (trace-item "s=" s)
	    (trace-item "current-thread=" current-thread)
	    (trace-item "tosuspend/resume=" tosuspend/resume)
	    (trace-item "threads-runnable=" threads-runnable)
	    (trace-item "thread-yield=" threads-yield)
	    (trace-item "thread-timeout=" threads-timeout)
	    (trace-item "thread-waiting=" (%scheduler-waiting-threads s))
	    ;; terminate all the running threads
	    (thread-terminate! current-thread)
	    (for-each thread-terminate! tosuspend/resume)
	    (for-each thread-terminate! threads-runnable)
	    (for-each thread-terminate! threads-yield)
	    (for-each thread-terminate! threads-timeout)
	    ;; terminate all the threads waiting for an event
	    (for-each thread-terminate! (%scheduler-waiting-threads s))
	    ;; reset all threads lists
	    (set! tosuspend/resume '())
	    (set! tostart '())
	    ;; mark that the scheduler must terminate at the end of instant
	    (%thread-is-dead s #t)))))

;*---------------------------------------------------------------------*/
;*    scheduler-instant ...                                            */
;*---------------------------------------------------------------------*/
(define (scheduler-instant . s)
   (let ((s (cond
	       ((null? s)
		(default-scheduler))
	       ((isa? (car s) scheduler)
		(car s))
	       (else
		(error "scheduler-instant" "Illegal scheduler" (car s))))))
      (with-access::%scheduler s (env+)
	 (with-access::ftenv (car env+) (instant)
	    instant))))
