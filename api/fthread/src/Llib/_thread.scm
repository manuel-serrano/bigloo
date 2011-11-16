;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/src/Llib/_thread.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 29 07:43:14 2003                          */
;*    Last change :  Tue Nov 15 16:32:41 2011 (serrano)                */
;*    Copyright   :  2003-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The private FairThreads implementation.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_%thread
   
   (library pthread)

   (import  __ft_types
            __ft_%types
            __ft_signal
	    __ft_scheduler
	    __ft_%scheduler
	    __ft_%pthread)
   
   (export  (%thread-awake! ::fthread)
	    (%thread-cooperate ::fthread)
	    (%thread-yield! ::fthread)
	    (%thread-timeout! ::fthread ::int)
	    (%thread-kill! ::fthread)
	    (inline %thread-unregister-signals! ::fthread)
	    (%thread-attached?::bool ::fthread)
	    (%thread-is-dead::bool ::fthread . ::obj)
	    (%thread-is-toterminate::bool ::fthread . ::obj)
	    (%thread-is-terminated::bool ::fthread . ::obj)
	    (%thread-asynchronize! t::fthread id::symbol)
	    (%thread-synchronize! t)))

;*---------------------------------------------------------------------*/
;*    %thread-awake! ...                                               */
;*---------------------------------------------------------------------*/
(define (%thread-awake! t::fthread)
   (with-access::fthread t (scheduler %timeout)
      (with-access::%scheduler scheduler (threads-runnable
					  threads-runnable-last-pair)
	 (set! %timeout 1)
	 (if (pair? threads-runnable-last-pair)
	     (let ((p (cons t '())))
		(set-cdr! threads-runnable-last-pair p)
		(set! threads-runnable-last-pair p))
	     (begin
		(set! threads-runnable (list t))
		(set! threads-runnable-last-pair threads-runnable))))))

;*---------------------------------------------------------------------*/
;*    %thread-cooperate ...                                            */
;*---------------------------------------------------------------------*/
(define (%thread-cooperate t::fthread)
   (with-access::fthread t (scheduler %terminate)
      (with-trace 3 '%thread-cooperate
	 (trace-item "thread=" (trace-string t))
	 (trace-item "toterminate=" (%thread-is-toterminate t))
	 (trace-item "terminated=" (%thread-is-terminated t))
	 (%scheduler-switch-to-next-thread t scheduler)
	 (if (%thread-is-terminated t)
	     (%terminate t)))))
   
;*---------------------------------------------------------------------*/
;*    %thread-yield! ...                                               */
;*---------------------------------------------------------------------*/
(define (%thread-yield! t::fthread)
   (with-access::fthread t (scheduler)
      (with-access::%scheduler scheduler (threads-yield %threads-ready)
	 (set! threads-yield (cons t threads-yield))
	 (set! %threads-ready #t)
	 (%thread-cooperate t))))

;*---------------------------------------------------------------------*/
;*    %thread-timeout! ...                                             */
;*---------------------------------------------------------------------*/
(define (%thread-timeout! t::fthread tmt::int)
   (with-access::fthread t (scheduler %timeout)
      (set! %timeout tmt)
      (with-access::%scheduler scheduler (threads-timeout %threads-ready)
	 (set! threads-timeout (cons t threads-timeout))
	 (set! %threads-ready #t)
	 (%thread-cooperate t))))
	 
;*---------------------------------------------------------------------*/
;*    %thread-kill! ...                                                */
;*---------------------------------------------------------------------*/
(define (%thread-kill! t::fthread)
   (with-access::fthread t (%builtin scheduler %state)
      (with-trace 3 '%thread-kill!
	 (%thread-kill-debug t)
	 (%thread-is-dead t #t)
	 (with-access::%scheduler scheduler (threads-runnable
					     threads-runnable-last-pair
					     %live-thread-number)
	    (set! %live-thread-number (-fx %live-thread-number 1))
	    (set! threads-runnable (remq! t threads-runnable))
	    (if (pair? threads-runnable)
		(set! threads-runnable-last-pair (last-pair threads-runnable))
		(set! threads-runnable-last-pair '()))
	    (%thread-unregister-signals! t)
	    (%scheduler-switch-to-next-thread t scheduler)
	    #unspecified))))

;*---------------------------------------------------------------------*/
;*    %thread-kill-debug ...                                           */
;*---------------------------------------------------------------------*/
(define (%thread-kill-debug t)
   (trace-item "thread=" (trace-string t)))

;*---------------------------------------------------------------------*/
;*    %thread-unregister-signals! ...                                  */
;*    -------------------------------------------------------------    */
;*    Unregister the thread from its waiting signal queues             */
;*---------------------------------------------------------------------*/
(define-inline (%thread-unregister-signals! t::fthread)
   (with-access::fthread t (%signals)
      (for-each (lambda (s) (signal-unbind-thread! s t)) %signals)))

;*---------------------------------------------------------------------*/
;*    %thread-attached? ...                                            */
;*---------------------------------------------------------------------*/
(define (%thread-attached? t::fthread)
   (with-access::fthread t ((scdl scheduler))
      (isa? scdl scheduler)))

;*---------------------------------------------------------------------*/
;*    %thread-is-dead ...                                              */
;*---------------------------------------------------------------------*/
(define (%thread-is-dead t::fthread . v)
   (with-access::fthread t (%state)
      (if (and (pair? v) (car v))
	  (set! %state 'dead)
	  (eq? %state 'dead))))

;*---------------------------------------------------------------------*/
;*    %thread-is-toterminate ...                                       */
;*---------------------------------------------------------------------*/
(define (%thread-is-toterminate t::fthread . v)
   (with-access::fthread t (%state)
      (if (and (pair? v) (car v))
	  (set! %state 'toterminate)
	  (eq? %state 'toterminate))))

;*---------------------------------------------------------------------*/
;*    %thread-is-terminated ...                                        */
;*---------------------------------------------------------------------*/
(define (%thread-is-terminated t::fthread . v)
   (with-access::fthread t (%state)
      (if (and (pair? v) (car v))
	  (set! %state 'terminated)
	  (eq? %state 'terminated))))

;*---------------------------------------------------------------------*/
;*    %thread-asynchronize! ...                                        */
;*---------------------------------------------------------------------*/
(define (%thread-asynchronize! t::fthread id::symbol)
   (with-trace 3 '%thread-asynchronize!
      (trace-item "thread=" (trace-string t))
      (trace-item "id=" id))
   (with-access::fthread t (scheduler %builtin)
      (with-access::%pthread %builtin ((pid id))
	 (set! pid id))
      (let ((nt (%scheduler-next-thread t scheduler)))
	 (with-access::fthread nt ((nbuiltin %builtin))
	    (%pthread-switch %builtin nbuiltin)))
      #unspecified))

;*---------------------------------------------------------------------*/
;*    %thread-synchronize! ...                                         */
;*---------------------------------------------------------------------*/
(define (%thread-synchronize! t)
   (with-trace 2 '%thread-synchronize!
      (trace-item "thread=" (trace-string t))
      (with-access::fthread t (name %builtin scheduler)
	 ;; add the current thread to the list of asynchronous thread
	 ;; ready to be synchronized (i.e. cooperative)
	 (%scheduler-add-async-runnable! scheduler t)
	 ;; wait for the cooperative token
	 (%pthread-wait %builtin)
	 (with-access::%pthread %builtin ((pid id))
	    (set! pid name)))))
