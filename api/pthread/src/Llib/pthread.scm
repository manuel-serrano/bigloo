;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pthread/src/Llib/pthread.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 11:49:11 2002                          */
;*    Last change :  Thu Aug  3 08:52:55 2017 (serrano)                */
;*    Copyright   :  2002-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public Posix Thread implementation.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_thread

   (option (set! *dlopen-init-gc* #t))
   
   (import  __pth_backend)
   
   (include "pthread.sch")

   (extern ($bgl_debug_top_stack::int () "bgl_debug_top_stack"))
   
   (export (class pthread::thread
	      ;; the user thunk
	      (body::procedure read-only)
	      ;; is the thread detached
	      (detachedp::bool (default #f))
	      ;; the result of the thread
	      (end-result (default #unspecified))
	      ;; field for storing uncaught exceptions
	      (end-exception (default #unspecified))
	      ;; the actual native thread
	      ($builtin::$pthread (default ($pthread-nil))))

	   (class &thread-error::&error)
	   
	   (class uncaught-exception::&exception
	      (reason::obj read-only))

	   (class terminated-thread-exception::&exception)

	   ($pthread-nil::$pthread))

   (pragma ($pthread-nil side-effect-free)))

;*---------------------------------------------------------------------*/
;*    Initialization for dynamic library loading                       */
;*---------------------------------------------------------------------*/
(library-multithread-set! #t)

;*---------------------------------------------------------------------*/
;*    pthread-timedjoin property                                       */
;*---------------------------------------------------------------------*/
(cond-expand (bigloo-jvm (register-srfi! 'pthread-timedjoin)))

;*---------------------------------------------------------------------*/
;*    object-write ::uncaught-exception ...                            */
;*---------------------------------------------------------------------*/
(define-method (object-write o::uncaught-exception . port)
   (with-access::uncaught-exception o (reason)
      (apply object-write reason port)))

;*---------------------------------------------------------------------*/
;*    object-display ::uncaught-exception ...                          */
;*---------------------------------------------------------------------*/
(define-method (object-display o::uncaught-exception . port)
   (with-access::uncaught-exception o (reason)
      (apply object-display reason port)))

;*---------------------------------------------------------------------*/
;*    object-print ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-print o::uncaught-exception port print-slot)
   (with-access::uncaught-exception o (reason)
      (object-print reason port print-slot)))

;*---------------------------------------------------------------------*/
;*    %user-thread-yield! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-yield! o::pthread)
   ($pthread-sched-yield))

;*---------------------------------------------------------------------*/
;*    thread-initialize! ::pthread ...                                 */
;*---------------------------------------------------------------------*/
(define-method (thread-initialize! o::pthread)
   (unless (bigloo-initialized?)
      (warning "make-thread"
	 "Thread created before module initialization completed -- "
	 (typeof o)))
   (with-access::pthread o ($builtin body end-result end-exception name)
      (let ((b (lambda ()
		  (let ((id (if (symbol? name)
				(symbol-append '& name)
				(gensym '&pthread-))))
		     (let ()
			($push-trace id #unspecified)
			($set-uncaught-exception-handler!
			   (lambda (val)
			      (error (format "unwind-until!, ~a" o)
				 "exit out of thread dynamic scope"
				 val)))
			(with-handler
			   (lambda (e)
			      (let ((u (instantiate::uncaught-exception
					  (reason e))))
				 (set! end-exception  u)
				 (exception-notify e)
				 #f))
			   (cond-expand
			      (bigloo-c
			       (bind-exit (exit)
				  (signal $pthread-term-sig
				     (lambda (s)
					($set-uncaught-exception-handler!
					   (lambda (val) val))
					(exit #f)))
				  (set! end-result (body))))
			      (else
			       (set! end-result (body))))))))))
	 (set! $builtin ($pthread-new b)))))
    
;*---------------------------------------------------------------------*/
;*    thread-start! ::pthread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (thread-start! t::pthread . scd)
   (with-access::pthread t (detachedp $builtin)
      (set! detachedp #t)
      ($pthread-start! $builtin t #t))
   t)

;*---------------------------------------------------------------------*/
;*    thread-start-joinable! ::pthread ...                             */
;*---------------------------------------------------------------------*/
(define-method (thread-start-joinable! t::pthread)
   (with-access::pthread t (detachedp $builtin)
      (set! detachedp #f)
      ($pthread-start! $builtin t #f))
   t)

;*---------------------------------------------------------------------*/
;*    thread-join! ::pthread ...                                       */
;*---------------------------------------------------------------------*/
(define-method (thread-join! t::pthread . timeout)
   (with-access::pthread t (detachedp $builtin end-result end-exception)
      (if detachedp
	  (raise
	     (instantiate::&thread-error
		(proc 'thread-join!)
		(msg "detached thread")
		(obj t)))
	  (begin
	     ($pthread-join! $builtin (if (pair? timeout) (car timeout) #f))
	     (if (isa? end-exception &exception)
		 (raise end-exception)
		 end-result)))))

;*---------------------------------------------------------------------*/
;*    thread-terminate! ::pthread ...                                  */
;*---------------------------------------------------------------------*/
(define-method (thread-terminate! t::pthread)
   (with-access::pthread t ($builtin end-exception)
      (when ($pthread-terminate! $builtin)
	 (set! end-exception (instantiate::terminated-thread-exception)))
      t))

;*---------------------------------------------------------------------*/
;*    thread-specific ::pthread ...                                    */
;*---------------------------------------------------------------------*/
(define-method (thread-specific t::pthread)
   (with-access::pthread t ($builtin)
      ($pthread-specific $builtin)))

;*---------------------------------------------------------------------*/
;*    thread-specific-set! ::pthread ...                               */
;*---------------------------------------------------------------------*/
(define-method (thread-specific-set! t::pthread v)
   (with-access::pthread t ($builtin)
      ($pthread-specific-set! $builtin v))
   v)

;*---------------------------------------------------------------------*/
;*    thread-cleanup ::pthread ...                                     */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup t::pthread)
   (with-access::pthread t ($builtin)
      ($pthread-cleanup $builtin)))

;*---------------------------------------------------------------------*/
;*    thread-cleanup-set! ::pthread ...                                */
;*---------------------------------------------------------------------*/
(define-method (thread-cleanup-set! t::pthread p)
   (with-access::pthread t ($builtin)
      (if (correct-arity? p 1)
	  (begin
	     ($pthread-cleanup-set! $builtin p)
	     p)
	  (error "thread-cleanup-set!" "Illegal procedure arity" p))))

;*---------------------------------------------------------------------*/
;*    $pthread-nil ...                                                 */
;*---------------------------------------------------------------------*/
(define ($pthread-nil)
   ($pthread-get-nil))

;*---------------------------------------------------------------------*/
;*    Initialization                                                   */
;*---------------------------------------------------------------------*/
(pthread-setup-backend!)
