;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pthread/src/Llib/pthread.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 11:49:11 2002                          */
;*    Last change :  Sat Dec 11 09:11:17 2010 (serrano)                */
;*    Copyright   :  2002-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public Posix Thread implementation.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_thread

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

	   ($pthread-nil::$pthread)))

;*---------------------------------------------------------------------*/
;*    object-write ::uncaught-exception ...                            */
;*---------------------------------------------------------------------*/
(define-method (object-write o::uncaught-exception . port)
   (apply object-write (uncaught-exception-reason o) port))

;*---------------------------------------------------------------------*/
;*    object-display ::uncaught-exception ...                          */
;*---------------------------------------------------------------------*/
(define-method (object-display o::uncaught-exception . port)
   (apply object-display (uncaught-exception-reason o) port))

;*---------------------------------------------------------------------*/
;*    object-print ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (object-print o::uncaught-exception port print-slot)
   (object-print (uncaught-exception-reason o) port print-slot))

;*---------------------------------------------------------------------*/
;*    %user-thread-yield! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%user-thread-yield! o::pthread)
   ($pthread-sched-yield))

;*---------------------------------------------------------------------*/
;*    thread-initialize! ::pthread ...                                 */
;*---------------------------------------------------------------------*/
(define-method (thread-initialize! o::pthread)
   (if (not (bigloo-initialized?))
       (error 'make-thread
	      "Threads cannot be created until modules are initialized (see the documentation)"
	      (find-runtime-type o))
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
	     (set! $builtin ($pthread-new b))))))
    
;*---------------------------------------------------------------------*/
;*    thread-start! ::pthread ...                                      */
;*---------------------------------------------------------------------*/
(define-method (thread-start! t::pthread . scd)
   (pthread-detachedp-set! t #t)
   ($pthread-start! (pthread-$builtin t) t #t)
   t)

;*---------------------------------------------------------------------*/
;*    thread-start-joinable! ::pthread ...                             */
;*---------------------------------------------------------------------*/
(define-method (thread-start-joinable! t::pthread)
   (pthread-detachedp-set! t #f)
   ($pthread-start! (pthread-$builtin t) t #f)
   t)

;*---------------------------------------------------------------------*/
;*    thread-join! ::pthread ...                                       */
;*---------------------------------------------------------------------*/
(define-method (thread-join! t::pthread . timeout)
   (if (pthread-detachedp t)
       (raise (instantiate::&thread-error
		 (proc 'thread-join!)
		 (msg "detached thread")
		 (obj t)))
       (with-access::pthread t ($builtin end-result end-exception)
	  ($pthread-join! $builtin)
	  (if (&exception? end-exception)
	      (raise end-exception)
	      end-result))))

;*---------------------------------------------------------------------*/
;*    thread-terminate! ::pthread ...                                  */
;*---------------------------------------------------------------------*/
(define-method (thread-terminate! t::pthread)
   (with-access::pthread t ($builtin end-exception)
      (when ($pthread-terminate! $builtin)
	 (set! end-exception (instantiate::terminated-thread-exception)))
      t))

;*---------------------------------------------------------------------*/
;*    thread-get-specific ::pthread ...                                */
;*---------------------------------------------------------------------*/
(define-method (thread-get-specific t::pthread)
   ($pthread-specific (pthread-$builtin t)))

;*---------------------------------------------------------------------*/
;*    thread-set-specific! ::pthread ...                               */
;*---------------------------------------------------------------------*/
(define-method (thread-set-specific! t::pthread v)
   ($pthread-specific-set! (pthread-$builtin t) v)
   v)

;*---------------------------------------------------------------------*/
;*    thread-get-cleanup ::pthread ...                                 */
;*---------------------------------------------------------------------*/
(define-method (thread-get-cleanup t::pthread)
   ($pthread-cleanup (pthread-$builtin t)))

;*---------------------------------------------------------------------*/
;*    thread-set-cleanup! ::pthread ...                                */
;*---------------------------------------------------------------------*/
(define-method (thread-set-cleanup! t::pthread p)
   (if (correct-arity? p 1)
       (begin
	  ($pthread-cleanup-set! (pthread-$builtin t) p)
	  p)
       (error 'thread-cleanup-set! "Illegal procedure arity" p)))

;*---------------------------------------------------------------------*/
;*    $pthread-nil ...                                                 */
;*---------------------------------------------------------------------*/
(define ($pthread-nil)
   ($pthread-get-nil))

;*---------------------------------------------------------------------*/
;*    Initialization                                                   */
;*---------------------------------------------------------------------*/
(pthread-setup-backend!)
