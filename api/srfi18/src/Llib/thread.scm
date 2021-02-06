;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/src/Llib/thread.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 11:49:11 2002                          */
;*    Last change :  Fri Dec 13 12:49:07 2013 (serrano)                */
;*    Copyright   :  2002-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public srfi18 Thread implementation.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi18_thread

   (option (set! *dlopen-init-gc* #t))
   
   (library pthread)
   
   (import  __srfi18_backend)
   
   (include "srfi18.sch")

   (export (class srfi18thread::pthread)))

;*---------------------------------------------------------------------*/
;*    Initialization for dynamic library loading                       */
;*---------------------------------------------------------------------*/
(library-multithread-set! #t)

;*---------------------------------------------------------------------*/
;*    srfi18read-timedjoin property                                    */
;*---------------------------------------------------------------------*/
(cond-expand (bigloo-jvm (register-srfi! 'srfi18-timedjoin)))

;*---------------------------------------------------------------------*/
;*    thread-initialize! ::srfi18thread ...                            */
;*---------------------------------------------------------------------*/
(define-method (thread-initialize! o::srfi18thread)
   (if (not (bigloo-initialized?))
       (error 'make-thread
	  "Threads cannot be created until modules are initialized (see the documentation)"
	  (find-runtime-type o))
       (with-access::srfi18thread o ($builtin body end-result end-exception name)
	  (let ((b (lambda ()
		      (let ((id (if (symbol? name)
				    (symbol-append '& name)
				    (gensym '&srfi18read-))))
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
	     (set! $builtin ($srfi18thread-new b))))))

;*---------------------------------------------------------------------*/
;*    thread-start! ::srfi18 ...                                       */
;*---------------------------------------------------------------------*/
(define-method (thread-start! t::srfi18thread . scd)
   (with-access::srfi18thread t (detachedp $builtin)
      (set! detachedp #t)
      ($srfi18thread-start! $builtin t #t))
   t)

;*---------------------------------------------------------------------*/
;*    thread-start-joinable! ::pthread ...                             */
;*---------------------------------------------------------------------*/
(define-method (thread-start-joinable! t::srfi18thread)
   (with-access::srfi18thread t (detachedp $builtin)
      (set! detachedp #f)
      ($srfi18thread-start! $builtin t #f))
   t)

;*---------------------------------------------------------------------*/
;*    Initialization                                                   */
;*---------------------------------------------------------------------*/
(srfi18-setup-backend!)
