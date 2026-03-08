;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0a/api/srfi18/src/Llib/thread.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 11:49:11 2002                          */
;*    Last change :  Sun Mar  8 21:02:08 2026 (serrano)                */
;*    Copyright   :  2002-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public srfi18 Thread implementation.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi18_thread

   (option (set! *dlopen-init-gc* #t))
   
   (library pthread)
   
   (include "srfi18.sch")

   (static (class srfi18-backend::thread-backend))
   
   (export (class srfi18thread::pthread)))

;*---------------------------------------------------------------------*/
;*    Initialization for dynamic library loading                       */
;*---------------------------------------------------------------------*/
(library-multithread-set! #t)

;*---------------------------------------------------------------------*/
;*    *srfi18-backend* ...                                             */
;*---------------------------------------------------------------------*/
(define *srfi18-backend* #unspecified)

;*---------------------------------------------------------------------*/
;*    srfi18-setup-backend! ...                                        */
;*---------------------------------------------------------------------*/
(define (srfi18-setup-backend!)
   (cond-expand
      (bigloo-jvm
       ($srfi18thread-setup)))
   (set! *srfi18-backend* (instantiate::srfi18-backend (name "srfi18")))
   (default-thread-backend-set! *srfi18-backend*)
   (current-thread-backend-set! (get-srfi18-backend)))

;*---------------------------------------------------------------------*/
;*    get-srfi18-backend ...                                           */
;*---------------------------------------------------------------------*/
(define (get-srfi18-backend)
   *srfi18-backend*)

;*---------------------------------------------------------------------*/
;*    tb-make-thread ::srfi18-backend ...                              */
;*---------------------------------------------------------------------*/
(define-method (tb-make-thread tb::srfi18-backend body name)
   (instantiate::srfi18thread
      (body body)
      (name name)))

;*---------------------------------------------------------------------*/
;*    tb-current-thread ::srfi18-backend ...                           */
;*---------------------------------------------------------------------*/
(define-method (tb-current-thread tb::srfi18-backend)
   ($pthread-current-thread))

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
