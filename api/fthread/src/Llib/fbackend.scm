;*=====================================================================*/
;*    .../prgm/project/bigloo/api/fthread/src/Llib/fbackend.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 24 06:42:48 2008                          */
;*    Last change :  Fri Jun 19 16:55:29 2009 (serrano)                */
;*    Copyright   :  2008-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Fair Thread backend                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_backend

   (import  __ft_%thread
	    __ft_%types
	    __ft_%scheduler
	    __ft_types
	    __ft_thread
            __ft_signal
	    __ft_scheduler
	    __fthread)
   
   (static (class fthread-backend::thread-backend))
   
   (export (get-fthread-backend)
	   (inline current-fthread)))

;*---------------------------------------------------------------------*/
;*    *fthread-backend* ...                                            */
;*---------------------------------------------------------------------*/
(define *fthread-backend* #unspecified)

;*---------------------------------------------------------------------*/
;*    fthread-setup-backend! ...                                       */
;*---------------------------------------------------------------------*/
(define (fthread-setup-backend!)
   (set! *fthread-backend* (instantiate::fthread-backend (name "fthread")))
   (default-thread-backend-set! *fthread-backend*)
   (current-thread-backend-set! *fthread-backend*))

;*---------------------------------------------------------------------*/
;*    get-fthread-backend ...                                          */
;*---------------------------------------------------------------------*/
(define (get-fthread-backend)
   *fthread-backend*)

;*---------------------------------------------------------------------*/
;*    tb-make-thread ::fthread-backend ...                             */
;*---------------------------------------------------------------------*/
(define-method (tb-make-thread tb::fthread-backend thunk name)
   (if (not (correct-arity? thunk 0))
       (error 'make-thread
	      "Illegal thread body (should be a procedure of 0 arguments)"
	      thunk)
       (let ((the-name (cond
			  ((symbol? name)
			   name)
			  ((string? name)
			   (string->symbol name))
			  (else
			   (gensym 'fairthread)))))
	  (letrec ((t (instantiate::fthread
			 (body thunk)
			 (name the-name))))
	     (%thread-setup! t)
	     t))))

;*---------------------------------------------------------------------*/
;*    tb-thread-sleep! ::fthread-backend ...                           */
;*---------------------------------------------------------------------*/
(define-method (tb-thread-sleep! tb::fthread-backend timeout)
   (let ((t (tb-current-thread tb)))
      (if (thread? t)
	  (when (>fx timeout 0) (%thread-timeout! t timeout))
	  (error 'thread-sleep! "No current thread" t))))

;*---------------------------------------------------------------------*/
;*    tb-current-thread ::fthread-backend ...                          */
;*---------------------------------------------------------------------*/
(define-method (tb-current-thread tb::fthread-backend)
   (if (not (scheduler? (current-scheduler)))
       #f
       (%scheduler-current-thread (current-scheduler))))

;*---------------------------------------------------------------------*/
;*    current-fthread ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (current-fthread)
   (tb-current-thread (get-fthread-backend)))

;*---------------------------------------------------------------------*/
;*    tb-thread-yield! ::fthread-backend ...                           */
;*---------------------------------------------------------------------*/
(define-method (tb-thread-yield! tb::fthread-backend)
   (let ((t (tb-current-thread tb)))
      (if (thread? t)
	  (%thread-yield! t)
	  (error 'thread-yield! "No current thread" t))))

;*---------------------------------------------------------------------*/
;*    FairThread initialization                                        */
;*---------------------------------------------------------------------*/
(fthread-setup-backend!)
