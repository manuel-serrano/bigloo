;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/src/Llib/signal.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 13 14:36:25 2002                          */
;*    Last change :  Tue Nov 15 14:52:25 2011 (serrano)                */
;*    Copyright   :  2002-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Fair thread signal handling                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_signal
   
   (library pthread)
   
   (import  __ft_types
	    __ft_env
	    __ft_%thread)
   
   (export  (class %signal
	       (id::obj read-only)
	       (vals::pair-nil (default '()))
	       (stamp::int (default -1))
	       (threads::pair-nil (default '())))
	    
	    (signal-lookup ::obj ::pair)
	    (signal-emit ::obj ::obj ::pair)
	    (signal-value ::obj ::pair)
	    (signal-last-values ::obj ::pair)
	    (signal-register-thread! ::obj ::pair ::thread)
	    (signal-unregister-thread! ::obj ::pair ::thread)
	    (signal-unbind-thread! ::obj ::thread)))

;*---------------------------------------------------------------------*/
;*    object-display ::%signal ...                                     */
;*---------------------------------------------------------------------*/
(define-method (object-display o::%signal . port)
   (with-output-to-port (if (pair? port) (car port) (current-output-port))
      (lambda ()
	 (with-access::%signal o (id)
	    (display* "#<signal:" id ">")))))

;*---------------------------------------------------------------------*/
;*    signal-lookup ...                                                */
;*---------------------------------------------------------------------*/
(define (signal-lookup id envs)
   (define (%signal-lookup id env)
      (let ((s (ftenv-lookup env id)))
	 (if (isa? s %signal)
	     (with-access::%signal s (id stamp)
		(with-access::ftenv env (instant)
		   (if (=fx stamp instant) s #f)))
	     #f)))
   (let loop ((envs envs))
      (if (ftenv-handles? (car envs) id)
	  (%signal-lookup id (car envs))
	  (loop (cdr envs)))))

;*---------------------------------------------------------------------*/
;*    signal-last-lookup ...                                           */
;*    -------------------------------------------------------------    */
;*    This function differs of SIGNAL-LOOKUP because it searches       */
;*    in the environment of the last instant, not the one of the       */
;*    current instant.                                                 */
;*---------------------------------------------------------------------*/
(define (signal-last-lookup id envs)
   (define (%signal-last-lookup id env)
      (let ((s (ftenv-last-lookup env id)))
	 (if (isa? s %signal)
	     (with-access::%signal s (id stamp)
		(with-access::ftenv env (instant)
		   (if (=fx (+fx 1 stamp) instant) s #f)))
	     #f)))
   (let loop ((envs envs))
      (if (ftenv-handles? (car envs) id)
	  (%signal-last-lookup id (car envs))
	  (loop (cdr envs)))))

;*---------------------------------------------------------------------*/
;*    signal-awake-threads ...                                         */
;*---------------------------------------------------------------------*/
(define (signal-awake-threads s::%signal v)
   (with-access::%signal s (id)
      (define (awake-thread t::thread)
	 (with-trace 2 'awake-thread
	    (awake-thread-trace t id)
	    (if (not (%thread-is-dead t))
		(with-access::fthread t (%signals %awake-signal %awake-value)
		   ;; awake the thread
		   (%thread-awake! t)
		   ;; store, inside the thread, why it is awaken
		   (set! %awake-signal id)
		   (set! %awake-value v)
		   ;; unbind the signal in the thread
		   (for-each (lambda (s) (signal-unbind-thread! s t)) %signals)
		   ;; empties the signal wait queue
		   (set! %signals '())))))
      (with-access::%signal s (threads)
	 (let ((ths threads))
	    (set! threads '())
	    (for-each awake-thread ths)))))

;*---------------------------------------------------------------------*/
;*    awake-thread-trace ...                                           */
;*---------------------------------------------------------------------*/
(define (awake-thread-trace t id)
   (trace-item "id=" id)
   (trace-item " thread=") (trace-bold t))
   
;*---------------------------------------------------------------------*/
;*    signal-emit ...                                                  */
;*---------------------------------------------------------------------*/
(define (signal-emit id value envs)
   (define (%signal-emit id value env)
      (with-access::ftenv env (instant)
	 (let ((o (ftenv-lookup env id)))
	    (with-trace 2 'signal-emit
	       (signal-emit-trace id value envs o)
	       (if (isa? o %signal)
		   (with-access::%signal o (vals stamp threads)
		      (set! stamp instant)
		      (set! vals (cons value vals))
		      (signal-awake-threads o value))
		   (ftenv-bind! env
				id
				(instantiate::%signal
				   (id id)
				   (vals (list value))
				   (stamp instant))))))))
   (let loop ((envs envs))
      (if (ftenv-handles? (car envs) id)
	  (%signal-emit id value (car envs))
	  (loop (cdr envs)))))

;*---------------------------------------------------------------------*/
;*    signal-emit-trace ...                                            */
;*---------------------------------------------------------------------*/
(define (signal-emit-trace id value envs o)
   (trace-item " id=" (trace-bold id))
   (trace-item " o=" (find-runtime-type o)))

;*---------------------------------------------------------------------*/
;*    signal-values ...                                                */
;*---------------------------------------------------------------------*/
(define (signal-values e envs)
   (let ((o (signal-lookup e envs)))
      (if (isa? o %signal)
	  (with-access::%signal o (vals) vals)
	  #unspecified)))

;*---------------------------------------------------------------------*/
;*    signal-value ...                                                 */
;*---------------------------------------------------------------------*/
(define (signal-value e envs)
   (let ((vs (signal-values e envs)))
      (if (pair? vs)
	  (car vs)
	  vs)))

;*---------------------------------------------------------------------*/
;*    signal-last-values ...                                           */
;*---------------------------------------------------------------------*/
(define (signal-last-values e envs)
   (let ((o (signal-last-lookup e envs)))
      (if (isa? o %signal)
	  (with-access::%signal o (vals) vals)
	  '())))

;*---------------------------------------------------------------------*/
;*    signal-register-thread! ...                                      */
;*---------------------------------------------------------------------*/
(define (signal-register-thread! id envs thread)
   (define (%signal-register-thread! id env thread)
      (let* ((o (ftenv-lookup env id))
	     (s (if (isa? o %signal)
		    ;; re-use the existing signal
		    (with-access::%signal o (threads)
		       (set! threads (cons thread threads))
		       o)
		    ;; the signal does not exist, we bind it
		    (let ((new (instantiate::%signal
				  (id id)
				  (vals '())
				  (stamp -1)
				  (threads (list thread)))))
		       (ftenv-bind! env id new)
		       new))))
	 (with-access::fthread thread (%signals)
	    (set! %signals (cons s %signals)))))
   (with-trace 3 'signal-register-thread!
      (signal-register-thread-trace id envs thread)
      (let loop ((envs envs))
	 (if (ftenv-handles? (car envs) id)
	     (%signal-register-thread! id (car envs) thread)
	     (loop (cdr envs))))))

;*---------------------------------------------------------------------*/
;*    signal-register-thread-trace ...                                 */
;*---------------------------------------------------------------------*/
(define (signal-register-thread-trace id envs thread)
   (trace-item " id=" id)
   (trace-item " thread=" (trace-bold thread)))

;*---------------------------------------------------------------------*/
;*    signal-unbind-thread! ...                                        */
;*---------------------------------------------------------------------*/
(define (signal-unbind-thread! s thread)
   ;; the signal already exists, we add thread to the threads list
   (with-access::%signal s (threads)
      (set! threads (remq! thread threads)))
   ;; we remove the signal from the thread signals list
   (with-access::fthread thread (%signals)
      (set! %signals (remq! s %signals))))

;*---------------------------------------------------------------------*/
;*    signal-unregister-thread! ...                                    */
;*---------------------------------------------------------------------*/
(define (signal-unregister-thread! id envs thread)
   (define (%signal-unregister-thread! id env thread)
      (signal-unbind-thread! (ftenv-lookup env id) thread))
   (with-trace 3 signal-unregister-thread!
      (signal-unregister-thread-trace id envs thread)
      (let loop ((envs envs))
	 (if (ftenv-handles? (car envs) id)
	     (%signal-unregister-thread! id (car envs) thread)
	     (loop (cdr envs))))))

;*---------------------------------------------------------------------*/
;*    signal-unregister-thread-trace ...                               */
;*---------------------------------------------------------------------*/
(define (signal-unregister-thread-trace id envs thread)
   (trace-item " id=" id)
   (trace-item " thread=") (trace-bold thread))

