;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/src/Llib/types.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 13 15:49:42 2002                          */
;*    Last change :  Fri Jun 19 15:38:53 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The user types defined by the fair threads library               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_types

   (export (class fthread::thread
	      ;; the scheduler the threads belongs to
	      (scheduler (default #f))
	      ;; the actual native thread
	      (%builtin::$fthread (default ($fthread-nil)))
	      ;; the timeout for idle threads
	      (%timeout::int (default 1))
	      ;; state
	      (%state::symbol (default 'unattached))
	      ;; a boolean set to #t iff the thread is suspended
	      (%is-suspend::bool (default #f))
	      ;; terminate entry point
	      (%terminate::procedure (default list))
	      ;; the value associated with the dead thread
	      (%result::obj (default #unspecified))
	      ;; end exception value
	      (%exc-result (default #unspecified))
	      ;; does the thread terminates because of an exception
	      (%exc-raised::bool (default #f))
	      ;; identifier
	      (%ident::int (default 0))
	      ;; the signals that have registered this threads
	      (%signals::pair-nil (default '()))
	      ;; the signals that have registered this threads
	      (%awake-signal (default #f))
	      ;; the signal value that have registered this threads
	      (%awake-value (default #f))
	      ;; finalizer to be invoked when the thread terminates
              (%cleanup (default #f))
	      ;; local memory
              (%specific (default #unspecified))
	      ;; the user thunk
	      (body::procedure read-only))

	   (abstract-class scheduler::fthread
	      ;; the environment
	      (env+::pair read-only))
	   
	   (abstract-class ftenv
	      (instant::long (default 0)))

	   (class %sigjoin
	      (thread::fthread read-only))

	   (class $fthread
	      (thread::thread read-only)
	      (mutex::mutex (default (make-mutex)))
	      (condvar::condvar (default (make-condition-variable)))
	      (id::symbol (default (gensym '$fth)))
	      (parent (default #f)))
	   
	   *thread-strict-order*))

;*---------------------------------------------------------------------*/
;*    *thread-strict-order*                                            */
;*---------------------------------------------------------------------*/
(define *thread-strict-order* #f)

