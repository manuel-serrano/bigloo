;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/src/Llib/_types.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 14 14:38:43 2002                          */
;*    Last change :  Fri Jun 19 15:38:55 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The private types of the fair thread library.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_%types

   (library pthread)

   (import __ft_types)

   (export (class %scheduler::scheduler
	      ;; a predicate telling if the next instant must be executed
	      (next-instant::procedure (default (lambda (s i) #f)))
	      ;; the running thread
	      (current-thread (default #f))
	      ;; the number of live (non terminated) threads
	      (%live-thread-number::int (default 0))
	      ;; is the scheduler ready because a thread yield or timeout
	      (%threads-ready::bool (default #f))
	      ;; the list of threads ready to run
	      (threads-runnable::pair-nil (default '()))
	      ;; the list of threads ready to run
	      (threads-runnable-last-pair::pair-nil (default '()))
	      ;; the list of asynchronous threads ready to run cooperatively
	      (async-runnable::pair-nil (default '()))
	      ;; the list of threads waiting with a timeout
	      (threads-timeout::pair-nil (default '()))
	      ;; the list of stopped (yield) threads 
	      (threads-yield::pair-nil (default '()))
	      ;; threads to be started at the next instant
	      (tostart::pair-nil (default '()))
	      ;; threads to be terminated at the next instant
	      (toterminate::pair-nil (default '()))
	      ;; the threads to be suspended or resumed at the next instant
	      (tosuspend/resume::pair-nil (default '()))
	      ;; the list of signals to be generated at the next instant
	      (tobroadcast::pair-nil (default '()))
	      ;; the list of asynchronous signal to start
	      (tospawn::pair-nil (default '()))
	      ;; the scheduler current running thread
	      (current-token (default #f)))
	   
	   
	   (class %sigasync
	      ;; is this asynchronous signal already running?
	      (spawned::bool (default #f))
	      ;; !!! mem identifier, don't remove
	      (id::symbol read-only)
	      ;; the asynchronous body
	      (thunk::procedure read-only))

	   ; This class represents the underlaying physical thread
	   (class %pthread::pthread
	      ; The fthread object associated with this thread
	      (fthread (default #f))
	      ; A mutex
	      (mutex::mutex (default (make-mutex)))
	      ; A condition variable
	      (condvar::condvar (default (make-condition-variable)))
	      ; An id
	      (id::symbol (default (gensym '$fth)))
	      ; The parent thread, used while entering and leaving the scheduler
	      (parent (default #f)))))
