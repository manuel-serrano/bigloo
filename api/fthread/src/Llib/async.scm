;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/src/Llib/async.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Oct 19 16:47:03 2002                          */
;*    Last change :  Tue Nov 15 16:17:17 2011 (serrano)                */
;*    Copyright   :  2002-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Fair thread asynchronous signals.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_async
   
   (library pthread)

   (import __ft_types
	   __ft_%types
	   __ft_%thread
	   __ft_scheduler
	   __ft_%scheduler
	   __ft_env
	   __ft_%env
	   __ft_signal
	   __ft_thread)

   (export (make-asynchronous-signal ::procedure)))

;*---------------------------------------------------------------------*/
;*    object-equal? ::%sigasync ...                                    */
;*---------------------------------------------------------------------*/
(define-method (object-equal? o1::%sigasync o2)
   (eq? o1 o2))

;*---------------------------------------------------------------------*/
;*    make-asynchronous-signal ...                                     */
;*---------------------------------------------------------------------*/
(define (make-asynchronous-signal proc::procedure)
   (if (not (correct-arity? proc 1))
       (error 'make-asynchronous-signal "Illegal procedure arity proc" proc)
       (letrec ((sig (instantiate::%sigasync
			(id 'make-asynchronous-signal)
			(thunk (lambda () (proc sig))))))
	  (%scheduler-add-async! (current-scheduler) sig)
	  sig)))

