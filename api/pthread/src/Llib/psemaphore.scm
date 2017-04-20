;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pthread/src/Llib/psemaphore.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 20 09:27:10 2017                          */
;*    Last change :  Thu Apr 20 11:36:47 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Semaphore                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_semaphore

   (include "psemaphore.sch")
   
   (export  (open-semaphore::semaphore ::bstring
	       #!key (create #t) (excl #t) (read #t) (write #t) (value 1))
	    
	    (inline close-semaphore ::semaphore)
	    (inline delete-semaphore ::bstring)
	    (inline semaphore-wait ::semaphore #!optional (timeout::long 0))
	    (inline semaphore-trywait ::semaphore)
	    (inline semaphore-value::long ::semaphore)
	    (inline semaphore-post ::semaphore)))

;*---------------------------------------------------------------------*/
;*    open-semaphore ...                                               */
;*---------------------------------------------------------------------*/
(define (open-semaphore name::bstring
	   #!key (create #t) (excl #t) (read #t) (write #t) (value 1))
   ($psemaphore-open name create excl read write value))

;*---------------------------------------------------------------------*/
;*    close-semaphore ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (close-semaphore s::semaphore)
   (=fx ($psemaphore-close s) 0))

;*---------------------------------------------------------------------*/
;*    delete-semaphore ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (delete-semaphore n::bstring)
   (=fx ($psemaphore-delete n) 0))

;*---------------------------------------------------------------------*/
;*    semaphore-wait ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (semaphore-wait s::semaphore #!optional (timeout::long 0))
   (if (=fx timeout 0)
       (=fx ($psemaphore-wait s) 0)
       (=fx ($psemaphore-timed-wait s timeout) 0)))

;*---------------------------------------------------------------------*/
;*    semaphore-trywait ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (semaphore-trywait s::semaphore)
   (=fx ($psemaphore-trywait s) 0))

;*---------------------------------------------------------------------*/
;*    semaphore-value ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (semaphore-value s::semaphore)
   ($psemaphore-value s))

;*---------------------------------------------------------------------*/
;*    semaphore-post ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (semaphore-post s::semaphore)
   (=fx ($psemaphore-post s) 0))
