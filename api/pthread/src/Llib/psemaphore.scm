;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pthread/src/Llib/psemaphore.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 20 09:27:10 2017                          */
;*    Last change :  Thu Apr 27 14:31:01 2017 (serrano)                */
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
	       #!key (create #t) (excl #f) (read #t) (write #t) (value 1))
	    
	    (close-semaphore ::semaphore)
	    (delete-semaphore ::bstring)
;* 	    (semaphore-wait ::semaphore #!optional (timeout::long 0))  */
	    (semaphore-wait ::semaphore)
	    (semaphore-trywait ::semaphore)
	    (semaphore-value::long ::semaphore)
	    (semaphore-post ::semaphore)))

;*---------------------------------------------------------------------*/
;*    open-semaphore ...                                               */
;*---------------------------------------------------------------------*/
(define (open-semaphore name::bstring
	   #!key (create #t) (excl #f) (read #t) (write #t) (value 1))
   ($psemaphore-open name create excl read write value))

;*---------------------------------------------------------------------*/
;*    close-semaphore ...                                              */
;*---------------------------------------------------------------------*/
(define (close-semaphore s::semaphore)
   (=fx ($psemaphore-close s) 0))

;*---------------------------------------------------------------------*/
;*    delete-semaphore ...                                             */
;*---------------------------------------------------------------------*/
(define (delete-semaphore n::bstring)
   (=fx ($psemaphore-delete n) 0))

;*---------------------------------------------------------------------*/
;*    semaphore-wait ...                                               */
;*---------------------------------------------------------------------*/
(define (semaphore-wait s::semaphore)
   (=fx ($psemaphore-wait s) 0))

;* (define (semaphore-wait s::semaphore #!optional (timeout::long 0))  */
;*    (if (=fx timeout 0)                                              */
;*        (=fx ($psemaphore-wait s) 0)                                 */
;*        (=fx ($psemaphore-timed-wait s timeout) 0)))                 */

;*---------------------------------------------------------------------*/
;*    semaphore-trywait ...                                            */
;*---------------------------------------------------------------------*/
(define (semaphore-trywait s::semaphore)
   (=fx ($psemaphore-trywait s) 0))

;*---------------------------------------------------------------------*/
;*    semaphore-value ...                                              */
;*---------------------------------------------------------------------*/
(define (semaphore-value s::semaphore)
   ($psemaphore-value s))

;*---------------------------------------------------------------------*/
;*    semaphore-post ...                                               */
;*---------------------------------------------------------------------*/
(define (semaphore-post s::semaphore)
   (=fx ($psemaphore-post s) 0))
