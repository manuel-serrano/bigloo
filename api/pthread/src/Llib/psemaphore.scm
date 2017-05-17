;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pthread/src/Llib/psemaphore.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 20 09:27:10 2017                          */
;*    Last change :  Wed May 17 09:26:12 2017 (serrano)                */
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
	       #!key
	       (create #t) (excl #f)
	       (mode '(S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP S_IROTH S_IWOTH))
	       (value 1))
	    
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
	   #!key (create #t) (excl #f)
	   (mode '(S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP S_IROTH S_IWOTH))
	   (value 1))
   ($psemaphore-open name create excl (scheme->mode mode) value))

;*---------------------------------------------------------------------*/
;*    scheme->mode ...                                                 */
;*---------------------------------------------------------------------*/
(define (scheme->mode::long mode)
   (cond
      ((integer? mode)
       mode)
      ((pair? mode)
       (let ((res 0))
	  (cond-expand
	     (bigloo-c
	      (for-each (lambda (m)
			   (case m
			      ((S_IRWXU) (set! res (bit-or res $S_IRWXU)))
			      ((S_IRUSR) (set! res (bit-or res $S_IRUSR)))
			      ((S_IWUSR) (set! res (bit-or res $S_IWUSR)))
			      ((S_IXUSR) (set! res (bit-or res $S_IXUSR)))
			      ((S_IRWXG) (set! res (bit-or res $S_IRWXG)))
			      ((S_IRGRP) (set! res (bit-or res $S_IRGRP)))
			      ((S_IWGRP) (set! res (bit-or res $S_IWGRP)))
			      ((S_IXGRP) (set! res (bit-or res $S_IXGRP)))
			      ((S_IRWXO) (set! res (bit-or res $S_IRWXO)))
			      ((S_IROTH) (set! res (bit-or res $S_IROTH)))
			      ((S_IWOTH) (set! res (bit-or res $S_IWOTH)))
			      ((S_IXOTH) (set! res (bit-or res $S_IXOTH)))
			      (else (error "open-semaphore" "unknown mode" m))))
		 mode)
	      res)
	     (else
	      res))))
      (else
       (error "open-semaphore" "illegal mode" mode))))
       
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
