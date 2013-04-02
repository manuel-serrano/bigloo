;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pthread/src/Llib/pbackend.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 24 06:42:48 2008                          */
;*    Last change :  Tue Mar 12 11:58:00 2013 (serrano)                */
;*    Copyright   :  2008-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Posix thread backend                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_backend

   (include "pthread.sch")
   
   (import __pth_thread)
   
   (export (class pthread-backend::thread-backend))
   
   (export (pthread-setup-backend!)
	   (get-pthread-backend)))

;*---------------------------------------------------------------------*/
;*    *pthread-backend* ...                                            */
;*---------------------------------------------------------------------*/
(define *pthread-backend* #unspecified)

;*---------------------------------------------------------------------*/
;*    pthread-setup-backend! ...                                       */
;*---------------------------------------------------------------------*/
(define (pthread-setup-backend!)
   (cond-expand
      (bigloo-jvm
       ($pthread-setup)))
   (set! *pthread-backend* (instantiate::pthread-backend (name "pthread")))
   (default-thread-backend-set! *pthread-backend*)
   (current-thread-backend-set! (get-pthread-backend)))

;*---------------------------------------------------------------------*/
;*    get-pthread-backend ...                                          */
;*---------------------------------------------------------------------*/
(define (get-pthread-backend)
   *pthread-backend*)

;*---------------------------------------------------------------------*/
;*    tb-make-thread ::pthread-backend ...                             */
;*---------------------------------------------------------------------*/
(define-method (tb-make-thread tb::pthread-backend body name)
   (instantiate::pthread
      (body body)
      (name name)))

;*---------------------------------------------------------------------*/
;*    tb-current-thread ::pthread-backend ...                          */
;*---------------------------------------------------------------------*/
(define-method (tb-current-thread tb::pthread-backend)
   ($pthread-current-thread))
