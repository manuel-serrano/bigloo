;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/src/Llib/backend.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 24 06:42:48 2008                          */
;*    Last change :  Tue Dec 11 18:14:45 2012 (serrano)                */
;*    Copyright   :  2008-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Posix thread backend                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi18_backend

   (library pthread)
   
   (include "srfi18.sch")
   
   (import __srfi18_thread)
   
   (static (class srfi18-backend::thread-backend))
   
   (export (srfi18-setup-backend!)
	   (get-srfi18-backend)))

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
