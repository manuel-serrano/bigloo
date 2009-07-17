;*=====================================================================*/
;*    serrano/prgm/project/bigloo/fthread/src/Llib/env.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 26 10:45:00 2001                          */
;*    Last change :  Wed Mar 24 15:48:41 2004 (serrano)                */
;*    Copyright   :  2001-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The representation of Fair environments                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_env
   
   (library pthread)

   (import __ft_types
	   __ft_signal)
   
   (export (instant-env! ::ftenv)
	   (ft-is-signal?::bool ::obj ::obj)
	   
	   (generic ftenv-bind! ::ftenv ::obj ::obj)
	   (generic ftenv-lookup ::ftenv ::obj)
	   (generic ftenv-last-lookup ::ftenv ::obj)
	   (generic ftenv-filter! ::ftenv ::procedure)
	   (generic ftenv-handles? ::ftenv ::obj)
	   (ftenv-threads::pair-nil ::ftenv)))

;*---------------------------------------------------------------------*/
;*    instant-env! ...                                                 */
;*---------------------------------------------------------------------*/
(define (instant-env! env::ftenv)
   (with-access::ftenv env (instant)
      (set! instant (+fx 1 instant))
      (ftenv-filter! env (lambda (s)
			    (and (%signal? s)
				 (pair? (%signal-threads s)))))))
   
;*---------------------------------------------------------------------*/
;*    ftenv-bind! ::ftenv ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (ftenv-bind! env::ftenv id sig))

;*---------------------------------------------------------------------*/
;*    ftenv-lookup ::ftenv ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (ftenv-lookup env::ftenv id))

;*---------------------------------------------------------------------*/
;*    ftenv-last-lookup ::ftenv ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (ftenv-last-lookup env::ftenv id))

;*---------------------------------------------------------------------*/
;*    ftenv-filter! ::ftenv ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (ftenv-filter! env::ftenv procedure))

;*---------------------------------------------------------------------*/
;*    ft-is-signal? ...                                                */
;*---------------------------------------------------------------------*/
(define (ft-is-signal? sig::obj id)
   (and (%signal? sig) (equal? (%signal-id sig) id)))

;*---------------------------------------------------------------------*/
;*    ftenv-handles? ::ftenv ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (ftenv-handles? env::ftenv object))

;*---------------------------------------------------------------------*/
;*    ftenv-threads ...                                                */
;*---------------------------------------------------------------------*/
(define (ftenv-threads ftenv)
   (let ((res '()))
      (ftenv-filter! ftenv
		     (lambda (t)
			(set! res (cons t res))
			#t))
      res))
