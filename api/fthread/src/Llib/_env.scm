;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/src/Llib/_env.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 13 14:38:14 2002                          */
;*    Last change :  Tue Nov 15 11:10:00 2011 (serrano)                */
;*    Copyright   :  2002-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of fair environments.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_%env
   
   (library pthread)
   
   (import __ft_types
	   __ft_env
	   __ft_signal)
   
   (export (class %env::ftenv
	      (%signals::pair-nil (default '()))
	      (%last-signals::pair-nil (default '())))
	   
	   (%env-waiting-signals::pair-nil ::%env)))

;*---------------------------------------------------------------------*/
;*    ftenv-bind! ::%env ...                                           */
;*---------------------------------------------------------------------*/
(define-method (ftenv-bind! env::%env id sig)
   (with-access::%env env (%signals)
      (set! %signals (cons sig %signals))))
  
;*---------------------------------------------------------------------*/
;*    ftenv-lookup ::ft-env ...                                        */
;*---------------------------------------------------------------------*/
(define-method (ftenv-lookup env::%env id)
   (with-access::%env env (%signals)
      (let loop ((sigs %signals))
	 (cond
	    ((null? sigs)
	     #f)
	    ((ft-is-signal? (car sigs) id)
	     (car sigs))
	    (else
	     (loop (cdr sigs)))))))
	   
;*---------------------------------------------------------------------*/
;*    ftenv-last-lookup ::ft-env ...                                   */
;*---------------------------------------------------------------------*/
(define-method (ftenv-last-lookup env::%env id)
   (with-access::%env env (%last-signals)
      (let loop ((sigs %last-signals))
	 (cond
	    ((null? sigs)
	     #f)
	    ((ft-is-signal? (car sigs) id)
	     (car sigs))
	    (else
	     (loop (cdr sigs)))))))
	   
;*---------------------------------------------------------------------*/
;*    ftenv-filter! ...                                                */
;*---------------------------------------------------------------------*/
(define-method (ftenv-filter! env::%env pred)
   (with-access::%env env (%signals %last-signals)
      (set! %last-signals %signals)
      (set! %signals (filter pred %signals))))

;*---------------------------------------------------------------------*/
;*    ftenv-handles? ::%env ...                                        */
;*---------------------------------------------------------------------*/
(define-method (ftenv-handles? env::%env obj)
   #t)

;*---------------------------------------------------------------------*/
;*    %env-waiting-signals ...                                         */
;*---------------------------------------------------------------------*/
(define (%env-waiting-signals env::%env)
   (with-access::%env env (%signals)
      (filter (lambda (s)
		 (and (isa? s %signal)
		      (with-access::%signal s (threads)
			 (pair? threads))))
	      %signals)))
