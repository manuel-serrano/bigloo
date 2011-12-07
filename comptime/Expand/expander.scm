;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/expander.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 16:05:29 1994                          */
;*    Last change :  Wed Dec  7 13:53:06 2011 (serrano)                */
;*    Copyright   :  1994-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The O-expander creation.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_expander
   (include "Expand/expander.sch")
   (import  tools_error
	    engine_param
	    tools_misc)
   (export  (initialize-Oenv!)
	    (get-O-macro-toplevel)
	    (add-O-macro-toplevel!       ::obj)
	    (install-O-comptime-expander ::symbol ::procedure)
	    (find-O-expander             ::symbol)
	    (unbind-O-expander!          ::symbol)
	    (initialize-Genv!)
	    (install-G-comptime-expander ::symbol ::procedure)
	    (find-G-expander             ::symbol)
	    (unbind-G-expander!          ::symbol)
	    (to-be-macro!                ::symbol ::pair)
	    (check-to-be-macros)))

;*---------------------------------------------------------------------*/
;*    *Oenv* ...                                                       */
;*---------------------------------------------------------------------*/
(define *Oenv* '())

;*---------------------------------------------------------------------*/
;*    initialize-Oenv! ...                                             */
;*---------------------------------------------------------------------*/
(define (initialize-Oenv!)
   (set! *Oenv* (make-hashtable)))

;*---------------------------------------------------------------------*/
;*    *O-macro-toplevel* ...                                           */
;*---------------------------------------------------------------------*/
(define *O-macro-toplevel* '())

;*---------------------------------------------------------------------*/
;*    add-O-macro-toplevel! ...                                        */
;*---------------------------------------------------------------------*/
(define (add-O-macro-toplevel! exp)
   (set! *O-macro-toplevel* (cons exp *O-macro-toplevel*)))

;*---------------------------------------------------------------------*/
;*    get-O-macro-toplevel ...                                         */
;*---------------------------------------------------------------------*/
(define (get-O-macro-toplevel)
   (let ((v *O-macro-toplevel*))
      (set! *O-macro-toplevel* '())
      v))

;*---------------------------------------------------------------------*/
;*    install-O-comptime-expander ...                                  */
;*    -------------------------------------------------------------    */
;*    Les O-expanders sont des expanseurs d'optimisation. La difference*/
;*    avec les expanseurs normaux est que si une fonction portent le   */
;*    meme nom qu'eux, il sont retires de l'environment.               */
;*---------------------------------------------------------------------*/
(define (install-O-comptime-expander keyword function)
   (if (expander? (find-O-expander keyword))
       (internal-error "install-O-comptime-expander"
		       "Illegal re-installation of O-expander"
		       keyword)
       (let ((new (expander keyword (lambda (x e)
				       (let ((newx (function x e)))
					  (if (pair? newx)
					      (epairify newx x)
					      newx))))))
	  (hashtable-put! *Oenv* keyword new)
	  new)))

;*---------------------------------------------------------------------*/
;*    find-O-expander ...                                              */
;*    -------------------------------------------------------------    */
;*    We look for O-expander iff the variable *optim-O-macro?* has     */
;*    been set to #t.                                                  */
;*---------------------------------------------------------------------*/
(define (find-O-expander symbol)
   (and *optim-O-macro?* (hashtable-get *Oenv* symbol)))

;*---------------------------------------------------------------------*/
;*    unbind-O-expander! ...                                           */
;*---------------------------------------------------------------------*/
(define (unbind-O-expander! symbol)
   (hashtable-remove! *Oenv* symbol))

;*---------------------------------------------------------------------*/
;*    *Genv* ...                                                       */
;*---------------------------------------------------------------------*/
(define *Genv* '())

;*---------------------------------------------------------------------*/
;*    initialize-Genv! ...                                             */
;*---------------------------------------------------------------------*/
(define (initialize-Genv!)
   (set! *Genv* (make-hashtable)))

;*---------------------------------------------------------------------*/
;*    install-G-comptime-expander ...                                  */
;*    -------------------------------------------------------------    */
;*    Les G-expanders sont des expanseurs d'optimisation. La difference*/
;*    avec les expanseurs normaux est que si une fonction portent le   */
;*    meme nom qu'eux, il sont retires de l'environment.               */
;*---------------------------------------------------------------------*/
(define (install-G-comptime-expander keyword function)
   (if (expander? (find-G-expander keyword))
       (internal-error "install-G-comptime-expander"
		       "Illegal re-installation of G-expander"
		       keyword)
       (let ((new (expander keyword function)))
	  (hashtable-put! *Genv* keyword new)
	  new)))

;*---------------------------------------------------------------------*/
;*    find-G-expander ...                                              */
;*    -------------------------------------------------------------    */
;*    We look for G-expander iff the variable *optim-G-macro?* has     */
;*    been set to #t.                                                  */
;*---------------------------------------------------------------------*/
(define (find-G-expander symbol)
   (and (number? *compiler-debug*) (>= *compiler-debug* 1)
	(hashtable-get *Genv* symbol)))

;*---------------------------------------------------------------------*/
;*    unbind-G-expander! ...                                           */
;*---------------------------------------------------------------------*/
(define (unbind-G-expander! symbol)
   (hashtable-remove! *Genv* symbol))

;*---------------------------------------------------------------------*/
;*    *to-be-macros* ...                                               */
;*---------------------------------------------------------------------*/
(define *to-be-macros* '())

;*---------------------------------------------------------------------*/
;*    to-be-macro! ...                                                 */
;*---------------------------------------------------------------------*/
(define (to-be-macro! id src)
   (unless (assq id *to-be-macros*)
      (set! *to-be-macros* (cons (cons id src) *to-be-macros*))))

;*---------------------------------------------------------------------*/
;*    check-to-be-macros ...                                           */
;*---------------------------------------------------------------------*/
(define (check-to-be-macros)
   (for-each (lambda (m)
		(match-case (cdr m)
		   ((syntax ?id)
		    (unless (get-compiler-expander id)
		       (error "expand"
			      "Can't find syntax definition"
			      id)))
		   ((expander ?id)
		    (unless (get-compiler-expander id)
		       (error "expand"
			      "Can't find syntax definition"
			      id)))
		   (else
		    (unless (get-compiler-expander (car m))
		       (error "expand"
			      "Can't find macro definition"
			      (cdr m))))))
	     *to-be-macros*))
