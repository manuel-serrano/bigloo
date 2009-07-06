;*=====================================================================*/
;*    .../prgm/project/bigloo/comptime/Globalize/integration.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 26 17:10:12 1995                          */
;*    Last change :  Tue Oct  3 11:26:26 2006 (serrano)                */
;*    Copyright   :  1995-2006 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The L property, L(f,g) stands for `f be integrated in g?'        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_integration
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    type_type
	    ast_var
	    ast_node
	    globalize_ginfo
	    globalize_globalize
	    (union globalize_kapture)
	    tools_error)
   (export  (set-integration! ::global ::pair-nil ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    set-integration! ...                                             */
;*---------------------------------------------------------------------*/
(define (set-integration! global E G0 G1)
   (trace (globalize 2) "set-integration! (" (shape global) ")\n")
   ;; the escaping functions are globalized
   (for-each (lambda (f)
		(with-access::local/Ginfo f (globalized?)
		   (set! globalized? #t)))
	     E)
   ;; a fix point iteration to get all the local functions
   ;; that cannot be integrated
   (let loop ((fns (cons global E)))
      (when (pair? fns)
	 (loop (apply append (map traverse-call-to! fns)))))
   ;; mark all the none globalized, not integrated yet local functions
   (for-each (lambda (f)
		(unless (is-globalized? f)
		   ;; we have to find the good stop to integrate the
		   ;; function, the deeper the better
		   (with-access::sfun/Ginfo (local-value f) (integrator imark)
		      (set! integrator imark))))
	     G1)
   ;; actually integrate the functions
   (for-each (lambda (f)
		(with-access::sfun/Ginfo (local-value f) (integrator)
		   (trace (globalize 2) "  " (shape f) ": ")
		   (cond
		      ((is-globalized? f)
		       (trace (globalize 2) "not integrated (globalized)\n")
		       (force-globalize! f))
		      ((variable? integrator)
		       (trace (globalize 2)
			      "integrated in "
			      (shape integrator) #\Newline)
		       (integrate-in! f integrator))
		      (else
		       (trace (globalize 2) "no integrator yet\n")
		       (internal-error "set-integration!"
				       "No integration for "
				       (shape f))))))
	     G1))

;*---------------------------------------------------------------------*/
;*    traverse-call-to! ...                                            */
;*---------------------------------------------------------------------*/
(define (traverse-call-to! f)
   (let ((integrator (if (is-globalized? f)
			 f
			 (sfun/Ginfo-imark (local-value f)))))
      (filter-map (lambda (g)
		     (unless (or (is-globalized? g) (eq? f g))
			(with-access::sfun/Ginfo (local-value g) (imark)
			   (cond
			      ((eq? imark integrator)
			       #f)
			      ((eq? imark #unspecified)
			       (set! imark integrator)
			       g)
			      (else
			       (local/Ginfo-globalized?-set! g #t)
			       (set! imark #f)
			       g)))))
		  (sfun/Ginfo-cto (variable-value f)))))

;*---------------------------------------------------------------------*/
;*    is-globalized? ...                                               */
;*    -------------------------------------------------------------    */
;*    Is a function globalized, in the most general sense. A           */
;*    global is globalized, a local is globalized if it escapes        */
;*    or cannot be integrated.                                         */
;*---------------------------------------------------------------------*/
(define (is-globalized? v)
   (or (global? v) (local/Ginfo-globalized? v)))

;*---------------------------------------------------------------------*/
;*    integrate-in! ...                                                */
;*---------------------------------------------------------------------*/
(define (integrate-in! f g)
   (with-access::local f (value)
      (sfun/Ginfo-integrator-set! value g)
      (sfun/Ginfo-G?-set! value #f)
      (with-access::sfun/Ginfo (variable-value g) (integrated)
	 (set! integrated (cons f integrated)))))

;*---------------------------------------------------------------------*/
;*    force-globalize! ...                                             */
;*---------------------------------------------------------------------*/
(define (force-globalize! f)
   (sfun/Ginfo-integrator-set! (local-value f) #f))
