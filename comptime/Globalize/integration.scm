;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/Globalize/integration.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 26 17:10:12 1995                          */
;*    Last change :  Fri Oct 26 08:06:40 2018 (serrano)                */
;*    Copyright   :  1995-2018 Manuel Serrano, see LICENSE file        */
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
	 (loop
	    (delete-duplicates!
	       (apply append (map traverse-call-to! fns))))))
   ;; mark all location functions not yet globalized, or integrated
   (for-each (lambda (f)
		(unless (is-globalized? f)
		   ;; we have to find the good location to integrate the
		   ;; function, the deeper the better
		   (with-access::local f (id)
		      (with-access::sfun/Ginfo (local-value f) (integrator imark)
			 (set! integrator (car imark))))))
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
   (let ((integrators (if (is-globalized? f)
			  (list f)
			  (append (sfun/Ginfo-imark (local-value f)) (list f)))))
      (filter-map (lambda (g)
		     (with-access::variable g (id)
			(unless (or (is-globalized? g) (eq? f g))
			   (with-access::sfun/Ginfo (local-value g) (imark)
			      (if (eq? imark '())
				  (begin
				     (set! imark integrators)
				     g)
				  (let ((nimark (filter (lambda (i) (memq i integrators)) imark)))
				     (cond
					((null? nimark)
					 (local/Ginfo-globalized?-set! g #t)
					 (set! imark '())
					 g)
					((not (=fx (length nimark) (length imark)))
					 (set! imark nimark)
					 g))))))))
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
;*    force-globalize! ...                                             */
;*---------------------------------------------------------------------*/
(define (force-globalize! f)
   (sfun/Ginfo-integrator-set! (local-value f) #f))

;*---------------------------------------------------------------------*/
;*    integrate-in! ...                                                */
;*---------------------------------------------------------------------*/
(define (integrate-in! f integrator)
   (with-access::local f (value id)
      (sfun/Ginfo-integrator-set! value integrator)
      (sfun/Ginfo-G?-set! value #f)
      (with-access::sfun/Ginfo (variable-value integrator) (integrated)
	 (set! integrated (cons f integrated)))))

