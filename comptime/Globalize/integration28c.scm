;*=====================================================================*/
;*    .../project/bigloo/comptime/Globalize/integration28c.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 26 17:10:12 1995                          */
;*    Last change :  Tue Oct  3 07:43:27 2006 (serrano)                */
;*    Copyright   :  1995-2006 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The L property, L(f,g) stands for `f be integrated in g?'        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*    -------------------------------------------------------------    */
;*    The old integration scheme (prior to bigloo 2.8d).               */
;*---------------------------------------------------------------------*/
(module globalize_integration-28c
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
   (export  (set-integration-28c! ::global ::pair-nil ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    set-integration-28c! ...                                         */
;*---------------------------------------------------------------------*/
(define (set-integration-28c! global E G0 G1)
   (trace (globalize 2) "set-integration-28c! (" (shape global) ")\n")
   (for-each (lambda (f)
		(let ((g (find-integrator f)))
		   (if (variable? g)
		       (integrate-in! f g)
		       (force-globalize! f))))
	     G1))

;*---------------------------------------------------------------------*/
;*    integrate-in! ...                                                */
;*---------------------------------------------------------------------*/
(define (integrate-in! f g)
   (with-access::local f (value)
      (trace (globalize 2)
	     "   -> integrating " (shape f) " inside " (shape g) #\Newline)
      (sfun/Ginfo-integrator-set! value g)
      (sfun/Ginfo-G?-set! value #f)
      (with-access::sfun/Ginfo (variable-value g) (integrated)
	 (set! integrated (cons f integrated)))))

;*---------------------------------------------------------------------*/
;*    force-globalize! ...                                             */
;*---------------------------------------------------------------------*/
(define (force-globalize! f)
   (trace (globalize 2)
	  "   -> globalizing " (shape f) #\Newline)
   (sfun/Ginfo-integrator-set! (local-value f) #f))

;*---------------------------------------------------------------------*/
;*    find-integrator ...                                              */
;*---------------------------------------------------------------------*/
(define (find-integrator f::local)
   (with-access::sfun/Ginfo (local-value f) (integrator imark owner cfrom cto)
      (trace (globalize 3)
	     "      find integrator f=" (shape f) #\Newline
	     "                      integrator=" (shape integrator) #\Newline
	     "                      imark=" (shape imark) #\Newline
	     "                      owner=" (shape owner) #\Newline
	     "                      cfrom="
	     (shape (sfun/Ginfo-cfrom (local-value f))) #\Newline
	     "                      cto="
	     (shape (sfun/Ginfo-cto (local-value f))) #\Newline
	     "                      guess=" (shape (find-integrator* f)) #\Newline)
      (and *optim-integrate?*
	   (let ((guess (find-integrator* f)))
	      (cond
		 ((null? guess)
		  ;; an internal error because all the functions send to
		  ;; FIND-INTEGRATOR belong to G1 which is the set of functions
		  ;; called by at least one global or escape function
		  (internal-error 'find-integrator "empty integrator*" f)
		  #f)
		 ((pair? (cdr guess))
		  ;; cannot integrate it because it is called
		  ;; from two globalized functions
		  #f)
		 ((integrate? f (car guess))
		  ;; it can be integrated
		  (car guess))
		 (else
		  #f))))))

;*---------------------------------------------------------------------*/
;*    integrator? ...                                                  */
;*    -------------------------------------------------------------    */
;*    Is g a possible integrator for f?                                */
;*---------------------------------------------------------------------*/
(define (integrator? f g)
   (or (eq? (sfun/Ginfo-owner (local-value f)) g)
       (or (global? g)
	   (and (local? g) (local/Ginfo-escape? g)))))
   
;*---------------------------------------------------------------------*/
;*    *escape-mark* ...                                                */
;*---------------------------------------------------------------------*/
(define *escape-mark* 0)

;*---------------------------------------------------------------------*/
;*    find-integrator* ...                                             */
;*---------------------------------------------------------------------*/
(define (find-integrator* f)
   (or (owner-integrator f)
       (find-other-integrator* f)))

;*---------------------------------------------------------------------*/
;*    owner-integrator ...                                             */
;*    -------------------------------------------------------------    */
;*    The owner is the integrator if it is the only one to call        */
;*    the function (and self recursive calls to the function).         */
;*---------------------------------------------------------------------*/
(define (owner-integrator f)
   (let ((owner (sfun/Ginfo-owner (local-value f))))
      (if (every? (lambda (x)
		     (or (eq? x f) (eq? x owner)))
		  (sfun/Ginfo-cfrom (local-value f)))
	  (list owner)
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    find-other-integrator* ...                                       */
;*---------------------------------------------------------------------*/
(define (find-other-integrator* f)
   (set! *escape-mark* (+fx 1 *escape-mark*))
   (let loop ((cfrom (sfun/Ginfo-cfrom (local-value f)))
	      (escape* '()))
      (if (null? cfrom)
	  escape*
	  (let ((g (car cfrom)))
	     (if (eq? (sfun/Ginfo-cfrom* (variable-value g)) *escape-mark*)
		 (loop (cdr cfrom) escape*)
		 (begin 
		    (sfun/Ginfo-cfrom*-set! (variable-value g) *escape-mark*)
		    (cond
		       ((integrator? f g)
			(loop (cdr cfrom) (cons g escape*)))
		       (else
			(loop (cdr cfrom)
			      (loop (sfun/Ginfo-cfrom (local-value g))
				    escape*))))))))))

;*---------------------------------------------------------------------*/
;*    *integrate-mark* ...                                             */
;*---------------------------------------------------------------------*/
(define *integrate-mark* 0)

;*---------------------------------------------------------------------*/
;*    integrate? ...                                                   */
;*    -------------------------------------------------------------    */
;*    It is possible to integrate f in g                               */
;*---------------------------------------------------------------------*/
(define (integrate? f::variable g::local)
   [assert (g) (integrator? f g)]
   (set! *integrate-mark* (+fx *integrate-mark* 1))
   (define (visible?-avant-2oct06 h)
      ;; is h visible from g?
      (or (eq? h g)
	  (and (not (global? h))
	       (not (local/Ginfo-escape? h))
	       (integrate? h))))
   (define (ok-call? g)
      (or (eq? f g)
	  (eq? (sfun/Ginfo-owner (local-value f)) g)
	  (local/Ginfo-escape? g)
	  (global? g)
	  (eq? (sfun/Ginfo-integrator (local-value g)) f)))
   (define (visible? h)
      ;; is h visible from g?
      (or (eq? h g)
	  (or (global? h)
	      (not (local/Ginfo-escape? h))
	      (and (every? ok-call? (sfun/Ginfo-cto (local-value h)))
		   (every? ok-call? (sfun/Ginfo-cfrom (local-value h))))
	      (eq? (sfun/Ginfo-owner (local-value g)) h)
	      (integrate? h))))
   (define (integrate? f)
      ;; is f integrable in g?
      (or (eq? f g)
	  (and (local? g)
	       (let ((v (local-value f)))
		  (with-access::sfun/Ginfo v (cfrom cto imark)
		     (and (fixnum? imark)
			  (or (=fx imark *integrate-mark*)
			      (and (<fx imark *integrate-mark*)
				   (begin
				      (set! imark *integrate-mark*)
				      (let ((r (and (every? visible? cfrom)
						    (every? visible? cto))))
					 (if (not r) (set! imark #f))
					 r))))))))))
   (let ((r (integrate? f)))
      (trace (globalize 4) "         >>> integrate? "
	     (shape f) " in " (shape g) " = " r
	     ", because imark=" (shape (sfun/Ginfo-imark (local-value f))) #\Newline)
      r))

      
       
