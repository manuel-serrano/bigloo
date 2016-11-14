;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Effect/walk.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:25:23 1995                          */
;*    Last change :  Mon Nov 14 14:46:33 2016 (serrano)                */
;*    Copyright   :  1995-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The computation of the effect property.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module effect_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    ast_var
	    ast_node
	    effect_cgraph
	    effect_spread
	    effect_feffect
	    engine_param)
   (export  (effect-walk! globals ::bool)
	    (write-effect globals)))

;*---------------------------------------------------------------------*/
;*    effect-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (effect-walk! globals feffect)
   (pass-prelude "Effect")
   ;; we compute the inversed call-graph
   (trace effect "cgraph..." #\Newline)
   (for-each (lambda (global)
		(trace effect+ "call-graph " (shape global) #\Newline)
		(trace effect "call-graph " (shape global) #\Newline)
		(fun-call-graph! global))
	     globals)
   ;; then we iterate to mark all function/side-effect
   (trace effect "fix point..." #\Newline)
   (iterate-to-fix-point! (get-var/side-effect))
   ;; we mark functions which are known not to make side effect
   (for-each (lambda (var)
		(let ((fun (variable-value var)))
		   (when (eq? (fun-side-effect fun) #unspecified)
		      (fun-side-effect-set! fun #f))))
	     (get-var/all))
   ;; we spread the effect properties
   (trace effect "spread..." #\Newline)
   (for-each (lambda (global)
		(spread-side-effect! (sfun-body (global-value global))))
      globals)
   ;; the refined effect computation
   (trace effect "feffect..." #\Newline)
   ;; CARE: *saw* (formerly only *saw*)
   (if feffect (feffect! globals))
   ;; we are done now.
   (trace effect "Effect done." #\Newline)
   (pass-postlude globals reset-effect-tables!))
	   
;*---------------------------------------------------------------------*/
;*    iterate-to-fix-point! ...                                        */
;*---------------------------------------------------------------------*/
(define (iterate-to-fix-point! W)
   (when (pair? W)
      (for-each
       (lambda (var)
	  (let ((fun (variable-value var)))
	     (unless (eq? (fun-side-effect fun) #t)
		(fun-side-effect-set! fun #t)
		(cond
		   ((local/from? var)
		    (iterate-to-fix-point! (local/from-from var)))
		   ((global/from? var)
		    (iterate-to-fix-point! (global/from-from var)))))))
       W)))

;*---------------------------------------------------------------------*/
;*    write-effect ...                                                 */
;*---------------------------------------------------------------------*/
(define (write-effect globals)
   (define (write-feffect g)
      (let ((fe (fun-effect (variable-value g))))
	 (if (feffect? fe)
	     (with-access::feffect fe (read write)
		(if (not (and (eq? read 'top) (eq? write 'top)))
		    (begin
		       (display* "   (" (shape g) " (effect")
		       (if (not (null? read)) (display* " (read " read ")"))
		       (if (not (null? write)) (display* " (write " write ")"))
		       (print "))")))))))
   (let* ((oname (if (string? *dest*)
		     *dest*
		     (if (and (pair? *src-files*) (string? (car *src-files*)))
			 (string-append (prefix (car *src-files*)) ".sch")
			 #f)))
	  (port (if (string? oname)
		    (open-output-file oname)
		    (current-output-port))))
      (if (not (output-port? port))
	  (error "write-effect" "Can't open output file" oname)
	  (unwind-protect
	     (begin
		(display "(directives\n (pragma\n")
		(for-each write-feffect globals)
		(display " ))\n"))
	     (if (not (eq? port (current-output-port)))
		 (close-output-port port))))))
   
