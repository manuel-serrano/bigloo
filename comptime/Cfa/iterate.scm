;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/iterate.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 22 18:11:52 1995                          */
;*    Last change :  Sun Jun 26 06:41:58 2016 (serrano)                */
;*    Copyright   :  1995-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    THE control flow analysis engine                                 */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_iterate
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_unit
	    cfa_cfa
	    cfa_info
	    cfa_info2
	    cfa_loose
	    cfa_approx)
   (export  (cfa-iterate-to-fixpoint! globals)
	    (cfa-intern-sfun!::approx ::intern-sfun/Cinfo ::obj)
	    (generic cfa-export-var! ::value ::obj)
	    (continue-cfa! reason)
	    (cfa-iterate! globals)
	    (cfa-current)
	    *cfa-stamp*))

;*---------------------------------------------------------------------*/
;*    cfa-iterate-to-fixpoint! ...                                     */
;*---------------------------------------------------------------------*/
(define (cfa-iterate-to-fixpoint! globals)
   (trace cfa "================== iterate ===========================\n")
   ;; we reset the global stamp
   (set! *cfa-stamp* -1)
   ;; we collect all the exported variables (both functions and
   ;; variables). They are the root of the iteration process.
   (let ((glodefs '()))
      (for-each (lambda (g)
		   (if (or (eq? (global-import g) 'export)
			   (exported-closure? g))
		       (set! glodefs (cons g glodefs))))
	 globals)
      ;; add the top level forms
      (set! glodefs (append (unit-initializers) glodefs))
      ;; start iterations
      (continue-cfa! 'init)
      ;; and loop
      (let loop ()
	 (if (continue-cfa?)
	     (begin
		(cfa-iterate! glodefs)
		(loop))
	     glodefs))))

;*---------------------------------------------------------------------*/
;*    exported-closure? ...                                            */
;*---------------------------------------------------------------------*/
(define (exported-closure? g)
   (and (eq? (global-removable g) 'never)
	(let ((val (global-value g)))
	   (and (sfun? val)
		(global? (sfun-the-closure-global val))
		(eq? (global-import (sfun-the-closure-global val)) 'export)))))

;*---------------------------------------------------------------------*/
;*    cfa-iterate! ...                                                 */
;*---------------------------------------------------------------------*/
(define (cfa-iterate! globals)
   (stop-cfa!)
   (set! *cfa-stamp* (+fx 1 *cfa-stamp*))
   (trace cfa #\Newline "=======> Cfa iteration!: " *cfa-stamp* #\Newline)
   (for-each (lambda (g)
		(trace (cfa 2) "Exporting " (shape g) #\: #\Newline)
		(cfa-export-var! (global-value g) g)
		(trace (cfa 2) #\Newline)) 
      globals)
   (trace cfa #\Newline))

;*---------------------------------------------------------------------*/
;*    cfa-export-var! ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (cfa-export-var! value::value owner))

;*---------------------------------------------------------------------*/
;*    cfa-export-var! ::svar ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cfa-export-var! value::svar/Cinfo owner)
   (with-access::svar/Cinfo value (stamp)
      (trace (cfa 3) "~~~ cfa-export/var!::svar/Cinfo[stamp: " stamp
	 " *cfa-stamp*: " *cfa-stamp*
	 "]" #\Newline)
      (if (=fx stamp *cfa-stamp*)
	  (cfa-variable-value-approx value)
	  (begin
	     (set! stamp *cfa-stamp*)
	     (loose! (cfa-variable-value-approx value) 'all)))))

;*---------------------------------------------------------------------*/
;*    cfa-export-var! ::intern-sfun/Cinfo ...                          */
;*---------------------------------------------------------------------*/
(define-method (cfa-export-var! value::intern-sfun/Cinfo owner)
   (with-access::intern-sfun/Cinfo value (stamp args approx)
      (trace (cfa 3) "  ~~~ cfa-export-var!::intern-sfun/Cinfo[stamp: " stamp 
	 " *cfa-stamp*: " *cfa-stamp*
	 "] " (shape owner) #\Newline)
      (if (=fx stamp *cfa-stamp*)
	  (begin
	     (set! stamp *cfa-stamp*)
	     approx)
	  (begin
	     ;; For each iteration, we re-loose the approximation of
	     ;; the formal parameters. Doing this, we don't have to do
	     ;; check when we add an approximation of a previous set
	     ;; if this new set contains `top' or not.
	     (for-each (lambda (local)
			  (let ((val (local-value local)))
			     (trace (cfa 3) " ~~~ formal " (shape local)
				" clo-env?: " (svar/Cinfo-clo-env? val)
				" val: " (shape (svar/Cinfo-approx val))
				#\Newline)
			     (unless (svar/Cinfo-clo-env? val)
				(approx-set-top! (svar/Cinfo-approx val)))))
		args)
	     ;; after the formals, we loose the result.
	     (loose! (cfa-intern-sfun! value owner) 'all)))))
 
;*---------------------------------------------------------------------*/
;*    cfa-intern-sfun! ::intern-sfun/Cinfo ...                         */
;*---------------------------------------------------------------------*/
(define (cfa-intern-sfun!::approx sfun::intern-sfun/Cinfo owner)
   
   (define (polymorphic approx)
      (with-access::intern-sfun/Cinfo sfun (polymorphic?)
	 (when polymorphic?
	    (with-access::approx approx (type)
	       (set! type (get-bigloo-type type))))
	 approx))
   
   (with-access::intern-sfun/Cinfo sfun (stamp body approx args)
      (if (=fx stamp *cfa-stamp*)
	  (begin
	     (trace (cfa 2) "<<< " (shape owner)
		" <- " (shape approx) #\Newline)
	     (polymorphic approx))
	  (begin
	     (trace (cfa 2) ">>> " (shape owner) #\Newline)
	     (set! stamp *cfa-stamp*)
	     (let ((cur *cfa-current*))
		(set! *cfa-current* (format "~a[~a]" (variable-id owner) stamp))
		(union-approx-filter! approx (cfa! body))
		(trace (cfa 3) "<<< " (shape owner) " <= " (shape approx)
		   #\Newline)
		(set! *cfa-current* cur))
	     (polymorphic approx)))))

;*---------------------------------------------------------------------*/
;*    The iteration process control                                    */
;*---------------------------------------------------------------------*/
(define *cfa-continue?* #unspecified)
(define *cfa-stamp* -1)
(define *cfa-current* #unspecified)

;*---------------------------------------------------------------------*/
;*    cfa-current ...                                                  */
;*---------------------------------------------------------------------*/
(define (cfa-current)
   *cfa-current*)

;*---------------------------------------------------------------------*/
;*    continue-cfa! ...                                                */
;*---------------------------------------------------------------------*/
(define (continue-cfa! reason)
   (when (not *cfa-continue?*)
      (trace (cfa 2) (cfa-current) ": continue-cfa! (" reason ")\n"))
   (set! *cfa-continue?* #t))

;*---------------------------------------------------------------------*/
;*    continue-cfa? ...                                                */
;*---------------------------------------------------------------------*/
(define (continue-cfa?)
   *cfa-continue?*)

;*---------------------------------------------------------------------*/
;*    stop-cfa! ...                                                    */
;*---------------------------------------------------------------------*/
(define (stop-cfa!)
   (set! *cfa-continue?* #f))


   

