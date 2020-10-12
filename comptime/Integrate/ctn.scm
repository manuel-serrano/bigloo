;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/ctn.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 15 14:10:09 1995                          */
;*    Last change :  Thu Nov 17 08:35:44 2016 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The computation of `Cn' and `Ct'.                                */
;*       Cn(f,g) === exists k, A(f,g,k)                                */
;*       Ct(f,g) === A(f,g,t)                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_ctn
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node
	    integrate_info
	    integrate_a)
   (export  (Cn&Ct! <a-set>)))

;*---------------------------------------------------------------------*/
;*    Cn&Ct! ...                                                       */
;*    -------------------------------------------------------------    */
;*    This function returns the list of the globalized functions due   */
;*    to the Cn property.                                              */
;*---------------------------------------------------------------------*/
(define (Cn&Ct! A)
   (let loop ((As    A)
	      (G/cn '()))
      (if (null? As)
	  (begin
	     (trace-ctn)
	     G/cn)
	  (let* ((A  (car As))
		 (f  (car A))
		 (g  (cadr A))
		 (k  (caddr A))
		 (fi (variable-value f))
		 (gi (variable-value g)))
	     (cond
		((global? g)
		 (loop (cdr As) G/cn))
		((sfun/Iinfo-forceG? gi)
		 ;; CARE, MS 28Mar2011: fix the trick a integrate_a)
		 (if (not (sfun/Iinfo-G? gi))
		     (begin
			(sfun/Iinfo-G?-set! gi #t)
			(loop (cdr As) (cons g G/cn)))
		     (loop (cdr As) G/cn)))
		((eq? k 'tail)
		 (sfun/Iinfo-Ct-set! fi (cons g (sfun/Iinfo-Ct fi)))
		 (when (and (not (eq? f g))
			    (not (memq g (sfun/Iinfo-kont fi))))
		     (sfun/Iinfo-kont-set! fi (cons g (sfun/Iinfo-kont fi))))
		 (loop (cdr As) G/cn))
		((eq? k 'escape)
		 (error 'Cn&Ct! "SHould not be here" A))
		((sfun/Iinfo-U gi)
		 (sfun/Iinfo-Ct-set! fi (cons g (sfun/Iinfo-Ct fi)))
		 (if (not (memq g (sfun/Iinfo-kont fi)))
		     (sfun/Iinfo-kont-set! fi (cons g (sfun/Iinfo-kont fi))))
		 (loop (cdr As) G/cn))
		(else
		 (sfun/Iinfo-Cn-set! fi (cons g (sfun/Iinfo-Cn fi)))
		 (if (not (sfun/Iinfo-G? gi))
		     (begin
			(sfun/Iinfo-G?-set! gi #t)
			(loop (cdr As) (cons g G/cn)))
		     (loop (cdr As) G/cn))))))))

;*---------------------------------------------------------------------*/
;*    trace-ctn ...                                                    */
;*---------------------------------------------------------------------*/
(define (trace-ctn)
   (trace (integrate 2)
	  (begin
	     (fprint *trace-port* "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
	     (fprint *trace-port* "C: " #\Newline)
	     (for-each (lambda (p)
			  (let ((ifun (variable-value p)))
			     (fprint *trace-port*
				     " " (shape p) #\: #\Newline
				     "   Cn        : "
				     (shape (sfun/Iinfo-Cn ifun))
				     #\Newline
				     "   Ct        : "
				     (shape (sfun/Iinfo-Ct ifun))
				     #\Newline
				     "   Cont      : "
				     (shape (sfun/Iinfo-kont ifun)))))
		       *phi*)
	     "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
	  #\Newline))

