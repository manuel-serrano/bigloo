;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/kaptured.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 11:03:12 1995                          */
;*    Last change :  Thu Aug  9 07:07:20 2007 (serrano)                */
;*    Copyright   :  1995-2007 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We compute the list of the kaptured variables for each           */
;*    globalized function. The used method is very close to            */
;*    the one of the Globalization pass.                               */
;*=====================================================================*/
   
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_kaptured
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node
	    integrate_info
	    integrate_free
	    integrate_cto)
   (export  (set-kaptured! locals)))

;*---------------------------------------------------------------------*/
;*    set-kaptured! ...                                                */
;*---------------------------------------------------------------------*/
(define (set-kaptured! local*)
   (for-each (lambda (local)
		(set-one-kaptured! local local))
	     local*))

;*---------------------------------------------------------------------*/
;*    set-one-kaptured! ...                                            */
;*    -------------------------------------------------------------    */
;*    This function computes the set of kaptured variables and         */
;*    the future globalized body.                                      */
;*    -------------------------------------------------------------    */
;*    Take care: `get-free-vars' also compute the list `cto'. There    */
;*    is `a priori' no reason for this but the reason we do it now     */
;*    is that we compute a restricted subset of the real `cto', we     */
;*    are just interested in computed cto of local globalized          */
;*    functions.                                                       */
;*---------------------------------------------------------------------*/
(define (set-one-kaptured! local locking)
   (trace (integrate 2) "### set-one-kaptured: " (shape local) " [locking:"
	  (shape locking) #\]
	  #\Newline)
   (let* ((info     (local-value local))
	  (kaptured (sfun/Iinfo-kaptured info)))
      (cond
	 ((or (pair? kaptured) (null? kaptured))
	  (trace (integrate 3) "--> (or pair? null?) [" (shape local)
		 "] " (shape kaptured) #\Newline)
	  (vector #t locking kaptured))
	 ((local? kaptured)
	  (trace (integrate 3) "--> local? [" (shape local)
		 "] " (shape kaptured) #\Newline)
	  (vector #f locking '()))
	 (else
	  (let ((body (sfun-body (local-value local))))
	     (set-cto! body local)
	     (trace (integrate 3) "--> cto [" (shape local) "] "
		    (shape (sfun/Iinfo-cto info)) " or "
		    (shape (sfun/Iinfo-cto (local-value local))) #\Newline)
	     ;; before entering the recursion we mark this function
	     ;; with it self.
	     (sfun/Iinfo-kaptured-set! info local)
	     ;; we walk across the call-graph
	     (let loop ((kaptured '())
			(cto      (sfun/Iinfo-cto info))
			(setter?  #t))
		(trace (integrate 4)
		       "   [" (shape local)"].cto     : " (shape cto)
		       #\Newline
		       "   [" (shape local)"].kaptured: " (shape kaptured)
		       #\Newline)
		(cond
		   ((null? cto)
		    (let* ((free      (get-free-vars body local))
			   (fkaptured (free-from kaptured local))
			   (rkaptured (union (cons free fkaptured))))
		       (trace (integrate 4)
			      "        free(" (shape local) ") : "
			      (shape free) #\Newline)
		       (trace (integrate 4)
			      "    kaptured(" (shape local) ") : "
			      (shape kaptured) #\Newline)
		       (trace (integrate 4)
			      "   fkaptured(" (shape local) ") : "
			      (shape fkaptured) #\Newline)
		       (trace (integrate 4)
			      "   rkaptured(" (shape local) ") : "
			      (shape rkaptured) #\Newline)
		       (if setter?
			   (begin
			      ;; we store kaptured variables
			      (sfun/Iinfo-kaptured-set! info rkaptured)
			      ;; we mark kaptured variables
			      (for-each (lambda (local)
		 			   (svar/Iinfo-kaptured?-set!
					    (local-value local)
					    #t))
					rkaptured))
			   ;; we restore the uncomputed value (see tools.scm)
			   (sfun/Iinfo-kaptured-set! info #unspecified))
		       (vector setter? locking rkaptured)))
		   ((eq? (car cto) local)
		    (loop kaptured
			  (cdr cto)
			  setter?))
		   ((sfun/Iinfo-G? (local-value (car cto)))
		    (let ((other-kaptured (set-one-kaptured! (car cto)
							     locking)))
		       (if (not (vector-ref other-kaptured 0))
			   (loop (cons (vector-ref other-kaptured 2) kaptured)
				 (cdr cto)
				 (and setter?
				      (eq? (vector-ref other-kaptured 1)
					   local)))
			   (loop (cons (vector-ref other-kaptured 2) kaptured)
				 (cdr cto)
				 setter?))))
		   (else
		    (loop kaptured
			  (cdr cto)
			  setter?)))))))))

;*---------------------------------------------------------------------*/
;*    *union-round* ...                                                */
;*---------------------------------------------------------------------*/
(define *union-round* 0)

;*---------------------------------------------------------------------*/
;*    union ...                                                        */
;*---------------------------------------------------------------------*/
(define (union sets)
   (set! *union-round* (+fx 1 *union-round*))
   (let loop ((sets  sets)
	      (union '()))
      (if (null? sets)
	  union
	  (let liip ((set   (car sets))
		     (union union))
	     (cond
		((null? set)
		 (loop (cdr sets) union))
		((eq? (svar/Iinfo-u-mark (local-value (car set)))
		      *union-round*)
		 (liip (cdr set) union))
		(else
		 (svar/Iinfo-u-mark-set! (local-value (car set)) *union-round*)
		 (liip (cdr set) (cons (car set) union))))))))
		 

