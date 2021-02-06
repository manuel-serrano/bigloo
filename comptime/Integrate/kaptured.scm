;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/kaptured.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 11:03:12 1995                          */
;*    Last change :  Wed Jan 22 08:46:01 2014 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
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
   (for-each set-one-kaptured! local*))

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
(define (set-one-kaptured-old! local locking)
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
		    (let ((other-kaptured (set-one-kaptured-old! (car cto)
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
(define (set-one-kaptured! local)
   (trace (integrate 2) "### set-one-kaptured: " (shape local)
      #\Newline)
   (let ((info (local-value local))
	 (kaptured (get-one-kaptured local '())))
      (sfun/Iinfo-kaptured-set! info kaptured)
      (for-each (lambda (local)
		   (svar/Iinfo-kaptured?-set!
		      (local-value local)
		      #t))
	 kaptured)))

;*---------------------------------------------------------------------*/
;*    local-cto ...                                                    */
;*---------------------------------------------------------------------*/
(define (local-cto local::local)
   (let* ((info (local-value local))
	  (cto (sfun/Iinfo-cto info)))
      (if (or (pair? cto) (null? cto))
	  cto
	  (let ((body (sfun-body info)))
	     (sfun/Iinfo-cto-set! info '())
	     (set-cto! body local)
	     (sfun/Iinfo-cto info)))))

;*---------------------------------------------------------------------*/
;*    get-one-kaptured ...                                             */
;*---------------------------------------------------------------------*/
(define (get-one-kaptured local stack)
   (trace (integrate 2) "### get-one-kaptured: " (shape local)
      #\Newline)
   (let* ((info (local-value local))
	  (kaptured (sfun/Iinfo-kaptured info)))
      (cond
	 ((or (pair? kaptured) (null? kaptured))
	  kaptured)
	 ((memq local stack)
	  '())
	 (else
	  (let loop ((kaptured '())
		     (cto (local-cto local))
		     (nstack (cons local stack)))
	     (cond
		((null? cto)
		 (let* ((body (sfun-body info))
			(free (get-free-vars body local))
			(fkaptured (free-from kaptured local)))
		    (union (cons free fkaptured))))
		((sfun/Iinfo-G? (local-value (car cto)))
		 (let ((other-kaptured (get-one-kaptured (car cto) nstack)))
		    (loop (cons other-kaptured kaptured)
		       (cdr cto)
		       (cons (car cto) nstack))))
		(else
		 (loop kaptured (cdr cto) nstack))))))))
   
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
		 

