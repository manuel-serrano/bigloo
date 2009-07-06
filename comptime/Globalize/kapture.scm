;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/kapture.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 30 07:30:09 1995                          */
;*    Last change :  Tue Oct  3 09:46:21 2006 (serrano)                */
;*    Copyright   :  1995-2006 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `kaptured' computation                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_kapture
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    tools_args
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_sexp
	    ast_local
	    globalize_ginfo
	    globalize_node
	    globalize_free
	    globalize_clocto)
   (export  (set-kaptured! <local>*)
	    (union sets)))

;*---------------------------------------------------------------------*/
;*    set-kaptured! ...                                                */
;*---------------------------------------------------------------------*/
(define (set-kaptured! local*)
   ;; first we compute the transitive closure of the `cto' property
   ;; (i.e. we add to local the cto fields of its including local functions).
   (for-each (lambda (local)
		(cto-transitive-closure! local))
	     local*)
   ;; then we compute the kaptured property.
   (for-each (lambda (local)
		(set-one-kaptured! local local))
	     local*))

;*---------------------------------------------------------------------*/
;*    set-one-kaptured! ...                                            */
;*    -------------------------------------------------------------    */
;*    This function computes the set of kaptured variables and         */
;*    the future globalized body.                                      */
;*---------------------------------------------------------------------*/
(define (set-one-kaptured! local::local locking)
   (trace (globalize 5) "set-one-kaptured: " (shape local)
	  " [locking:" (shape locking) "]"
	  " [cto=" (shape (sfun/Ginfo-cto* (local-value local))) "]"
	  #\Newline)
   (let* ((info     (local-value local))
	  (kaptured (sfun/Ginfo-kaptured info)))
      (cond
	 ((or (pair? kaptured) (null? kaptured))
	  (trace (globalize 5) "--> (or pair? null?) [" (shape local)
		 "] " (shape kaptured) #\Newline)
	  (vector #t locking kaptured))
	 ((local? kaptured)
	  (trace (globalize 5) "--> local? [" (shape local)
		 "] " (shape kaptured) #\Newline)
	  (vector #f locking '()))
	 (else
	  (trace (globalize 5) "--> cto* [" (shape local) "] "
		 (shape (sfun/Ginfo-cto* info)) #\Newline)
	  (let ((new-body (sfun/Ginfo-new-body info)))
	     ;; before entering the recursion we mark this function
	     (sfun/Ginfo-kaptured-set! info local)
	     ;; we walk across the call-graph
	     (let loop ((kaptured '())
			(cto      (append (sfun/Ginfo-cto* info)
					  (sfun/Ginfo-cfunction info)))
			(setter?  #t))
		(trace (globalize 5)
		       "loop( " (shape local) " ) : " #\Newline
		       "      "  (shape kaptured) #\Newline
		       "      "  (shape cto) #\Newline
		       "      "  setter? #\Newline)
		(cond
		   ((null? cto)
		    (let* ((free      (get-free-vars new-body local))
			   (fkaptured (free-from kaptured local))
			   (kaptured  (union (cons free fkaptured))))
		       (trace globalize
			      "   kaptured(" (shape local) ") : "
			      (shape kaptured) #\Newline)
		       (trace (globalize 5)
			      "       free(" (shape local) ") : "
			      (shape free) #\Newline)
		       (if setter?
			   (begin
			      ;; we store kaptured variables
			      (sfun/Ginfo-kaptured-set! info kaptured)
			      ;; we mark that kaptured variable are
			      (for-each (lambda (local)
					   (svar/Ginfo-kaptured?-set!
					    (local-value local) #t))
					kaptured))
			   (sfun/Ginfo-kaptured-set! info #f))
		       (vector setter? locking kaptured)))
		   ((eq? (car cto) local)
		    (trace (globalize 5) "  (eq? (car cto) local)"
			   #\Newline)
		    (loop kaptured
			  (cdr cto)
			  setter?))
		   ((sfun/Ginfo-G? (local-value (car cto)))
		    (trace (globalize 5) "  (sfun/Ginfo-G? (car cto))"
			   #\Newline)
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
		    (trace (globalize 5) "  not globalized (car cto)"
			   #\Newline)
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
		 (loop (cdr sets)
		       union))
		((fun? (local-value (car set)))
		 (if (eq? (sfun/Ginfo-umark (local-value (car set)))
			  *union-round*)
		     (liip (cdr set) union)
		     (begin
			(sfun/Ginfo-umark-set! (local-value (car set))
					      *union-round*)
			(liip (cdr set)
			      (cons (car set) union)))))
		((eq? (svar/Ginfo-mark (local-value (car set))) *union-round*)
		 (liip (cdr set) union))
		(else
		 (svar/Ginfo-mark-set! (local-value (car set)) *union-round*)
		 (liip (cdr set)
		       (cons (car set) union))))))))
		 

	  
