;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/kapture.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 30 07:30:09 1995                          */
;*    Last change :  Wed Jan 22 08:36:10 2014 (serrano)                */
;*    Copyright   :  1995-2014 Manuel Serrano, see LICENSE file        */
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
   (for-each set-one-kaptured! local*))

;*---------------------------------------------------------------------*/
;*    set-one-kaptured! ...                                            */
;*    -------------------------------------------------------------    */
;*    This function computes the set of kaptured variables and         */
;*    the future globalized body.                                      */
;*---------------------------------------------------------------------*/
(define (set-one-kaptured-old! local::local locking)
   (trace globalize "set-one-kaptured: " (shape local)
      " [locking:" (shape locking) "]"
      " [cto=" (shape (sfun/Ginfo-cto* (local-value local))) "]"
      #\Newline)
   (let* ((info (local-value local))
	  (kaptured (sfun/Ginfo-kaptured info)))
      (cond
	 ((or (pair? kaptured) (null? kaptured))
	  (trace (globalize 2) "--> (or pair? null?) [" (shape local)
	     "] " (shape kaptured) #\Newline)
	  (vector #t locking kaptured))
	 ((local? kaptured)
	  (trace (globalize 2) "--> local? [" (shape local)
	     "] " (shape kaptured) #\Newline)
	  (vector #f locking '()))
	 (else
	  (trace (globalize 2) "--> cto* [" (shape local) "] "
	     (shape (sfun/Ginfo-cto* info)) #\Newline)
	  (let ((new-body (sfun/Ginfo-new-body info)))
	     ;; before entering the recursion we mark this function
	     (sfun/Ginfo-kaptured-set! info local)
	     ;; we walk across the call-graph
	     (let loop ((kaptured '())
			(cto (append (sfun/Ginfo-cto* info)
				(sfun/Ginfo-efunctions info)))
			(setter?  #t))
		(trace (globalize 4)
		   "loop( " (shape local) " ) : " #\Newline
		   "      "  (shape kaptured) #\Newline
		   "      "  (shape cto) #\Newline
		   "      "  setter? #\Newline)
		(cond
		   ((null? cto)
		    (let* ((free (get-free-vars new-body local))
			   (fkaptured (free-from kaptured local))
			   (kaptured (union (cons free fkaptured))))
		       (trace (globalize 4)
			  "   kaptured(" (shape local) ") : "
			  (shape kaptured) #\Newline)
		       (trace (globalize 5)
			  "       free(" (shape local) ") : "
			  (shape free) #\Newline)
		       (when (eq? local locking) (set! setter? #t))
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
		    (loop kaptured (cdr cto) setter?))
		   ((sfun/Ginfo-G? (local-value (car cto)))
		    (trace (globalize 5) "  (sfun/Ginfo-G? (car cto))"
		       #\Newline)
		    (let ((other-kaptured
			     (set-one-kaptured-old! (car cto) locking)))
		       (if (not (vector-ref other-kaptured 0))
			   (loop (cons (vector-ref other-kaptured 2)
				    kaptured)
			      (cdr cto)
			      (and setter?
				   (eq? (vector-ref other-kaptured 1) local)))
			   (loop (cons (vector-ref other-kaptured 2)
				    kaptured)
			      (cdr cto)
			      setter?))))
		   (else
		    (trace (globalize 5) "  not globalized (car cto)"
		       #\Newline)
		    (loop kaptured (cdr cto) setter?)))))))))

;*---------------------------------------------------------------------*/
;*    set-one-kaptured! ...                                            */
;*    -------------------------------------------------------------    */
;*    This function computes the set of kaptured variables and         */
;*    the future globalized body.                                      */
;*---------------------------------------------------------------------*/
(define (set-one-kaptured! local::local)
   (trace globalize "set-one-kaptured: " (shape local)
      " [cto=" (shape (sfun/Ginfo-cto* (local-value local))) "]"
      #\Newline)
   (let ((kaptured (get-one-kaptured local '()))
	 (info (local-value local)))
      (sfun/Ginfo-kaptured-set! info kaptured)
      (for-each (lambda (local)
		   (svar/Ginfo-kaptured?-set! (local-value local) #t))
	 kaptured)
      kaptured))

;*---------------------------------------------------------------------*/
;*    get-one-kaptured ...                                             */
;*---------------------------------------------------------------------*/
(define (get-one-kaptured local stack)
   (trace (globalize 1)
      (trace-tab (length stack))
      ">>> get-one-kaptured: " (shape local)
      " [stack=" (length stack) "]"
      #\Newline)
   (let* ((info (local-value local))
	  (kaptured (sfun/Ginfo-kaptured info)))
      (cond
	 ((or (pair? kaptured) (null? kaptured))
	  (trace (globalize 1)
	     (trace-tab (length stack))
	     "<<< get-one-kaptured: " (shape local)
	     " already known: " (map shape kaptured) "\n")
	  kaptured)
	 ((memq local stack)
	  (trace (globalize 1)
	     (trace-tab (length stack))
	     "<<< get-one-kaptured: " (shape local)
	     " in stack\n")
	  '())
	 (else
	  (let loop ((kaptured '())
		     (cto (append (sfun/Ginfo-cto* info)
			     (sfun/Ginfo-efunctions info)))
		     (nstack (cons local stack)))
	     (cond
		((null? cto)
		 (let* ((new-body (sfun/Ginfo-new-body info))
			(free (get-free-vars new-body local))
			(fkaptured (free-from kaptured local)))
		    (trace (globalize 1)
		       (trace-tab (length stack))
		       "<<< get-one-kaptured: " (shape local)
		       " efunctions=" (map shape (sfun/Ginfo-efunctions info))
		       " cto*=" (map shape (sfun/Ginfo-cto* info))
		       " free-vars=" (map shape free)
		       " fkaptured=" (map shape fkaptured)
		       "\n")
		    (union (cons free fkaptured))))
		((sfun/Ginfo-G? (local-value (car cto)))
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
		 

	  
