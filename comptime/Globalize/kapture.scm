;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/kapture.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 30 07:30:09 1995                          */
;*    Last change :  Wed Jan 22 08:36:10 2014 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
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
(define (set-kaptured! localfun*)
   ;; first we compute the transitive closure of the `cto' property
   ;; (i.e. we add to local the cto fields of its including local functions).
   (for-each (lambda (localfun)
		(cto-transitive-closure! localfun))
	     localfun*)
   ;; then we compute the kaptured property.
   (for-each set-one-kaptured! localfun*))

;*---------------------------------------------------------------------*/
;*    set-one-kaptured! ...                                            */
;*    -------------------------------------------------------------    */
;*    This function computes the set of kaptured variables and         */
;*    the future globalized body.                                      */
;*---------------------------------------------------------------------*/
(define (set-one-kaptured! localfun::local)
   (trace globalize "set-one-kaptured: " (shape localfun)
      " [cto=" (shape (sfun/Ginfo-cto* (local-value localfun))) "]"
      #\Newline)
   (let ((kaptured (get-one-kaptured localfun '()))
	 (info (local-value localfun))
	 (stackable (eq? (sfun-stackable (local-value localfun)) #t)))
      (sfun/Ginfo-kaptured-set! info kaptured)
      (for-each (lambda (local)
		   (let ((lv (local-value local)))
		      ;; a box can be stack allocated if it is only refeenced
		      ;; by _one_ stackable closure
		      (svar/Ginfo-stackable-set! lv
			 (and stackable (not (svar/Ginfo-kaptured? lv))))
		      (svar/Ginfo-kaptured?-set! lv #t)))
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
		 

	  
