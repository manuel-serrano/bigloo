;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/coercion.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 18:43:04 1994                          */
;*    Last change :  Fri Nov 18 07:14:29 2011 (serrano)                */
;*    Copyright   :  1994-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The coercion management                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_coercion
   (include "Type/coercer.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    engine_param)
   (export  (add-coercion! ::type ::type ::obj ::obj)
	    (find-coercer::obj ::type ::type)
	    (coercer-exists?::bool ::type ::type)))

;*---------------------------------------------------------------------*/
;*    find-coercer ...                                                 */
;*    -------------------------------------------------------------    */
;*    We look for the coercion between `from' and `to'.                */
;*---------------------------------------------------------------------*/
(define (find-coercer from::type to::type)
   (let ((from (get-aliased-type from))
	 (to (get-aliased-type to)))
      (let loop ((coercer (type-coerce-to from)))
	 (cond
	    ((null? coercer)
	     #f)
	    ((eq? (coercer-to (car coercer)) to)
	     (car coercer))
	    (else
	     (loop (cdr coercer)))))))

;*---------------------------------------------------------------------*/
;*    add-coercion! ...                                                */
;*    -------------------------------------------------------------    */
;*    Coercion inherit from parent to children and children to         */
;*    parent.                                                          */
;*    -------------------------------------------------------------    */
;*    Here is an example, supose we have the following type hierachy   */
;*                                                                     */
;*       obj               foreign                                     */
;*        ^                   ^                                        */
;*        |                   |                                        */
;*        |                   |                                        */
;*      bint                 int                                       */
;*                                                                     */
;*    Now, we add the coercion between `bint' and `int'. Fisrt, we add */
;*    the simple coercion.                                             */
;*                                                                     */
;*       obj               foreign                                     */
;*        ^                   ^                                        */
;*        |                   |                                        */
;*        |                   |                                        */
;*      bint --------------> int                                       */
;*                                                                     */
;*    Then, we add parent coercion.                                    */
;*                                                                     */
;*       obj ------   -----> foreign                                   */
;*        ^        \ /        ^                                        */
;*        |         X         |                                        */
;*        |  ______/ \_____>  |                                        */
;*      bint --------------> int                                       */
;*                                                                     */
;*    Adding `obj' to `int' will not build the link between            */
;*    `obj' and `foreign' since this link already exists.              */
;*---------------------------------------------------------------------*/
(define (add-coercion! from to check coerce)
   (assert (check coerce) (check-coercion? check coerce))
   (trace (ast 2) "add-coercion!: " (shape from) " -> " (shape to)
	  " check: " (shape check) " coerce: " (shape coerce) #\Newline)
;*    (if (null? check) (set! check '(())))                            */
;*    (if (null? coerce) (set! coerce '(())))                          */
   (let ((from (get-aliased-type from))
	 (to (get-aliased-type to)))
      (if (coercer? (find-coercer from to))
	  (unless *lib-mode*
	     (warning "add-coercion!"
		      "Type coercion redefinition -- "
		      (shape (list from to check coerce))))
	  (begin
	     ;; we set the coercion between `from' and `to'
	     (let ((new (coercer from to check coerce)))
		(type-coerce-to-set! from (cons new (type-coerce-to from))))
	     ;; we set the coercion between `from' and `to's parents'
	     (for-each
	      (lambda (parent)
		 (if (and (not (eq? from parent))
			  (not (eq? to parent))
			  (not (coercer? (find-coercer from parent))))
		     (let ((coercer-p (find-coercer to parent)))
			(if (not (coercer? coercer-p))
			    (user-error "Can't find coercion"
					(format "~a -> ~a"
						(shape to) (shape parent))
					(format "while adding: ~a -> ~a"
						(shape from)
						(shape to)))
			    (let ((check-p (coercer-check-op coercer-p))
				  (coerce-p (coercer-coerce-op coercer-p)))
			       (add-coercion! from
					      parent
					      (append check check-p)
					      (append coerce coerce-p)))))))
	      (type-parents to))
	     ;; we set the coercion between `from's parent' and `to'
	     (for-each
	      (lambda (parent)
		 (if (and (not (eq? from parent))
			  (not (eq? to parent))
			  (not (coercer? (find-coercer parent to))))
		     (let ((coercer-p (find-coercer parent from)))
			(if (not (coercer? coercer-p))
			    (user-error "Can't find coercion"
					(format "~a -> ~a"
						(shape parent) (shape from))
					(format "while adding: ~a -> ~a"
						(shape from) (shape to)))
			    (let ((check-p (coercer-check-op coercer-p))
				  (coerce-p (coercer-coerce-op coercer-p)))
			       (add-coercion! parent
					      to
					      (append check-p check)
					      (append coerce-p coerce)))))))
		       (type-parents from))))))
       
;*---------------------------------------------------------------------*/
;*    check-coercion? ...                                              */
;*---------------------------------------------------------------------*/
(define (check-coercion? check coerce)
   (let loop ((check check))
      (cond
	 ((null? check)
	  (let loop ((coerce coerce))
	     (cond
		((null? coerce)
		 #t)
		((match-case (car coerce)
		    (((? symbol?) . ?-) #f)
		    (((@ (? symbol?) (? symbol?)) . ?-) #f)
		    ((#t . ?-) #f)
		    (((lambda (?-) . ?-) . ?-) #f)
		    (else #t))
		 #f)
		(else
		 (loop (cdr coerce))))))
	 ((and (not (symbol? (caar check)))
	       (not (eq? (caar check) #t))
	       (not (match-case (caar check)
		       ((@ (? symbol?) (? symbol?)) #t)
		       ((lambda (?-) . ?-) #t)
		       (else #f))))
	  #f)
	 (else
	  (loop (cdr check))))))

;*---------------------------------------------------------------------*/
;*    coercer-exists? ...                                              */
;*---------------------------------------------------------------------*/
(define (coercer-exists? to from)
   (let ((to   (get-aliased-type to))
	 (from (get-aliased-type from)))
      (if (or (eq? from to) (type-magic? from))
	  #t
	  (coercer? (find-coercer from to)))))
   
