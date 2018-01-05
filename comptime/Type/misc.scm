;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Type/misc.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  5 12:50:52 2004                          */
;*    Last change :  Fri Jan  5 19:08:02 2018 (serrano)                */
;*    Copyright   :  2004-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Misc type functions                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_misc

   (import type_type
	   type_cache
	   type_coercion
	   type_env
	   object_class
	   tools_shape
	   ast_var
	   ast_node
	   ast_env)
   
   (export (type-less-specific?::bool ::type ::type)
	   (type-disjoint?::bool ::type ::type)
	   (c-subtype?::bool ::type ::type)
	   (isa-of ::node)
	   (app-predicate-of ::app)))

;*---------------------------------------------------------------------*/
;*    type-less-specific? ...                                          */
;*    -------------------------------------------------------------    */
;*    Is the type T1 less specific than the type T2?                   */
;*---------------------------------------------------------------------*/
(define (type-less-specific? t1 t2)
   (cond
      ((eq? t1 t2)
       #t)
      ((eq? t2 *_*)
       #f)
      ((and (bigloo-type? t1) (not (bigloo-type? t2)))
       (eq? t1 (get-bigloo-type t2)))
      ((or (not (bigloo-type? t1)) (not (bigloo-type? t2)))
       #f)
      ((type-subclass? t2 t1)
       #t)
      ((eq? t1 *obj*)
       #t)
      ((and (eq? t1 *pair-nil*)
	    (or (eq? t2 *pair*) (eq? t2 *epair*) (eq? t2 *bnil*)))
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    type-disjoint? ...                                               */
;*    -------------------------------------------------------------    */
;*    Are the type T1 and T2 disjoint?                                 */
;*---------------------------------------------------------------------*/
(define (type-disjoint? t1 t2)
   (cond
      ((eq? t1 t2)
       #f)
      ((eq? t2 *_*)
       #f)
      ((or (and (bigloo-type? t1) (not (bigloo-type? t2)))
	   (and (not (bigloo-type? t1)) (bigloo-type? t2)))
       ;; because of automatic cast between foreign and bigloo types we have
       ;; to check if it exists a coercion between the two types
       (not (or (find-coercer t1 t2) (find-coercer t2 t1))))
      ((or (type-less-specific? t1 t2)
	   (type-less-specific? t2 t1))
       #f)
      ((or (find-coercer t1 t2) (find-coercer t2 t1))
       #f)
      (else
       #t)))
   
;*---------------------------------------------------------------------*/
;*    c-subtype? ...                                                   */
;*    -------------------------------------------------------------    */
;*    let t1 and t2 two C types, is t1 a subtype of t2?                */
;*---------------------------------------------------------------------*/
(define (c-subtype? t1 t2)
   
   (define (c-weight t)
      (case (type-id t)
	 ((char) 1)
	 ((short) 2)
	 ((int) 3)
	 ((long) 4)
	 ((elong) 4)
	 ((llong) 5)
	 ((int8) 6)
	 ((uint8) 7)
	 ((int16) 8)
	 ((uint16) 9)
	 ((int32) 10)
	 ((uint32) 11)
	 ((int64) 12)
	 ((uint64) 13)
	 ((double) -1)
	 ((real) -2)
	 (else -1)))
   
   (when (and (not (bigloo-type? t1)) (not (bigloo-type? t2)))
      (let ((w1 (c-weight t1))
	    (w2 (c-weight t2)))
	 (and (>fx (*fx w1 w2) 0) (<fx w1 w2)))))

;*---------------------------------------------------------------------*/
;*    *isa* ...                                                        */
;*---------------------------------------------------------------------*/
(define *isa* #f)

;*---------------------------------------------------------------------*/
;*    isa-of ...                                                       */
;*---------------------------------------------------------------------*/
(define (isa-of node::node)
   (when (app? node)
      (unless (global? *isa*)
	 (set! *isa* (find-global/module 'isa? '__object)))
      (with-access::app node (fun args)
	 (when (pair? args)
	    (when (and (or (eq? (var-variable fun) *isa*))
		       (var? (cadr args))
		       (global? (var-variable (cadr args))))
	       (find-type (global-id (var-variable (cadr args)))))))))

;*---------------------------------------------------------------------*/
;*    app-predicate-of ...                                             */
;*---------------------------------------------------------------------*/
(define (app-predicate-of node::app)
   (with-access::app node (fun)
      (let ((val (variable-value (var-variable fun))))
	 (or (fun-predicate-of val) (isa-of node)))))
   
