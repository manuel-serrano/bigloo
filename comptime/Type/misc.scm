;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/misc.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  5 12:50:52 2004                          */
;*    Last change :  Wed Oct  6 15:35:46 2004 (serrano)                */
;*    Copyright   :  2004 Manuel Serrano                               */
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
	   object_class
	   ast_var
	   ast_node)
   
   (export (type-less-specific?::bool ::type ::type)
	   (type-disjoint?::bool ::type ::type)))

;*---------------------------------------------------------------------*/
;*    type-less-specific? ...                                          */
;*    -------------------------------------------------------------    */
;*    Is the type T1 less specific than the type T2?                   */
;*---------------------------------------------------------------------*/
(define (type-less-specific? t1 t2)
   (cond
      ((eq? t1 t2)
       #t)
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
      ((or (and (bigloo-type? t1) (not (bigloo-type? t2)))
	   (and (not (bigloo-type? t1)) (bigloo-type? t2)))
       ;; because of automatic cast between foreign and bigloo types we have
       ;; to check if it exists a coercion between the two types
       (not (or (find-coercer t1 t2) (find-coercer t2 t1))))
      ((or (type-less-specific? t1 t2)
	   (type-less-specific? t2 t1))
       #f)
      (else
       #t)))
   
