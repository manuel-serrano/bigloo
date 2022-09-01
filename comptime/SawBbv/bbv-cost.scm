;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-cost.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 18 11:31:11 2022                          */
;*    Last change :  Thu Sep  1 14:18:32 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Instructions, blocks, and paths, cost computations.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-cost
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
	    "SawMill/bbset.sch"
	    "SawBbv/bbv-types.sch")
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    tools_shape
	    tools_speek
	    backend_backend
	    saw_lib
	    saw_defs
	    saw_regset
	    saw_regutils
	    saw_bbv-types
	    saw_bbv-cache
	    saw_bbv-utils)

   (export  (rtl_ins-cost::long ::rtl_ins)))

;*---------------------------------------------------------------------*/
;*    rtl_fun-cost ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (rtl_fun-cost this args) 0)

;*---------------------------------------------------------------------*/
;*    rtl_fun-cost ::rtl_ifne ...                                      */
;*---------------------------------------------------------------------*/
(define-method (rtl_fun-cost this::rtl_ifne args)
   (cond
      ((and (isa? (car args) rtl_ins)
	    (rtl_ins-call? (car args))
	    (rtl_call-predicate (car args)))
       ;; type predicate
       (+fx 1 (apply + (map (lambda (a) (rtl_fun-cost a '())) args))))
      ((and (=fx (length args) 2)
	    (or (eq? var *<fx*) (eq? var *<=fx*)
		(eq? var *>fx*) (eq? var *>=fx*)
		(eq? var *=fx*)
		(eq? var *+fx*) (eq? var *-fx*)))
       ;; fixnum operators
       (+fx 1 (apply + (map (lambda (a) (rtl_fun-cost a '())) args))))
      ((and (=fx (length args) 2)
	    (or (eq? var *<fl*) (eq? var *<=fl*)
		(eq? var *>fl*) (eq? var *>=fl*)
		(eq? var *=fl*)
		(eq? var *+fl*) (eq? var *-fl*)))
       ;; flonum operators
       (+fx 2 (apply + (map (lambda (a) (rtl_fun-cost a '())) args))))
      (else
       0)))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-cost ...                                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-cost this::rtl_ins)
   (with-access::rtl_ins this (fun args)
      (rtl_fun-cost fun args)))

;*---------------------------------------------------------------------*/
;*    block-cost ...                                                   */
;*---------------------------------------------------------------------*/
(define (block-cost b::blockS)
   (with-access::blockS b (first cost)
      (apply + (map rtl_ins-cost first))))

;*---------------------------------------------------------------------*/
;*    path-cost ...                                                    */
;*---------------------------------------------------------------------*/
(define (path-cost b::blockS)
   (let loop ((bs (list b))
	      (cost 0)
	      (acc (make-empty-bbset)))
      (cond
	 ((null? bs)
	  cost)
	 ((bbset-in? (car bs) acc)
	  0)
	 (else
	  (let ((b (car bs)))
	     (with-access::blockS b (preds)
		(loop (append preds (cdr bs))
		   (+ cost (block-cost (car bs)))
		   (bbset-cons (car bs) acc))))))))
