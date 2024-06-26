;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawBbv/bbv-profile.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  6 09:30:19 2023                          */
;*    Last change :  Wed Jun 26 16:43:39 2024 (serrano)                */
;*    Copyright   :  2023-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    bbv profileging tools                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-profile
   
   (include "Tools/trace.sch"
	    "Tools/location.sch"
	    "SawMill/regset.sch"
	    "SawMill/bbset.sch")
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    type_typeof
	    tools_shape
	    tools_speek
	    backend_backend
	    type_cache
	    saw_lib
	    saw_defs
	    saw_regset
	    saw_regutils
	    saw_bbv-config
	    saw_bbv-types
	    saw_bbv-specialize
	    saw_bbv-cache
	    saw_bbv-range
	    saw_bbv-debug)

   (export  (profile! b::blockS)))

;*---------------------------------------------------------------------*/
;*    profile! ...                                                     */
;*    -------------------------------------------------------------    */
;*    !!! CURRENTLY not used (see SawC/code.scm) !!!                   */
;*    -------------------------------------------------------------    */
;*    Insert profiling information to each block                       */
;*---------------------------------------------------------------------*/
(define (profile! b::blockS)
   
   (define (pragma-ins cexpr::bstring ctx loc)
      (let ((pr (instantiate::rtl_pragma
		   (srfi0 'bigloo-c)
		   (format cexpr)))
	    (es (instantiate::regset (msize 0) (regv '#()) (regl '()))))
	 (instantiate::rtl_ins/bbv
	    (loc loc)
	    (fun pr)
	    (ctx ctx)
	    (in es)
	    (out es)
	    (def es)
	    (args '()))))
   
   (define (profile-ins label i::rtl_ins/bbv)
      (let ((cexpr (format "BBV_ENTER(~a)" label)))
	 (with-access::rtl_ins/bbv i (loc ctx)
	    (pragma-ins cexpr ctx loc))))
   
   (assert-blocks b "before profile!")
   (if *bbv-profile*
       (let loop ((bs (list b))
		  (acc (make-empty-bbset)))
	  (cond
	     ((null? bs)
	      b)
	     ((bbset-in? (car bs) acc)
	      (loop (cdr bs) acc))
	     (else
	      (with-access::blockS (car bs) (first succs label)
		 (when (pair? first)
		    (let ((ins (profile-ins label (car first))))
		       (set! first (cons ins first))))
		 (loop (append succs (cdr bs)) (bbset-cons (car bs) acc))))))
       b))


