;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-utils.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 27 08:57:51 2017                          */
;*    Last change :  Mon Jul 18 07:57:41 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BB manipulations                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-utils
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
	    "SawMill/bbset.sch")
   
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
	    saw_bbv-specialize
	    saw_bbv-cache)

   (export  (get-bb-mark)
	    (set-max-label! blocks::pair-nil)
	    (genlabel)
	    (replace ::pair-nil ::obj ::obj)
	    (block->block-list regs b::block)
	    (redirect-block! b::blockS old::blockS new::blockS)
	    (replace-block! ::blockS ::blockS)
	    (filter-live-in-regs::pair-nil ins::rtl_ins/bbv ctx::pair-nil)
	    (extend-live-out-regs::pair-nil ins::rtl_ins/bbv ctx::pair-nil)
	    (ctx-live-versions::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    *label* ...                                                      */
;*---------------------------------------------------------------------*/
(define *label* 0)
(define (genlabel)
   (set! *label* (+fx 1 *label*))
   *label*)

;*---------------------------------------------------------------------*/
;*    set-max-label! ...                                               */
;*---------------------------------------------------------------------*/
(define (set-max-label! blocks::pair-nil)
   (set! *label* 0)
   (for-each (lambda (b)
		(with-access::block b (label)
		   (when (>fx label *label*)
		      (set! *label* label))))
      blocks))

;*---------------------------------------------------------------------*/
;*    *bb-mark* ...                                                    */
;*---------------------------------------------------------------------*/
(define *bb-mark* -1)

;*---------------------------------------------------------------------*/
;*    get-bb-mark ...                                                  */
;*---------------------------------------------------------------------*/
(define (get-bb-mark)
   (set! *bb-mark* (+fx 1 *bb-mark*))
   *bb-mark*)

;*---------------------------------------------------------------------*/
;*    replace ...                                                      */
;*---------------------------------------------------------------------*/
(define (replace lst old new)
   (let loop ((l lst))
      (cond
	 ((null? l) l)
	 ((eq? (car l) old) (cons new (cdr l)))
	 (else (cons (car l) (loop (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    block->block-list ...                                            */
;*---------------------------------------------------------------------*/
(define (block->block-list regs b::block)
   (let loop ((bs (list b))
	      (acc (make-empty-bbset)))
      (cond
	 ((null? bs)
	  (reverse! (bbset-list acc)))
	 ((bbset-in? (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (succs)
	     (cond
		((null? succs)
		 (loop (cdr bs) (bbset-cons (car bs) acc)))
		(else
		 (loop (append succs (cdr bs))
		    (bbset-cons (car bs) acc)))))))))

;*---------------------------------------------------------------------*/
;*    redirect-block! ...                                              */
;*---------------------------------------------------------------------*/
(define (redirect-block! b::blockS old::blockS new::blockS)
   (with-trace 'bbv "redirect-block!"
      (trace-item "b=" (block-label b))
      (trace-item "old=" (block-label old) " "
	 (map block-label (block-succs old)))
      (trace-item "new="(block-label new) " "
	 (map block-label (block-succs new)))
      (with-access::blockS b (succs first)
	 (set! succs (replace succs old new))
	 (with-access::block old (preds)
	    (set! preds (remq! b preds)))
	 (with-access::block new (preds)
	    (set! preds (cons b preds)))
	 (for-each (lambda (ins)
		      (cond
			 ((rtl_ins-ifeq? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_ifeq fun (then)
				(when (eq? then old)
				   (set! then new)))))
			 ((rtl_ins-ifne? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_ifne fun (then)
				(when (eq? then old)
				   (set! then new)))))
			 ((rtl_ins-go? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_go fun (to)
				(when (eq? to old)
				   (set! to new)))))))
	    first)))
   b)

;*---------------------------------------------------------------------*/
;*    replace-block! ...                                               */
;*    -------------------------------------------------------------    */
;*    Replace one "old" block with a "new" one. Reconnect the preds    */
;*    and succs of the old block.                                      */
;*---------------------------------------------------------------------*/
(define (replace-block! old::blockS new::blockS)
   (with-trace 'bbv "replace-block!"
      (trace-item "old=" (block-label old) " "
	 (map block-label (block-succs old)))
      (trace-item "new="(block-label new) " "
	 (map block-label (block-succs new)))
      (with-access::blockS old (succs preds)
	 (for-each (lambda (b)
		      (with-access::blockS b (preds)
			 (set! preds (replace preds old new))))
	    succs)
	 (for-each (lambda (b)
		      (with-access::blockS b (succs first)
			 (set! succs (replace succs old new))
			 (for-each (lambda (ins)
				      (cond
					 ((rtl_ins-ifeq? ins)
					  (with-access::rtl_ins ins (fun)
					     (with-access::rtl_ifeq fun (then)
						(when (eq? then old)
						   (set! then new)))))
					 ((rtl_ins-ifne? ins)
					  (with-access::rtl_ins ins (fun)
					     (with-access::rtl_ifne fun (then)
						(when (eq? then old)
						   (set! then new)))))
					 ((rtl_ins-go? ins)
					  (with-access::rtl_ins ins (fun)
					     (with-access::rtl_go fun (to)
						(when (eq? to old)
						   (set! to new)))))))
			    first)))
	    preds)
	 new)))

;*---------------------------------------------------------------------*/
;*    filter-live-in-regs ...                                          */
;*---------------------------------------------------------------------*/
(define (filter-live-in-regs ins::rtl_ins/bbv ctx)
   (with-access::rtl_ins/bbv ins (in)
      (filter-map (lambda (e)
		     (let ((reg (bbv-ctxentry-reg e)))
			(when (or (not (isa? reg rtl_reg/ra))
				  (regset-member? reg in))
			   (duplicate::bbv-ctxentry e
			      (aliases (filter (lambda (reg)
						  (regset-member? reg in))
					  (bbv-ctxentry-aliases e)))))))
	 ctx)))
   
;*---------------------------------------------------------------------*/
;*    extend-live-out-regs ...                                         */
;*---------------------------------------------------------------------*/
(define (extend-live-out-regs ins::rtl_ins/bbv ctx)
   (with-access::rtl_ins/bbv ins (out)
      (let ((nctx (filter (lambda (e)
			     (regset-member? (bbv-ctxentry-reg e) out))
		     ctx)))
	 (regset-for-each (lambda (r)
			     (unless (ctx-get nctx r)
				(with-access::rtl_reg r (type)
				   (set! nctx (extend-ctx nctx r (list type) #t)))))
	    out)
	 nctx)))

;*---------------------------------------------------------------------*/
;*    ctx-live-versions ...                                            */
;*---------------------------------------------------------------------*/
(define (ctx-live-versions versions)
   (filter (lambda (v)
	      (with-access::blockS (cdr v) (wblock)
		 (not wblock)))
      versions))

;*---------------------------------------------------------------------*/
;*    block-merger ...                                                 */
;*---------------------------------------------------------------------*/
(define (block-merger b::blockS)
   (let loop ((b b))
      (with-access::blockS b (wblock)
	 (if wblock
	     (loop wblock)
	     b))))
