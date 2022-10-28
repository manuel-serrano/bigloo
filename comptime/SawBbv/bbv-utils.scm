;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-utils.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 27 08:57:51 2017                          */
;*    Last change :  Fri Oct 28 10:34:38 2022 (serrano)                */
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
	    saw_bbv-config
	    saw_bbv-types
	    saw_bbv-specialize
	    saw_bbv-cache
	    saw_bbv-range)

   (export  (type-in?::bool ::type ::pair-nil)
	    (get-bb-mark)
	    (set-max-label! blocks::pair-nil)
	    (genlabel)
	    (replace ::pair-nil ::obj ::obj)
	    (block->block-list regs b::block)
	    (redirect-block! b::blockS old::blockS new::blockS)
	    (replace-block! ::blockS ::blockS #!key debug)
	    (bbv-ctx-filter-live-in-regs::bbv-ctx ::bbv-ctx ::rtl_ins/bbv)
	    (bbv-ctx-extend-live-out-regs::bbv-ctx ::bbv-ctx ::rtl_ins/bbv)
	    (live-versions::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    type-in? ...                                                     */
;*---------------------------------------------------------------------*/
(define (type-in? type types)
   
   (define (type-eq? x y)
      (cond
	 ((eq? x y) #t)
	 ((eq? x *bint*) (eq? y *long*))
	 ((eq? x *long*) (eq? y *bint*))
	 (else #f)))
   
   (any (lambda (t) (type-eq? type t)) types))

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
   (with-trace 'bbv-utils
	 (format "redirect-block! ~a" (block-label b))
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
				   (set! to new)))))
			 ((rtl_ins-switch? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_switch fun (labels)
				(set! labels (replace labels old new)))))))
	    first)))
   b)

;*---------------------------------------------------------------------*/
;*    ctx>=? ...                                                       */
;*---------------------------------------------------------------------*/
(define (ctx>=? x::bbv-ctx y::bbv-ctx)
   
   (define (bbv-ctxentry>=? x y)
      (with-access::bbv-ctxentry x ((xtypes types)
				    (xvalue value)
				    (xpolarity polarity))
	 (with-access::bbv-ctxentry y ((ytypes types)
				       (yvalue value)
				       (ypolarity polarity))
	    (cond
	       ((memq *obj* xtypes)
		#t)
	       ((memq *obj* ytypes)
		#f)
	       ((not (eq? xpolarity ypolarity))
		#f)
	       ((not (every (lambda (t) (memq t ytypes)) xtypes))
		#f)
	       ((isa? xvalue bbv-range)
		(when (isa? yvalue bbv-range)
		   (with-access::bbv-range xvalue ((xlo lo) (xup up))
		      (with-access::bbv-range yvalue ((ylo lo) (yup up))
			 (let ((r (and (<=fx xlo ylo) (>= xup yup))))
			    (unless r
			       (tprint "ERROR ctx>=? " (shape x) " " (shape y)))
			    r)))))
	       (else #t)))))
	       
   (with-access::bbv-ctx x (entries)
      (every (lambda (ex)
		(with-access::bbv-ctxentry ex (reg)
		   (let ((ey (bbv-ctx-get y reg)))
		      (bbv-ctxentry>=? ex ey))))
	 entries)))

;*---------------------------------------------------------------------*/
;*    replace-block! ...                                               */
;*    -------------------------------------------------------------    */
;*    Replace one "old" block with a "new" one. Reconnect the preds    */
;*    and succs of the old block.                                      */
;*---------------------------------------------------------------------*/
(define (replace-block! old::blockS new::blockS #!key debug)
   (with-trace 'bbv-utils "replace-block!"
      (trace-item "old=" (block-label old) " "
	 (map block-label (block-succs old)))
      (trace-item "new="(block-label new) " "
	 (map block-label (block-succs new)))
      ;; debugging
      (when (and debug *bbv-debug*)
	 (with-access::blockS old ((octx ctx))
	    (with-access::blockS new ((nctx ctx))
	       (unless (ctx>=? nctx octx)
		  (tprint "REPLACE-BLOCK-ERROR! " (block-label old)
		     " -> " (block-label new))
		  (tprint " octx=" (shape octx))
		  (tprint " nctx=" (shape nctx))
		  (error "replace-block!"
		     (format "Wrong block replacement ~a" (block-label old))
		     (block-label new))))))
      (with-access::blockS old (succs preds mblock)
	 ;; mark the replacement
	 (set! mblock new)
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
						   (set! to new)))))
					 ((rtl_ins-switch? ins)
					  (with-access::rtl_ins ins (fun)
					     (with-access::rtl_switch fun (labels)
						(set! labels (replace labels old new)))))))
			    first)))
	    preds)
	 (with-access::blockS new ((npreds preds))
	    (set! npreds (delete-duplicates! (append preds npreds) eq?)))
	 new)))

;*---------------------------------------------------------------------*/
;*    bbv-ctx-filter-live-in-regs ...                                  */
;*    -------------------------------------------------------------    */
;*    Filter out non-live registers from the environment.              */
;*---------------------------------------------------------------------*/
(define (bbv-ctx-filter-live-in-regs ctx ins::rtl_ins/bbv)

   (define filtered #f)
   
   (define (filter-entries ctx ins)
      (with-access::rtl_ins/bbv ins (in)
	 (filter-map (lambda (e)
			(let ((reg (bbv-ctxentry-reg e)))
			   (if (or (not (isa? reg rtl_reg/ra))
				   (regset-member? reg in))
			       (let ((aliases (filter (lambda (reg)
							 (regset-member? reg in))
						 (bbv-ctxentry-aliases e))))
				  (if (=fx (length aliases)
					 (length (bbv-ctxentry-aliases e)))
				      e
				      (begin
					 (set! filtered #t)
					 (duplicate::bbv-ctxentry e
					    (aliases aliases)))))
			       (begin
				  (set! filtered #t)
				  #f))))
	    (bbv-ctx-entries ctx))))

   (let ((entries (filter-entries ctx ins)))
      (if filtered
	  (duplicate::bbv-ctx ctx
	     (entries entries))
	  ctx)))
   
;*---------------------------------------------------------------------*/
;*    bbv-ctx-extend-live-out-regs ...                                 */
;*---------------------------------------------------------------------*/
(define (bbv-ctx-extend-live-out-regs ctx ins::rtl_ins/bbv)
   (with-access::rtl_ins/bbv ins (out)
      (let ((entries (filter (lambda (e)
				(regset-member? (bbv-ctxentry-reg e) out))
			(bbv-ctx-entries ctx))))
	 (let ((nctx (if (=fx (length entries) (length (bbv-ctx-entries ctx)))
			 ctx
			 (duplicate::bbv-ctx ctx
			    (entries entries)))))
	    (regset-for-each (lambda (r)
				(unless (bbv-ctx-get nctx r)
				   (with-access::rtl_reg r (type)
				      (set! nctx (extend-ctx nctx r
						    (list type) #t)))))
	       out)
	    nctx))))

;*---------------------------------------------------------------------*/
;*    live-versions ...                                                */
;*---------------------------------------------------------------------*/
(define (live-versions versions)
   (filter (lambda (v)
	      (with-access::blockS (cdr v) (mblock)
		 (not mblock)))
      versions))
