;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-utils.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 27 08:57:51 2017                          */
;*    Last change :  Fri Jun 28 08:48:02 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BBB manipulations                                                */
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
	    type_typeof
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
	    saw_bbv-range
	    saw_bbv-debug
	    saw_bbv-gc)

   (export  (<=ty ::obj ::obj)
	    (set-max-label! blocks::pair-nil)
	    (genlabel)
	    (list-replace ::pair-nil ::obj ::obj)
	    (block->block-list regs b::block)
	    (redirect-block! b::blockS old::blockS new::blockS)
	    (replace-block! ::blockS ::blockS #!key debug)
	    (bbv-ctx-filter-live-in-regs::bbv-ctx ::bbv-ctx ::rtl_ins/bbv)
	    (bbv-ctx-extend-live-out-regs::bbv-ctx ::bbv-ctx ::rtl_ins/bbv)))

;*---------------------------------------------------------------------*/
;*    <=ty ...                                                         */
;*---------------------------------------------------------------------*/
(define (<=ty x y)
   (cond
      ((eq? y *obj*) #t)
      ((eq? x y) #t)
      ((eq? x *bint*) (or (eq? y *long*) (eq? y 'number)))
      ((eq? x *long*) (or (eq? y *bint*) (eq? y 'number)))
      ((eq? x *real*) (or (eq? y *breal*) (eq? y 'number)))
      ((eq? x *breal*) (or (eq? y *real*) (eq? y 'number)))
      ((or (eq? x 'number) (eq? y 'number)) #f)
      ((eq? y *pair-nil*) #f)
      (else (is-subtype? x y))))

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
;*    list-replace ...                                                 */
;*---------------------------------------------------------------------*/
(define (list-replace lst old new)
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
	 (set! succs (list-replace succs old new))
	 (with-access::blockS old (preds)
	    (block-preds-update! old (remq! b preds)))
	 (with-access::block new (preds)
	    (unless (memq b preds)
	       (block-preds-update! new (cons b preds))))
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
				(set! labels (list-replace labels old new)))))))
	    first)))
   b)

;*---------------------------------------------------------------------*/
;*    replace-block! ...                                               */
;*    -------------------------------------------------------------    */
;*    Replace one "old" block with a "new" one. Reconnect the preds    */
;*    of the old block and disconnect the succs of the old block.      */
;*---------------------------------------------------------------------*/
(define (replace-block! old::blockS new::blockS #!key debug)
   (with-trace 'bbv-utils "replace-block!"
      (trace-item "old=#" (block-label old)
	 " succs=" (map (lambda (s)
			   (format "#~a~a" (block-label s)
			      (if (block-live? s) "+" "-")))
		      (block-succs old))
	 " preds=" (map (lambda (s)
			   (format "#~a~a" (block-label s)
			      (if (block-live? s) "+" "-")))
		      (block-preds old)))
      (trace-item "new=#"(block-label new)
	 " succs=" (map (lambda (s)
			   (format "#~a~a" (block-label s)
			      (if (block-live? s) "+" "-")))
		      (block-succs new))
	 " preds=" (map (lambda (s)
			   (format "#~a~a" (block-label s)
			      (if (block-live? s) "+" "-")))
		      (block-preds new)))
      (when *bbv-debug*
	 (assert-block old "replace-block!.old>")
	 (assert-block new "replace-block!.new>"))
      (unless (eq? old new)
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
	 (with-access::blockS old ((old-succs succs) (old-preds preds) mblock gccnt)
	    ;; remove "old" from all its successors's pred list
	    (for-each (lambda (b)
			 (with-access::blockS b (preds)
			    (trace-item (format "replace-block.update #~a~a"
					   (block-label b)
					   (if (block-live? b) "+" "-")))
			    (block-preds-update! b 
			       (filter! (lambda (n) (not (eq? n old))) preds))))
	       old-succs)
	    (for-each (lambda (b)
			 (with-access::blockS b (succs first preds)
			    (set! succs (list-replace succs old new))
			    (bbv-gc-connect! b new)
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
						   (set! labels (list-replace labels old new)))))))
			       first)))
	       old-preds)
	    (with-access::blockS new ((npreds preds))
	       (block-preds-update! new
		  (delete-duplicates (append old-preds npreds) eq?)))
	    ;; mark the redirection
	    (set! mblock new)
	    (set! old-preds '())
	    (set! old-succs '())
	    (set! gccnt 0)
	    (bbv-gc-redirect! old new)
	    (when *bbv-debug*
	       (assert-block old "replace-block!.old<")
	       (assert-block new "replace-block!.new<"))
	    (trace-item "after-merge new=#"(block-label new)
	       " succs=" (map (lambda (s)
				 (format "#~a~a" (block-label s)
				    (if (block-live? s) "+" "-")))
			    (block-succs new))
	       " preds=" (map (lambda (s)
				 (format "#~a~a" (block-label s)
				    (if (block-live? s) "+" "-")))
			    (block-preds new)))
	    new))))

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
			   [assert (reg) (isa? reg rtl_reg/ra)]
			   (if (regset-member? reg in)
			       (if (every (lambda (reg)
					     (regset-member? reg in))
				      (bbv-ctxentry-aliases e))
				   e
				   (let ((aliases (filter (lambda (reg)
							     (regset-member? reg in))
						     (bbv-ctxentry-aliases e))))
				      (set! filtered #t)
				      (duplicate::bbv-ctxentry e
					 (aliases aliases))))
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
      (let ((entries (filter-map (lambda (e)
				    (when (regset-member? (bbv-ctxentry-reg e) out)
				       (with-access::bbv-ctxentry e (aliases)
					  (if (null? aliases)
					      e
					      (duplicate::bbv-ctxentry e
						 (aliases (filter (lambda (r) (regset-member? r out)) aliases)))))))
			(bbv-ctx-entries ctx))))
	 (let ((nctx (duplicate::bbv-ctx ctx
			(entries entries))))
	    (regset-for-each (lambda (reg)
				[assert (reg) (isa? reg rtl_reg/ra)]
				(unless (bbv-ctx-get nctx reg)
				   (with-access::rtl_reg reg (type)
				      (extend-ctx! nctx reg (list type) #t))))
	       out)
	    nctx))))
