;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawBbv/bbv-specialize.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 07:42:00 2017                          */
;*    Last change :  Tue Dec 19 15:28:33 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BBV instruction specialization                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-specialize
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
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
	    saw_bbv-utils
	    saw_bbv-range
	    saw_bbv-merge
	    saw_bbv-config
	    saw_bbv-cost
	    saw_bbv-debug)

   (export (bbv-block*::blockS ::blockV ::bbv-ctx)))

;*---------------------------------------------------------------------*/
;*    bbv-block* ...                                                   */
;*---------------------------------------------------------------------*/
(define (bbv-block*::blockS bv::blockV ctx::bbv-ctx)
   (with-trace 'bbv-block
	 (format "bbv-block* ~a" (with-access::blockV bv (label) label))
      (let ((queue (instantiate::bbv-queue))
	    (bs (new-blockS bv ctx :cnt 1)))
	 (block-specialize! bs queue)
	 (let loop ((n 0))
	    (trace-item "loop(" n ") queue="
	       (map (lambda (bs)
		       (with-access::blockS bs (label) label))
		  (with-access::bbv-queue queue (blocks)
			blocks)))
	    (if (bbv-queue-empty? queue)
		bs
		(let ((bs (bbv-queue-pop! queue)))
		   (with-access::blockS bs ((bv parent) label)
		      (with-access::blockV bv ((plabel label) versions)
			 (trace-item "pop {" plabel "}->" label
			    " versions=" (map (lambda (b)
						 (with-access::blockS b (label)
						    label))
					    versions)))
		      (when (block-need-merge? bv)
			 (trace-item "need-merge #" (block-label bv) "...")
			 (when *bbv-debug*
			    (for-each (lambda (b)
					 (with-access::blockS b (label ctx)
					    (trace-item label ": " (shape ctx))))
			       (blockV-versions bv)))
			 (block-merge-some! bv queue)))
		   (unless (block-merged? bs)
		      (with-access::blockS bs ((bv parent))
			 (trace-item "need-specialize #" (block-label bv) "..."))
		      (block-specialize! bs queue))
		   (loop (+fx n 1))))))))

;*---------------------------------------------------------------------*/
;*    block-specialize! ...                                            */
;*---------------------------------------------------------------------*/
(define (block-specialize!::blockS bs::blockS queue::bbv-queue)
   (with-trace 'bbv-block
	 (format "block-specialize! #~a->#~a succs: ~a"
	    (block-label (blockS-parent bs))
	    (block-label bs)
	    (map block-label (block-succs (blockS-parent bs))))
      (with-access::blockS bs ((bv parent) first ctx)
	 (trace-item "ctx=" (shape ctx))
	 (with-access::blockV bv ((pfirst first))
	    (set! first (ins-specialize! pfirst bs ctx queue))
	    (when *bbv-debug*
	       (assert-block bs "block-specialize!"))
	    bs))))

;*---------------------------------------------------------------------*/
;*    block-merge-some! ...                                            */
;*---------------------------------------------------------------------*/
(define (block-merge-some! bv::blockV queue)
   (with-trace 'bbv-block-merge "block-merge-some!"
      (let ((lvs (blockV-live-versions bv)))
	 (multiple-value-bind (bs1 bs2 mctx)
	    (bbv-block-merge lvs)
	    ;; Find an already existing block for mctx, if such a block
	    ;; exists, get its representant (i.e., the block it might had
	    ;; already been merge into). If no such block exists, create
	    ;; a fresh new one
	    (let ((mbs (bbv-block bv mctx queue)))
	       (trace-item "blockV=" (blockV-label bv)
		  " -> #" (blockS-label mbs) " {" (length lvs) "} "
		  (blockS-%merge-info mbs))
	       (cond
		  ((eq? bs1 mbs)
		   (blockS-%merge-info-add! mbs 'merge-target (list bs2))
		   (block-merge! bs2 mbs))
		  ((eq? bs2 mbs)
		   (blockS-%merge-info-add! mbs 'merge-target (list bs1))
		   (block-merge! bs1 mbs))
		  (else
		   (blockS-%merge-info-add! mbs
		      (with-access::blockS mbs (cnt)
			 (if (=fx cnt 0) 'merge-new 'merge-target))
		      (list bs1 bs2))
		   (block-merge! bs1 mbs)
		   (block-merge! bs2 mbs)))
	       (when *bbv-debug*
		  (assert-block mbs "block-merge-some!")))))))
   
;*---------------------------------------------------------------------*/
;*    ins-specialize! ...                                              */
;*---------------------------------------------------------------------*/
(define (ins-specialize! first bs::blockS ctx::bbv-ctx queue::bbv-queue)

   (define (debug-connect msg bs n)
      (when *bbv-debug*
	 (with-access::blockS n (preds succs)
	    (trace-item msg " #" 
	       (block-label bs) " to #" (block-label n)
	       " " (map block-label preds)
	       " -> " (map (lambda (b) (if (isa? b block) (block-label b) '-))
			 succs)))))
   
   (define (connect! bs::blockS ins::rtl_ins)
      (cond
	 ((rtl_ins-ifne? ins)
	  (with-access::rtl_ins ins (fun)
	     (let ((n (rtl_ifne-then fun)))
		(debug-connect "connect.ifne" bs n)
		(block-succs-set! bs (list #unspecified n))
		(block-preds-update! n (cons bs (block-preds n))))))
	 ((rtl_ins-go? ins)
	  (with-access::rtl_ins ins (fun)
	     (let ((n (rtl_go-to fun)))
		(debug-connect "connect.go" bs n)
		(if (pair? (block-succs bs))
		    (set-car! (block-succs bs) n)
		    (block-succs-set! bs (list n)))
		(block-preds-update! n (cons bs (block-preds n))))))
	 ((rtl_ins-switch? ins)
	  (with-access::rtl_ins ins (fun)
	     (block-succs-set! bs (rtl_switch-labels fun))
	     (for-each (lambda (n)
			  (block-preds-update! n (cons bs (block-preds n))))
		(rtl_switch-labels fun))))))
   
   (with-trace 'bbv-ins (format "ins-specialize! #~a->#~a [@~a]"
			   (block-label (blockS-parent bs))
			   (block-label bs)
			   (gendebugid))
      (let loop ((oins first)
		 (nins '())
		 (ctx ctx))
	 (when (pair? oins)
	    (trace-item "ins=" (shape (car oins)) )
	    (trace-item "ctx=" (shape ctx)))
	 (cond
	    ((null? oins)
	     (reverse! nins))
	    ((rtl_ins-specializer (car oins))
	     =>
	     (lambda (specialize)
		;; instruction specialization
		(multiple-value-bind (ins nctx)
		   (specialize (car oins) nins bs ctx queue)
		   (trace-item "iins=" (shape ins))
		   (trace-item "nctx=" (shape nctx))
		   (with-access::rtl_ins ins (args)
		      (set! args (map (lambda (i)
					 (if (isa? i rtl_ins)
					     (car (loop (list i) '() ctx))
					     i))
				    args))
		      (let ((lctx (bbv-ctx-extend-live-out-regs nctx (car oins))))
			 (trace-item "lctx=" (shape lctx))
			 (cond
			    ((rtl_ins-br? ins)
			     (connect! bs ins)
			     (loop (cdr oins) (cons ins nins) lctx))
			    ((rtl_ins-go? ins)
			     (connect! bs ins)
			     (loop '() (cons ins nins) lctx))
			    ((rtl_ins-ifne? ins)
			     (connect! bs ins)
			     (loop (cdr oins) (cons ins nins) lctx))
			    (else
			     (loop (cdr oins) (cons ins nins) lctx))))))))
	    ((rtl_ins-last? (car oins))
	     ;; a return, fail, ...
	     (with-access::rtl_ins (car oins) (args)
		(let ((args (map (lambda (i)
				    (if (isa? i rtl_ins)
					(car (loop (list i) '() ctx))
					i))
			       args)))
		   (loop '()
		      (cons (duplicate-ins/args (car oins) args ctx) nins)
		      ctx))))
	    ((rtl_ins-go? (car oins))
	     (with-access::rtl_ins (car oins) (fun)
		(with-access::rtl_go fun (to)
		   (let* ((n (bbv-block to ctx queue :creator bs))
			  (ins (duplicate::rtl_ins/bbv (car oins)
				  (ctx ctx)
				  (fun (duplicate::rtl_go fun
					  (to n))))))
		      (connect! bs ins)
		      (loop '() (cons ins nins) ctx)))))
	    ((rtl_ins-switch? (car oins))
	     (with-access::rtl_ins (car oins) (fun)
		(with-access::rtl_switch fun (labels)
		   (let ((ins (duplicate::rtl_ins/bbv (car oins)
				 (ctx ctx)
				 (fun (duplicate::rtl_switch fun
					 (labels (map (lambda (b)
							 (bbv-block b ctx queue :creator bs))
						    labels)))))))
		      (connect! bs ins)
		      (loop '() (cons ins nins) ctx)))))
	    ((rtl_ins-ifne? (car oins))
	     (with-access::rtl_ins (car oins) (fun args)
		(with-access::rtl_ifne fun (then)
		   (let* ((n (bbv-block then ctx queue :creator bs))
			  (ins (duplicate::rtl_ins/bbv (car oins)
				  (ctx ctx)
				  (fun (duplicate::rtl_ifne fun
					  (then n))))))
		      (connect! bs ins)
		      (loop (cdr oins) (cons ins nins) ctx)))))
	    (else
	     (with-access::rtl_ins (car oins) (args)
		(let ((args (map (lambda (i)
				    (if (isa? i rtl_ins)
					(car (loop (list i) '() ctx))
					i))
			       args)))
		   (loop (cdr oins)
		      (cons (duplicate-ins/args (car oins) args ctx) nins)
		      (bbv-ctx-extend-live-out-regs ctx (car oins))))))))))

;*---------------------------------------------------------------------*/
;*    block-need-merge? ...                                            */
;*---------------------------------------------------------------------*/
(define (block-need-merge?::bool bv::blockV)
   (with-access::blockV bv (merge)
      (>fx (length (blockV-live-versions bv))
	 (if merge *max-block-merge-versions* *max-block-limit*))))

;*---------------------------------------------------------------------*/
;*    block-merge! ...                                                 */
;*---------------------------------------------------------------------*/
(define (block-merge! bs::blockS mbs::blockS)
   (with-trace 'bbv-block-merge
	 (format "block-merge! ~a -> ~a"
	    (with-access::blockS bs (label) label)
	    (with-access::blockS mbs (label) label))
      (with-access::blockS bs (mblock cnt succs)
	 (set! mblock mbs)
	 (set! cnt 0)
	 (replace-block! bs mbs)
	 (when *bbv-blocks-gc*
	    (for-each collect-block! succs))
	 (blockS-%merge-info-add! bs 'merge-into (block-label mbs)))))

;*---------------------------------------------------------------------*/
;*    collect-block! ...                                               */
;*    -------------------------------------------------------------    */
;*    Collect the unreachable specialized blocks.                      */
;*---------------------------------------------------------------------*/
(define (collect-block! bs::blockS)
   (when *bbv-debug*
      (assert-block bs "collect-block!"))
   (with-access::blockS bs (label succs cnt)
      (with-trace 'bbv-gc (format "collect-block! ~a (~a)" label cnt)
	 (when (>fx cnt 0)
	    (set! cnt (-fx cnt 1))
	    (when (=fx cnt 0)
	       (for-each collect-block! succs))))))

;*---------------------------------------------------------------------*/
;*    block-merged? ...                                                */
;*---------------------------------------------------------------------*/
(define (block-merged?::bool bs::blockS)
   (with-access::blockS bs (mblock)
      mblock))

;*---------------------------------------------------------------------*/
;*    new-blockS ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-blockS::blockS bv::blockV ctx::bbv-ctx #!key (cnt 0) creator)
   (with-trace 'bbv-block "new-blockS"
      (let* ((lbl (genlabel))
	     (bs (instantiate::blockS
		    (ctx ctx)
		    (parent bv)
		    (label lbl)
		    (cnt cnt)
		    (first '())
		    (%merge-info (if creator `((creator . ,creator)) '())))))
	 (trace-item "lbl=#" lbl)
	 (trace-item "ctx=" (shape ctx))
	 (with-access::blockV bv (versions)
	    (set! versions (cons bs versions))
	    bs))))

;*---------------------------------------------------------------------*/
;*    live-blockS ...                                                  */
;*    -------------------------------------------------------------    */
;*    If "b" is dead (because it has been merged), this function       */
;*    returns the live block into which it has been merged.            */
;*---------------------------------------------------------------------*/
(define (live-blockS b::blockS)
   (with-access::blockS b (mblock)
      (if mblock
	  (live-blockS mblock)
	  b)))

;*---------------------------------------------------------------------*/
;*    bbv-block ...                                                    */
;*---------------------------------------------------------------------*/
(define (bbv-block b::block ctx::bbv-ctx queue::bbv-queue #!key creator)
   (with-trace 'bbv-block (format "bbv-block #~a" (block-label b))
      (trace-item "ctx=" (shape ctx))
      (let ((bv (if (isa? b blockV) b (with-access::blockS b (parent) parent))))
	 (with-access::blockV bv (first label)
	    (trace-item "parent=" label)
	    (let ((ctx (bbv-ctx-filter-live-in-regs ctx (car first))))
	       (cond
		  ((bbv-ctx-assoc ctx (blockV-live-versions bv))
		   =>
		   (lambda (b)
		      (let ((obs (live-blockS b)))
			 (with-access::blockS obs (label)
			    (trace-item "<- old=" label)
			    obs))))
		  (else
		   (let ((nbs (new-blockS bv ctx :creator creator)))
		      (with-access::blockS nbs (label)
			 (trace-item "<- new=" label)
			 (bbv-queue-push! queue nbs)
			 nbs)))))))))
   
;*---------------------------------------------------------------------*/
;*    duplicate-ins/args ...                                           */
;*---------------------------------------------------------------------*/
(define (duplicate-ins/args ins args ctx)
   (duplicate::rtl_ins/bbv ins
      (args args)
      (ctx ctx)))

;*---------------------------------------------------------------------*/
;*    duplicate-ins ...                                                */
;*---------------------------------------------------------------------*/
(define (duplicate-ins ins ctx)
   (duplicate::rtl_ins/bbv ins
      (ctx ctx)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specializer ...                                          */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specializer i::rtl_ins)
   (cond
      ((rtl_ins-fxovop? i) rtl_ins-specialize-fxovop)
      ((rtl_ins-fxcmp? i) rtl_ins-specialize-fxcmp)
      ((rtl_ins-fxop? i) rtl_ins-specialize-fxop)
      ((rtl_ins-typecheck? i) rtl_ins-specialize-typecheck)
      ((rtl_ins-mov? i) rtl_ins-specialize-mov)
      ((rtl_ins-loadi? i) rtl_ins-specialize-loadi)
      ((rtl_ins-loadg? i) rtl_ins-specialize-loadg)
      ((rtl_ins-call-specialize? i) rtl_ins-specialize-call)
      ((rtl_ins-return-specialize? i) rtl_ins-specialize-return)
      ((rtl_ins-vlen? i) rtl_ins-specialize-vlen)
      ((rtl_ins-strlen? i) rtl_ins-specialize-vlen)
      ((rtl_ins-vector-bound-check? i) rtl_ins-specialize-vector-bound-check)
      ((rtl_ins-resolved-ifne? i) rtl_ins-specialize-resolved-ifne)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-typecheck? ...                                           */
;*---------------------------------------------------------------------*/
(define (rtl_ins-typecheck? i::rtl_ins)
   (when (rtl_ins-ifne? i)
      (with-access::rtl_ins i (args)
	 (when (and (isa? (car args) rtl_ins) (rtl_ins-call? (car args)))
	    (when (rtl_call-predicate (car args))
	       (let ((args (rtl_ins-args* i)))
		  (and (pair? args) (null? (cdr args)) (rtl_reg? (car args)))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-resolved-ifne? ...                                       */
;*---------------------------------------------------------------------*/
(define (rtl_ins-resolved-ifne? i::rtl_ins)
   (when (rtl_ins-ifne? i)
      (with-access::rtl_ins i (args)
	 (and (isa? (car args) rtl_ins) (rtl_ins-loadi? (car args))))))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-typecheck ...                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-typecheck i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)

   (define (min-value x y)
      (cond
	 ((eq? x '_) y)
	 ((eq? y '_) x)
	 ((and (isa? x bbv-range) (isa? y bbv-range)) (bbv-range-intersection x y))
	 ((isa? x bbv-range) x)
	 ((isa? y bbv-range) y)
	 (else '_)))
   
   (with-trace 'bbv-ins "rtl_ins-specialize-typecheck"
      (multiple-value-bind (reg type polarity value)
	 (rtl_ins-typecheck i)
	 (let ((e (bbv-ctx-get ctx reg)))
	    (with-access::rtl_ins i (fun)
	       (trace-item "ins=" (shape i))
	       (trace-item "typ=" (shape type) " polarity=" polarity)
	       (trace-item "value=" (shape value))
	       (trace-item "e=" (shape e))
	       (trace-item "ctx=" (shape ctx))
	       (cond
		  ((and (type-in? type (bbv-ctxentry-types e))
			(bbv-ctxentry-polarity e))
		   ;; positive type simplification
		   (let ((ctx+ (extend-ctx ctx reg (list type) #t
				  :value (min-value value (bbv-ctxentry-value e)))))
		      (trace-item "ctx+=" (shape ctx+))
		      (with-access::rtl_ins/bbv i (fun)
			 (with-access::rtl_ifne fun (then)
			    (let ((s (duplicate::rtl_ins/bbv i
					(ctx ctx)
					(fun (instantiate::rtl_go
						(to (bbv-block then ctx+ queue :creator bs))))
					(dest #f)
					(args '()))))
			       ;; the next ctx will be ignored...
			       (values s (instantiate::bbv-ctx)))))))
		  ((or (and (type-in? type (bbv-ctxentry-types e))
			    (not (bbv-ctxentry-polarity e)))
		       (and (not (type-in? type (bbv-ctxentry-types e)))
			    (not (type-in? *obj* (bbv-ctxentry-types e)))
			    (bbv-ctxentry-polarity e)))
		   ;; negative type simplification
		   (with-access::rtl_ins/bbv i (fun)
		      (let ((s (duplicate::rtl_ins/bbv i
				  (ctx ctx)
				  (fun (instantiate::rtl_nop))
				  (dest #f)
				  (args '()))))
			 (values s ctx))))
		  ((isa? fun rtl_ifne)
		   (with-access::bbv-ctxentry e (aliases)
		      (let ((regs (cons reg aliases)))
			 (with-access::rtl_ifne fun (then)
			    (let* ((ctx+ (extend-ctx* ctx regs (list type) #t
					    :value (min-value value (bbv-ctxentry-value e))))
				   (ctx- (if (bbv-ctxentry-polarity e)
					     (extend-ctx* ctx regs
						(list type) #f)
					     (extend-ctx* ctx regs
						(cons type (bbv-ctxentry-types e)) #f)))
				   (s (duplicate::rtl_ins/bbv i
					 (ctx ctx)
					 (fun (duplicate::rtl_ifne fun
						 (then (bbv-block then ctx+ queue :creator bs)))))))
			       (trace-item "ctx+.2=" (shape ctx+))
			       (trace-item "ctx-.2=" (shape ctx-))
			       (values s ctx-))))))
		  (else
		   (error "rtl_ins-specialize-typecheck"
		      "should not be here"
		      (shape i)))))))))

;*---------------------------------------------------------------------*/
;*    range->loadi ...                                                 */
;*---------------------------------------------------------------------*/
(define (range->loadi i::rtl_ins dest value types)
   (with-access::bbv-range value (up)
      (let* ((atom (instantiate::literal
		      (type *long*)
		      (value up)))
	     (loadi (instantiate::rtl_loadi
		       (constant atom))))
	 (if (memq *long* types)
	     (duplicate::rtl_ins/bbv i
		(dest dest)
		(args '())
		(fun loadi))
	     (duplicate::rtl_ins/bbv i
		(dest dest)
		(args (list (duplicate::rtl_ins/bbv i
			       (dest dest)
			       (args '())
			       (fun loadi))))
		(fun (instantiate::rtl_call 
			(var *long->bint*))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-mov ...                                       */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-mov i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   (with-trace 'bbv-ins "rtl_ins-specialize-mov"
      (with-access::rtl_ins i (dest args fun)
	 (let ((ctx (unalias-ctx ctx dest)))
	    (cond
	       ((and (pair? args) (null? (cdr args)) (rtl_reg/ra? (car args)))
		(trace-item "rtl_ins-specialize-mov.1")
		(let ((e (bbv-ctx-get ctx (car args))))
		   (with-access::bbv-ctxentry e (types value polarity)
		      (if (bbv-singleton? value)
			  (values (range->loadi i dest value types)
			     (extend-ctx ctx dest types polarity :value value))
			  (values (duplicate-ins i ctx)
			     (alias-ctx
				(extend-ctx ctx dest types polarity :value value)
				dest (car args)))))))
	       ((and *type-call* (pair? args) (rtl_ins-call? (car args)))
		(trace-item "rtl_ins-specialize-mov.2")
		(with-access::rtl_ins (car args) (fun (fargs args))
		   (with-access::rtl_call fun (var)
		      (with-access::global var (value type)
			 (if (and (eq? var *long->bint*)
				  (isa? (car fargs) rtl_ins)
				  (rtl_ins-loadi? (car fargs)))
			     (with-access::rtl_ins (car fargs) (fun)
				(with-access::rtl_loadi fun (constant)
				   (with-access::atom constant (value type)
				      (values (duplicate-ins i ctx)
					 (extend-ctx ctx dest (list *bint*) #t
					    :value (if (fixnum? value)
						       (fixnum->range value)
						       (fixnum-range)))))))
			     (let ((nins (ins-specialize! args bs ctx queue)))
				(with-access::rtl_ins/bbv (car nins) ((nctx ctx))
				   (with-access::rtl_ins (car nins) ((adest dest))
				      (let ((e (bbv-ctx-get nctx adest)))
					 (values (duplicate-ins i ctx)
					    (if e
						(extend-ctx ctx dest
						   (bbv-ctxentry-types e)
						   (bbv-ctxentry-polarity e)
						   :value (bbv-ctxentry-value e))
						(extend-ctx ctx dest
						   (list type) #t))))))))))))
	       ((and *type-loadi* (pair? args) (rtl_ins-loadi? (car args)))
		(trace-item "rtl_ins-specialize-mov.3")
		(with-access::rtl_ins (car args) (fun)
		   (with-access::rtl_loadi fun (constant)
		      (with-access::atom constant (value type)
			 (values (duplicate-ins i ctx)
			    (extend-ctx ctx dest (list type) #t :value
			       (if (fixnum? value) (fixnum->range value) '_)))))))
	       ((and *type-loadg* (pair? args) (rtl_ins-loadg? (car args)))
		(trace-item "rtl_ins-specialize-mov.4")
		(with-access::rtl_ins (car args) (fun)
		   (with-access::rtl_loadg fun (var)
		      (with-access::variable var (value type)
			 (values (duplicate-ins i ctx)
			    (extend-ctx ctx dest (list type) #t))))))
	       (else
		(trace-item "rtl_ins-specialize-mov.5")
		(with-access::rtl_reg dest (type)
		   (values (duplicate-ins i ctx)
		      (extend-ctx ctx dest (list (if (eq? type *obj*) (rtl_ins-type i) type)) #t)))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-type ...                                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-type i::rtl_ins)
   (with-access::rtl_ins i (fun args)
      (cond
	 ((isa? fun rtl_mov)
	  (rtl_ins-type (car args)))
	 ((isa? fun rtl_call)
	  (with-access::rtl_call fun (var)
	     (with-access::variable var (value type)
		type)))
	 (else
	  *obj*))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-loadi ...                                     */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-loadi i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   (with-trace 'bbv-ins "rtl_ins-specialize-loadi"
      (with-access::rtl_ins i (dest args fun)
	 (with-access::rtl_loadi fun (constant)
	    (with-access::atom constant (value type)
	       (let* ((s (duplicate::rtl_ins/bbv i
			    (ctx ctx)
			    (fun (duplicate::rtl_loadi fun))))
		      (v (if (fixnum? value) (fixnum->range value) '_))
		      (nctx (extend-ctx ctx dest (list type) #t :value v)))
		  (trace-item "load.i " (shape nctx))
		  (values s nctx)))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-loadg ...                                     */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-loadg i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   (with-trace 'bbv-ins "rtl_ins-specialize-loadg"
      (with-access::rtl_ins i (dest args fun)
	 (with-access::rtl_loadg fun (var)
	    (with-access::variable var (type)
	       (let ((s (duplicate::rtl_ins/bbv i
			   (ctx ctx)
			   (fun (duplicate::rtl_loadg fun))))
		     (nctx (extend-ctx ctx dest (list type) #t)))
		  (trace-item "load.g " (shape nctx))
		  (values s nctx)))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-call-specialize? ...                                     */
;*---------------------------------------------------------------------*/
(define (rtl_ins-call-specialize? i::rtl_ins)
   (when (rtl_ins-call? i)
      (with-access::rtl_ins i (dest fun)
	 dest)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-call ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-call i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   
   (define (new-value-loadi i)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_loadi fun (constant)
	    (with-access::atom constant (value type)
	       (if (fixnum? value) (fixnum->range value) '_)))))
   
   (define (new-value-call i)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (with-access::global var (value type)
	       (cond
		  ((eq? var *bint->long*)
		   (trace-item "new-value-call.bint->long")
		   (cond
		      ((rtl_reg? (car args))
		       (let ((e (bbv-ctx-get ctx (car args))))
			  (if (and e (bbv-range? (bbv-ctxentry-value e)))
			      (bbv-ctxentry-value e)
			      (fixnum-range))))
		      ((rtl_ins-loadi? (car args))
		       (new-value-loadi (car args)))
		      (else
		       (fixnum-range))))
		  ((eq? var *long->bint*)
		   (trace-item "new-value-call.long->bint")
		   (let loop ((args args))
		      (cond
			 ((rtl_reg? (car args))
			  (trace-item "new-value-call.long->bint.reg")
			  (let ((e (bbv-ctx-get ctx (car args))))
			     (if (and e (bbv-range? (bbv-ctxentry-value e)))
				 (bbv-ctxentry-value e)
				 (fixnum-range))))
			 ((rtl_ins-loadi? (car args))
			  (trace-item "new-value-call.long->bint.loadi")
			  (new-value-loadi (car args)))
			 ((or (rtl_ins-vlen? (car args)) (rtl_ins-strlen? (car args)))
			  (trace-item "new-value-call.long->bint.vlen")
			  (with-access::rtl_ins (car args) (dest)
			     (multiple-value-bind (i vctx)
				(rtl_ins-specialize-vlen (car args) ins bs ctx queue)
				(let ((e (bbv-ctx-get vctx dest)))
				   (if (bbv-ctxentry? e)
				       (bbv-ctxentry-value e)
				       (vlen-range))))))
			 ((rtl_ins-mov? (car args))
			  (trace-item "new-value-call.long->bint.mov")
			  (with-access::rtl_ins (car args) (args)
			     (loop args)))
			 ((rtl_ins-call? (car args))
			  (trace-item "new-value-call.long->bint.call")
			  (with-access::rtl_ins (car args) (dest)
			     (multiple-value-bind (i cctx)
				;; don't invoke directly rtl_ins-specialize-call
				;; has the call might be a fixnum op or
				;; a vlength
				((rtl_ins-specializer (car args))
				 (car args) ins bs ctx queue)
				(with-access::rtl_ins/bbv i (dest)
				   (let ((e (bbv-ctx-get cctx dest)))
				      (trace-item "e=" (shape e))
				      (trace-item "dest=" (shape dest))
				      (trace-item "cctx=" (shape cctx))
				      (if (bbv-ctxentry? e)
					  (bbv-ctxentry-value e)
					  (fixnum-range)))))))
			 (else
			  (trace-item "new-value-call.long->bint.default")
			  (trace-item "arg0=" (shape (car args)))
			  (fixnum-range)))))
		  (else
		   (trace-item "new-value-call.default")
		   '_))))))
   
   (define (new-value i)
      (cond
	 ((rtl_ins-strlen? i) (rtl-range i ctx))
	 ((rtl_ins-call? i) (new-value-call i))
	 ((rtl_ins-loadi? i) (new-value-loadi i))
	 (else '_)))

   (with-trace 'bbv-ins (format "rtl_ins-specialize-call [@~a]" (gendebugid))
      (trace-item "ins=" (shape i))
      (trace-item "ctx=" (shape ctx))
      (with-access::rtl_ins i (dest fun)
	 (with-access::rtl_call fun (var)
	    (with-access::global var (value type)
	       (let* ((s (duplicate::rtl_ins/bbv i
			    (ctx ctx)
			    (fun (duplicate::rtl_call fun))))
		      (nv (new-value i)))
		  (trace-item "nv=" (shape nv))
		  (values s
		     (extend-ctx ctx dest (list (if (fun? value) type *obj*)) #t
			:value nv))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-return-specialize? ...                                   */
;*---------------------------------------------------------------------*/
(define (rtl_ins-return-specialize? i::rtl_ins)
   (with-access::rtl_ins i (fun args)
      (when (isa? fun rtl_return)
	 (rtl_reg? (car args)))))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-return ...                                    */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-return i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   (with-access::rtl_ins i (args dest)
      (let ((e (bbv-ctx-get ctx (car args))))
	 (with-access::bbv-ctxentry e (types value)
	    (if (bbv-singleton? value)
		(let ((loadi (duplicate::rtl_ins/bbv i
				(args (list (range->loadi i
					       (car args) value types)))
				(fun (instantiate::rtl_mov))
				(dest (car args)))))
		   (values (duplicate::rtl_ins/bbv i
			      (args (cons loadi (cdr args))))
		      ctx))
		(values i ctx))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-vlen ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-vlen i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   (with-trace 'bbv-ins "rtl_ins-specialize-vlen"
      (with-access::rtl_ins/bbv i (dest args)
	 (let* ((range (rtl-range i ctx))
		(nctx (extend-ctx ctx dest (list *long*) #t :value range)))
	    (trace-item "range=" range)
	    (trace-item "nctx=" (shape nctx))
	    (values (duplicate::rtl_ins/bbv i
		       (ctx ctx))
	       nctx)))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-vector-bound-check? ...                                  */
;*---------------------------------------------------------------------*/
(define (rtl_ins-vector-bound-check? i::rtl_ins)
   (when (rtl_ins-ifne? i)
      (with-access::rtl_ins i (args fun)
	 (when (and (isa? (car args) rtl_ins) (rtl_ins-call? (car args)))
	    (with-access::rtl_ins (car args) (fun args)
	       (with-access::rtl_call fun (var)
		  (when (or (eq? var *vector-bound-check*) (eq? var *string-bound-check*))
		     (let ((args (rtl_ins-args* i)))
			(and (rtl_reg? (car args)) (rtl_reg? (cadr args)))))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-vector-bound-check ...                        */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-vector-bound-check i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   
   (define (true)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #t)))))
   
   (define (false)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #f)))))
   
   (define (dup-ifne i ctx+ ctx-)
      (with-access::rtl_ins i (fun)
	 (with-access::rtl_ifne fun (args then)
	    (let ((nfun (duplicate::rtl_ifne fun
			   (then (bbv-block then ctx+ queue :creator bs)))))
	       (values (duplicate::rtl_ins/bbv i
			  (ctx ctx)
			  (fun nfun))
		  ctx-)))))

   (with-trace 'bbv-ins
	 (format "rtl_ins-specialize-vector-bound-check [@~a]" (gendebugid))
      (with-access::rtl_ins i (args fun)
	 (trace-item "ins=" (shape i))
	 (trace-item "ctx=" (shape ctx))
	 (with-access::rtl_ins (car args) (fun args)
	    (with-access::rtl_call fun (var)
	       (let* ((vec (car args))
		      (len (cadr args))
		      (a (bbv-ctx-get ctx vec))
		      (l (bbv-ctx-get ctx len))
		      (va (bbv-ctxentry-value a))
		      (vl (bbv-ctxentry-value l)))
		  (trace-item "a=" (shape a))
		  (trace-item "l=" (shape l))
		  (cond
		     ((not *bbv-optim-bound*)
		      (trace-item "noopt")
		      (let ((ctx+ (extend-ctx ctx vec (list *bint*) #t
				     :value (bbv-range-lt vl (vlen-range))))
			    (ctx- ctx))
			 (trace-item "ctx+=" (shape ctx+))
			 (dup-ifne i ctx+ ctx-)))
		     ((not (bbv-range? va))
		      (trace-item "not bbv-range? va")
		      (if (bbv-range? vl)
			  (let ((ctx+ (extend-ctx ctx vec (list *vector*) #t
					 :value vl))
				(ctx- ctx))
			     (trace-item "ctx+.1=" (shape ctx+))
			     (dup-ifne i ctx+ ctx-))
			  (dup-ifne i ctx ctx)))
		     ((not (bbv-range? vl))
		      (trace-item "not bbv-range? vl")
		      (dup-ifne i ctx ctx))
		     ((eq? (bbv-range< va vl) #t)
		      (trace-item "in bound" (shape ctx))
		      (with-access::rtl_ins/bbv i (fun in out args)
			 (with-access::rtl_ifne fun (then)
			    (let* ((fun (duplicate::rtl_go fun
					   (to (bbv-block then ctx queue :creator bs))))
				   (vlenreg (bbv-ctxentry-reg l))
				   (ni (duplicate::rtl_ins/bbv i
					  (ctx ctx)
					  (args '())
					  (fun fun))))
			       (when (rtl_reg? vlenreg)
				  ;; update the liveness property so that
				  ;; the previous vlength instruction can be
				  ;; removed too
				  (unless (regset-member? vlenreg out)
				     (let ((inset (duplicate-regset in)))
					(regset-remove! inset vlenreg)
					(with-access::rtl_ins/bbv ni (in)
					   (set! in inset))
					(when (and (pair? ins)
						   (or (rtl_ins-vlen? (car ins))
						       (rtl_ins-strlen? (car ins))))
					   (with-access::rtl_ins/bbv (car ins) (dest out fun args)
					      (when (eq? dest vlenreg)
						 (let ((outset (duplicate-regset out)))
						    (regset-remove! outset dest)
						    (set! out outset))
						 (set! fun (instantiate::rtl_nop))
						 (set! args '())))))))
			       (values ni ctx)))))
;* 		     ((and (eq? (bbv-range< va vl) #t)                 */
;* 			   (eq? (bbv-range>= va (vlen-range)) #t))     */
;* 		      (trace-item "bbv-range>va and bbvrange>=va")     */
;* 		      (let ((ctx- (extend-ctx ctx vec (list *vector*) #t */
;* 				     :value (bbv-range-lt va vl))))    */
;* 			 (trace-item "ctx-=" (shape ctx-))             */
;* 		      (values (duplicate::rtl_ins/bbv i (fun (true)) (args '())) nctx))) */
		     ((or (eq? (bbv-range> va vl) #t)
			  (eq? (bbv-range< va (vlen-range)) #t))
		      (trace-item "bbv-range>va or bbvrange <va")
		      (values (duplicate::rtl_ins/bbv i (fun (false)) (args '()))
			 ctx))
		     (else
		      (trace-item "bound-checke.else")
		      (let ((ctx+ (extend-ctx ctx vec (list *bint*) #t
				     :value (bbv-range-lt vl (vlen-range))))
			    (ctx- ctx))
			 (trace-item "ctx+.2=" (shape ctx+))
			 (dup-ifne i ctx+ ctx-))))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-resolved-ifne ...                             */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-resolved-ifne i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   (with-access::rtl_ins i (args)
      (with-access::rtl_ins (car args) (fun)
	 (with-access::rtl_loadi fun (constant)
	    (with-access::rtl_ins i (fun)
	       (with-access::rtl_ifne fun (then)
		  (if (eq? constant #f)
		      (let* ((fun (duplicate::rtl_nop fun))
			     (ni (duplicate::rtl_ins/bbv i
				    (ctx ctx)
				    (args '())
				    (fun fun))))
			 (values ni ctx))
		      (with-access::rtl_ifne fun (then)
			 (let* ((fun (duplicate::rtl_go fun
					(to (bbv-block then ctx queue
					       :creator bs))))
				(ni (duplicate::rtl_ins/bbv i
				       (ctx ctx)
				       (args '())
				       (fun fun))))
			    (values ni ctx))))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-fxcmp? ...                                               */
;*---------------------------------------------------------------------*/
(define (rtl_ins-fxcmp? i)
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (rtl_reg? dest))))))

   (define (rtl_call-fxcmp? i)
      (when (isa? i rtl_ins)
	 (with-access::rtl_ins i (dest fun args)
	    (when (isa? fun rtl_call)
	       (with-access::rtl_call fun (var)
		  (and (=fx (length args) 2)
		       (or (eq? var *<fx*) (eq? var *<=fx*)
			   (eq? var *>fx*) (eq? var *>=fx*)
			   (eq? var *=fx*))))))))
   
   (with-access::rtl_ins i (dest fun args)
      (cond
	 ((isa? fun rtl_call) (rtl_call-fxcmp? i))
	 ((isa? fun rtl_ifne) (rtl_call-fxcmp? (car args)))
	 (else #f))))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-fxcmp ...                                     */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-fxcmp i::rtl_ins ins::pair-nil bs ctx::bbv-ctx queue::bbv-queue)
   
   (define (true)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #t)))))
   
   (define (false)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #f)))))

   (define (rtl_bint->long? a)
      (when (isa? a rtl_ins)
	 (with-access::rtl_ins a (fun)
	    (when (isa? fun rtl_call)
	       (with-access::rtl_call fun (var)
		  (eq? var *bint->long*))))))

   (define (rtl_$int->long? a)
      (when (isa? a rtl_ins)
	 (with-access::rtl_ins a (fun)
	    (when (isa? fun rtl_call)
	       (with-access::rtl_call fun (var)
		  (eq? var *$int->long*))))))
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (with-access::rtl_call fun (var)
			(if (eq? var *bint->long*)
			    (rtl_reg? (car args))
			    (rtl_reg? dest))))))))
   
   (define (reg a)
      (cond
	 ((rtl_reg? a) a)
	 ((rtl_bint->long? a) (car (rtl_ins-args a)))
	 ((rtl_$int->long? a) (car (rtl_ins-args a)))
	 (else (rtl_ins-dest a))))
   
   (define (fxcmp-op i)
      (with-access::rtl_ins i (fun)
	 (with-access::rtl_call fun (var)
	    (cond
	       ((eq? var *<fx*) '<)
	       ((eq? var *<=fx*) '<=)
	       ((eq? var *>fx*) '>)
	       ((eq? var *>=fx*) '>=)
	       ((eq? var *=fx*) '=)))))
   
   (define (inv-op op)
      (case op
	 ((<) '>)
	 ((<=) '>=)
	 ((>) '<)
	 ((>=) '<=)
	 ((=) '!=)
	 (else op)))
   
   (define (resolve/op i op intl::bbv-range intr::bbv-range)
      (case op
	 ((<) (bbv-range< intl intr))
	 ((<=) (bbv-range<= intl intr))
	 ((>) (bbv-range> intl intr))
	 ((>=) (bbv-range>= intl intr))
	 ((=) (bbv-range= intl intr))
	 (else #unspecified)))

   (define (narrowing/op reg intr::bbv-range inte::bbv-range op+ op- ctx::bbv-ctx)
      (let* ((inte+ (op+ intr inte))
	     (inte- (op- intr inte))
	     (ctx+ (extend-ctx ctx reg (list *bint*) #t :value inte+))
	     (ctx- (extend-ctx ctx reg (list *bint*) #t :value inte-)))
	 (values ctx+ ctx-)))
   
   (define (narrowing reg intr::bbv-range inte::bbv-range op ctx::bbv-ctx)
      (with-trace 'bbv-ins (format "rtl_ins-specialize-fxcmp/narrowing [@~a]"
			      (gendebugid))
	 (trace-item "op=" op)
	 (trace-item "reg=" (shape reg))
	 (trace-item "ctx=" (shape ctx))
	 (case op
	    ((<)
	     (narrowing/op reg intr inte bbv-range-lt bbv-range-gte ctx))
	    ((<=)
	     (narrowing/op reg intr inte bbv-range-lte bbv-range-gt ctx))
	    ((>)
	     (narrowing/op reg intr inte bbv-range-gt bbv-range-lte ctx))
	    ((>=)
	     (narrowing/op reg intr inte bbv-range-gte bbv-range-lt ctx))
	    ((=)
	     (narrowing/op reg intr inte bbv-range-eq bbv-range-neq ctx))
	    ((!=)
	     (narrowing/op reg intr inte bbv-range-neq bbv-range-eq ctx))
	    (else
	     (values ctx ctx)))))
   
   (define (specialize/op op lhs rhs intl::bbv-range intr::bbv-range ctx::bbv-ctx)
      (with-trace 'bbv-ins
	    (format "rtl_ins-specialize-fxcmp/op ~a [@~a]" op (gendebugid))
	 (cond
	    ((and (isa? lhs rtl_ins) (rtl_ins-mov? lhs))
	     (with-access::rtl_ins lhs (args)
		(specialize/op op (car args) intl rhs intr ctx)))
	    ((and (isa? rhs rtl_ins) (rtl_ins-mov? rhs))
	     (with-access::rtl_ins rhs (args)
		(specialize/op op lhs intl (car args) intr ctx)))
	    ((and (reg? lhs) (reg? rhs))
	     ;; two registers comparison
	     (let ((lreg (reg lhs))
		   (rreg (reg rhs)))
		(multiple-value-bind (lctx+ lctx-)
		   (narrowing lreg intl intr op ctx)
		   (trace-item "lctx+=" (shape lctx+))
		   (trace-item "lctx-=" (shape lctx-))
		   (multiple-value-bind (rctx+ rctx-)
		      (narrowing rreg intr intl (inv-op op) ctx)
		      (trace-item "rctx+=" (shape rctx+))
		      (trace-item "rctx-=" (shape rctx-))
		      (let ((lreg+ (bbv-ctx-get lctx+ lreg))
			    (lreg- (bbv-ctx-get lctx- lreg))
			    (rreg+ (bbv-ctx-get rctx+ rreg))
			    (rreg- (bbv-ctx-get rctx- rreg)))
			 (values
			    (extend-ctx/entry* ctx lreg+ rreg+)
			    (extend-ctx/entry* ctx lreg- rreg-)))))))
	    ((reg? lhs)
	     ;; single register comparison
	     (narrowing (reg lhs) intl intr op ctx))
	    ((reg? rhs)
	     ;; single register comparison
	     (narrowing (reg rhs) intr intl (inv-op op) ctx))
	    (else
	     ;; no register involed
	     (values ctx ctx)))))
   
   (define (specialize-call i::rtl_ins ctx::bbv-ctx)
      (with-trace 'bbv-ins
	    (format "rtl_ins-specialize-fxcmp/call ~a [@~a]"
	       (shape i) (gendebugid))
	 (trace-item "ctx=" (shape ctx))
	 (with-access::rtl_ins i (fun args)
	    (with-access::rtl_call fun (var)
	       (let* ((lhs (car args))
		      (rhs (cadr args))
		      (intl (rtl-range lhs ctx))
		      (intr (rtl-range rhs ctx))
		      (op (fxcmp-op i)))
		  (trace-item "op=" op)
		  (trace-item "lhs=" (shape lhs))
		  (trace-item "rhs=" (shape rhs))
		  (trace-item "intl=" (shape intl))
		  (trace-item "intr=" (shape intr))
		  (if (not (and (bbv-range? intl) (bbv-range? intr)))
		      (values (duplicate::rtl_ins/bbv i)
			 ctx ctx)
		      (case (resolve/op i op intl intr)
			 ((#t)
			  (trace-item "resolve/op.true")
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx)
				     (fun (true))
				     (args '()))
			     ctx ctx))
			 ((#f)
			  (trace-item "resolve/op.false")
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx)
				     (fun (false))
				     (args '()))
			     ctx ctx))
			 (else
			  (multiple-value-bind (ctx+ ctx-)
			     (specialize/op op lhs rhs intl intr ctx)
			     (trace-item "resolve/op.unspec+=" (shape ctx+))
			     (trace-item "resolve/op.unspec-=" (shape ctx-))
			     (values (duplicate::rtl_ins/bbv i)
				ctx+ ctx-))))))))))

   (with-trace 'bbv-ins (format "rtl_ins-specialize-fxcmp [@~a]" (gendebugid))
      (trace-item "ins=" (shape i))
      (with-access::rtl_ins i (dest fun args)
	 (cond
	    ((isa? fun rtl_ifne)
	     (with-access::rtl_ifne fun (then)
		(multiple-value-bind (ins ctx+ ctx-)
		   (specialize-call (car args) ctx)
		   (trace-item "ctx+=" (shape ctx+))
		   (trace-item "ctx-=" (shape ctx-))
		   (cond
		      ((rtl_ins-true? ins)
		       (with-access::rtl_ifne fun (then)
			  (let* ((fun (duplicate::rtl_go fun
					 (to (bbv-block then ctx+ queue
						:creator bs))))
				 (ni (duplicate::rtl_ins/bbv i
					(ctx ctx)
					(args '())
					(fun fun))))
			     (values ni ctx+))))
		      ((rtl_ins-false? ins)
		       (let* ((fun (duplicate::rtl_nop fun))
			      (ni (duplicate::rtl_ins/bbv i
				     (ctx ctx)
				     (args '())
				     (fun fun))))
			  (values ni ctx-)))
		      (else
		       (let ((fun (duplicate::rtl_ifne fun
				     (then (bbv-block then ctx+ queue
					      :creator bs)))))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx)
				     (args (list ins))
				     (fun fun))
			     ctx-)))))))
	    ((isa? fun rtl_call)
	     (multiple-value-bind (ins ctx+ ctx-)
		(specialize-call i ctx)
		(cond
		   ((rtl_ins-true? ins) (values ins ctx+))
		   ((rtl_ins-false? ins) (values ins ctx-))
		   (else (values ins ctx)))))
	    (else
	     (error "rtl_ins-specialize-fxcmp"
		"Should not be here" (shape i)))))))

;*---------------------------------------------------------------------*/
;*    tbr                                                              */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-fxcmp-TBR114dec2023 i::rtl_ins ins::pair-nil bs ctx::bbv-ctx queue::bbv-queue)
   
   (define (true)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #t)))))
   
   (define (false)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #f)))))

   (define (rtl_bint->long? a)
      (when (isa? a rtl_ins)
	 (with-access::rtl_ins a (fun)
	    (when (isa? fun rtl_call)
	       (with-access::rtl_call fun (var)
		  (eq? var *bint->long*))))))

   (define (rtl_$int->long? a)
      (when (isa? a rtl_ins)
	 (with-access::rtl_ins a (fun)
	    (when (isa? fun rtl_call)
	       (with-access::rtl_call fun (var)
		  (eq? var *$int->long*))))))
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (with-access::rtl_call fun (var)
			(if (eq? var *bint->long*)
			    (rtl_reg? (car args))
			    (rtl_reg? dest))))))))
   
   (define (reg a)
      (cond
	 ((rtl_reg? a) a)
	 ((rtl_bint->long? a) (car (rtl_ins-args a)))
	 ((rtl_$int->long? a) (car (rtl_ins-args a)))
	 (else (rtl_ins-dest a))))
   
   (define (fxcmp-op i)
      (with-access::rtl_ins i (fun)
	 (with-access::rtl_call fun (var)
	    (cond
	       ((eq? var *<fx*) '<)
	       ((eq? var *<=fx*) '<=)
	       ((eq? var *>fx*) '>)
	       ((eq? var *>=fx*) '>=)
	       ((eq? var *=fx*) '=)))))
   
   (define (inv-op op)
      (case op
	 ((<) '>)
	 ((<=) '>=)
	 ((>) '<)
	 ((>=) '<=)
	 ((=) '!=)
	 (else op)))
   
   (define (resolve/op i op intl intr)
      (case op
	 ((<) (bbv-range< intl intr))
	 ((<=) (bbv-range<= intl intr))
	 ((>) (bbv-range> intl intr))
	 ((>=) (bbv-range>= intl intr))
	 ((=) (bbv-range= intl intr))
	 (else #f)))
   
   (define (test-ctxs-ref reg intl intr op ctx::bbv-ctx)
      (with-trace 'bbv-ins (format "rtl_ins-specialize-fxcmp/text-ctxs-ref [@~a]"
			      (gendebugid))
	 (trace-item "op=" op)
	 (trace-item "reg=" (shape reg))
	 (trace-item "intl=" (shape intl))
	 (trace-item "intr=" (shape intr))
	 (trace-item "ctx=" (shape ctx))
	 (if (or (not (bbv-range? intl)) (not (bbv-range? intr)))
	     (values ctx ctx)
	     (let ((e (bbv-ctx-get ctx reg)))
		(if (not e)
		    (values ctx ctx)
		    (let ((types (list *bint*)))
		       (case op
			  ((<)
			   (let ((intr+ (bbv-range-lt intl intr))
				 (intr- (bbv-range-gte intl intr)))
			      (trace-item "reg=" (shape reg))
			      (trace-item "<.intr+=" (shape intr+))
			      (trace-item "<.intr-=" (shape intr-))
			      (let ((ctx+ (unless (empty-range? intr+)
					     (extend-ctx ctx reg types #t :value intr+)))
				    (ctx- (unless (empty-range? intr-)
					     (extend-ctx ctx reg types #t :value intr-))))
				 (trace-item "ctx+.1=" (shape ctx+))
				 (trace-item "ctx-.1=" (shape ctx-))
				 (values ctx+ ctx-))))
			  ((<=)
			   (let ((intr+ (bbv-range-lte intl intr))
				 (intr- (bbv-range-gt intl intr)))
			      (trace-item "reg=" (shape reg))
			      (trace-item "<=.intr+=" (shape intr+))
			      (trace-item "<=.intr-=" (shape intr-))
			      (let ((ctx+ (unless (empty-range? intr+)
					     (extend-ctx ctx reg types #t :value intr+)))
				    (ctx- (unless (empty-range? intr-)
					     (extend-ctx ctx reg types #t :value intr-))))
				 (trace-item "ctx+.1=" (shape ctx+))
				 (trace-item "ctx-.1=" (shape ctx-))
				 (values ctx+ ctx-))))
			  ((>)
			   (let ((intr+ (bbv-range-gt intl intr))
				 (intr- (bbv-range-lte intl intr)))
			      (trace-item "reg=" (shape reg))
			      (trace-item ">.intr+=" (shape intr+))
			      (trace-item ">.intr-=" (shape intr-))
			      (let ((ctx+ (unless (empty-range? intr+)
					     (extend-ctx ctx reg types #t :value intr+)))
				    (ctx- (unless (empty-range? intr-)
					     (extend-ctx ctx reg types #t :value intr-))))
				 (trace-item "ctx+.1=" (shape ctx+))
				 (trace-item "ctx-.1=" (shape ctx-))
				 (values ctx+ ctx-))))
			  ((>=)
			   (let ((intr+ (bbv-range-gte intl intr))
				 (intr- (bbv-range-lt intl intr)))
			      (trace-item "reg=" (shape reg))
			      (trace-item ">=.intr+=" (shape intr+))
			      (trace-item ">=.intr-=" (shape intr-))
			      (let ((ctx+ (unless (empty-range? intr+)
					     (extend-ctx ctx reg types #t :value intr+)))
				    (ctx- (unless (empty-range? intr-)
					     (extend-ctx ctx reg types #t :value intr-))))
				 (trace-item "ctx+.1=" (shape ctx+))
				 (trace-item "ctx-.1=" (shape ctx-))
				 (values ctx+ ctx-))))
			  ((=)
			   (let ((ctx+ (extend-ctx ctx reg types #t :value intl)))
			      (trace-item "reg=" (shape reg))
			      (trace-item "ctx+.1=" (shape ctx+))
			      (values ctx+ ctx)))
			  ((!=)
			   (values ctx ctx))
			  (else
			   (values ctx ctx)))))))))
   
   (define (specialize/op op lhs intl rhs intr ctx::bbv-ctx)
      (with-trace 'bbv-ins (format "rtl_ins-specialize-fxcmp/op ~a [@a]" op (gendebugid))
	 (trace-item "lhs=" (shape lhs) " intl=" (shape intl))
	 (trace-item "rhs=" (shape rhs) " intr=" (shape intr))
	 (cond
	    ((and (reg? lhs) (reg? rhs))
	     (multiple-value-bind (lctx+ lctx-)
		(test-ctxs-ref (reg lhs) intl intr op ctx)
		(cond
		   ((not lctx+)
		    (values lctx+ lctx-))
		   ((not lctx-)
		    (values lctx+ lctx-))
		   (else
		    (multiple-value-bind (rctx+ _)
		       (test-ctxs-ref (reg rhs) intr intl (inv-op op) lctx+)
		       (multiple-value-bind (_ rctx-)
			  (test-ctxs-ref (reg rhs) intr intl (inv-op op) lctx-)
			  (values rctx+ rctx-)))))))
	    ((and (isa? lhs rtl_ins) (rtl_ins-mov? lhs))
	     (with-access::rtl_ins lhs (args)
		(specialize/op op (car args) intl rhs intr ctx)))
	    ((and (isa? rhs rtl_ins) (rtl_ins-mov? rhs))
	     (with-access::rtl_ins rhs (args)
		(specialize/op op lhs intl (car args) intr ctx)))
	    ((reg? lhs)
	     (test-ctxs-ref (reg lhs) intl intr op ctx))
	    ((reg? rhs)
	     (test-ctxs-ref (reg rhs) intr intl (inv-op op) ctx))
	    (else
	     (values ctx ctx)))))
   
   (define (specialize-call i::rtl_ins ctx::bbv-ctx)
      (with-trace 'bbv-ins (format "rtl_ins-specialize-fxcmp/call ~a [@~a]" (shape i) (gendebugid))
	 (with-access::rtl_ins i (fun args)
	    (with-access::rtl_call fun (var)
	       (let* ((lhs (car args))
		      (rhs (cadr args))
		      (intl (rtl-range lhs ctx))
		      (intr (rtl-range rhs ctx))
		      (op (fxcmp-op i)))
		  (trace-item "lhs=" (shape lhs))
		  (trace-item "rhs=" (shape rhs))
		  (trace-item "intl=" (shape intl))
		  (trace-item "intr=" (shape intr))
		  (cond
		     ((not (and (bbv-range? intl) (bbv-range? intr)))
		      (values (duplicate::rtl_ins/bbv i) ctx ctx))
		     ((resolve/op i op intl intr)
		      =>
		      (lambda (val)
			 (trace-item "resolve/op.val=" val)
			 (case val
			    ((#t)
			     (multiple-value-bind (ctx+ ctx-)
				(specialize/op op lhs (or intl (fixnum-range))
				   rhs (or intr (fixnum-range)) ctx)
				(trace-item "resolve/op.true.ctx+=" (shape ctx+))
				(trace-item "resolve/op.true.ctx-=" (shape ctx-))
				(values (duplicate::rtl_ins/bbv i
					   (ctx ctx+)
					   (fun (true))
					   (args '()))
				   ctx+ ctx-)))
			    ((#f)
			     (multiple-value-bind (ctx+ ctx-)
				(specialize/op op lhs (or intl (fixnum-range))
				   rhs (or intr (fixnum-range)) ctx)
				(trace-item "resolve/op.false.ctx+=" (shape ctx+))
				(trace-item "resolve/op.false.ctx-=" (shape ctx-))
				(values (duplicate::rtl_ins/bbv i
					   (ctx ctx-)
					   (fun (false))
					   (args '()))
				   ctx+ ctx-)))
			    (else
			     (multiple-value-bind (ctx+ ctx-)
				(specialize/op op lhs intl rhs intr ctx)
				(trace-item "resolve/op.unknown.ctx+=" (shape ctx+))
				(trace-item "resolve/op.unknown.ctx-=" (shape ctx-))
				(cond
				   ((not ctx+)
				    (values (duplicate::rtl_ins/bbv i
					       (ctx ctx-)
					       (fun (false))
					       (args '()))
				       ctx ctx-))
				   ((not ctx-)
				    (values (duplicate::rtl_ins/bbv i
					       (ctx ctx+)
					       (fun (true))
					       (args '()))
				       ctx+ ctx))
				   (else
				    (values (duplicate::rtl_ins/bbv i) ctx+ ctx-))))))))
		     (else
		      (multiple-value-bind (ctx+ ctx-)
			 (specialize/op op lhs intl rhs intr ctx)
			 (trace-item "default.ctx+=" (shape ctx+))
			 (trace-item "default.ctx-=" (shape ctx-))
			 (values (duplicate::rtl_ins/bbv i) ctx+ ctx-)))))))))

   (with-trace 'bbv-ins (format "rtl_ins-specialize-fxcmp [@~a]" (gendebugid))
      (trace-item "i=" (shape i))
      (with-access::rtl_ins i (dest fun args)
	 (cond
	    ((isa? fun rtl_ifne)
	     (with-access::rtl_ifne fun (then)
		(multiple-value-bind (ins ctx+ ctx-)
		   (specialize-call (car args) ctx)
		   (trace-item "ins=" (shape ins))
		   (trace-item "istrue=" (rtl_ins-true? ins) " isfalse=" (rtl_ins-false? ins))
		   (trace-item "ctx+=" (shape ctx+))
		   (trace-item "ctx-=" (shape ctx-))
		   (cond
		      ((rtl_ins-true? ins)
		       (with-access::rtl_ifne fun (then)
			  (let* ((fun (duplicate::rtl_go fun
					 (to (bbv-block then ctx+ queue :creator bs))))
				 (ni (duplicate::rtl_ins/bbv i
					(ctx ctx)
					(args '())
					(fun fun))))
			     (values ni ctx+))))
		      ((rtl_ins-false? ins)
		       (let* ((fun (duplicate::rtl_nop fun))
			      (ni (duplicate::rtl_ins/bbv i
				     (ctx ctx)
				     (args '())
				     (fun fun))))
			  (values ni ctx-)))
		      (else
		       (let ((fun (duplicate::rtl_ifne fun
				     (then (bbv-block then ctx+ queue :creator bs)))))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx)
				     (args (list ins))
				     (fun fun))
			     ctx-)))))))
	    ((isa? fun rtl_call)
	     (multiple-value-bind (ins ctx+ nctx)
		(specialize-call i ctx)
		(cond
		   ((rtl_ins-true? ins) (values ins ctx+))
		   ((rtl_ins-false? ins) (values ins nctx))
		   (else (values ins ctx)))))
	    (else
	     (error "rtl_ins-specialize-fxcmp"
		"Should not be here" (shape i)))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-fxop? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-fxop? i)
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (rtl_reg? dest))))))

   (define (rtl_ins-integer? i)
      (or (reg? i) (rtl_ins-vlen? i) (rtl_ins-strlen? i) (rtl_ins-loadi? i)))

   (define (rtl_call-fxop? i)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (and (=fx (length args) 2)
		 (or (eq? var *+fx*)
		     (eq? var *-fx*)
		     (eq? var *-fx-safe*)
		     (eq? var *+fx-safe*)
		     (eq? var *2+*)
		     (eq? var *subfx*)
		     (eq? var *addfx*))
		 (rtl_ins-integer? (car args))
		 (rtl_ins-integer? (cadr args))))))
   
   (with-access::rtl_ins i (dest fun args)
      (cond
	 ((isa? fun rtl_call)
	  (rtl_call-fxop? i))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-fxovop ...                                    */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-fxovop i::rtl_ins ins::pair-nil bs ctx::bbv-ctx queue::bbv-queue)
   
   (define (fxovop-ctx ctx reg i::rtl_ins)
      (with-access::rtl_ins i (fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (intl (rtl-range lhs ctx))
		   (intr (rtl-range rhs ctx))
		   (intv (cond
			    ((not (and (bbv-range? intl) (bbv-range? intr)))
			     (fixnum-range))
			    ((eq? var *$-fx/ov*)
			     (bbv-range-sub intl intr))
			    ((eq? var *$+fx/ov*)
			     (bbv-range-add intl intr))
			    ((eq? var *$*fx/ov*)
			     (bbv-range-mul intl intr))
			    (else
			     (error "fxovop-ctx" "wrong operator" (shape fun))))))
	       (with-access::bbv-range intv (lo up)
		  (let* ((intx (instantiate::bbv-range
				  (lo (maxrv lo (bbv-min-fixnum) (bbv-min-fixnum)))
				  (up (minrv up (bbv-max-fixnum) (bbv-max-fixnum)))))
			 (nctx (extend-ctx ctx reg (list *bint*) #t
				  :value intx)))
		     (trace-item "nctx=" (shape nctx))
		     nctx))))))
   
   (define (fx-ov->fx var ins reg ctx nctx)
      (with-access::rtl_ins ins (dest fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (subcall (duplicate::rtl_call fun
			       (var (cond
				       ((eq? var *$-fx/ov*) *$-fx/w-ov*)
				       ((eq? var *$+fx/ov*) *$+fx/w-ov*)
				       ((eq? var *$*fx/ov*) *$*fx/w-ov*)
				       (else (error "fx/ov" "unknown op" (shape var))))))))
	       (values (duplicate::rtl_ins/bbv ins
			  (dest reg)
			  (fun subcall)
			  (args (list lhs rhs))
			  (ctx nctx))
		  nctx)))))
   
   (define (default i call fun reg value)
      (with-access::rtl_ifne fun (then)
	 (let* ((ctx+ (extend-ctx ctx reg (list *bint*) #f :value value))
		(nctx (fxovop-ctx ctx reg call))
		(s (duplicate::rtl_ins/bbv i
		      (ctx ctx)
		      (fun (duplicate::rtl_ifne fun
			      (then (bbv-block then ctx+ queue :creator bs)))))))
	    (trace-item "ctx+=" (shape ctx+))
	    (trace-item "nctx=" (shape nctx))
	    (values s nctx))))
   
   (with-trace 'bbv-ins "rtl_ins-specialize-fxovop"
      (trace-item "i=" (shape i))
      (with-access::rtl_ins i (dest (ifne fun) args)
	 (let ((call (car args)))
	    (with-access::rtl_ins (car args) (args fun)
	       (let* ((reg (caddr args))
		      (lhs (car args))
		      (rhs (cadr args))
		      (intl (rtl-range lhs ctx))
		      (intr (rtl-range rhs ctx)))
		  (with-access::rtl_call fun (var)
		     (cond
			((or (not (bbv-range? intl)) (not (bbv-range? intr)))
			 (default i call ifne reg '_))
			((eq? var *$+fx/ov*)
			 (let ((range (bbv-range-add intl intr)))
			    (trace-item "+fx/ov "
			       (shape intl) " " (shape intr)
			       " -> " (shape range))
			    (if (bbv-range-fixnum? range)
				(let ((nctx (extend-ctx ctx reg
					       (list *bint*) #t
					       :value range)))
				   (trace-item "nctx.2=" (shape nctx))
				   (values (fx-ov->fx var call reg ctx nctx)
				      nctx))
				(default i call ifne reg (fixnum-range)))))
			((eq? var *$-fx/ov*)
			 (let ((range (bbv-range-sub intl intr)))
			    (trace-item "-fx/ov "
			       (shape intl) " " (shape intr)
			       " -> " (shape range))
			    (if (bbv-range-fixnum? range)
				(let ((nctx (extend-ctx ctx reg
					       (list *bint*) #t
					       :value range)))
				   (trace-item "nctx.3=" (shape nctx))
				   (values (fx-ov->fx var call reg ctx nctx)
				      nctx))
				(default i call ifne reg (fixnum-range)))))
			((eq? var *$*fx/ov*)
			 (let ((range (bbv-range-mul intl intr)))
			    (if (bbv-range-fixnum? range)
				(let ((nctx (extend-ctx ctx reg
					       (list *bint*) #t
					       :value range)))
				   (trace-item "nctx.4=" (shape nctx))
				   (values (fx-ov->fx var call reg ctx nctx)
				      nctx))
				(default i call ifne reg (fixnum-range)))))
			(else
			 (default i call ifne reg '_))))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-fxop ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-fxop i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   
   (define (fx-safe->fx var i ctx nctx)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (subcall (duplicate::rtl_call fun
			       (var (if (eq? var *-fx-safe*) *-fx* *+fx*))))
		   (castres (duplicate::rtl_call fun
			       (var *long->bint*))))
	       (values (duplicate::rtl_ins/bbv i
			  (fun castres)
			  (args (list (duplicate::rtl_ins/bbv i
					 (fun subcall)
					 (args (list lhs rhs))
					 (ctx ctx))))
			  (ctx nctx))
		  nctx)))))
   
   (define (2->fx var i ctx nctx)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (castlhs (duplicate::rtl_ins/bbv i
			       (fun (duplicate::rtl_call fun
				       (var *bint->long*)))
			       (args (list lhs))))
		   (castrhs (duplicate::rtl_ins/bbv i
			       (fun (duplicate::rtl_call fun
				       (var *bint->long*)))
			       (args (list rhs))))
		   (subcall (duplicate::rtl_call fun
			       (var (if (eq? var *2-*) *-fx* *+fx*))))
		   (castres (duplicate::rtl_call fun
			       (var *long->bint*))))
	       (values (duplicate::rtl_ins/bbv i
			  (fun castres)
			  (args (list (duplicate::rtl_ins/bbv i
					 (fun subcall)
					 (args (list castlhs castrhs))
					 (ctx ctx))))
			  (ctx nctx))
		  nctx)))))
   
   (with-trace 'bbv-ins (format "rtl_ins-specialize-fxop [@~a]" (gendebugid))
      (trace-item "i=" (shape i))
      (trace-item "ctx=" (shape ctx))
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (intl (rtl-range lhs ctx))
		   (intr (rtl-range rhs ctx)))
	       (cond
		  ((not (and (bbv-range? intl) (bbv-range? intr)))
		   (trace-item "not.range intl=" (shape intl) " intr=" (shape intr))
		   (values (duplicate::rtl_ins/bbv i
			      (ctx ctx))
		      ctx))
		  ((or (eq? var *-fx*) (eq? var *subfx*))
		   (let ((range (bbv-range-sub intl intr)))
		      (trace-item "-fx.range=" (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *long*) #t
					 :value range)))
			     (trace-item "nctx=" (shape nctx))
			     (values (duplicate::rtl_ins/bbv i
					(ctx ctx))
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((eq? var *-fx-safe*)
		   (let ((range (bbv-range-sub intl intr)))
		      (trace-item "-fx-safe.range=" (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *bint*) #t
					 :value range)))
			     (trace-item "nctx=" (shape nctx))
			     (values (fx-safe->fx var i ctx nctx)
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((or (eq? var *+fx*) (eq? var *addfx*))
		   (let ((range (bbv-range-add intl intr)))
		      (trace-item "+fx.range=" (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *long*) #t
					 :value range)))
			     (trace-item "nctx=" (shape nctx))
			     (values (duplicate::rtl_ins/bbv i
					(ctx ctx))
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((eq? var *+fx-safe*)
		   (let ((range (bbv-range-add intl intr)))
		      (trace-item "+fx-safe.range=" (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *bint*) #t
					 :value range)))
			     (trace-item "nctx=" (shape nctx))
			     (values (fx-safe->fx var i ctx nctx)
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((eq? var *2-*)
		   (let ((range (bbv-range-sub intl intr)))
		      (trace-item "2-.range=" (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *bint*) #t
					 :value range)))
			     (trace-item "nctx=" (shape nctx))
			     (values (2->fx var i ctx nctx)
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((eq? var *2+*)
		   (let ((range (bbv-range-add intl intr)))
		      (trace-item "2+.range=" (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *bint*) #t
					 :value range)))
			     (trace-item "nctx=" (shape nctx))
			     (values (2->fx var i ctx nctx)
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  (else
		   (trace-item "else ctx=" (shape ctx))
		   (values (duplicate::rtl_ins/bbv i
			      (ctx ctx))
		      ctx))))))))
   
