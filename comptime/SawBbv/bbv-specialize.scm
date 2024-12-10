;*=====================================================================*/
;*    .../project/bigloo/flt/comptime/SawBbv/bbv-specialize.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 07:42:00 2017                          */
;*    Last change :  Tue Dec 10 09:29:23 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
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
	    saw_bbv-gc
	    saw_bbv-debug)

   (export (bbv-root-block::blockS ::blockV ::bbv-ctx)))

;*---------------------------------------------------------------------*/
;*    *root-block* ...                                                 */
;*---------------------------------------------------------------------*/
(define *root-block* #f)

;*---------------------------------------------------------------------*/
;*    bbv-root-block ...                                               */
;*---------------------------------------------------------------------*/
(define (bbv-root-block::blockS bv::blockV ctx::bbv-ctx)
   (with-trace 'bbv-block "bbv-root-block"
      (trace-item "label=#" (block-label bv))
      (set! *root-block* bv)
      (let ((queue (instantiate::bbv-queue))
	    (bs (new-blockS bv ctx :gccnt 1 :creator 'root)))
	 (bbv-gc-init! bs)
	 (block-specialize! bs queue)
	 (let loop ((n 0))
	    (trace-item "loop(" n ") queue: "
	       (map (lambda (bs)
		       (with-access::blockS bs (label parent)
			  (format "#~a~a<-#~a" label
			     (if (block-live? bs) "+" "-")
			     (block-label parent))))
		  (with-access::bbv-queue queue (blocks)
		     blocks)))
	    (if (bbv-queue-empty? queue)
		bs
		(let ((bs (bbv-queue-pop! queue)))
		   (with-access::blockS bs ((bv parent) label gccnt)
		      (with-access::blockV bv ((plabel label) versions)
			 (trace-item "pop {#" plabel "}->#" label
			    (if (block-live? bs) "+" "-")
			    " versions: "
			    (map (lambda (b)
				    (with-access::blockS b (label)
				       (format "#~a~a" label
					  (if (block-live? b) "+" "-"))))
			       versions)))
		      (when (block-need-merge? bv)
			 (trace-item "need-merge #" (block-label bv) "...")
			 (when *bbv-debug*
			    (for-each (lambda (b)
					 (with-access::blockS b (label ctx)
					    (trace-item "#" label ": "
					       (shape ctx))))
			       (blockV-live-versions bv)))
			 (block-merge-some! bv queue))
		      (when (block-live? bs)
			 (with-access::blockS bs ((bv parent) label first)
			    (if (pair? first)
				;; the block has already been specialized
				;; but later on became unreachable, it must
				;; must be updated
				(block-update! bs queue)
				(block-specialize! bs queue)))))
		   (loop (+fx n 1))))))))

;*---------------------------------------------------------------------*/
;*    block-specialize! ...                                            */
;*---------------------------------------------------------------------*/
(define (block-specialize!::blockS bs::blockS queue::bbv-queue)
   (with-trace 'bbv-block
	 (format "block-specialize! #~a~a<-#~a parent.succs: ~a preds: ~a"
	    (block-label bs)
	    (if (block-live? bs) "+" "-")
	    (block-label (blockS-parent bs))
	    (map block-label (block-succs (blockS-parent bs)))
	    (map (lambda (b)
		    (format "#~a~a" (block-label b)
		       (if (block-live? b) "+" "-")))
	       (block-preds bs)))
      (with-access::blockS bs ((bv parent) first ctx)
	 (trace-item "ctx: " (shape ctx))
	 (with-access::blockV bv ((pfirst first))
	    (set! first (ins-specialize! pfirst bs ctx queue))
	    (when *bbv-debug*
	       (assert-block bs "block-specialize!"))
	    bs))))

;*---------------------------------------------------------------------*/
;*    block-update! ...                                                */
;*---------------------------------------------------------------------*/
(define (block-update!::blockS bs::blockS queue::bbv-queue)
   (with-trace 'bbv-block
	 (format "block-update! #~a~a<-#~a parent.succs: ~a preds: ~a"
	    (block-label bs)
	    (if (block-live? bs) "+" "-")
	    (block-label (blockS-parent bs))
	    (map block-label (block-succs (blockS-parent bs)))
	    (map (lambda (b)
		    (format "#~a~a" (block-label b)
		       (if (block-live? b) "+" "-")))
	       (block-preds bs)))
      (with-access::blockS bs ((bv parent) first ctx)
	 (trace-item "ctx: " (shape ctx))
	 (set! first (ins-update! first bs ctx queue))
	 (when *bbv-debug*
	    (assert-block bs "block-specialize!"))
	 bs)))

;*---------------------------------------------------------------------*/
;*    block-merge-some! ...                                            */
;*---------------------------------------------------------------------*/
(define (block-merge-some! bv::blockV queue)
   (with-trace 'bbv-block "block-merge-some!"
      (let ((lvs (blockV-live-versions bv)))
	 (trace-item "blockV: " (blockV-label bv)
	    " versions: " (map (lambda (b)
				  (format "#~a~a" (block-label b)
				     (if (block-live? b) "+" "-")))
			     (blockV-versions bv)))
	 (multiple-value-bind (bs1 bs2 mctx)
	    (bbv-block-merge lvs)
	    ;; Find an already existing block for mctx, if such a block
	    ;; exists, get its representant (i.e., the block it might had
	    ;; already been merge into). If no such block exists, create
	    ;; a fresh new one
	    (let ((mbs (bbv-block bv mctx queue :creator (cons bs1 bs2))))
	       (trace-item "#" (block-label bs1) "+#" (block-label bs2)
		  " -> #" (blockS-label mbs)
		  (if (block-live? mbs) "+" "-")
		  " mctx: " (shape mctx))
	       (cond
		  ((eq? bs1 mbs)
		   (with-access::blockS mbs (merges)
		      (set! merges (cons (cons bs1 bs2) merges)))
		   (block-merge! bs2 mbs))
		  ((eq? bs2 mbs)
		   (with-access::blockS mbs (merges)
		      (set! merges (cons (cons bs1 bs2) merges)))
		   (block-merge! bs1 mbs))
		  (else
		   (block-merge! bs1 mbs)
		   (block-merge! bs2 mbs)))
	       (when *bbv-debug*
		  (assert-block mbs "block-merge-some!")))))))

;*---------------------------------------------------------------------*/
;*    block-merge! ...                                                 */
;*---------------------------------------------------------------------*/
(define (block-merge! bs::blockS mbs::blockS)
   (with-trace 'bbv-block "block-merge!"
      (trace-item "bs=#" (block-label bs) (if (block-live? bs) "+" "-")
	 " preds: "
	 (map (lambda (b)
		 (format "#~a~a" (block-label b)
		    (if (block-live? b) "+" "-")))
	    (block-preds bs)))
      (trace-item "mbs=#" (block-label mbs) (if (block-live? mbs) "+" "-")
	 " preds: "
	 (map (lambda (b)
		 (format "#~a~a" (block-label b)
		    (if (block-live? b) "+" "-")))
	    (block-preds mbs)))
      (when *bbv-debug* (assert-block bs "block-merge!"))
      (with-access::blockS bs (mblock succs parent label)
	 (trace-item "parent=#" (block-label parent))
	 (set! mblock mbs)
	 (let ((osuccs succs))
	    (replace-block! bs mbs)
	    (when *bbv-debug*
	       (assert-block bs "block-merge!.bs")
	       (assert-block mbs "block-merge!.mbs"))
	    (bbv-gc! *root-block*))
	 (with-access::blockV parent (versions)
	    (trace-item "<- #" label
	       " #{" (block-label parent)
	       "} versions: "
	       (map (lambda (b)
		       (with-access::blockS b (label)
			  (format "#~a~a" label
			     (if (block-live? b) "+" "-"))))
		  versions))))))

;*---------------------------------------------------------------------*/
;*    ins-specialize! ...                                              */
;*---------------------------------------------------------------------*/
(define (ins-specialize! first bs::blockS ctx::bbv-ctx queue::bbv-queue)
   (with-trace 'bbv-ins (format "ins-specialize! #~a<-#~a [@~a]"
			   (block-label bs)
			   (block-label (blockS-parent bs))
			   (gendebugid))
      (let loop ((oins first)
		 (nins '())
		 (ctx ctx))
	 (when (pair? oins)
	    (trace-item "ins-specialize.loop ins: " (shape (car oins)))
	    (trace-item "ins-specialize.loop ctx: " (shape ctx)))
	 (cond
	    ((null? oins)
	     (reverse! nins))
	    ((rtl_ins-specializer (car oins))
	     =>
	     (lambda (specialize)
		;; instruction specialization
		(multiple-value-bind (ins nctx)
		   (specialize (car oins) nins bs ctx queue)
		   (trace-item "nins: " (shape ins))
		   (with-access::rtl_ins ins (args)
;* 		      (set! args (map (lambda (i)                      */
;* 					 (if (isa? i rtl_ins)          */
;* 					     (let ((ni (car (loop (list i) '() ctx)))) */
;* 						(trace-item "ni=" (shape ni)) */
;* 						ni)                    */
;* 					     i))                       */
;* 				    args))                             */
		      (let ((lctx (bbv-ctx-extend-live-out-regs nctx (car oins))))
			 (trace-item "nctx: " (shape nctx))
			 (trace-item "lctx: " (shape lctx))
			 (cond
			    ((rtl_ins-go? ins)
			     (connect! bs ins)
			     (loop '() (cons ins nins) lctx))
			    ((rtl_ins-ifne? ins)
			     (connect! bs ins)
			     (loop (cdr oins) (cons ins nins) lctx))
			    ((rtl_ins-ifeq? ins)
			     (error "bbv" "should not be here, all ifeq should have been removed" (shape ins)))
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
		(let laap ((args args)
			   (nargs '())
			   (actx ctx))
		   (cond
		      ((null? args)
		       (let ((nctx (bbv-ctx-extend-live-out-regs actx (car oins))))
			  (trace-item "actx=" (shape actx))
			  (trace-item "nctx=" (shape nctx))
			  (loop (cdr oins)
			     (cons (duplicate-ins/args (car oins) (reverse! nargs) actx) nins)
			     nctx)))
		      ((isa? (car args) rtl_ins)
		       (let ((ni (car (loop (list (car args)) '() actx))))
			  (with-access::rtl_ins/bbv ni (ctx)
			     (laap (cdr args) (cons ni nargs) ctx))))
		      (else
		       (laap (cdr args) (cons (car args) nargs) actx))))))))))

;*---------------------------------------------------------------------*/
;*    ins-update! ...                                                  */
;*---------------------------------------------------------------------*/
(define (ins-update!::pair-nil first bs::blockS ctx::bbv-ctx queue::bbv-queue)
   (with-trace 'bbv-ins (format "ins-update! #~a<-#~a [@~a]"
			   (block-label bs)
			   (block-label (blockS-parent bs))
			   (gendebugid))
      (with-access::blockS bs (asleep succs)
	 (set! asleep #f)
;* 	 (for-each (lambda (s)                                         */
;* 		      (bbv-gc-disconnect! bs s))                       */
;* 	    succs)                                                     */
	 )
      (map! (lambda (oin)
	       (with-access::rtl_ins/bbv oin (ctx)
		  (cond
		     ((rtl_ins-go? oin)
		      (trace-item "ins-update.go...")
		      (with-access::rtl_ins oin (fun)
			 (with-access::rtl_go fun (to)
			    (let* ((n (bbv-block to ctx queue :creator bs))
				   (ins (duplicate::rtl_ins/bbv oin
					   (ctx ctx)
					   (fun (duplicate::rtl_go fun
						   (to n))))))
			       (connect! bs ins)
			       ins))))
		     ((rtl_ins-switch? oin)
		      (trace-item "ins-update.switch...")
		      (with-access::rtl_ins oin (fun)
			 (with-access::rtl_switch fun (labels)
			    (let ((ins (duplicate::rtl_ins/bbv oin
					  (ctx ctx)
					  (fun (duplicate::rtl_switch fun
						  (labels (map (lambda (b)
								  (bbv-block b ctx queue :creator bs))
							     labels)))))))
			       (connect! bs ins)
			       ins))))
		     ((rtl_ins-ifne? oin)
		      (trace-item "ins-update.ifne...")
		      (with-access::rtl_ins oin (fun args)
			 (with-access::rtl_ifne fun (then)
			    (let* ((n (bbv-block then ctx queue :creator bs))
				   (ins (duplicate::rtl_ins/bbv oin
					   (ctx ctx)
					   (fun (duplicate::rtl_ifne fun
						   (then n))))))
			       (connect! bs ins)
			       ins))))
		     (else
		      oin))))
	 first)
      first))

;*---------------------------------------------------------------------*/
;*    connect! ...                                                     */
;*---------------------------------------------------------------------*/
(define (connect! bs::blockS ins::rtl_ins)
   
   (define (debug-connect msg bs n)
      (when *bbv-debug*
	 (with-access::blockS n (preds succs)
	    (trace-item msg " #" 
	       (block-label bs) " to #" (block-label n)
	       " preds: " (map (lambda (s)
				  (format "#~a" (block-label s)))
			     preds)
	       " succs: " (map (lambda (b)
				 (if (isa? b block)
				     (format "#~a" (block-label b))
				     '-))
			    succs)))))

   (with-trace 'bbv-ins "connect!"
      (trace-item "#" (block-label bs) " ins=" (shape ins))
      (cond
	 ((rtl_ins-ifne? ins)
	  (with-access::rtl_ins ins (fun)
	     (let ((n (rtl_ifne-then fun)))
		(debug-connect "connect.ifne" bs n)
		(bbv-gc-connect! bs n)
		(block-succs-set! bs (list #unspecified n))
		(block-preds-update! n (cons bs (block-preds n))))))
	 ((rtl_ins-go? ins)
	  (with-access::rtl_ins ins (fun)
	     (let ((n (rtl_go-to fun)))
		(debug-connect "connect.go" bs n)
		(bbv-gc-connect! bs n)
		(if (pair? (block-succs bs))
		    (set-car! (block-succs bs) n)
		    (block-succs-set! bs (list n)))
		(block-preds-update! n (cons bs (block-preds n))))))
	 ((rtl_ins-switch? ins)
	  (with-access::rtl_ins ins (fun)
	     (for-each (lambda (n)
			  (bbv-gc-connect! bs n))
		(rtl_switch-labels fun))
	     (block-succs-set! bs (rtl_switch-labels fun))
	     (for-each (lambda (n)
			  (block-preds-update! n (cons bs (block-preds n))))
		(rtl_switch-labels fun)))))))

;*---------------------------------------------------------------------*/
;*    block-need-merge? ...                                            */
;*---------------------------------------------------------------------*/
(define (block-need-merge?::bool bv::blockV)
   (with-access::blockV bv (merge)
      (>fx (length (blockV-live-versions bv))
	 (if merge *max-block-merge-versions* *max-block-limit*))))

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
   (with-trace 'bbv-block "bbv-block"
      (trace-item (format "block: #~a" (block-label b))
	 (if (isa? b blockS) (if (block-live? b) "+" "-") "")
	 " preds: " (map (lambda (b)
			    (format "#~a~a" (block-label b)
			       (if (isa? b blockS)
				   (if (block-live? b) "+" "-")
				   "")))
		       (block-preds b))
	 " succs: " (map (lambda (b)
			    (format "#~a~a" (block-label b)
			       (if (isa? b blockS)
				   (if (block-live? b) "+" "-")
				   "")))
		       (block-succs b)))
      (trace-item "ctx: " (shape ctx))
      (let ((bv (if (isa? b blockV) b (with-access::blockS b (parent) parent))))
	 (with-access::blockV bv (first label versions)
	    (trace-item "parent: #" label)
	    (trace-item "versions: " (filter-map (lambda (b)
						   (with-access::blockS b (label)
						      (format "#~a~a" label
							 (if (block-live? b) "+" "-"))))
				       versions))
	    (let ((ctx (bbv-ctx-filter-live-in-regs ctx (car first))))
	       (cond
		  ((bbv-ctx-assoc ctx versions)
		   =>
		   (lambda (b)
		      (let ((obs (live-blockS b)))
			 (with-access::blockS obs (label preds)
;* 			    (when (eq? *bbv-blocks-gc* 'ssr)           */
;* 			       (set! preds (filter block-live? preds)) */
;* 			       (set! cnt (length preds)))              */
			    (trace-item "<- old: #" label
			       (if (block-live? obs) "+" "-")
			       " preds: "
			       (map (lambda (b)
				       (format "#~a~a" (block-label b)
					  (if (block-live? b) "+" "-")))
				  (blockS-preds obs)))
			    (when *bbv-debug*
			       (assert-block obs "bbv-block"))
			    (unless (block-live? obs)
			       ;; this block was unreachable we have to
			       ;; update it
			       (unless (bbv-queue-has? queue obs)
				  (bbv-queue-push! queue obs)))
			    obs))))
		  (else
		   (let ((nbs (new-blockS bv ctx :creator creator)))
		      (with-access::blockS nbs (label)
			 (trace-item "<- new: #" label)
			 (bbv-queue-push! queue nbs)
			 nbs)))))))))
   
;*---------------------------------------------------------------------*/
;*    new-blockS ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-blockS::blockS bv::blockV ctx::bbv-ctx #!key (gccnt 0) creator)
   (with-trace 'bbv-block "new-blockS"
      (let* ((lbl (genlabel))
	     (bs (instantiate::blockS
		    (ctx ctx)
		    (parent bv)
		    (label lbl)
		    (gccnt gccnt)
		    (first '())
		    (creator creator))))
	 (bbv-gc-add-block! bs)
	 (trace-item "lbl=#" lbl)
	 (trace-item "ctx: " (shape ctx))
	 (with-access::blockV bv (versions label)
	    (trace-item "parent=#" label)
	    (trace-item "versions=" (filter-map (lambda (b)
						   (with-access::blockS b (label)
						      (format "#~a~a" label
							 (if (block-live? b) "+" "-"))))
				       versions))
	    (set! versions (cons bs versions))
	    bs))))

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
	       (let ((vals (rtl_ins-args* i)))
		  (when (and (pair? vals) (null? (cdr vals)) (rtl_reg? (car vals)))
		     (with-access::rtl_ins (car args) (args)
			(or (rtl_reg? (car args))
			    (with-access::rtl_ins (car args) (fun)
			       (when (isa? fun rtl_call)
				  (with-access::rtl_call fun (var)
				     (with-access::variable var (value type)
					(eq? var *long->bint*))))))))))))))

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

   (define (specialize+ reg type polarity value e)
      (let ((ctx+ (extend-ctx ctx reg (list type) #t
		     :value (min-value value (bbv-ctxentry-value e)))))
	 (trace-item "ctx+: " (shape ctx+))
	 (with-access::rtl_ins/bbv i (fun loc args)
	    (with-access::rtl_ifne fun (then)
	       (let* ((assert (when (>=fx *bbv-assert* 3)
                                 (rtl-assert-expr-type
                                    reg type polarity ctx loc
                                    "BBV-ASSERT-FAILURE:TYPECHECK+")))
		      (s (if assert
			     (duplicate::rtl_ins/bbv i
				(ctx ctx)
				(fun (duplicate::rtl_ifne fun
					(then (bbv-block then ctx+ queue
						 :creator bs))))
				(dest #f)
				(args (list (duplicate::rtl_ins/bbv i
					       (ctx ctx)
					       (fun assert)
					       (args args)))))
			     (if (and #f (equal? (getenv "BIGLOOBBVGOODORBAD") "good"))
				 (duplicate::rtl_ins/bbv i
				    (ctx ctx)
				    (fun (duplicate::rtl_ifne fun
					    (then (bbv-block then ctx+ queue
						     :creator bs))))
				    (dest #f)
				    (args (list (duplicate::rtl_ins/bbv i
						   (ctx ctx)
						   (args '())
						   (fun (instantiate::rtl_loadi
							   (constant (instantiate::literal
									(type *bool*)
									(loc loc)
									(value #t)))))))))
				 (duplicate::rtl_ins/bbv i
				    (ctx ctx)
				    (fun (instantiate::rtl_go
					    (to (bbv-block then ctx+ queue :creator bs))))
				    (dest #f)
				    (args '()))))))
		  ;; the next ctx will be ignored...
		  (values s (instantiate::bbv-ctx)))))))

   (define (specialize- reg type polarity value e)
      (with-access::rtl_ins/bbv i (fun loc)
	 (let* ((assert (when (>=fx *bbv-assert* 3)
			   (rtl-assert-reg-type reg type polarity ctx loc
			      "BBV-ASSERT-FAILURE:TYPECHECK-")))
		(s (duplicate::rtl_ins/bbv i
		      (ctx ctx)
		      (fun (or assert (instantiate::rtl_nop)))
		      (dest #f)
		      (args '()))))
	    (values s ctx))))

   (with-trace 'bbv-ins "rtl_ins-specialize-typecheck"
      (multiple-value-bind (reg type polarity value)
	 (rtl_ins-typecheck i)
	 (let ((e (bbv-ctx-get ctx reg)))
	    (with-access::bbv-ctxentry e ((epolarity polarity))
	       (with-access::rtl_ins i (fun)
		  (trace-item "ins: " (shape i))
		  (trace-item "type: " (if (not polarity) "!" "") (shape type))
		  (trace-item "value: " (shape value))
		  (trace-item "e: " (shape e))
		  (trace-item "ctx: " (shape ctx))
		  #;(tprint "TCHECK " (shape i) " type: " (shape type)
		     " env: " (shape (bbv-ctxentry-types e))
		     " every: " (every (lambda (t) (<=ty t type))
				  (bbv-ctxentry-types e))
		     " nany: " (not (any (lambda (t) (<=ty t type))
				      (bbv-ctxentry-types e))))
		  (cond
		     ((and polarity epolarity
			   (every (lambda (t) (<=ty t type))
			      (bbv-ctxentry-types e)))
		      (trace-item "TCHECK++ type: " (shape type)
			 " " (shape (bbv-ctxentry-types e)))
		      ;; positive type simplification
		      (specialize+ reg type epolarity value e))
		     ((and polarity (not epolarity)
			   (any (lambda (t) (<=ty t type))
			      (bbv-ctxentry-types e)))
		      (trace-item "TCHECK+- type: " (shape type)
			 " " (shape (bbv-ctxentry-types e)))
		      ;; positive type simplification
		      (specialize- reg type epolarity value e))
		     ((and polarity epolarity
			   (pair? (bbv-ctxentry-types e))
			   (not (any (lambda (t) (<=ty type t))
				   (bbv-ctxentry-types e)))
			   (not (memq 'number (bbv-ctxentry-types e)))
			   (not (memq *pair-nil* (bbv-ctxentry-types e)))
			   (or (not (eq? type 'fast-flonum))
			       (not (or (memq *breal* (bbv-ctxentry-types e))
					(memq 'number (bbv-ctxentry-types e)))))
			   (not (eq? type 'number)))
		      ;; negative type simplification
		      (trace-item "TCHECK-- type: " (shape type)
			 " e: " (shape (bbv-ctxentry-types e))
			 " <=? *obj*: " (<=ty type *obj*))
		      (specialize- reg type #f value e))
		     ((isa? fun rtl_ifne)
		      (with-access::bbv-ctxentry e (aliases)
			 (let ((regs (if (memq reg aliases)
					 aliases
					 (cons reg aliases))))
			    (with-access::rtl_ifne fun (then)
			       (let* ((ctx+ (extend-ctx* ctx regs (list type) #t
					       :value (min-value value (bbv-ctxentry-value e))))
				      (ctx- (if (bbv-ctxentry-polarity e)
						(extend-ctx* ctx regs
						   (list type) #f)
						(extend-ctx* ctx regs
						   (list type) #f)))
				      (s (duplicate::rtl_ins/bbv i
					    (ctx ctx)
					    (fun (duplicate::rtl_ifne fun
						    (then (bbv-block then ctx+ queue
							     :creator bs)))))))
				  (trace-item "ctx+.2: " (shape ctx+))
				  (trace-item "ctx-.2: " (shape ctx-))
				  (values s ctx-))))))
		     (else
		      (error "rtl_ins-specialize-typecheck"
			 "should not be here"
			 (shape i))))))))))

;*---------------------------------------------------------------------*/
;*    range->loadi ...                                                 */
;*---------------------------------------------------------------------*/
(define (range->loadi i::rtl_ins dest value type)
   (with-trace 'bbv-ins "range->loadi"
      (trace-item "i=" (shape i))
      (trace-item "types=" (shape type))
      (with-access::bbv-range value (up)
	 (let* ((atom (instantiate::literal
			 (type *long*)
			 (value up)))
		(loadi (instantiate::rtl_loadi
			  (constant atom))))
	    (if (eq? *long* type)
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
			   (var *long->bint*)))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-mov ...                                       */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-mov i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   (with-trace 'bbv-ins "rtl_ins-specialize-mov"
      (trace-item "ins: " (shape i))
      (trace-item "ctx: " (shape ctx))
      (with-access::rtl_ins i (dest args fun)
	 (let ((ctx (unalias-ctx ctx dest)))
	    (trace-item "unalias ctx: " (shape ctx))
	    (cond
	       ((and (pair? args) (null? (cdr args)) (rtl_reg/ra? (car args)))
		(trace-item "rtl_ins-specialize-mov.1")
		(trace-item "dest: " (shape dest))
		(trace-item "arg: " (shape (car args)))
		(let ((e (bbv-ctx-get ctx (car args))))
		   (with-access::bbv-ctxentry e (types value polarity count)
		      (if (bbv-singleton? value)
			  (values (range->loadi i dest value (rtl_reg-type dest))
			     (extend-ctx ctx dest types polarity :count count :value value))
			  (with-access::rtl_reg dest (var)
			     (trace-item "dest.v: " (shape var))
			     (values (duplicate-ins i ctx)
				(alias-ctx
				   (extend-ctx ctx dest types polarity :count count :value value)
				   dest (car args))))))))
	       ((and *type-call* (pair? args) (rtl_ins-call? (car args)))
		(trace-item "rtl_ins-specialize-mov.2")
		(with-access::rtl_ins (car args) (fun (fargs args))
		   (with-access::rtl_call fun (var)
		      (with-access::variable var (value type)
			 (if (and (eq? var *long->bint*)
				  (isa? (car fargs) rtl_ins)
				  (rtl_ins-loadi? (car fargs)))
			     (with-access::rtl_ins (car fargs) (fun)
				(with-access::rtl_loadi fun (constant)
				   (with-access::atom constant (value type)
				      (values (duplicate-ins i ctx)
					 (extend-ctx ctx dest (list *bint*) #t
					    :count 0
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
						   :count 0
						   :value (bbv-ctxentry-value e))
						(extend-ctx ctx dest
						   (list type) #t
						   :count 0))))))))))))
	       ((and *type-loadi* (pair? args) (rtl_ins-loadi? (car args)))
		(trace-item "rtl_ins-specialize-mov.3")
		(with-access::rtl_ins (car args) (fun)
		   (with-access::rtl_loadi fun (constant)
		      (with-access::atom constant (value type)
			 (values (duplicate-ins i ctx)
			    (extend-ctx ctx dest (list type) #t
			       :value (if (fixnum? value) (fixnum->range value) '_)
			       :count 0))))))
	       ((and *type-loadg* (pair? args) (rtl_ins-loadg? (car args)))
		(trace-item "rtl_ins-specialize-mov.4")
		(with-access::rtl_ins (car args) (fun)
		   (with-access::rtl_loadg fun (var)
		      (with-access::variable var (value type)
			 (values (duplicate-ins i ctx)
			    (extend-ctx ctx dest (list type) #t :count 0))))))
	       (else
		(trace-item "rtl_ins-specialize-mov.5")
		(with-access::rtl_reg dest (type)
		   (values (duplicate-ins i ctx)
		      (extend-ctx ctx dest
			 (list (if (eq? type *obj*) (rtl_ins-type i) type)) #t
			 :count 0)))))))))

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
	       (let* ((s (duplicate::rtl_ins/bbv i
			   (ctx ctx)
			   (fun (duplicate::rtl_loadg fun))))
		      (ro (or (global-read-only? var)
			      (and (=fx (global-occurrencew var) 1)
				   (not (eq? type *obj*)))))
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
	    (with-access::variable var (value type)
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
				;; as the call might be a fixnum op or a vlength
				((rtl_ins-specializer (car args))
				 (car args) ins bs ctx queue)
				(with-access::rtl_ins/bbv i (dest)
				   (let ((e (bbv-ctx-get cctx dest)))
				      (trace-item "e: " (shape e))
				      (trace-item "dest: " (shape dest))
				      (trace-item "cctx: " (shape cctx))
				      (if (bbv-ctxentry? e)
					  (bbv-ctxentry-value e)
					  (fixnum-range)))))))
			 (else
			  (trace-item "new-value-call.long->bint.default")
			  (trace-item "arg0: " (shape (car args)))
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
      (trace-item "ins: " (shape i))
      (trace-item "ctx: " (shape ctx))
      (with-access::rtl_ins i (dest fun)
	 (trace-item "dest: " (shape dest))
	 (let ((ctx (unalias-ctx ctx dest)))
	    (trace-item "unalias ctx: " (shape ctx))
	    (with-access::rtl_call fun (var)
	       (with-access::global var (value type)
		  (let* ((s (duplicate::rtl_ins/bbv i
			       (ctx ctx)
			       (fun (duplicate::rtl_call fun))))
			 (nv (new-value i))
			 (nctx (extend-ctx ctx dest (list (if (fun? value) type *obj*)) #t
				  :count 0
				  :value nv)))
		     (trace-item "nv: " (shape nv))
		     (trace-item "nctx: " (shape nctx))
		     (values s nctx))))))))

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
   (with-access::rtl_ins i (args dest fun)
      (let ((e (bbv-ctx-get ctx (car args))))
	 (with-access::bbv-ctxentry e (types value)
	    (if (bbv-singleton? value)
		(with-access::rtl_return fun (type)
		   (let ((loadi (duplicate::rtl_ins/bbv i
				   (args (list (range->loadi i
						  (car args) value type)))
				   (fun (instantiate::rtl_mov))
				   (dest (car args)))))
		      (values (duplicate::rtl_ins/bbv i
				 (args (cons loadi (cdr args))))
			 ctx)))
		(values i ctx))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-vlen ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-vlen i::rtl_ins ins::pair-nil bs ctx queue::bbv-queue)
   (with-trace 'bbv-ins "rtl_ins-specialize-vlen"
      (with-access::rtl_ins/bbv i (dest args)
	 (let* ((range (rtl-range i ctx))
		(nctx (extend-ctx ctx dest (list *long*) #t :value range)))
	    (trace-item "range: " range)
	    (trace-item "nctx: " (shape nctx))
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
	 (trace-item "ins: " (shape i))
	 (trace-item "ctx: " (shape ctx))
	 (with-access::rtl_ins (car args) (fun args)
	    (with-access::rtl_call fun (var)
	       (let* ((vec (car args))
		      (len (cadr args))
		      (a (bbv-ctx-get ctx vec))
		      (l (bbv-ctx-get ctx len))
		      (va (bbv-ctxentry-value a))
		      (vl (bbv-ctxentry-value l)))
		  (trace-item "a: " (shape a))
		  (trace-item "l: " (shape l))
		  (cond
		     ((not *bbv-optim-bound*)
		      (trace-item "noopt")
		      (let ((ctx+ (extend-ctx ctx vec (list *bint*) #t
				     :value (bbv-range-lt vl (vlen-range))))
			    (ctx- ctx))
			 (trace-item "ctx+: " (shape ctx+))
			 (dup-ifne i ctx+ ctx-)))
		     ((not (bbv-range? va))
		      (trace-item "not bbv-range? va")
		      (if (bbv-range? vl)
			  (let ((ctx+ (extend-ctx ctx vec (list *vector*) #t
					 :value vl))
				(ctx- ctx))
			     (trace-item "ctx+.1: " (shape ctx+))
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
;* 			 (trace-item "ctx-: " (shape ctx-))             */
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
			 (trace-item "ctx+.2: " (shape ctx+))
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

   (define (reg a)
      (if (rtl_reg? a)
	  a
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (with-access::rtl_call fun (var)
			(if (or (eq? var *bint->long*)
				(eq? var *int->long*))
			    (when (rtl_reg? (car args)) (car args))
			    (when (rtl_reg? dest) dest))))))))

   (define (reg? a)
      (reg a))
   
   (define (fxcmp-op i)
      (with-access::rtl_ins i (fun)
	 (with-access::rtl_call fun (var)
	    (cond
	       ((eq? var *<fx*) '<)
	       ((eq? var *<=fx*) '<=)
	       ((eq? var *>fx*) '>)
	       ((eq? var *>=fx*) '>=)
	       ((eq? var *=fx*) '=)))))
   
   (define (commute-op op)
      (case op
	 ((<) '>)
	 ((<=) '>=)
	 ((>) '<)
	 ((>=) '<=)
	 ((=) '=)
	 (else op)))
   
   (define (resolve/op i op intl::bbv-range intr::bbv-range)
      (case op
	 ((<) (bbv-range< intl intr))
	 ((<=) (bbv-range<= intl intr))
	 ((>) (bbv-range> intl intr))
	 ((>=) (bbv-range>= intl intr))
	 ((=) (bbv-range= intl intr))
	 (else #unspecified)))

   (define (narrowing/op reg::rtl_reg intr::bbv-range inte::bbv-range op+ op- ctx::bbv-ctx)
      (let* ((inte+ (op+ intr inte))
	     (inte- (op- intr inte))
	     (ctx+ (extend-ctx ctx reg (list *bint*) #t :value inte+))
	     (ctx- (extend-ctx ctx reg (list *bint*) #t :value inte-)))
	 (values ctx+ ctx-)))
   
   (define (narrowing reg::rtl_reg intr::bbv-range inte::bbv-range op ctx::bbv-ctx)
      (with-trace 'bbv-ins (format "rtl_ins-specialize-fxcmp/narrowing [@~a]"
			      (gendebugid))
	 (trace-item "op: " op)
	 (trace-item "reg: " (shape reg))
	 (trace-item "ctx: " (shape ctx))
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
		   (trace-item "lctx+: " (shape lctx+))
		   (trace-item "lctx-: " (shape lctx-))
		   (multiple-value-bind (rctx+ rctx-)
		      (narrowing rreg intr intl (commute-op op) ctx)
		      (trace-item "rctx+: " (shape rctx+))
		      (trace-item "rctx-: " (shape rctx-))
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
	     (narrowing (reg rhs) intr intl (commute-op op) ctx))
	    (else
	     ;; no register involed
	     (values ctx ctx)))))
   
   (define (specialize-fxcmp/call i::rtl_ins ctx::bbv-ctx)
      (with-trace 'bbv-ins
	    (format "rtl_ins-specialize-fxcmp/call ~a [@~a]"
	       (shape i) (gendebugid))
	 (trace-item "ctx: " (shape ctx))
	 (with-access::rtl_ins i (fun args)
	    (with-access::rtl_call fun (var)
	       (let* ((lhs (car args))
		      (rhs (cadr args))
		      (intl (rtl-range lhs ctx))
		      (intr (rtl-range rhs ctx))
		      (op (fxcmp-op i)))
		  (trace-item "op: " op)
		  (trace-item "lhs: " (shape lhs))
		  (trace-item "rhs: " (shape rhs))
		  (trace-item "intl: " (shape intl))
		  (trace-item "intr: " (shape intr))
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
			  (trace-item "resolve/op.unknown")
			  (multiple-value-bind (ctx+ ctx-)
			     (specialize/op op lhs rhs intl intr ctx)
			     (trace-item "resolve/op.unspec+: " (shape ctx+))
			     (trace-item "resolve/op.unspec-: " (shape ctx-))
			     (values (duplicate::rtl_ins/bbv i)
				ctx+ ctx-))))))))))

   (define (specialize+ i ins ctx+)
      (with-access::rtl_ins i (dest fun args loc)
	 (with-access::rtl_ifne fun (then)
	    (let* ((assert (when (>=fx *bbv-assert* 3)
			      (let ((ins (car args)))
				 (with-access::rtl_ins ins (args)
				    (rtl-assert-fxcmp
				       (fxcmp-op ins) (car args) (cadr args)
				       #t ctx+ loc
				       "BBV-ASSERT-FAILURE:FXCMP+")))))
		   (ni (if assert
			   (duplicate::rtl_ins/bbv i
			      (ctx ctx)
			      (fun (duplicate::rtl_ifne fun
				      (then (bbv-block then ctx+ queue
					       :creator bs))))
			      (args (let ((i (car args)))
				       (with-access::rtl_ins i (args)
					  (list (duplicate::rtl_ins/bbv i
						   (ctx ctx)
						   (fun assert)))))))
			   (duplicate::rtl_ins/bbv i
			      (ctx ctx)
			      (fun (duplicate::rtl_go fun
				      (to (bbv-block then ctx+ queue
					     :creator bs))))
			      (args '())))))
	       (values ni ctx+)))))

   (define (specialize- i ins ctx-)
      (with-access::rtl_ins i (dest fun args loc)
	 (let* ((assert (when (>=fx *bbv-assert* 3)
			   (let ((ins (car args)))
			      (with-access::rtl_ins ins (args)
				 (rtl-assert-fxcmp
				    (fxcmp-op ins) (car args) (cadr args)
				    #f ctx- loc
				    "BBV-ASSERT-FAILURE:FXCMP-")))))
		(ni (duplicate::rtl_ins/bbv i
		       (ctx ctx)
		       (args '())
		       (fun (or assert (duplicate::rtl_nop fun))))))
	    (values ni ctx-))))

   (with-trace 'bbv-ins (format "rtl_ins-specialize-fxcmp [@~a]" (gendebugid))
      (trace-item "ins: " (shape i))
      (with-access::rtl_ins i (dest fun args loc)
	 (cond
	    ((isa? fun rtl_ifne)
	     (with-access::rtl_ifne fun (then)
		(multiple-value-bind (ins ctx+ ctx-)
		   (specialize-fxcmp/call (car args) ctx)
		   (trace-item "ctx+: " (shape ctx+))
		   (trace-item "ctx-: " (shape ctx-))
		   (cond
		      ((rtl_ins-true? ins)
		       (specialize+ i ins ctx+))
		      ((rtl_ins-false? ins)
		       (specialize- i ins ctx-))
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
		(specialize-fxcmp/call i ctx)
		(cond
		   ((rtl_ins-true? ins) (values ins ctx+))
		   ((rtl_ins-false? ins) (values ins ctx-))
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
	       (trace-item "intv=" (shape intv))
	       (with-access::bbv-range intv (lo up)
		  (let* ((intx (instantiate::bbv-range
				  (lo (maxrv lo (bbv-min-fixnum) (bbv-min-fixnum)))
				  (up (minrv up (bbv-max-fixnum) (bbv-max-fixnum)))))
			 (nctx (extend-ctx ctx reg (list *bint*) #t
				  :value intx)))
		     (trace-item "intx=" (shape intx))
		     (trace-item "nctx: " (shape nctx))
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
	    (trace-item "ctx+: " (shape ctx+))
	    (trace-item "nctx: " (shape nctx))
	    (values s nctx))))
   
   (with-trace 'bbv-ins "rtl_ins-specialize-fxovop"
      (trace-item "i: " (shape i))
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
				   (trace-item "nctx.2: " (shape nctx))
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
				   (trace-item "nctx.3: " (shape nctx))
				   (values (fx-ov->fx var call reg ctx nctx)
				      nctx))
				(default i call ifne reg (fixnum-range)))))
			((eq? var *$*fx/ov*)
			 (let ((range (bbv-range-mul intl intr)))
			    (if (bbv-range-fixnum? range)
				(let ((nctx (extend-ctx ctx reg
					       (list *bint*) #t
					       :value range)))
				   (trace-item "nctx.4: " (shape nctx))
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
      (trace-item "i: " (shape i))
      (trace-item "ctx: " (shape ctx))
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (intl (rtl-range lhs ctx))
		   (intr (rtl-range rhs ctx)))
	       (cond
		  ((not (and (bbv-range? intl) (bbv-range? intr)))
		   (trace-item "not.range intl: " (shape intl) " intr: " (shape intr))
		   (values (duplicate::rtl_ins/bbv i
			      (ctx ctx))
		      ctx))
		  ((or (eq? var *-fx*) (eq? var *subfx*))
		   (let ((range (bbv-range-sub intl intr)))
		      (trace-item "-fx.range: " (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *long*) #t
					 :value range)))
			     (trace-item "nctx: " (shape nctx))
			     (values (duplicate::rtl_ins/bbv i
					(ctx ctx))
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((eq? var *-fx-safe*)
		   (let ((range (bbv-range-sub intl intr)))
		      (trace-item "-fx-safe.range: " (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *bint*) #t
					 :value range)))
			     (trace-item "nctx: " (shape nctx))
			     (values (fx-safe->fx var i ctx nctx)
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((or (eq? var *+fx*) (eq? var *addfx*))
		   (let ((range (bbv-range-add intl intr)))
		      (trace-item "+fx.range: " (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *long*) #t
					 :value range)))
			     (trace-item "nctx: " (shape nctx))
			     (values (duplicate::rtl_ins/bbv i
					(ctx ctx))
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((eq? var *+fx-safe*)
		   (let ((range (bbv-range-add intl intr)))
		      (trace-item "+fx-safe.range: " (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *bint*) #t
					 :value range)))
			     (trace-item "nctx: " (shape nctx))
			     (values (fx-safe->fx var i ctx nctx)
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((eq? var *2-*)
		   (let ((range (bbv-range-sub intl intr)))
		      (trace-item "2-.range: " (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *bint*) #t
					 :value range)))
			     (trace-item "nctx: " (shape nctx))
			     (values (2->fx var i ctx nctx)
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  ((eq? var *2+*)
		   (let ((range (bbv-range-add intl intr)))
		      (trace-item "2+.range: " (shape range))
		      (if (bbv-range-fixnum? range)
			  (let ((nctx (extend-ctx ctx dest (list *bint*) #t
					 :value range)))
			     (trace-item "nctx: " (shape nctx))
			     (values (2->fx var i ctx nctx)
				nctx))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx))
			     ctx))))
		  (else
		   (trace-item "else ctx: " (shape ctx))
		   (values (duplicate::rtl_ins/bbv i
			      (ctx ctx))
		      ctx))))))))
   
