;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/bbv.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 10:05:41 2017                          */
;*    Last change :  Fri Jul 28 09:40:11 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Basic Blocks versioning experiment.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch")
   
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
	    saw_bbv-cache
	    saw_bbv-utils)

   (export  (bbv::pair-nil ::backend ::global ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    *cleanup* ...                                                    */
;*---------------------------------------------------------------------*/
(define *cleanup* #t)

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
;*    bbv ...                                                          */
;*---------------------------------------------------------------------*/
(define (bbv back global params blocks)
   (if *saw-bbv?*
       (with-trace 'bbv (global-id global)
	  (start-bbv-cache!)
	  (verbose 2 "        bbv " (global-id global))
	  (when (>=fx (bigloo-debug) 1)
	     (dump-blocks global params blocks ".plain.bb"))
	  (set-max-label! blocks)
	  (let ((blocks (normalize-goto! (remove-temps! (car blocks)))))
	     (when (>=fx (bigloo-debug) 1)
		(dump-blocks global params blocks ".norm.bb"))
	     (let ((regs (liveness! back blocks params)))
		(tprint ">>> " (global-id global))
		(unwind-protect
		   (if (null? blocks)
		       '()
		       (let* ((s (get-specialize-block (car blocks)
				    (params->ctx params)))
			      (b (block->block-list regs
				    (if *cleanup*
					(remove-nop!
					   (simplify-branch!
					      (merge! (get-bb-mark)
						 s)))
					s))))
			  (tprint "<<< " (global-id global))
			  (verbose 3 " " (length blocks) " -> " (length b))
			  (verbose 2 "\n")
			  (when (>=fx (bigloo-debug) 1)
			     (dump-blocks global params
				(block->block-list regs s) ".spec.bb")
			     (dump-blocks global params
				b ".bb"))
			  (map! (lambda (b) (shrink! b)) b)
			  b))
		   (begin
		      (for-each (lambda (r) (shrink! r)) regs))))))
       blocks))

;*---------------------------------------------------------------------*/
;*    dump-blocks ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-blocks global params blocks suffix)
   
   (define (dump-blocks port)
      (let* ((id (global-id global)))
	 (fprint port ";; -*- mode: bee -*-")
	 (fprint port ";; *** " id ":")
	 (fprint port ";; " (map shape params))
	 (for-each (lambda (b)
		      (dump b port 0)
		      (newline port))
	    blocks)
	 id))
   
   (let ((oname (if (string? *dest*)
		    *dest*
		    (if (and (pair? *src-files*) (string? (car *src-files*)))
			(prefix (car *src-files*))
			#f)))
	 (id (global-id global)))
      (if oname
	  (call-with-output-file (format "~a-~a~a" oname id suffix) dump-blocks)
	  (dump-blocks (current-error-port)))))

;*---------------------------------------------------------------------*/
;*    widen-bbv! ...                                                   */
;*    -------------------------------------------------------------    */
;*    Widen the blocks and instructions for preparing the register     */
;*    allocation.                                                      */
;*---------------------------------------------------------------------*/
(define (widen-bbv! o regs::pair-nil)
   
   (define (get-args o)
      (filter rtl_reg/ra? (rtl_ins-args* o)))
   
   (define (args-widen-bbv! o)
      (when (rtl_ins? o)
	 (with-access::rtl_ins o (args fun)
	    (widen!::rtl_ins/bbv o
	       (def (make-empty-regset regs))
	       (in (list->regset (get-args o) regs))
	       (out (make-empty-regset regs)))
	    (for-each args-widen-bbv! args))))
   
   (define (ins-widen-bbv! o)
      (with-access::rtl_ins o (dest args fun)
	 (widen!::rtl_ins/bbv o
	    (def (if (or (not dest) (rtl_reg-onexpr? dest))
		     (make-empty-regset regs)
		     (list->regset (list dest) regs)))
	    (in (list->regset (get-args o) regs))
	    (out (make-empty-regset regs)))
	 (for-each args-widen-bbv! args)))
   
   (define (block-widen-bbv! o)
      (widen!::blockV o)
      (with-access::block o (first)
	 (for-each ins-widen-bbv! first)))

   (for-each block-widen-bbv! o))

;*---------------------------------------------------------------------*/
;*    liveness! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Computes the liveness of a list of blocks. Returns the           */
;*    list of used registers.                                          */
;*---------------------------------------------------------------------*/
(define (liveness! back blocks params)
   
   (define (rtl_ins/bbv-in i)
      (with-access::rtl_ins/bbv i (in)
	 in))
   
   (define (liveness-block! block)
      (with-access::block block (succs first)
	 (let loop ((inss (reverse first))
		    (succ (map (lambda (b) (car (block-first b))) succs))
		    (t #f))
	    (if (pair? inss)
		(with-access::rtl_ins/bbv (car inss) (out fun in def)
		   (let ((u (cond
			       ((rtl_ins? succ)
				(regset-union! out (rtl_ins/bbv-in succ)))
			       ((pair? succ)
				(regset-union*! out (map rtl_ins/bbv-in succ)))
			       (else
				#f))))
		      (regset-for-each
			 (lambda (r)
			    (when (not (regset-member? r def))
			       (set! u (or (regset-add! in r) u))))
			 out)
		      (loop (cdr inss) (car inss) (or t u))))
		t))))
   
   
   (let* ((cregs (collect-registers! blocks))
	  (hregs (append-map! collect-register! (backend-registers back)))
	  (pregs (filter rtl_reg/ra? params))
	  (regs (append hregs cregs)))
      ;; pre the instructions
      (widen-bbv! blocks regs)
      ;; add the argument of the function to the IN set of the
      ;; first instruction of the first block
      (when (pair? blocks)
	 (let ((inss (block-first (car blocks))))
	    (when (pair? inss)
	       (let ((ins (car inss)))
		  (with-access::rtl_ins/bbv ins (in)
		     (for-each (lambda (a) (regset-add! in a)) pregs))))))
      ;; fix-point iteration
      (let loop ((i 0))
	 (let liip ((bs blocks)
		    (t #f))
	    (if (null? bs)
		(if t
		    (loop (+fx i 1))
		    regs)
		(liip (cdr bs) (or (liveness-block! (car bs)) t)))))))

;*---------------------------------------------------------------------*/
;*    merge! ...                                                       */
;*    -------------------------------------------------------------    */
;*    Merge equivalent basic blocks subgraphs                          */
;*---------------------------------------------------------------------*/
(define (merge! mark b::blockS)
   (with-access::blockS b (%parent succs)
      (with-access::blockV %parent (versions %mark)
	 (unless (=fx mark %mark)
	    (set! %mark mark)
	    (with-trace 'bbv "merge"
	       (trace-item "b=" (block-label b))
	       (if (or (null? versions) (null? (cdr versions)))
		   (for-each (lambda (s) (merge! mark s)) succs)
		   (let ((ks (sort (lambda (v1 v2)
				      (<=fx (car v1) (car v2)))
				(map (lambda (v)
					(cons (bbv-hash (cdr v)) (cdr v)))
				   versions))))
		      (trace-item "merge "
			 (map (lambda (k)
				 (cons (block-label (cdr k)) (car k)))
			    ks))
		      (let loop ((ks ks))
			 (cond
			    ((null? (cdr ks))
			     (for-each (lambda (s) (merge! mark s)) succs))
			    ((>=fx (blockS-%mark (cdar ks)) mark)
			     (loop (cdr ks)))
			    (else
			     (let ((k (car ks)))
				(let liip ((ls (cdr ks)))
				   (cond
				      ((null? ls)
				       (loop (cdr ks)))
				      ((and (=fx (car k) (caar ls))
					    (not (eq? (cdr k) (cdar ls)))
					    (merge? (cdr k) (cdar ls) '()))
				       (merge-block! mark (cdr k) (cdar ls))
				       (liip (cdr ls)))
				      (else
				       (liip (cdr ls)))))))))))))))
   b)

;*---------------------------------------------------------------------*/
;*    merge-block! ...                                                 */
;*---------------------------------------------------------------------*/
(define (merge-block! mark b by)
   (with-trace 'bbv "merge-block!"
      (trace-item "b=" (block-label b) " <- " (block-label by))
      ;; merge by into bx
      (let loop ((by (list by))
		 (bx (list b)))
	 (cond
	    ((null? by)
	     b)
	    ((=fx (blockS-%mark (car by)) mark)
	     b)
	    ((eq? (car by) (car bx))
	     b)
	    (else
	     (with-access::blockS (car by) (preds (ysuccs succs) first %mark)
		(set! %mark mark)
		(for-each (lambda (d)
			     (with-access::blockS d (%mark)
				(unless (=fx mark %mark)
				   (redirect-block! d (car by) (car bx)))))
		   preds)
		(with-access::blockS (car bx) ((xsuccs succs))
		   (loop ysuccs xsuccs))))))))

;*---------------------------------------------------------------------*/
;*    merge? ...                                                       */
;*    -------------------------------------------------------------    */
;*    Is it possible to merge two blocks with the hash?                */
;*---------------------------------------------------------------------*/
(define (merge? bx::blockS by::blockS stack)
   (with-access::blockS bx ((xbl %blacklist) (xsuccs succs))
      (with-access::blockS by ((ybl %blacklist) (ysuccs succs) first)
	 (cond
	    ((eq? bx by)
	     #t)
	    ((and (memq bx stack) (memq by stack))
	     #t)
	    ((not (=fx (bbv-hash bx) (bbv-hash by)))
	     #f)
	    ((not (bbv-equal? bx by))
	     #f)
	    ((or (eq? xbl '*) (eq? ybl '*))
	     #f)
	    ((or (memq bx ybl) (memq by xbl))
	     #f)
	    ((and (=fx (length ysuccs) 0) (=fx (length first) 1))
	     #f)
	    ((=fx (length ysuccs) (length xsuccs))
	     (let ((ns (cons* bx by stack)))
		(or (every (lambda (x y) (merge? x y ns)) xsuccs ysuccs)
		    (begin
		       (set! xbl (cons by xbl))
		       (set! ybl (cons bx ybl))
		       #f))))
	    (else
	     (begin
		(set! xbl (cons by xbl))
		(set! ybl (cons bx ybl))
		#f))))))

;*---------------------------------------------------------------------*/
;*    get-specialize-block ::blockV ...                                */
;*---------------------------------------------------------------------*/
(define (get-specialize-block::blockS b::blockV ctx::pair-nil)
   
   (define (get-specialize-succ! b s ctx)
      (let ((n (get-specialize-block s ctx)))
	 (block-succs-set! b (cons n (block-succs b)))
	 (block-preds-set! n (cons b (block-preds n)))
	 n))
   
   (with-access::blockV b (label versions succs)
      (with-trace 'bbv (format "get-specialize block ~a" label)
	 (trace-item "ctx=" (ctx->string ctx))
	 (trace-item "versions=" (length versions) " "
	    (map ctx->string (map car versions)))
	 (let ((old (assoc ctx versions)))
	    (if (pair? old)
		(cdr old)
		(let ((s (specialize-block! b ctx)))
		   (trace-item "succs=" (map block-label succs))
		   (set! versions (cons (cons ctx s) versions))
		   (with-access::blockS s ((ssuccs succs) ictx octxs first)
		      (when (pair? first)
			 (let ((last (car (last-pair first))))
			    (cond
			       ((rtl_ins-go? last)
				(with-access::rtl_ins last (fun)
				   (with-access::rtl_go fun (to)
				      (let ((n (get-specialize-succ!
						  s to (car octxs))))
					 (set! to n)
					 (set! octxs (list (car octxs)))))))
			       ((rtl_ins-nop? last)
				(unless (null? (cdr first))
				   (set! first (remq! last first)))
				(let ((n (get-specialize-succ!
					    s (car succs) (car octxs))))
				   (set! octxs (list (car octxs)))))
			       ((rtl_ins-ifeq? last)
				(let* ((n2 (get-specialize-succ!
					      s (cadr succs) (cadr octxs)))
				       (n1 (get-specialize-succ!
					      s (car succs) (car octxs))))
				   (with-access::rtl_ins last (fun)
				      (with-access::rtl_ifeq fun (then)
					 (set! then n2)))))
			       ((rtl_ins-ifne? last)
				(let* ((n2 (get-specialize-succ!
					      s (cadr succs) (cadr octxs)))
				       (n1 (get-specialize-succ!
					      s (car succs) (car octxs))))
				   (with-access::rtl_ins last (fun)
				      (with-access::rtl_ifne fun (then)
					 (set! then n1)))))
			       (else
				(set! ssuccs
				   (map (lambda (u c)
					   (get-specialize-succ! s u c))
				      (reverse succs)
				      (reverse octxs)))))))
		      s)))))))

;*---------------------------------------------------------------------*/
;*    specialize-block! ...                                            */
;*---------------------------------------------------------------------*/
(define (specialize-block!::blockS b::block ctx0)

   (define (resolve-intcmp b sin next sis sctx gettrue getfalse)
      (with-access::block b (succs)
	 (let ((ni (duplicate::rtl_ins/bbv next
		      (fun (instantiate::rtl_go 
			      (to (gettrue succs)))))))
	    (block-preds-set! (getfalse succs)
	       (remq! b (block-preds (getfalse succs))))
	    (instantiate::blockS
	       (%parent b)
	       (label (genlabel))
	       (first (reverse! (cons* ni sin sis)))
	       (ictx ctx0)
	       (octxs (list sctx))
	       (succs (list (gettrue succs)))))))
   
   (define (resolve-intcmp-nop b sin next sis sctx gettrue getfalse)
      (let ((nsin (duplicate::rtl_ins/bbv next
		     (fun (instantiate::rtl_nop)))))
	 (resolve-intcmp b nsin next sis sctx gettrue getfalse)))
   
   [assert (b) (not (isa? b blockS))]
   (with-access::block b (first label succs)
      (with-trace 'bbv-block (format "specialize block ~a" label)
	 (let loop ((ois first)
		    (sis '())
		    (ctx ctx0))
	    (cond
	       ((null? (cdr ois))
		(multiple-value-bind (sin sctx)
		   (rtl_ins-specialize (car ois) ctx)
		   (cond
		      ((rtl_ins-last? sin)
		       (instantiate::blockS
			  (%parent b)
			  (label (genlabel))
			  (first (reverse! (cons sin sis)))
			  (ictx ctx0)))
		      ((rtl_ins-typecheck? sin)
		       (multiple-value-bind (reg type flag)
			  (rtl_ins-typecheck sin)
			  (with-trace 'bbv-block-ins "rtl_ins_typecheck"
			     (trace-item "reg=" (shape reg))
			     (trace-item "typ=" (shape type))
			     (instantiate::blockS 
				(%parent b)
				(label (genlabel))
				(first (reverse! (cons sin sis)))
				(ictx ctx0)
				(octxs (list
					  (filter-live-regs sin
					     (refine-ctx sctx reg type #t))
					  (filter-live-regs sin
					     (refine-ctx sctx reg type #f))))))))
		      (else
		       (instantiate::blockS
			  (%parent b)
			  (label (genlabel))
			  (first (reverse! (cons sin sis)))
			  (ictx ctx0)
			  (octxs (map (lambda (_) (filter-live-regs sin sctx))
				    succs)))))))
	       ((and #f (rtl_ins-intcmp? (car ois)) (rtl_ins-branch? (cadr ois)))
		;; tmp <- arg0 BINOP arg1
		;; ifeq tmp
		(tprint ">>> GOTONE.1.." (shape (car ois)))
		(tprint "              " (ctx->string ctx))
		(multiple-value-bind (sin sctx ctxt ctxo)
		   (rtl_ins-specialize-intcmp (car ois) ctx)
		   (cond
		      ((or (rtl_ins-true? sin) (rtl_ins-false? sin))
		       (let ((tmp (rtl_ins-dest (car ois))))
			  (if (regset-member? (rtl_ins-dest (car ois))
				 (rtl_ins/bbv-out (cadr ois)))
			      ;; the temp is live after the test, keep
			      ;; the constant load
			      (if (rtl_ins-true? sin)
				  (resolve-intcmp b sin (cadr ois) sis sctx
				     car cadr)
				  (resolve-intcmp b sin (cadr ois) sis sctx
				     cadr car))
			      ;; temp dead after testing
			      (if (rtl_ins-true? sin)
				  (resolve-intcmp-nop b sin (cadr ois) sis sctx
				     car cadr)
				  (resolve-intcmp-nop b sin (cadr ois) sis sctx
				     cadr car)))))
		      ((not ctxt)
		       (tprint "<<< GOTONE.3..")
		       (loop (cdr ois) (cons sin sis) sctx))
		      (else
		       (tprint "<<< GOTONE.4a."
			  (ctx->string (filter-live-regs (cadr ois) ctxt)))
		       (tprint "<<< GOTONE.4b."
			  (ctx->string (filter-live-regs (cadr ois) ctxo)))
		       (instantiate::blockS
			  (%parent b)
			  (label (genlabel))
			  (first (reverse! (cons* (cadr ois) sin sis)))
			  (ictx ctx0)
			  (octxs (list
				    (filter-live-regs (cadr ois) ctxt)
				    (filter-live-regs (cadr ois) ctxo))))))))
	       (else
		(let ((ins (car ois)))
		   [assert (ins) (not (rtl_notseq? ins))]
		   (multiple-value-bind (sin sctx)
		      (rtl_ins-specialize (car ois) ctx)
		      (loop (cdr ois) (cons sin sis) sctx)))))))))

;*---------------------------------------------------------------------*/
;*    filter-live-regs ...                                             */
;*---------------------------------------------------------------------*/
(define (filter-live-regs ins::rtl_ins/bbv ctx)
   (with-access::rtl_ins/bbv ins (out)
      (filter (lambda (e)
		 (let ((reg (bbv-ctxentry-reg e)))
		    (when (or (not (isa? reg rtl_reg/ra))
			      (regset-member? reg out))
		       (bbv-ctxentry-aliases-set! e
			  (filter (lambda (reg)
				     (regset-member? reg out))
			     (bbv-ctxentry-aliases e))))))
	 ctx)))
   
