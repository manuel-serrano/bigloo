;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/SawMill/bbv.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 10:05:41 2017                          */
;*    Last change :  Thu Jun 30 09:30:54 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
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
   (if (and *saw-bbv?*
	    (or (null? *saw-bbv-functions*)
		(memq (global-id global) *saw-bbv-functions*)))
       (with-trace 'bbv (global-id global)
	  (start-bbv-cache!)
	  (verbose 2 "        bbv " (global-id global))
	  (when (>=fx (bigloo-debug) 1)
	     (dump-blocks global params blocks ".plain.bb"))
	  (set-max-label! blocks)
	  (reorder-succs! blocks)
	  (let ((blocks (normalize-goto! (remove-temps! (car blocks)))))
	     (when (>=fx (bigloo-debug) 1)
		(dump-blocks global params blocks ".norm.bb"))
	     (let ((regs (liveness! back blocks params)))
		;; liveness also widen each block into a blockV
		(unwind-protect
		   (if (null? blocks)
		       '()
		       (let* ((s (get-specialize-block (car blocks)
				    (params->ctx params)))
			      (b (block->block-list regs
				    (if *cleanup*
					(remove-nop!
					   (remove-goto!
					      (simplify-branch!
						 (merge! (get-bb-mark) s))))
					s))))
			  (verbose 3 " " (length blocks) " -> " (length b))
			  (verbose 2 "\n")
			  (when (>=fx (bigloo-debug) 1)
			     (dump-blocks global params
				(block->block-list regs s) ".bbv.bb")
			     (dump-blocks global params
				b ".bb"))
			  (map! (lambda (b) (shrink! b)) b)
			  b))
		   ;; don't shrink, otherwise dump could no longer be used
		   (unless (or (>=fx *compiler-debug* 1)
			       (>=fx (bigloo-debug) 1))
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
;*    reorder-succs! ...                                               */
;*---------------------------------------------------------------------*/
(define (reorder-succs! blocks)
   (when (and (pair? blocks) (pair? (cdr blocks)))
      (let loop ((bs blocks))
	 (when (pair? (cdr bs))
	    (let ((b (car bs))
		  (n (cadr bs)))
	       (with-access::block b (succs)
		  (when (pair? succs)
		     (set! succs (cons n (remq! n succs)))))
	       (loop (cdr bs)))))))

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
;*    gAt-specialize-block ::blockV ...                                */
;*---------------------------------------------------------------------*/
(define (gAt-specialize-block-TBR::blockS b::blockV ctx::pair-nil)
   
   (define (get-specialize-succ! b s ctx)
      (let ((n (get-specialize-block s ctx)))
	 (block-succs-set! b (cons n (block-succs b)))
	 (block-preds-set! n (cons b (block-preds n)))
	 n))
   
   (with-access::blockV b (label versions succs)
      (with-trace 'bbv (format "get-specialize block ~a" label)
	 (trace-item "succs=" (map block-label succs))
	 (trace-item "ctx=" (ctx->string ctx))
	 (trace-item "versions= #" (length versions) " "
	    (map ctx->string (map car versions)))
	 (let ((old (assoc ctx versions)))
	    (if (pair? old)
		(cdr old)
		(let ((s (specialize-block! b ctx)))
		   (set! versions (cons (cons ctx s) versions))
		   (with-access::blockS s ((ssuccs succs) inctx outctxs first)
		      (trace-item "inctxs=" (shape inctx))
		      (trace-item "outctxs=" (map shape outctxs))
		      (for-each (lambda (ins)
				   (cond
				      ((rtl_ins-go? ins)
				       (with-access::rtl_ins ins (fun)
					  (with-access::rtl_go fun (to)
					     (let ((n (get-specialize-succ!
							 s to (car outctxs))))
						(set! to n)
						(set! outctxs (list (car outctxs)))))))
;* 				      ((rtl_ins-nop? ins)              */
;* 				       (unless (null? (cdr first))     */
;* 					  (set! first (remq! ins first))) */
;* 				       (let ((n (get-specialize-succ!  */
;* 						   s (car succs) (car outctxs)))) */
;* 					  (set! outctxs (list (car outctxs))))) */
				      ((rtl_ins-ifeq? ins)
				       (let* ((n2 (get-specialize-succ!
						     s (cadr succs) (cadr outctxs)))
					      (n1 (get-specialize-succ!
						     s (car succs) (car outctxs))))
					  (set-car! ssuccs n1)
					  (with-access::rtl_ins ins (fun)
					     (with-access::rtl_ifeq fun (then)
						(set! then n2)))))
				      ((rtl_ins-ifne? ins)
				       (let* ((n2 (get-specialize-succ!
						     s (cadr succs) (car outctxs)))
					      (n1 (get-specialize-succ!
						     s (car succs) (cadr outctxs))))
					  (set-car! ssuccs n1)
					  (with-access::rtl_ins ins (fun)
					     (with-access::rtl_ifne fun (then)
						(set! then n2)))))
				      ))
;* 				      (else                            */
;* 				       (trace-item "#succs=" (length succs) " " */
;* 					  (length ssuccs))             */
;* 				       (trace-item "#outctxs=" (length outctxs)) */
;* 				       (trace-item "ins-ins=" (typeof ins) " " */
;* 					  (rtl_ins-ins? ins)           */
;* 					  " " (with-access::rtl_ins ins (fun) */
;* 						 (typeof fun)))        */
;* 				       (set! ssuccs                    */
;* 					  (map (lambda (u c)           */
;* 						  (get-specialize-succ! s u c)) */
;* 					     (reverse succs)           */
;* 					     (reverse outctxs))))))      */
			 first)
		      s)))))))

;*---------------------------------------------------------------------*/
;*    get-specialize-block ::blockV ...                                */
;*---------------------------------------------------------------------*/
(define (get-specialize-block::blockS b::blockV ctx::pair-nil)
   (with-access::blockV b (label versions succs first)
      (with-trace 'bbv (format "get-specialize block ~a" label)
	 (trace-item "succs=" (map block-label succs))
	 (trace-item "ctx=" (ctx->string ctx))
	 (trace-item "versions= #" (length versions) " "
	    (map ctx->string (map car versions)))
	 (let ((fctx (filter-live-regs (car first) ctx)))
	    (trace-item "fctx=" (ctx->string fctx))
	    (let ((old (assoc fctx versions)))
	       (if (pair? old)
		   (cdr old)
		   (let ((s (specialize-block! b fctx)))
		      (set! versions (cons (cons fctx s) versions))
		      (with-access::blockS s ((ssuccs succs) inctx outctxs first)
			 (trace-item "inctxs=" (shape inctx))
			 (trace-item "outctxs=" (map shape outctxs))
			 (for-each (lambda (ins)
				      (specialize-block-ins! s ins))
			    first)
			 s))))))))
   
;*---------------------------------------------------------------------*/
;*    specialize-block-ins! ...                                        */
;*---------------------------------------------------------------------*/
(define (specialize-block-ins! s::blockS ins::rtl_ins)
   (with-access::blockS s (succs inctx outctxs)
      (cond
	 ((rtl_ins-go? ins)
	  (with-access::rtl_ins ins (fun)
	     (with-access::rtl_go fun (to)
		(let ((n (get-specialize-block to (car outctxs))))
		   (set! to n)
		   (block-succs-set! s (cons n succs))
		   (block-preds-set! n (cons s (block-preds n)))))))
	 ((rtl_ins-ifeq? ins)
	  (with-access::rtl_ins ins (fun)
	     (with-access::rtl_ifeq fun (then)
		(let ((n (get-specialize-block then (car outctxs))))
		   (set! then n)
		   (block-succs-set! s (list n))
		   (block-preds-set! n (cons s (block-preds n)))))))
	 ((rtl_ins-ifne? ins)
	  (with-access::rtl_ins ins (fun)
	     (with-access::rtl_ifne fun (then)
		(let ((n (get-specialize-block then (cadr outctxs))))
		   (set! then n)
		   (block-succs-set! s (list n))
		   (block-preds-set! n (cons s (block-preds n))))))))))

;*---------------------------------------------------------------------*/
;*    specialize-block! ...                                            */
;*---------------------------------------------------------------------*/
(define (specialize-block!::blockS b::blockV inctx)
   
;*    (define (resolve-intcmp b sin next sis sctx gettrue getfalse)    */
;*       (with-access::block b (succs)                                 */
;* 	 (let ((ni (duplicate::rtl_ins/bbv next                        */
;* 		      (fun (instantiate::rtl_go                        */
;* 			      (to (gettrue succs)))))))                */
;* 	    (block-preds-set! (getfalse succs)                         */
;* 	       (remq! b (block-preds (getfalse succs))))               */
;* 	    (instantiate::blockS                                       */
;* 	       (%parent b)                                             */
;* 	       (label (genlabel))                                      */
;* 	       (first (reverse! (cons* ni sin sis)))                   */
;* 	       (inctx ctx0)                                            */
;* 	       (outctxs (list sctx))                                   */
;* 	       (succs (list (gettrue succs)))))))                      */
;*                                                                     */
;*    (define (resolve-intcmp-nop b sin next sis sctx gettrue getfalse) */
;*       (let ((nsin (duplicate::rtl_ins/bbv next                      */
;* 		     (fun (instantiate::rtl_nop)))))                   */
;* 	 (resolve-intcmp b nsin next sis sctx gettrue getfalse)))      */
   
   (with-access::block b (first label succs)
      (with-trace 'bbv-block (format "specialize-block! ~a" label)
	 (trace-item "ctx=" (map shape inctx))
	 (let loop ((oins first)
		    (nins '())
		    (nctx inctx)
		    (pctx inctx))
	    (cond
	       ((null? oins)
		(let ((lbl (genlabel)))
		   (trace-item "new-label=" lbl)
		   (let* ((first (reverse! nins))
			  (inctx (filter-live-regs (car first) inctx))
			  (outctxs (cond
				      ((null? succs) '())
				      ((null? (cdr succs)) (list inctx))
				      (else (list nctx pctx)))))
		      (instantiate::blockS
			 (%parent b)
			 (label lbl)
			 (first first)
			 (inctx inctx)
			 (outctxs outctxs)))))
	       ((rtl_ins-last? (car oins))
		;; a return, fail, ...
		(multiple-value-bind (ins ctx)
		   (rtl_ins-specialize (car oins) inctx)
		   (loop '() (cons ins nins) '() '())))
	       ((rtl_ins-go? (car oins))
		(multiple-value-bind (ins ctx)
		   (rtl_ins-specialize (car oins) inctx)
		   (loop '() (cons ins nins) nctx pctx)))
	       ((rtl_ins-typecheck? (car oins))
		(multiple-value-bind (ins ctx)
		   (rtl_ins-specialize (car oins) inctx)
		   (cond
		      ((rtl_ins-typecheck? ins)
		       (multiple-value-bind (reg type flag)
			  (rtl_ins-typecheck ins)
			  (loop (cdr oins)
			     (cons ins nins)
			     (filter-live-regs ins (refine-ctx nctx reg type #f))
			     (filter-live-regs ins (refine-ctx pctx reg type #t)))))
		      ((rtl_ins-go? ins)
		       (loop '() (cons ins nins) ctx ctx))
		      (else
		       (loop (cdr oins) (cons ins nins) ctx ctx)))))
	       (else
		(multiple-value-bind (ins ctx)
		   (rtl_ins-specialize (car oins) inctx)
		   (loop (cdr oins) (cons ins nins) nctx pctx))))))))
;* 		   (instantiate::blockS                                */
;* 		      (%parent b)                                      */
;* 		      (label nlbl)                                     */
;* 		      (first (reverse! (cons sin sis)))                */
;* 		      (inctx ctx0)                                     */
;* 		      (outctxs (list                                   */
;* 				  (filter-live-regs sin                */
;* 				     (refine-ctx sctx reg type #t))    */
;* 				  (filter-live-regs sin                */
;* 				     (refine-ctx sctx reg type #f))))))) */
;* 	                                                               */
;* 		(let ((lbl (genlabel)))                                */
;* 		   (trace-item "new-label=" lbl)                       */
;* 		      (instantiate::blockS                             */
;* 			 (%parent b)                                   */
;* 			 (label nlbl)                                  */
;* 			 (first (reverse! (cons sin sis)))             */
;* 			 (inctx ctx0)))))                              */
;* 		                                                       */
;* 	    (cond                                                      */
;* 	       ((null? (cdr inss))                                     */
;* 		(let ((nlbl (genlabel)))                               */
;* 		   (trace-item "ins.null.cdr=" (shape (car inss)))     */
;* 		   (trace-item "new-label=" nlbl)                      */
;* 		   (multiple-value-bind (sin sctx)                     */
;* 		      (rtl_ins-specialize (car inss) ctx)              */
;* 		      (trace-item "sin=" (shape sin))                  */
;* 		      (trace-item "sis=" (map shape sis))              */
;* 		      (cond                                            */
;* 			 ((rtl_ins-last? sin)                          */
;* 			  (instantiate::blockS                         */
;* 			     (%parent b)                               */
;* 			     (label nlbl)                              */
;* 			     (first (reverse! (cons sin sis)))         */
;* 			     (inctx ctx0)))                            */
;* 			 ((rtl_ins-typecheck? sin)                     */
;* 			  (multiple-value-bind (reg type flag)         */
;* 			     (rtl_ins-typecheck sin)                   */
;* 			     (trace-item "reg=" (shape reg))           */
;* 			     (trace-item "typ=" (shape type))          */
;* 			     (instantiate::blockS                      */
;* 				(%parent b)                            */
;* 				(label nlbl)                           */
;* 				(first (reverse! (cons sin sis)))      */
;* 				(inctx ctx0)                           */
;* 				(outctxs (list                         */
;* 					  (filter-live-regs sin        */
;* 					     (refine-ctx sctx reg type #t)) */
;* 					  (filter-live-regs sin        */
;* 					     (refine-ctx sctx reg type #f))))))) */
;* 			 (else                                         */
;* 			  (instantiate::blockS                         */
;* 			     (%parent b)                               */
;* 			     (label nlbl)                              */
;* 			     (first (reverse! (cons sin sis)))         */
;* 			     (inctx ctx0)                              */
;* 			     (outctxs (map (lambda (_)                 */
;* 					    (filter-live-regs sin sctx)) */
;* 				       succs))))))))                   */
;* 	       ((and (rtl_ins-intcmp? (car inss)) (rtl_ins-ifxx? (cadr inss))) */
;* 		(trace-item "ins.intcmp=" (shape (car inss)))          */
;* 		;; tmp <- arg0 BINOP arg1                              */
;* 		;; ifeq tmp                                            */
;* 		(tprint ">>> GOTONE.1.." (shape (car inss)))           */
;* 		(tprint "              " (ctx->string ctx))            */
;* 		(multiple-value-bind (sin sctx ctxt ctxo)              */
;* 		   (rtl_ins-specialize-intcmp (car inss) ctx)          */
;* 		   (cond                                               */
;* 		      ((or (rtl_ins-true? sin) (rtl_ins-false? sin))   */
;* 		       (let ((tmp (rtl_ins-dest (car inss))))          */
;* 			  (if (regset-member? (rtl_ins-dest (car inss)) */
;* 				 (rtl_ins/bbv-out (cadr inss)))        */
;* 			      ;; the temp is live after the test, keep */
;* 			      ;; the constant load                     */
;* 			      (if (rtl_ins-true? sin)                  */
;* 				  (resolve-intcmp b sin (cadr inss) sis sctx */
;* 				     car cadr)                         */
;* 				  (resolve-intcmp b sin (cadr inss) sis sctx */
;* 				     cadr car))                        */
;* 			      ;; temp dead after testing               */
;* 			      (if (rtl_ins-true? sin)                  */
;* 				  (resolve-intcmp-nop b sin (cadr inss) sis sctx */
;* 				     car cadr)                         */
;* 				  (resolve-intcmp-nop b sin (cadr inss) sis sctx */
;* 				     cadr car)))))                     */
;* 		      ((not ctxt)                                      */
;* 		       (tprint "<<< GOTONE.3..")                       */
;* 		       (loop (cdr inss) (cons sin sis) sctx))          */
;* 		      (else                                            */
;* 		       (tprint "<<< GOTONE.4a."                        */
;* 			  (ctx->string (filter-live-regs (cadr inss) ctxt))) */
;* 		       (tprint "<<< GOTONE.4b."                        */
;* 			  (ctx->string (filter-live-regs (cadr inss) ctxo))) */
;* 		       (instantiate::blockS                            */
;* 			  (%parent b)                                  */
;* 			  (label (genlabel))                           */
;* 			  (first (reverse! (cons* (cadr inss) sin sis))) */
;* 			  (inctx ctx0)                                 */
;* 			  (outctxs (list                               */
;* 				    (filter-live-regs (cadr inss) ctxt) */
;* 				    (filter-live-regs (cadr inss) ctxo)))))))) */
;* 	       (else                                                   */
;* 		(let ((ins (car inss)))                                */
;* 		   (trace-item "ins.default=" (shape (car inss)))      */
;* 		   [assert (ins) (not (rtl_notseq? ins))]              */
;* 		   (multiple-value-bind (sin sctx)                     */
;* 		      (rtl_ins-specialize (car inss) ctx)              */
;* 		      (loop (cdr inss) (cons sin sis) sctx)))))))))    */

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
   
