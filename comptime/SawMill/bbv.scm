;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/bbv.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 10:05:41 2017                          */
;*    Last change :  Tue Jul 18 12:44:11 2017 (serrano)                */
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
	    saw_regutils)

   (static  (wide-class blockV::block
	       (versions::pair-nil (default '())))
	    (wide-class blockS::block
	       (ictx::pair-nil (default '()))
	       (octxs::pair-nil (default '())))
	    (wide-class rtl_ins/bbv::rtl_ins
	       (def (default #unspecified))
	       (out (default #unspecified))
	       (in (default #unspecified))))
   
   (export  (bbv::pair-nil ::backend ::global ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    basic-block versionning configuration                            */
;*---------------------------------------------------------------------*/
(define *type-call* #t)

;*---------------------------------------------------------------------*/
;*    bbv ...                                                          */
;*---------------------------------------------------------------------*/
(define (bbv back global params blocks)
   (if *saw-bbv?*
       (with-trace 'bbv (global-id global)
	  (verbose 2 "        bbv " (global-id global))
	  (when (>=fx (bigloo-debug) 1)
	     (dump-blocks global params blocks ".plain.bb"))
	  (set-max-label! blocks)
	  (let ((blocks (append-map normalize-goto! blocks)))
	     (let ((regs (liveness! back blocks params)))
		(unwind-protect
		   (if (null? blocks)
		       '()
		       (let ((b (block->block-list regs
				   (remove-nop!
				      (simplify-branch!
					 (get-specialize-block (car blocks)
					    (params->ctx params)))))))
			  (verbose 3 " " (length blocks) " -> " (length b))
			  (verbose 2 "\n")
			  (when (>=fx (bigloo-debug) 1)
			     (dump-blocks global params b ".bb"))
			  (map! (lambda (b) (shrink! b)) b)
			  b))
		   (begin
		      (for-each shrink! regs))))))
       blocks))

;*---------------------------------------------------------------------*/
;*    replace ...                                                      */
;*---------------------------------------------------------------------*/
(define (replace lst old new)
   (let loop ((l lst))
      (cond
	 ((null? l) lst)
	 ((eq? (car l) old) (cons new (cdr l)))
	 (else (cons (car l) (loop (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    ctx->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (ctx->string ctx)
   (call-with-output-string
      (lambda (op)
	 (fprintf op "~s"
	    (map (lambda (e)
		    (vector (shape (car e))
		       (if (cddr e)
			   (shape (cadr e))
			   (string-append "no-" (shape (cadr e))))))
	       ctx)))))

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
;*    *label* ...                                                      */
;*---------------------------------------------------------------------*/
(define *label* 0)
(define (genlabel)
   (set! *label* (+fx 1 *label*))
   *label*)

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

;* (define (widen-bbv-predicate-only! o regs::pair-nil)                */
;*                                                                     */
;*    (define (get-args o)                                             */
;*       (filter rtl_reg/ra? (rtl_ins-args* o)))                       */
;*                                                                     */
;*    (define (ins-in o regs)                                          */
;*       (if (rtl_ins-typecheck? o)                                    */
;* 	  (list->regset (get-args o) regs)                             */
;* 	  (make-empty-regset regs)))                                   */
;*                                                                     */
;*    (define (args-widen-bbv! o)                                      */
;*       (when (rtl_ins? o)                                            */
;* 	 (with-access::rtl_ins o (args fun)                            */
;* 	    (widen!::rtl_ins/bbv o                                     */
;* 	       (def (make-empty-regset regs))                          */
;* 	       (in (ins-in o regs))                                    */
;* 	       (out (make-empty-regset regs)))                         */
;* 	    (for-each args-widen-bbv! args))))                         */
;*                                                                     */
;*    (define (ins-widen-bbv! o)                                       */
;*       (with-access::rtl_ins o (dest args fun)                       */
;* 	 (widen!::rtl_ins/bbv o                                        */
;* 	    (def (if (or (not dest) (rtl_reg-onexpr? dest))            */
;* 		     (make-empty-regset regs)                          */
;* 		     (list->regset (list dest) regs)))                 */
;* 	    (in (ins-in o regs))                                       */
;* 	    (out (make-empty-regset regs)))                            */
;* 	 (for-each args-widen-bbv! args)))                             */
;*                                                                     */
;*    (define (block-widen-bbv! o)                                     */
;*       (widen!::blockV o)                                            */
;*       (with-access::block o (first)                                 */
;* 	 (for-each ins-widen-bbv! first)))                             */
;*                                                                     */
;*    (for-each block-widen-bbv! o))                                   */

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
		     (for-each (lambda (a) (regset-add! in a)) regs))))))
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
;*    block->block-list ...                                            */
;*---------------------------------------------------------------------*/
(define (block->block-list regs b::block)
   
   (define (swap-succ? b)
      (with-access::block b (first)
	 (when (pair? first)
	    (rtl_ins-ifne? (car (last-pair first))))))
   
   (define (goto-block? b)
      ;; is a block explicitly jumping to its successor
      (with-access::block b (first)
	 (when (pair? first)
	    (let ((i (car (last-pair first))))
	       (rtl_ins-go? i)))))
   
   (define (fallthru-block? b)
      ;; is a block implicitly followed by its successor
      (not (goto-block? b)))
   
   (define (branch-block? b)
      ;; is a block implicitly followed by its successor
      (with-access::block b (first)
	 (when (pair? first)
	    (let ((i (car (last-pair first))))
	       (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i))))))

   (define (make-go-block bs to)
      (duplicate::blockS bs
	 (label (genlabel))
	 (first (list
		   (instantiate::rtl_ins/bbv
		      (fun (instantiate::rtl_go
			      (to to)))
		      (dest #f)
		      (args '())
		      (def (make-empty-regset regs))
		      (in (make-empty-regset regs))
		      (out (make-empty-regset regs)))))
	 (succs (list to))
	 (preds (list bs))))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse! acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (succs)
	     (cond
		((null? succs)
		 (loop (cdr bs) (cons (car bs) acc)))
		((fallthru-block? (car bs))
		 (let* ((lp (last-pair (block-first (car bs))))
			(last (car lp))
			(succs (block-succs (car bs))))
		    (cond
		       ((rtl_ins-ifne? last)
			(cond
			   ((and (rtl_ins-typecheck? last)
				 (not (memq (car succs) acc)))
			    ;; change the ifne for an ifeq
			    (with-access::rtl_ins last (fun)
			       (set! fun
				  (instantiate::rtl_ifeq
				     (then (cadr succs)))))
			    (loop bs acc))
			   ((memq (cadr succs) acc)
			    (let ((hop (make-go-block (car bs) (car succs))))
			       (with-access::block (car succs) (preds)
				  (set! preds
				     (replace preds (car bs) hop))
				  (set-car! (cdr succs) hop))
			       (loop (cons (car succs) (cdr bs))
				  (cons* hop (car bs) acc))))
			   (else
			    (loop (append (reverse succs) (cdr bs))
			       (cons (car bs) acc)))))
		       ((rtl_ins-ifeq? last)
			(if (memq (car succs) acc)
			    (let ((hop (make-go-block (car bs) (car succs))))
			       (with-access::block (car succs) (preds)
				  (set! preds
				     (replace preds (car bs) hop))
				  (set-car! succs hop))
			       (loop (cons (cadr succs) (cdr bs))
				  (cons* hop (car bs) acc)))
			    (loop (append succs (cdr bs))
			       (cons (car bs) acc))))
		       ((memq (car succs) acc)
			(with-access::block (car bs) (first)
			   (set-cdr! (last-pair first)
			      (list (instantiate::rtl_ins/bbv
				       (fun (instantiate::rtl_go
					       (to (car succs))))
				       (dest #f)
				       (args '())
				       (def (make-empty-regset regs))
				       (in (make-empty-regset regs))
				       (out (make-empty-regset regs))))) 
			   (loop (cdr bs) (cons (car bs) acc))))
		       (else
			(loop (append succs (cdr bs))
			   (cons (car bs) acc))))))
		((and (goto-block? (car bs)) (not (memq (car succs) acc)))
		 (with-access::block (car bs) (first)
		    (set! first
		       (if (null? (cdr first))
			   (list (instantiate::rtl_ins/bbv
				    (fun (instantiate::rtl_nop))
				    (dest #f)
				    (args '())
				    (def (make-empty-regset regs))
				    (in (make-empty-regset regs))
				    (out (make-empty-regset regs))))
			   (reverse (cdr (reverse first)))))
		    (loop (append succs (cdr bs))
		       (cons (car bs) acc))))
		(else
		 (loop (append succs (cdr bs))
		    (cons (car bs) acc)))))))))

;*---------------------------------------------------------------------*/
;*    normalize-goto! ...                                              */
;*---------------------------------------------------------------------*/
(define (normalize-goto! b::block)
   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block b (succs preds first)
	     (let liip ((first first))
		(cond
		   ((or (null? first) (null? (cdr first)))
		    (loop (append succs (cdr bs)) (cons (car bs) acc)))
		   ((rtl_ins-ifeq? (car first))
		    [assert (first) (rtl_ins-go? (cadr first))]
		    (with-access::rtl_ins (car first) (fun)
		       (with-access::rtl_ifeq fun (then)
			  (let ((nb (instantiate::block
				       (label (genlabel))
				       (first (cdr first))
				       (preds (list (car bs)))
				       (succs (list then)))))
			     (set-car! succs nb)
			     (set-cdr! first '())
			     (with-access::block then (preds)
				(set! preds (replace preds (car bs) nb)))
			     (loop (append (reverse succs) (cdr bs))
				(cons (car bs) acc))))))
		   ((rtl_ins-ifne? (car first))
		    [assert (first) (rtl_ins-go? (cadr first))]
		    (with-access::rtl_ins (car first) (fun)
		       (with-access::rtl_ifne fun (then)
			  (let ((nb (instantiate::block
				       (label (genlabel))
				       (first (cdr first))
				       (preds (list (car bs)))
				       (succs (list then)))))
			     (set-car! (cdr succs) nb)
			     (set-cdr! first '())
			     (with-access::block then (preds)
				(set! preds (replace preds (car bs) nb)))
			     (loop (append (reverse succs) (cdr bs))
				(cons (car bs) acc))))))
		   (else
		    (loop (append (reverse succs) (cdr bs))
		       (cons (car bs) acc))))))))))

;*---------------------------------------------------------------------*/
;*    simplify-branch! ...                                             */
;*---------------------------------------------------------------------*/
(define (simplify-branch! b::block)
   
   (define (goto-block? b)
      ;; is a block explicitly jumping to its successor
      (with-access::block b (first)
	 (when (and (pair? first) (null? (cdr first)))
	    (let ((i (car first)))
	       (rtl_ins-go? i)))))
   
   (define (nop-block? b)
      ;; is a block explicitly jumping to its successor
      (with-access::block b (first)
	 (when (and (pair? first) (null? (cdr first)))
	    (let ((i (car first)))
	       (rtl_ins-nop? i)))))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  b)
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (succs first)
	     ;; remove useless nop instructions
	     (if (and (=fx (length succs) 1)
		      (=fx (length (block-succs (car succs))) 1))
		 ;; collapse the two blocks
		 (let* ((s (car succs))
			(ns (car (block-succs s))))
		    (block-preds-set! ns (list (car bs)))
		    (set! succs (list ns))
		    (let ((lp (last-pair first)))
		       (if (rtl_ins-go? (car lp))
			   (begin
			      (set-car! lp (car (block-first s)))
			      (set-cdr! lp (cdr (block-first s))))
			   (set! first (append! first (block-first s))))))
		 (set! succs
		    (map! (lambda (s)
			     (if (or (goto-block? s) (nop-block? s))
				 (with-access::block s (preds succs)
				    (let ((t (car succs)))
				       (set! preds (remq! (car bs) preds))
				       (with-access::block t (preds)
					  (set! preds
					     (cons (car bs) (remq! s preds))))
				       (let ((last (car (last-pair first))))
					  (cond
					     ((rtl_ins-ifeq? last)
					      (with-access::rtl_ins last (fun)
						 (with-access::rtl_ifeq fun (then)
						    (when (eq? then s)
						       (set! then t)))))
					     ((rtl_ins-ifne? last)
					      (with-access::rtl_ins last (fun)
						 (with-access::rtl_ifne fun (then)
						    (when (eq? then s)
						       (set! then t)))))
					     ((rtl_ins-go? last)
					      (with-access::rtl_ins last (fun)
						 (with-access::rtl_go fun (to)
						    (set! to t))))))
				       t))
				 s))
		       succs)))
	     (loop (append succs (cdr bs)) (cons (car bs) acc)))))))

;*---------------------------------------------------------------------*/
;*    remove-nop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (remove-nop! b::block)
   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  b)
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (first succs)
	     (when (and (pair? first) (pair? (cdr first)))
		(set! first
		   (filter! (lambda (i) (not (rtl_ins-nop? i))) first)))
	     (loop (append succs (cdr bs)) (cons (car bs) acc)))))))

;*---------------------------------------------------------------------*/
;*    params->ctx ...                                                  */
;*---------------------------------------------------------------------*/
(define (params->ctx params)
   ;; a context is an alist of:
   ;;   (reg . (type . #t|#f))
   (sort (lambda (left right)
	    (<=fx (rtl_reg/ra-num (car left)) (rtl_reg/ra-num (car right))))
      (filter-map (lambda (p)
		     (when (isa? p rtl_reg/ra)
			(with-access::rtl_reg p (type)
			   (unless (eq? type *obj*)
			      (cons p (cons type #t))))))
	 params)))

;*---------------------------------------------------------------------*/
;*    extend-ctx ...                                                   */
;*---------------------------------------------------------------------*/
(define (extend-ctx ctx reg typ flag)
   (cond
      ((not (isa? reg rtl_reg/ra))
       ctx)
      ((eq? typ *obj*)
       ;; simply remove the reg from the context
       (let ((rnum (rtl_reg/ra-num reg)))
	  (let loop ((ctx ctx))
	     (cond
		((null? ctx) ctx)
		((>fx (rtl_reg/ra-num (caar ctx)) rnum) ctx)
		((eq? (caar ctx) reg) (cdr ctx))
		(else (cons (car ctx) (loop (cdr ctx))))))))
      (else
       (let ((rnum (rtl_reg/ra-num reg)))
	  (let loop ((ctx ctx))
	     (cond
		((null? ctx)
		 (list (cons reg (cons typ flag))))
		((>fx (rtl_reg/ra-num (caar ctx)) rnum)
		 (cons (cons reg (cons typ flag)) ctx))
		((eq? (caar ctx) reg)
		 (let ((c (car ctx)))
		    (if (and (eq? (cadr c) typ) (eq? (cddr c) flag))
			ctx
			(cons (cons reg (cons typ flag)) (cdr ctx)))))
		(else
		 (cons (car ctx) (loop (cdr ctx))))))))))
   
;*---------------------------------------------------------------------*/
;*    get-specialize-block ::blockV ...                                */
;*---------------------------------------------------------------------*/
(define (get-specialize-block::blockS b::blockV ctx::pair-nil)
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
		      (set! ssuccs
			 (map (lambda (b o)
				 (let ((n (get-specialize-block b o)))
				    (with-access::block n (preds)
				       (set! preds (cons s preds)))
				    n))
			    succs octxs))
		      (when (pair? first)
			 (let ((last (car (last-pair first))))
			    (cond
			       ((rtl_ins-go? last)
				(with-access::rtl_ins last (fun)
				   (with-access::rtl_go fun (to)
				      (let ((bs (get-specialize-block to
						   (car octxs))))
					 (set! ssuccs (list bs))
					 (set! octxs (list (car octxs)))
					 (set! to bs)))))
			       ((rtl_ins-nop? last)
				(unless (null? (cdr first))
				   (set! first (remq! last first)))
				(set! ssuccs (list (car ssuccs)))
				(set! octxs (list (car octxs))))
			       ((rtl_ins-ifeq? last)
				(with-access::rtl_ins last (fun)
				   (with-access::rtl_ifeq fun (then)
				      (set! then (cadr ssuccs)))))
			       ((rtl_ins-ifne? last)
				(with-access::rtl_ins last (fun)
				   (with-access::rtl_ifne fun (then)
				      (set! then (car ssuccs))))))))
		      s)))))))

;*---------------------------------------------------------------------*/
;*    specialize-block! ...                                            */
;*---------------------------------------------------------------------*/
(define (specialize-block!::blockS b::block ctx0)
   [assert (b) (not (isa? b blockS))]
   (with-access::block b (first label succs)
      (with-trace 'bbv-block (format "specialize block ~a" label)
	 (let loop ((ois first)
		    (sis '())
		    (ctx ctx0))
	    (if (null? (cdr ois))
		(multiple-value-bind (sin sctx)
		   (rtl_ins-specialize (car ois) ctx)
		   (cond
		      ((rtl_ins-last? sin)
		       (instantiate::blockS 
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
				(label (genlabel))
				(first (reverse! (cons sin sis)))
				(ictx ctx0)
				(octxs (list
					  (filter-live-regs sin
					     (extend-ctx sctx reg type #t))
					  (filter-live-regs sin
					     (extend-ctx sctx reg type #f))))))))
		      (else
		       (instantiate::blockS
			  (label (genlabel))
			  (first (reverse! (cons sin sis)))
			  (ictx ctx0)
			  (octxs (map (lambda (_) (filter-live-regs sin sctx))
				    succs))))))
		(let ((ins (car ois)))
		   [assert (ins) (not (rtl_notseq? ins))]
		   (multiple-value-bind (sin sctx)
		      (rtl_ins-specialize (car ois) ctx)
		      (loop (cdr ois) (cons sin sis) sctx))))))))

;*---------------------------------------------------------------------*/
;*    filter-live-regs ...                                             */
;*---------------------------------------------------------------------*/
(define (filter-live-regs ins::rtl_ins/bbv ctx)
   (with-access::rtl_ins/bbv ins (out)
      (filter (lambda (ctx)
		 (or (not (isa? (car ctx) rtl_reg/ra))
		     (regset-member? (car ctx) out)))
	 ctx)))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-typecheck? ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns the type checked by the instruction or false.            */
;*---------------------------------------------------------------------*/
(define (rtl_ins-typecheck? i::rtl_ins)
   (when (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i))
      (with-access::rtl_ins i (args)
	 (when (and (isa? (car args) rtl_ins) (rtl_ins-call? (car args)))
	    (let ((typ (rtl_call-predicate (car args))))
	       (when typ
		  (let ((args (rtl_ins-args* i)))
		     (and (pair? args) (null? (cdr args)) (rtl_reg? (car args))))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-typecheck ...                                            */
;*---------------------------------------------------------------------*/
(define (rtl_ins-typecheck i::rtl_ins)
   (with-access::rtl_ins i (args)
      (let ((typ (rtl_call-predicate (car args))))
	 (let ((args (rtl_ins-args* i)))
	    (values (car args) typ (rtl_ins-ifeq? i))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-last? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-last? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_last)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-nop? ...                                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-nop? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_nop)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-mov? ...                                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-mov? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_mov)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-go? ...                                                  */
;*---------------------------------------------------------------------*/
(define (rtl_ins-go? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_go)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-ifeq? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-ifeq? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_ifeq)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-ifne? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-ifne? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_ifne)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-call? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-call? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_call)))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-vlen? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-vlen? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_vlength)))
   
;*---------------------------------------------------------------------*/
;*    rtl_call-predicate ...                                           */
;*---------------------------------------------------------------------*/
(define (rtl_call-predicate i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (with-access::rtl_call fun (var)
	 (let ((val (variable-value var)))
	    (fun-predicate-of val)))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize ...                                           */
;*    -------------------------------------------------------------    */
;*    Specialize a instruction according to the typing context.        */
;*    Returns the new instruction and the new context.                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize i::rtl_ins ctx::pair-nil)
   
   (with-trace 'bbv-ins "ins"
      (trace-item "ins=" (shape i))
      (trace-item "ctx=" (ctx->string ctx))
      (cond
	 ((rtl_ins-typecheck? i)
	  (multiple-value-bind (reg type flag)
	     (rtl_ins-typecheck i)
	     (trace-item "typ=" (shape type) " flag=" flag)
	     (let ((c (assq reg ctx)))
		(cond
		   ((not c)
		    (with-access::rtl_ins i (fun)
		       (let ((s (duplicate::rtl_ins/bbv i
				   (fun (if (isa? fun rtl_ifeq)
					    (duplicate::rtl_ifeq fun)
					    (duplicate::rtl_ifne fun))))))
			  (values s ctx))))
		   ((and (eq? (cadr c) type) (eq? flag (cddr c)))
		    ;;(tprint "NOPING " (shape i) " " (ctx->string ctx))
		    (with-access::rtl_ins/bbv i (fun args dest)
		       (let ((s (duplicate::rtl_ins/bbv i
				   (fun (instantiate::rtl_nop))
				   (dest #f)
				   (args '()))))
			  (values s ctx))))
		   (else
		    ;;(tprint "GOING " (shape i) " " (ctx->string ctx))
		    (with-access::rtl_ins i (fun args dest)
		       (let* ((then (if (isa? fun rtl_ifeq)
					(rtl_ifeq-then fun)
					(rtl_ifne-then fun)))
			      (s (duplicate::rtl_ins/bbv i
				    (fun (instantiate::rtl_go (to then)))
				    (dest #f)
				    (args '()))))
			  (values s ctx))))))))
	 ((rtl_ins-go? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (with-access::rtl_ins i (fun)
	     (let ((s (duplicate::rtl_ins/bbv i
			 (fun (duplicate::rtl_go fun)))))
		(values s ctx))))
	 ((rtl_ins-ifeq? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (with-access::rtl_ins i (fun)
	     (let ((s (duplicate::rtl_ins/bbv i
			 (fun (duplicate::rtl_ifeq fun)))))
		(values s ctx))))
	 ((rtl_ins-ifne? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (with-access::rtl_ins i (fun)
	     (let ((s (duplicate::rtl_ins/bbv i
			 (fun (duplicate::rtl_ifne fun)))))
		(values s ctx))))
	 ((not (rtl_reg? (rtl_ins-dest i)))
	  (values i ctx))
	 ((rtl_ins-mov? i)
	  (with-access::rtl_ins i (dest args fun)
	     (cond
		((and (pair? args) (null? (cdr args)) (rtl_reg/ra? (car args)))
		 (let ((c (assq (car args) ctx)))
		    (if c
			(values i (extend-ctx ctx dest (cadr c) (cddr c)))
			(values i ctx))))
		((and *type-call* (pair? args) (rtl_ins-call? (car args)))
		 (with-access::rtl_ins (car args) (fun)
		    (with-access::rtl_call fun (var)
		       (with-access::global var (value type)
			  (values i (extend-ctx ctx dest type #t))))))
		(else
		 (values i (extend-ctx ctx dest *obj* #t))))))
	 ((and *type-call* (rtl_ins-call? i))
	  (with-access::rtl_ins i (dest fun)
	     (with-access::rtl_call fun (var)
		(with-access::global var (value type)
		   (if (fun? value)
		       (values i (extend-ctx ctx dest type #t))
		       (values i (extend-ctx ctx dest *obj* #t)))))))
	 ((rtl_ins-vlen? i)
	  (with-access::rtl_ins i (dest)
	     (values i (extend-ctx ctx dest *int* #t))))
	 (else
	  (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t))))))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_ins/bbv ...                                           */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_ins/bbv p m)
   (with-access::rtl_ins/bbv o (%spill fun dest args def in out)
      (with-output-to-port p
	 (lambda ()
	    (when dest
	       (display "[" p)
	       (dump dest p m)
	       (display " <- " p))
	    (dump-ins-rhs o p m)
	    (when dest (display "]" p))
	    (display* " #|fun=" (typeof fun))
	    (display* " def=" (map shape (regset->list def)))
	    (display* " in=" (map shape (regset->list in)))
	    (display* " out=" (map shape (regset->list out)))
	    (display "|#")))))

;*---------------------------------------------------------------------*/
;*    dump ::blockS ...                                                */
;*---------------------------------------------------------------------*/
(define-method (dump o::blockS p m)
   (with-access::block o (label first)
      (fprint p "(block " label)
      (with-access::blockS o (ictx octxs)
	 (fprint p " ;; ictx=" (ctx->string ictx))
	 (for-each (lambda (ctx)
		      (fprint p " ;; octx="
			 (ctx->string ctx)))
	    octxs))
      (with-access::block o (preds succs)
	 (dump-margin p (+fx m 1))
	 (fprint p ":preds " (map block-label preds))
	 (dump-margin p (+fx m 1))
	 (fprint p ":succs " (map block-label succs)))
      (dump-margin p (+fx m 1))
      (dump* first p (+fx m 1))
      (display ")\n" p)))


