;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/bbv.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 10:05:41 2017                          */
;*    Last change :  Tue Jul 25 14:59:31 2017 (serrano)                */
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
	    saw_bbv-cache)

   (export  (bbv::pair-nil ::backend ::global ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    *cleanup* ...                                                    */
;*---------------------------------------------------------------------*/
(define *cleanup* #t)

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
;*    bbset ...                                                        */
;*---------------------------------------------------------------------*/
(define-struct bbset mark list)

;*---------------------------------------------------------------------*/
;*    make-empty-bbset ...                                             */
;*---------------------------------------------------------------------*/
(define (make-empty-bbset::struct)
   (bbset (get-bb-mark) '()))

;*---------------------------------------------------------------------*/
;*    bbset-in? ...                                                    */
;*---------------------------------------------------------------------*/
(define (bbset-in?::bool block::blockS set::struct)
   (with-access::blockS block (%mark)
      (eq? %mark (bbset-mark set))))

;*---------------------------------------------------------------------*/
;*    bbset-cons ...                                                   */
;*---------------------------------------------------------------------*/
(define (bbset-cons::struct b::blockS set::struct)
   (let ((m (bbset-mark set)))
      (with-access::blockS b (%mark)
	 (set! %mark m))
      (bbset-list-set! set (cons b (bbset-list set)))
      set))

;*---------------------------------------------------------------------*/
;*    bbset-cons* ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbset-cons*::struct b::blockS . rest)
   (cond
      ((null? (cdr rest))
       (bbset-cons b (car rest)))
      ((null? (cddr rest))
       (bbset-cons b (bbset-cons (car rest) (cadr rest))))
      (else
       (bbset-cons b (apply bbset-cons* rest)))))

;*---------------------------------------------------------------------*/
;*    bbset-append ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbset-append::struct blocks::pair-nil set::struct)
   (let ((m (bbset-mark set)))
      (for-each (lambda (b)
		   (with-access::blockS b (%mark)
		      (set! %mark m)))
	 blocks)
      (bbset-list-set! set (append blocks (bbset-list set)))
      set))

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
	  (let ((blocks (normalize-goto! (car blocks))))
	     (when (>=fx (bigloo-debug) 1)
		(dump-blocks global params blocks ".norm.bb"))
	     (let ((regs (liveness! back blocks params)))
		(tprint ">>> " (global-id global))
		(unwind-protect
		   (if (null? blocks)
		       '()
		       (let* ((s (get-specialize-block (car blocks)
				    (params->ctx params)))
;* 			      (b (block->block-list regs               */
;* 				    (remove-nop!                       */
;* 				       (simplify-branch!               */
;* 					  (merge! (get-bb-mark)        */
;* 					     s)))))                    */
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
;*    replace ...                                                      */
;*---------------------------------------------------------------------*/
(define (replace lst old new)
   (let loop ((l lst))
      (cond
	 ((null? l) l)
	 ((eq? (car l) old) (cons new (cdr l)))
	 (else (cons (car l) (loop (cdr l)))))))

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
		((fallthru-block? (car bs))
		 (let* ((lp (last-pair (block-first (car bs))))
			(last (car lp))
			(succs (block-succs (car bs))))
		    (cond
		       ((rtl_ins-ifne? last)
			(cond
			   ((and (rtl_ins-typecheck? last)
				 (not (bbset-in? (car succs) acc)))
			    ;; change the ifne for an ifeq
			    (with-access::rtl_ins last (fun)
			       (set! fun
				  (instantiate::rtl_ifeq
				     (then (cadr succs)))))
			    (loop bs acc))
			   ((bbset-in? (cadr succs) acc)
			    (let ((hop (make-go-block (car bs) (cadr succs))))
			       (with-access::block (car succs) (preds)
				  (set! preds
				     (replace preds (car bs) hop))
				  (set-car! (cdr succs) hop))
			       (loop (cons (car succs) (cdr bs))
				  (bbset-cons* hop (car bs) acc))))
			   (else
			    (loop (append (reverse succs) (cdr bs))
			       (bbset-cons (car bs) acc)))))
		       ((rtl_ins-ifeq? last)
			(if (bbset-in? (car succs) acc)
			    (let ((hop (make-go-block (car bs) (car succs))))
			       (with-access::block (car succs) (preds)
				  (set! preds
				     (replace preds (car bs) hop))
				  (set-car! succs hop))
			       (loop (cons (cadr succs) (cdr bs))
				  (bbset-cons* hop (car bs) acc)))
			    (loop (append succs (cdr bs))
			       (bbset-cons (car bs) acc))))
		       ((bbset-in? (car succs) acc)
			(with-access::block (car bs) (first)
			   (let ((lp (last-pair first))
				 (go (instantiate::rtl_ins/bbv
					(fun (instantiate::rtl_go
						(to (car succs))))
					(dest #f)
					(args '())
					(def (make-empty-regset regs))
					(in (make-empty-regset regs))
					(out (make-empty-regset regs)))))
			      (if (rtl_ins-nop? (car lp))
				  (set-car! lp go)
				  (set-cdr! lp (list go))))
			   (loop (cdr bs) (bbset-cons (car bs) acc))))
		       (else
			(loop (append succs (cdr bs))
			   (bbset-cons (car bs) acc))))))
		((and (goto-block? (car bs)) (not (bbset-in? (car succs) acc)))
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
		       (bbset-cons (car bs) acc))))
		(else
		 (loop (append succs (cdr bs))
		    (bbset-cons (car bs) acc)))))))))

;*---------------------------------------------------------------------*/
;*    normalize-goto! ...                                              */
;*---------------------------------------------------------------------*/
(define (normalize-goto! b::block)

   (define (collapse-branch b first)
      (let ((next (car (block-succs b))))
	 (when (and (rtl_ins-branch? (car (block-first next)))
		    (null? (cdr (block-first next))))
	    (let ((nsuccs (block-succs next)))
	       (if (rtl_ins-go? (car first))
		   (set-car! first (car (block-first next)))
		   (set-cdr! first (block-first next)))
	       (block-preds-set! next (remq! b (block-preds next)))
	       (block-succs-set! b nsuccs)
	       (for-each (lambda (ns)
			    (block-preds-set! ns
			       (replace (block-preds ns) next b)))
		  nsuccs)))))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (succs preds first)
	     (let liip ((first first))
		(cond
		   ((null? first)
		    (loop (append succs (cdr bs)) (cons (car bs) acc)))
		   ((rtl_ins-go? (car first))
		    (collapse-branch (car bs) first)
		    (loop (append succs (cdr bs)) (cons (car bs) acc)))
		   ((null? (cdr first))
		    (when (and (not (rtl_ins-branch? (car first)))
			       (pair? (block-succs (car bs))))
		       (collapse-branch (car bs) first))
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
		    (liip (cdr first))))))))))

;*---------------------------------------------------------------------*/
;*    simplify-branch! ...                                             */
;*---------------------------------------------------------------------*/
(define (simplify-branch! b::block)
   
   (define (goto-block? b)
      ;; is a block explicitly jumping to its successor
      (with-access::block b (first)
	 (every (lambda (i) (or (rtl_ins-nop? i) (rtl_ins-go? i))) first)))
   
   (let loop ((bs (list b))
	      (acc (make-empty-bbset)))
      (cond
	 ((null? bs)
	  b)
	 ((bbset-in? (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::blockS (car bs) (succs first octxs)
	     ;; remove useless nop instructions
	     (if (=fx (length succs) 1)
		 (if (=fx (length (block-preds (car succs))) 1)
		     ;; collapse the two blocks
		     (let ((s (car succs)))
			(for-each (lambda (ns)
				     (block-preds-set! ns
					(replace (block-preds ns) s (car bs))))
			   (block-succs s))
			(set! succs (block-succs s))
			(let ((lp (last-pair first)))
			   (if (rtl_ins-go? (car lp))
			       (begin
				  (set-car! lp (car (block-first s)))
				  (set-cdr! lp (cdr (block-first s))))
			       (set! first (append! first (block-first s)))))
			(set! octxs (blockS-octxs s))
			(loop bs acc))
		     (loop (cons (car succs) (cdr bs))
			(bbset-cons (car bs) acc)))
		 (let liip ((ss succs)
			    (nsuccs '()))
		    (if (null? ss)
			(loop (append (reverse succs) (cdr bs))
			   (bbset-cons (car bs) acc))
			(let ((s (car ss)))
			   (if (goto-block? s)
			       (let ((t (car (block-succs s))))
				  (if (=fx (length (block-preds s)) 1)
				      (begin
					 (block-preds-set! t
					    (remq s (block-preds t)))
					 (redirect-block! (car bs) s t)
					 (block-preds-set! s '())
					 (liip (cdr ss) (cons t nsuccs)))
				      (begin
					 (block-succs-set! (car bs)
					    (cons t
					       (remq s (block-succs (car bs)))))
					 (block-preds-set! s
					    (remq (car bs) (block-preds s)))
					 (block-preds-set! t
					    (cons (car bs)
					       (block-preds t)))
					 (liip (cdr ss) (cons s nsuccs)))))
			       (liip (cdr ss) (cons s nsuccs))))))))))))

;*---------------------------------------------------------------------*/
;*    remove-nop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (remove-nop! b::block)
   (let loop ((bs (list b))
	      (acc (make-empty-bbset)))
      (cond
	 ((null? bs)
	  b)
	 ((bbset-in? (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (first succs)
	     (when (and (pair? first) (pair? (cdr first)))
		(let ((f (filter! (lambda (i) (not (rtl_ins-nop? i))) first)))
		   (set! first
		      (if (null? f)
			  (list (car first))
			  f))))
	     (loop (append succs (cdr bs)) (bbset-cons (car bs) acc)))))))

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
;*    redirect-block! ...                                              */
;*---------------------------------------------------------------------*/
(define (redirect-block! b::blockS old::blockS new::blockS)
   (with-trace 'bbv "redirect-block"
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
	 (let ((last (car (last-pair first))))
	    (cond
	       ((rtl_ins-ifeq? last)
		(with-access::rtl_ins last (fun)
		   (with-access::rtl_ifeq fun (then)
		      (when (eq? then old)
			 (set! then new)))))
	       ((rtl_ins-ifne? last)
		(with-access::rtl_ins last (fun)
		   (with-access::rtl_ifne fun (then)
		      (when (eq? then old)
			 (set! then new)))))
	       ((rtl_ins-go? last)
		(with-access::rtl_ins last (fun)
		   (with-access::rtl_go fun (to)
		      (set! to new))))))))
   b)

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
;*    rtl_ins-branch? ...                                              */
;*---------------------------------------------------------------------*/
(define (rtl_ins-branch? i)
   (with-access::rtl_ins i (fun)
      (or (isa? fun rtl_ifne) (isa? fun rtl_ifeq))))

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
   
