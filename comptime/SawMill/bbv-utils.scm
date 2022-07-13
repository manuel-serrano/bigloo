;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-utils.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 27 08:57:51 2017                          */
;*    Last change :  Wed Jul 13 13:49:08 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BB manipulations                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-utils
   
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

   (export  (get-bb-mark)
	    (set-max-label! blocks::pair-nil)
	    (genlabel)
	    (block->block-list regs b::block)
	    (reorder-succs! ::pair-nil)
	    (remove-temps! b::block)
	    (normalize-goto!::pair b::block)
	    (normalize-ifeq!::pair b::block)
	    (simplify-branch! b::block)
	    (remove-nop! b::block)
	    (remove-goto! b::block)
	    (merge! mark b::blockS)	    
	    (redirect-block! b::blockS old::blockS new::blockS)
	    (filter-live-in-regs::pair-nil ins::rtl_ins/bbv ctx::pair-nil)
	    (extend-live-out-regs::pair-nil ins::rtl_ins/bbv ctx::pair-nil)))

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
;*    reorder-succs! ...                                               */
;*---------------------------------------------------------------------*/
(define (reorder-succs! blocks)
   (when (and (pair? blocks) (pair? (cdr blocks)))
      (let loop ((bs blocks))
	 (when (pair? (cdr bs))
	    (let ((b (car bs))
		  (n (cadr bs)))
	       (with-access::block b (succs first)
		  (when (pair? succs)
		     (unless (rtl_ins-go? (car (last-pair first)))
			(set! succs (cons n (remq! n succs))))))
	       (loop (cdr bs)))))))

;*---------------------------------------------------------------------*/
;*    remove-temps! ...                                                */
;*---------------------------------------------------------------------*/
(define (remove-temps! b::block)

   (define (remove-ins-temps! ins::rtl_ins)
      (with-access::rtl_ins ins (args)
	 (let loop ((args args))
	    (if (null? args)
		ins
		(let liip ((arg (car args)))
		   (if (isa? arg rtl_ins)
		       (if (rtl_ins-mov? arg)
			   (with-access::rtl_ins arg ((inner args))
			      (liip (car inner)))
			   (begin
			      (remove-ins-temps! arg)
			      (loop (cdr args))))
		       (begin
			  (set-car! args arg)
			  (loop (cdr args)))))))))
   
   (define (remove-block-temps! b::block)
      (with-access::block b (first)
	 (set! first (map! remove-ins-temps! first))))
   
   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  b)
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (succs)
	     (remove-block-temps! (car bs))
	     (loop (append succs (cdr bs)) (cons (car bs) acc)))))))

;*---------------------------------------------------------------------*/
;*    normalize-goto! ...                                              */
;*    -------------------------------------------------------------    */
;*    returns a list of blocks (those that follow* the argument).      */
;*    -------------------------------------------------------------    */
;*    Add an explicit goto to all BB so that they all end with         */
;*    a return/fail/exit or a goto.                                    */
;*---------------------------------------------------------------------*/
(define (normalize-goto!::pair b::block)

   (define (normalize-block! b::block)
      (with-access::block b (succs first)
	 (when (pair? succs)
	    (let* ((lp (last-pair first))
		   (last (car lp)))
	       (unless (rtl_ins-go? last)
		  (let ((go (instantiate::rtl_ins
			       (dest #f)
			       (fun (instantiate::rtl_go
				       (loc (rtl_ins-loc last))
				       (to (car succs))))
			       (args '()))))
		     (set-cdr! lp (list go))))))))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (normalize-block! (car bs))
	  (with-access::block (car bs) (succs)
	     (loop (append succs (cdr bs)) (cons (car bs) acc)))))))

;*---------------------------------------------------------------------*/
;*    normalize-ifeq! ...                                              */
;*    -------------------------------------------------------------    */
;*    Replace rtl_ifeq with rtl_ifne. After this pass, no rtl_ifeq     */
;*    remain.                                                          */
;*---------------------------------------------------------------------*/
(define (normalize-ifeq!::pair b::block)

   (define (normalize-block! b::block)
      (with-access::block b (succs first)
	 (when (and (pair? succs) (pair? (cdr succs)))
	    ;; the block ends with either if_eq or if_ne
	    (let loop ((first first))
	       (cond
		  ((null? first)
		   (error "normalize-ifeq!" "bad block" (shape b)))
		  ((rtl_ins-ifne? (car first))
		   #unspecified)
		  ((rtl_ins-ifeq? (car first))
		   ;; replace ifeq with ifne using the following goto
		   (if (or (null? (cdr first)) (not (rtl_ins-go? (cadr first))))
		       (error "normalize-ifeq!" "bad block" (shape b))
		       (with-access::rtl_ins (car first) ((ifeq fun))
			  (with-access::rtl_ifeq ifeq (then loc)
			     (with-access::rtl_ins (cadr first) ((go fun))
				(with-access::rtl_go go (to)
				   (set! ifeq
				      (instantiate::rtl_ifne
					 (loc loc)
					 (then to)))
				   (set! to then)
				   (set! succs (reverse! succs))))))))
		  (else
		   (loop (cdr first))))))))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (normalize-block! (car bs))
	  (with-access::block (car bs) (succs)
	     (loop (append succs (cdr bs)) (cons (car bs) acc)))))))

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
	  (with-access::blockS (car bs) (succs first)
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
			(loop bs acc))
		     (loop (cons (car succs) (cdr bs))
			(bbset-cons (car bs) acc)))
		 (let liip ((ss succs)
			    (nsuccs '()))
		    (if (null? ss)
			(loop (append (reverse succs) (cdr bs))
			   (bbset-cons (car bs) acc))
			(let ((s (car ss)))
			   (if (and #f (goto-block? s))
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
;*    remove-goto! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Remove useless gotos (fallthru).                                 */
;*---------------------------------------------------------------------*/
(define (remove-goto! b::block)
   
   (define (goto-block? b)
      ;; is a block explicitly jumping to its successor
      (with-access::block b (first)
	 (when (pair? first)
	    (let ((i (car (last-pair first))))
	       (rtl_ins-go? i)))))
   
   (let loop ((bs (list b))
	      (acc (make-empty-bbset)))
      (cond
	 ((null? bs)
	  b)
	 ((bbset-in? (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (succs)
	     (when (and (goto-block? (car bs))
			(not (bbset-in? (car succs) acc)))
		(with-access::block (car bs) (first)
		   (with-access::rtl_ins (car (last-pair first)) (fun)
		      (set! fun (instantiate::rtl_nop)))))
	     (loop (append succs (cdr bs))
		(bbset-cons (car bs) acc)))))))

;*---------------------------------------------------------------------*/
;*    merge! ...                                                       */
;*    -------------------------------------------------------------    */
;*    Merge equivalent basic blocks subgraphs                          */
;*---------------------------------------------------------------------*/
(define (merge! mark b::blockS)
   (with-access::blockS b (parent succs)
      (with-access::blockV parent (versions %mark)
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
      ;; merge by into b, i.e., replace all occurrence of by with b
      (let loop ((by (list by))
		 (bx (list b)))
	 (trace-item "by=" (map block-label by))
	 (trace-item "bx=" (map block-label bx))
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
;*    Is it possible to merge two blocks with the same hash?           */
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
		(let ((bad (filter (lambda (b) (not (isa? b blockS))) ysuccs)))
		   (when (pair? bad)
		      (tprint "PAS BON: " (map block-label bad) " "
			 (shape by))))
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
