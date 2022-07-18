;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/SawMill/bbv.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 10:05:41 2017                          */
;*    Last change :  Mon Jul 18 07:54:40 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Basic Blocks versioning experiment.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv
   
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
	    saw_bbv-cache
	    saw_bbv-utils
	    saw_bbv-merge)

   (export  (bbv::pair-nil ::backend ::global ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    *cleanup* ...                                                    */
;*---------------------------------------------------------------------*/
(define *cleanup*
   (let ((e (getenv "BIGLOOBBVCLEANUP")))
      (not e)))

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
	  (when (>=fx *trace-level* 1)
	     (dump-blocks global params blocks ".plain.cfg"))
	  (set-max-label! blocks)
	  (reorder-succs! blocks)
	  (when (>=fx *trace-level* 2)
	     (dump-blocks global params blocks ".reorder.cfg"))
	  (let ((blocks (normalize-goto! (remove-temps! (car blocks)))))
	     (when (>=fx *trace-level* 2)
		(dump-blocks global params blocks ".goto.cfg"))
	     (let ((blocks (normalize-ifeq! (car blocks))))
		(when (>=fx *trace-level* 2)
		   (dump-blocks global params blocks ".ifeq.cfg"))
		(let ((regs (liveness! back blocks params)))
		   ;; liveness also widen each block into a blockV
		   (mark-widener! (car blocks))
		   (when (>=fx *trace-level* 2)
		      (dump-blocks global params blocks ".liveness.cfg"))
		   (unwind-protect
		      (if (null? blocks)
			  '()
			  (let* ((s (relink-block!
				       (bbv-block (car blocks)
					  (params->ctx params))))
				 (b (block->block-list regs
				       (if *cleanup*
					   (remove-nop!
					      (remove-goto!
						 (simplify-branch!
						    (merge! (get-bb-mark) s))))
					   s))))
			     (verbose 3 " " (length blocks) " -> " (length b))
			     (verbose 2 "\n")
			     (when (>=fx *trace-level* 1)
				(dump-blocks global params
				   (block->block-list regs s) ".bbv.cfg"))
			     (map! (lambda (b) (shrink! b)) b)
			     b))
		      ;; don't shrink, otherwise dump could no longer be used
		      (unless (or (>=fx *compiler-debug* 1)
				  (>=fx *trace-level* 1))
			 (for-each (lambda (r) (shrink! r)) regs)))))))
       blocks))

;*---------------------------------------------------------------------*/
;*    dump-blocks ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-blocks global params blocks suffix)

   (define oname
      (if (string? *dest*)
	  *dest*
	  (if (and (pair? *src-files*) (string? (car *src-files*)))
	      (prefix (car *src-files*))
	      "./a.out")))
   (define filename (format "~a-~a~a" oname (global-id global) suffix))
   (define name (prefix filename))
      
   (define (dump-blocks port)
      (let* ((id (global-id global)))
	 (fprint port ";; -*- mode: bee -*-")
	 (fprint port ";; *** " id ":")
	 (fprint port ";; " (map shape params))
	 (fprintf port ";; bglcfg '~a' > '~a.dot' && dot '~a.dot' -Tpdf > ~a.pdf\n"
	    filename name name name)
	 (for-each (lambda (b)
		      (dump b port 0)
		      (newline port))
	    blocks)
	 id))
   
   (if oname
       (call-with-output-file filename dump-blocks)
       (dump-blocks (current-error-port))))

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
;*    liveness! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Computes the liveness of a list of blocks. Returns the           */
;*    list of used registers.                                          */
;*    -------------------------------------------------------------    */
;*    This function implements a fix point interation to find the      */
;*    maximal solution of the equations:                               */
;*       in[ n ] = use[ n ] U (out[ n ] - def[ n ])                    */
;*      out[ n ] = Union(succ[ n ]) in[ s ]                            */
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
      ;; pre widen the instructions
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
;*    relink-block! ...                                                */
;*---------------------------------------------------------------------*/
(define (relink-block! b::blockS)
   (let loop ((bs (list b))
	      (cost 0)
	      (acc (make-empty-bbset)))
      (cond
	 ((null? bs)
	  cost)
	 ((bbset-in? (car bs) acc)
	  0)
	 (else
	  (let ((b (car bs)))
	     (with-access::blockS b (preds)
		(loop (append preds (cdr bs))
		   (+ cost (block-cost (car bs)))
		   (bbset-cons (car bs) acc))))))))
