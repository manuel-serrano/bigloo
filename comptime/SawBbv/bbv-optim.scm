;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-optim.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  6 09:26:43 2023                          */
;*    Last change :  Mon Jul  1 08:17:25 2024 (serrano)                */
;*    Copyright   :  2023-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BBV optimizations                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-optim
   
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
	    saw_bbv-utils
	    saw_bbv-merge
	    saw_bbv-liveness
	    saw_bbv-debug)

   (export  (remove-nop! ::global ::block)
	    (remove-goto! ::global ::block)
	    (simplify-branch! ::global ::block)
	    (coalesce! ::global ::blockS)))

;*---------------------------------------------------------------------*/
;*    remove-nop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (remove-nop! global b::block)
   (assert-blocks b "before nop!")

   (define (nop! b)
      (let loop ((bs (list b))
		 (acc (make-empty-bbset)))
	 (cond
	    ((null? bs)
	     b)
	    ((bbset-in? (car bs) acc)
	     (loop (cdr bs) acc))
	    (else
	     (with-access::block (car bs) (first succs preds label)
		(when *bbv-debug* (assert-block (car bs) "remove-nop!"))
		(let ((f (filter! (lambda (i) (not (rtl_ins-nop? i))) first)))
		   (cond
		      ((pair? f)
		       ;; prune the instructions
		       (set! first f))
		      ((pair? succs)
		       ;; remove the block
		       (let ((succ (car succs)))
			  (for-each (lambda (p)
				       (redirect-block! p (car bs) succ))
			     preds)))
		      (else
		       (set! first
			  (list (instantiate::rtl_ins
				   (args '())
				   (fun (instantiate::rtl_nop))))))))
		(loop (append succs (cdr bs)) (bbset-cons (car bs) acc)))))))

   (if (or (eq? *bbv-blocks-cleanup* #t)
	   (and (string? *bbv-blocks-cleanup*)
		(string-contains *bbv-blocks-cleanup* "nop")))
       (with-trace 'bbv-cleanup "remove-nop!"
	  (trace-item "global: " (global-id global))
	  (nop! b))
       b))

;*---------------------------------------------------------------------*/
;*    remove-goto! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Remove useless gotos (fallthru).                                 */
;*---------------------------------------------------------------------*/
(define (remove-goto! global b::block)
   
   (define (fallthrough-block? b succs next)
      ;; is a block explicitly jumping to its successor
      (when (pair? succs)
	 (with-access::block b (first)
	    (when (pair? first)
	       (let ((i (car (last-pair first))))
		  (when (rtl_ins-go? i)
		     (with-access::rtl_ins i (fun)
			(with-access::rtl_go fun (to)
			   (when (eq? to (car succs))
			      (eq? to next))))))))))
   
   (assert-blocks b "before goto!")

   (define (remove! b)
      (let loop ((bs (list b))
		 (acc (make-empty-bbset)))
	 (cond
	    ((null? bs)
	     b)
	    ((bbset-in? (car bs) acc)
	     (loop (cdr bs) acc))
	    (else
	     (when *bbv-debug* (assert-block (car bs) "remove-goto!"))
	     (with-access::block (car bs) (succs)
		(when (and (and (pair? (cdr bs))
				(fallthrough-block? (car bs) succs (cadr bs)))
			   (not (bbset-in? (car succs) acc)))
		   (with-access::block (car bs) (first label)
		      (with-access::rtl_ins (car (last-pair first)) (fun)
			 (set! fun (instantiate::rtl_nop)))))
		(loop (append succs (cdr bs))
		   (bbset-cons (car bs) acc)))))))

   (if (or (eq? *bbv-blocks-cleanup* #t)
	   (and (string? *bbv-blocks-cleanup*)
		(string-contains *bbv-blocks-cleanup* "goto")))
       (with-trace 'bbv-cleanup "remove-goto!"
	  (trace-item "global: " (global-id global))
	  (remove! b))
       b))

;*---------------------------------------------------------------------*/
;*    simplify-branch! ...                                             */
;*---------------------------------------------------------------------*/
(define (simplify-branch! global b::block)
   
   (when *bbv-debug*
      (assert-blocks b "before simplify-branch!"))

   (define (simplify! b)
      (let loop ((bs (list b))
		 (acc (make-empty-bbset)))
	 (cond
	    ((null? bs)
	     b)
	    ((bbset-in? (car bs) acc)
	     (loop (cdr bs) acc))
	    (else
	     (with-access::blockS (car bs) (preds succs first label)
		(when *bbv-debug*
		   (assert-block (car bs) "simplify-branch!"))
		(if (=fx (length succs) 1)
		    (if (=fx (length (block-preds (car succs))) 1)
			;; collapse the two blocks
			(let ((s (car succs)))
			   (trace-item "collapse[" label "] " (block-label s))
			   (for-each (lambda (ns)
					(block-preds-set! ns
					   (list-replace (block-preds ns)
					      s (car bs))))
			      (block-succs s))
			   (set! succs (block-succs s))
			   (let ((lp (last-pair first)))
			      (if (rtl_ins-go? (car lp))
				  (let ((rev (reverse! (cdr (reverse first)))))
				     (set! first (append! rev (block-first s))))
				  (set! first
				     (append first
					(list-copy (block-first s))))))
			   (when *bbv-debug*
			      (assert-blocks s "simplify-branch!.1"))
			   (loop bs acc))
			(loop (cons (car succs) (cdr bs))
			   (bbset-cons (car bs) acc)))
		    (let liip ((ss succs)
			       (nsuccs '()))
		       (if (null? ss)
			   (loop (append (reverse nsuccs) (cdr bs))
			      (bbset-cons (car bs) acc))
			   (let ((s (car ss)))
			      (if (goto-block? s)
				  (let ((t (goto-target s)))
				     (trace-item "simplify[" label "] succ=" (block-label s)
					" goto-target=" (block-label t)
					" succ.preds: " (map block-label (block-preds s))
					" succ.succs: " (map block-label (block-succs s))
					" => " (block-label t))
				     (redirect-block! (car bs) s t)
				     (block-preds-set! t
					(remq s (block-preds t)))
				     (when *bbv-debug*
					(assert-blocks s "before simplify-branch!.2a")
					(assert-blocks t "before simplify-branch!.2b"))
				     (liip (cdr ss) nsuccs))
				  (liip (cdr ss) (cons s nsuccs))))))))))))
   
   (if (or (eq? *bbv-blocks-cleanup* #t)
	   (and (string? *bbv-blocks-cleanup*)
		(string-contains *bbv-blocks-cleanup* "simplify")))
       (with-trace 'bbv-cleanup "simplify!"
	  (trace-item "global: " (global-id global))
	  (simplify! b))
       b))

;*---------------------------------------------------------------------*/
;*    coalesce! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Coalesce equivalent basic blocks subgraphs                       */
;*---------------------------------------------------------------------*/
(define (coalesce! global::global b::blockS)
   
   (define (co! mark b)
      (with-trace 'bbv-cleanup "coalesce.co!"
	 (trace-item "b: #" (block-label b))
	 (with-access::blockS b (parent succs)
	    (trace-item
	       (block-label b) " parent: " (block-label parent)
	       " -> " (map block-label succs))
	    (with-access::blockV parent (%mark)
	       (unless (=fx mark %mark)
		  (set! %mark mark)
		  (trace-item "b=" (block-label b))
		  (let ((versions (blockV-live-versions parent)))
		     (if (or (null? versions) (null? (cdr versions)))
			 (for-each (lambda (s) (co! mark s)) succs)
			 (let ((ks (sort (lambda (v1 v2)
					    (<=fx (car v1) (car v2)))
				      (map (lambda (v)
					      (cons (bbv-hash v) v))
					 versions))))
			    (trace-item "coalesce "
			       (map (lambda (k)
				       (cons (block-label (cdr k)) (car k)))
				  ks))
			    (let loop ((ks ks))
			       (cond
				  ((null? (cdr ks))
				   (for-each (lambda (s) (co! mark s)) succs))
				  ((>=fx (blockS-%mark (cdar ks)) mark)
				   (loop (cdr ks)))
				  (else
				   (let ((k (car ks)))
				      (when *bbv-debug*
					 (assert-block (cdr k) "coalesce!"))
				      (let liip ((ls (cdr ks)))
					 (cond
					    ((null? ls)
					     (loop (cdr ks)))
					    ((and (=fx (car k) (caar ls))
						  (not (eq? (cdr k) (cdar ls)))
						  ;; (coalesce-TBR? (cdr k) (cdar ls) '())
						  (coalesce? (cdr k) (cdar ls) 0))
					     (coalesce-block! mark (cdr k) (cdar ls))
					     (liip (cdr ls)))
					    (else
					     (liip (cdr ls)))))))))))))))))
   
   (assert-blocks b "before coalesce!")
   
   (when (or (eq? *bbv-blocks-cleanup* #t)
	     (and (string? *bbv-blocks-cleanup*)
		  (string-contains *bbv-blocks-cleanup* "coalesce")))
      (with-trace 'bbv-cleanup "coalesce"
	 (trace-item "global: " (global-id global))
	 (co! (get-bb-mark) b)))
   b)

;*---------------------------------------------------------------------*/
;*    coalesce-block! ...                                              */
;*---------------------------------------------------------------------*/
(define (coalesce-block! mark b by)
   (with-trace 'bbv-cleanup "coalesce-block!"
      (trace-item "b=" (block-label b) " <- " (block-label by))
      ;; coalesce by into b, i.e., replace all occurrences of by with b
      (let loop ((by (list by))
		 (bx (list b)))
	 (trace-item "by=" (map block-label by))
	 (trace-item "bx=" (map block-label bx))
	 (cond
	    ((null? by)
	     #unspecified)
	    ((=fx (blockS-%mark (car by)) mark)
	     #unspecified)
	    ((eq? (car by) (car bx))
	     #unspecified)
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
;*    coalesce? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Is it possible to coalesce two blocks with the same hash?        */
;*---------------------------------------------------------------------*/
(define (coalesce? bx::blockS by::blockS depth)
   
   (define (mark-uncoalescable! bx by)
      (with-access::blockS bx ((xbl %blacklist) )
	 (with-access::blockS by ((ybl %blacklist))
	    (set! xbl (cons by xbl))
	    (set! ybl (cons bx ybl))
	    #f)))
   
   (with-trace 'bbv-cleanup "coalesce?"
      (trace-item "bx=" (block-label bx))
      (trace-item "bv=" (block-label by))
      (trace-item "stack.depth" depth)
      (with-access::blockS bx ((xbl %blacklist) (xsuccs succs))
	 (with-access::blockS by ((ybl %blacklist) (ysuccs succs) (yfirst first))
	    (cond
	       ((eq? bx by)
		#t)
	       ((>fx depth 40)
		(mark-uncoalescable! bx by))
	       ((not (=fx (bbv-hash bx) (bbv-hash by)))
		(mark-uncoalescable! bx by))
	       ((or (eq? xbl '*) (eq? ybl '*))
		(mark-uncoalescable! bx by))
	       ((or (memq bx ybl) (memq by xbl))
		#f)
	       ((and (=fx (length ysuccs) 0) (=fx (length yfirst) 1))
		(mark-uncoalescable! bx by))
	       ((=fx (length ysuccs) (length xsuccs))
		(or (every (lambda (x y) (coalesce? x y (+fx depth 1))) xsuccs ysuccs)
		    (mark-uncoalescable! bx by)))
	       (else
		(mark-uncoalescable! bx by)))))))

;*---------------------------------------------------------------------*/
;*    coalesce-TBR? ...                                                */
;*    -------------------------------------------------------------    */
;*    Is it possible to coalesce two blocks with the same hash?        */
;*---------------------------------------------------------------------*/
(define (coalesce-TBR? bx::blockS by::blockS stack)
   (with-trace 'bbv-cleanup "coalesce?"
      (trace-item "bx=" (block-label bx))
      (trace-item "bv=" (block-label by))
      (trace-item "stack.len=" (length stack))
      (with-access::blockS bx ((xbl %blacklist) (xsuccs succs))
	 (with-access::blockS by ((ybl %blacklist) (ysuccs succs) (yfirst first))
	    (cond
	       ((eq? bx by)
		#t)
	       ((memq bx stack)
		(memq by stack))
	       ((>fx (length stack) 20)
		#f)
	       ((not (=fx (bbv-hash bx) (bbv-hash by)))
		#f)
	       ((not (bbv-equal? bx by))
		#f)
	       ((or (eq? xbl '*) (eq? ybl '*))
		#f)
	       ((or (memq bx ybl) (memq by xbl))
		#f)
	       ((and (=fx (length ysuccs) 0) (=fx (length yfirst) 1))
		#f)
	       ((=fx (length ysuccs) (length xsuccs))
		(let ((ns (cons* bx by stack)))
		   (or (every (lambda (x y) (coalesce? x y ns)) xsuccs ysuccs)
		       (begin
			  (set! xbl (cons by xbl))
			  (set! ybl (cons bx ybl))
			  #f))))
	       (else
		(begin
		   (set! xbl (cons by xbl))
		   (set! ybl (cons bx ybl))
		   #f)))))))

;*---------------------------------------------------------------------*/
;*    goto-block? ...                                                  */
;*---------------------------------------------------------------------*/
(define (goto-block? b)
   (with-access::block b (succs)
      ;; is a block jumping to its successor and the succssor has
      ;; only b as predecessor
      (when (and (pair? succs) (null? (cdr succs)))
	 (with-access::block b (first)
	    (let loop ((first first))
	       (cond
		  ((null? first)
		   #f)
		  ((rtl_ins-go? (car first))
		   (when (null? (cdr first))
		      (with-access::rtl_ins (car first) (fun)
			 (with-access::rtl_go fun (to)
			    (eq? to (car succs))))))
		  ((rtl_ins-nop? (car first))
		   (loop (cdr first)))
		  (else
		   #f)))))))

;*---------------------------------------------------------------------*/
;*    goto-target ...                                                  */
;*---------------------------------------------------------------------*/
(define (goto-target b)
   (let ((t (car (block-succs b))))
      (if (goto-block? t)
	  (goto-target t)
	  t)))

