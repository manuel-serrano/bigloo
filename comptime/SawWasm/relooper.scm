;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/comptime/SawWasm/relooper.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Hubert Gruniaux                                   */
;*    Creation    :  Fri Sep 13 14:15:02 2024                          */
;*    Last change :  Fri Dec  6 08:38:58 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Relooper implementation                                          */
;*    -------------------------------------------------------------    */
;*    An implementation of Ramsey's algorithm.                         */
;*     @article{10.1145/3547621,                                       */
;*       author = {Ramsey, Norman},                                    */
;*       title = {Beyond Relooper: recursive translation               */
;*                of unstructured control flow to structured           */
;*    	    control flow (functional pearl)},                          */
;*       year = {2022},                                                */
;*       issue_date = {August 2022},                                   */
;*       publisher = {Association for Computing Machinery},            */
;*       address = {New York, NY, USA},                                */
;*       volume = {6},                                                 */
;*       number = {ICFP},                                              */
;*       url = {https://doi.org/10.1145/3547621},                      */
;*       doi = {10.1145/3547621},                                      */
;*       journal = {Proc. ACM Program. Lang.},                         */
;*       month = {aug},                                                */
;*       articleno = {90},                                             */
;*       numpages = {22}                                               */
;*     }                                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_wasm_relooper
   (import type_type
           ast_var
	   ast_node
	   saw_defs
	   engine_param
	   backend_backend
	   backend_cvm
	   saw_wasm_code
	   backend_wasm
	   tools_shape)
   (export (relooper v::global blocks::pair-nil)))

;*---------------------------------------------------------------------*/
;*    dom_tree_node                                                    */
;*---------------------------------------------------------------------*/
(define-struct dom_tree_node
   block
   preds
   succs
   is_merge_node
   is_loop_header
   parent
   children
   ;; Order attributed in reverse postorder.
   order)

;*---------------------------------------------------------------------*/
;*    dom_tree_node-block-label ...                                    */
;*---------------------------------------------------------------------*/
(define (dom_tree_node-block-label node)
   (when (dom_tree_node? node)
      (block-label (dom_tree_node-block node))))

;*---------------------------------------------------------------------*/
;*    relooper ...                                                     */
;*---------------------------------------------------------------------*/
(define (relooper v::global blocks::pair-nil)
   (when *wasm-use-relooper*
      (reloop v (dom-tree blocks))))

;*---------------------------------------------------------------------*/
;*    reloop ...                                                       */
;*---------------------------------------------------------------------*/
(define (reloop global tree)
   (with-trace 'relooper "reloop"
      (trace-item "function=" (shape global) " (" (global-name global) ")")
      (with-handler
	 (lambda (e)
	    (when (isa? e &exception)
	       (exception-notify e))
	    (tprint "relooper give up: " (shape global))
	    #f)
	 (if (compute-loop-headers tree)
	     ;; the graph is reducible
	     (begin
		(compute-merge-nodes tree)
		(do-tree tree '()))
	     ;; todo, transform build an equivalent reducible graph
	     (begin
		#f)))))

;*---------------------------------------------------------------------*/
;*    compute-spanning-tree ...                                        */
;*---------------------------------------------------------------------*/
(define (compute-spanning-tree entry-block)
   
   (define stack '())
   
   (define visited (make-hashtable))
   
   (define (dfs block)
      (let ((node (make-dom_tree_node block '() '() #f #f #f '() 0)))
	 (hashtable-put! visited block node)
	 (for-each (lambda (succ)
		      (unless (hashtable-contains? visited succ)
			 (dfs succ))) 
	    (block-succs block))
	 (set! stack (cons node stack))))
   
   (define (update-succs-and-preds! node)
      ;; FIXME: we use filter-map instead of map because basic blocks
      ;;        seems to have "phantom" predecessors. That is, predecessors
      ;;        that don't exist (were not visited but also do not show
      ;;        up in the dumped CFG).
      (let ((succs (filter-map (lambda (block) (hashtable-get visited block))
		      (block-succs (dom_tree_node-block node))))
	    (preds (filter-map (lambda (block) (hashtable-get visited block))
		      (block-preds (dom_tree_node-block node)))))
	 (cond
	    ((pair? succs)
	     (dom_tree_node-succs-set! node succs))
	    ((pair? (block-succs (dom_tree_node-block node)))
	     (error "update-succs-and-preds!" "Cannot find successor of"
		(shape (dom_tree_node-block node)))))
	 (cond
	    ((pair? preds)
	     (dom_tree_node-preds-set! node preds))
	    ((pair? (block-preds (dom_tree_node-block node)))
	     (error "update-succs-and-preds!" "Cannot find predecessor of"
		(shape (dom_tree_node-block node)))))))
   
   ;; Assign node orders in reverse postorder.
   (define (update-orders! stack order)
      (unless (null? stack)
	 (dom_tree_node-order-set! (car stack) order)
	 (update-orders! (cdr stack) (+fx order 1))))
   
   (dfs entry-block)
   (for-each update-succs-and-preds! stack)
   (update-orders! stack 0)
   
   stack)

;*---------------------------------------------------------------------*/
;*    lca ...                                                          */
;*    -------------------------------------------------------------    */
;*    Computes the lowest common ancestor of node1 and node2 in the    */
;*    given tree.                                                      */
;*---------------------------------------------------------------------*/
(define (lca tree node1 node2)
   ;; Actually, we implement LCA query using a naive algorithm that runs
   ;; in O(n) and takes O(n) space. The idea is the following:
   ;;   - We find a path from the root to node1.
   ;;   - We find a path from the root to node2.
   ;;   - We find the node just before the first mismatch between the two paths.
   ;; Better algorithms exist, and should probably be used instead.
   
   (define (find-path tree node path)
      (if (eq? tree node)
	  (reverse path)
	  (let ((result (filter-map 
			   (lambda (subtree) 
			      (find-path subtree node (cons tree path))) 
			   (dom_tree_node-children tree))))
	     (if (null? result)
		 '()
		 (car result)))))
   
   (define (find-before-first-mismatch path1 path2 last-node)
      (cond 
	 ((or (null? path1) (null? path2))
	  last-node)
	 ((eq? (car path1) (car path2))
	  (find-before-first-mismatch (cdr path1) (cdr path2) (car path1)))
	 (else
	  last-node)))
   
   (let ((path1 (find-path tree node1 '()))
	 (path2 (find-path tree node2 '())))
      (find-before-first-mismatch path1 path2 tree)))

;*---------------------------------------------------------------------*/
;*    dom-tree ...                                                     */
;*    -------------------------------------------------------------    */
;*    Computes the dominator tree for the given list of                */
;*    RTL basic blocks.                                                */
;*---------------------------------------------------------------------*/
(define (dom-tree blocks)
   ;; Algorithm reference:
   ;;   K. D. Cooper, T. J. Harvey, and K. Kennedy.
   ;;   A simple, fast dominance algorithm.
   ;;   Software Practice & Experience, 4:110, 2001.
   
   (define (parent-set! node new_parent)
      ; Remove node from its old parent children list.
      (let ((old_parent (dom_tree_node-parent node)))
	 (when old_parent
	    (dom_tree_node-children-set! old_parent 
	       (remq node (dom_tree_node-children old_parent)))))
      
      (dom_tree_node-parent-set! node new_parent)
      
      ;; Add node to its new parent children list.
      (when new_parent
	 (dom_tree_node-children-set! new_parent
	    (cons node (dom_tree_node-children new_parent)))))
   
   (define (intersect u v)
      (if (eq? u v)
	  u
	  (begin
	     (let loop1 ()
		(when (>fx (dom_tree_node-order u) (dom_tree_node-order v))
		   (set! u (dom_tree_node-parent u))
		   (loop1)))
	     (let loop2 ()
		(when (<fx (dom_tree_node-order u) (dom_tree_node-order v))
		   (set! v (dom_tree_node-parent v))
		   (loop2)))
	     (intersect u v))))
   
   (let ((nodes (compute-spanning-tree (car blocks))))
      (set! nodes
	 (sort (lambda (u v)
		  (<fx (dom_tree_node-order u) (dom_tree_node-order v)))
	    nodes))
      (let ((entry-node (car nodes)))
	 ;; Do not use parent-set! here because we don't want 
	 ;; to update the entry-node children list.
	 (dom_tree_node-parent-set! entry-node entry-node)
	 ;; Discard entry node, it is at start of the list
	 ;; because it always has order 0.
	 (set! nodes (cdr nodes))
	 
	 (with-trace 'relooper "dom-tree"
	    (let loop ()
	       (let ((changed #f))
		  (for-each (lambda (u)
			       (let ((new-parent (find dom_tree_node-parent (dom_tree_node-preds u))))
				  (when new-parent
				     (for-each (lambda (v)
						  (when (dom_tree_node-parent v) ;; already processed?
						     (set! new-parent (intersect v new-parent))))
					(dom_tree_node-preds u))
				     
				     (unless (eq? (dom_tree_node-parent u) new-parent)
					(parent-set! u new-parent)
					(set! changed #t))))) 
		     nodes)
		  
		  (if changed
		      (loop)
		      (begin
			 ;; Remove the parent of entry-node (it doesn't
			 ;; have one, but for the algorithm sake, we
			 ;; considered that it was its own parent).
			 (dom_tree_node-parent-set! entry-node #f)
			 entry-node))))))))

;*---------------------------------------------------------------------*/
;*    dump-tree ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-tree tree depth)
   ;; Only used for debugging purposes.
   (define (ppindent depth)
      (unless (=fx depth 0)
	 (display "  ")
	 (ppindent (-fx depth 1))))
   
   (ppindent depth)
   (display* "- Block " (dom_tree_node-block-label tree))
   (display* " (order=" (dom_tree_node-order tree) ")")
   (display* " (is_loop_header=" (dom_tree_node-is_loop_header tree) ")")
   (display* " (is_merge_node=" (dom_tree_node-is_merge_node tree) ")")
   (newline)
   (for-each (lambda (child)
		(dump-tree child (+fx depth 1)))
      (dom_tree_node-children tree)))

;*---------------------------------------------------------------------*/
;*    dump-context ...                                                 */
;*---------------------------------------------------------------------*/
(define (dump-context context)
   (map (lambda (f)
	   (if (pair? (cdr f))
	       (cons (car f)
		  (if (dom_tree_node? (cadr f))
		      (list (dom_tree_node-block-label (cadr f)))
		      '(#f)))
	       f))
      context))

;*---------------------------------------------------------------------*/
;*    flow-leaving ...                                                 */
;*    -------------------------------------------------------------    */
;*    Finds the terminator instruction of the block (the last          */
;*    instruction), in a form more suitable for the Relooper           */
;*    algorithm. We ignore rtl_last instructions (e.g. return)         */
;*    as they do not have any successors and therefore are not         */
;*    important for Relooper.                                          */
;*---------------------------------------------------------------------*/
(define (flow-leaving node)
   
   (define (find-then-succ-by-block block)
      (let ((succ (find (lambda (succ)
			   (eq? (dom_tree_node-block succ) block))
		     (dom_tree_node-succs node))))
	 (or succ
	     (error "find-then-succ-by-block"
		"Cannot find then successor"
		(shape block)))))
   
   (define (find-else-succ-by-block block)
      (let* ((succs (dom_tree_node-succs node))
	     (succ (find (lambda (succ)
			    (not (eq? (dom_tree_node-block succ) block)))
		      succs)))
	 (or succ
	     (when (and (pair? succs)
			(pair? (cdr succs))
			(eq? (car succs) (cadr succs)))
		(car succs))
	     (error "find-else-succ-by-block"
		"Cannot find else successor"
		(shape block)))))
   
   (let ((res (filter-map (lambda (ins)
			     (let ((fn (rtl_ins-fun ins)))
				(cond
				   ((rtl_ifeq? fn)
				    `(if ,@(rtl_ins-args ins)
					 ,(find-else-succ-by-block
					     (rtl_ifeq-then fn))
					 ,(find-then-succ-by-block
					     (rtl_ifeq-then fn))))
				   ((rtl_ifne? fn)
				    `(if ,@(rtl_ins-args ins)  
					 ,(find-then-succ-by-block
					     (rtl_ifne-then fn))
					 ,(find-else-succ-by-block
					     (rtl_ifne-then fn))))
				   ((rtl_switch? fn) 
				    (with-access::rtl_switch fn (type patterns labels)
				       `(switch
					   ,fn
					   ,type 
					   ,patterns 
					   ,(map find-then-succ-by-block labels) 
					   ,(rtl_ins-args ins))))
				   ((rtl_go? fn)
				    `(goto ,(car (dom_tree_node-succs node))))
				   ((rtl_notseq? fn)
				    (error "relooper"
				       "Unsupported notseq RTL instruction"
				       fn))
				   ((rtl_last? fn)
				    `(other ,ins))
				   (else #f))))
		 (block-first (dom_tree_node-block node)))))
      (if (null? res)
	  ;; In many cases, node2rtl generate basic blocks with an
	  ;; implicit goto at the end (fallthrough the next basic block).
	  `(goto ,(car (dom_tree_node-succs node)))
	  (car res))))

;*---------------------------------------------------------------------*/
;*    has-switch-terminator? ...                                       */
;*---------------------------------------------------------------------*/
(define (has-switch-terminator? block)
   (find (lambda (ins) (rtl_switch? (rtl_ins-fun ins))) (block-first block)))

;*---------------------------------------------------------------------*/
;*    back-edge? ...                                                   */
;*---------------------------------------------------------------------*/
(define (back-edge? u v)
   (cond
      ((or (not (dom_tree_node? u)) (not (fixnum? (dom_tree_node-order u))))
       (error "back-edge?" "wrong u node"
	  (if (dom_tree_node? u)
	      (shape (dom_tree_node-block u))
	      u)))
      ((or (not (dom_tree_node? v)) (not (fixnum? (dom_tree_node-order v))))
       (error "back-edge?" "wrong v node"
	  (if (dom_tree_node? v)
	      (shape (dom_tree_node-block v))
	      v)))
      (else
       (>=fx (dom_tree_node-order u) (dom_tree_node-order v)))))

;*---------------------------------------------------------------------*/
;*    dominates? ...                                                   */
;*---------------------------------------------------------------------*/
(define (dominates? a b)
   (cond
      ((eq? a b) #t)
      ((eq? a (dom_tree_node-parent b)) #t)
      ((not (dom_tree_node-parent b)) #f)
      (else (dominates? a (dom_tree_node-parent b)))))

;*---------------------------------------------------------------------*/
;*    compute-loop-headers ...                                         */
;*    -------------------------------------------------------------    */
;*    Updates the nodes of the given DOM tree with loop header         */
;*    information. That is, marks the loop header nodes.               */
;*    It returns true if the graph is reducible, false otherwise.      */
;*---------------------------------------------------------------------*/
(define (compute-loop-headers tree)
   
   (define (is-loop-header? node)
      (bind-exit (return)
	 (for-each (lambda (pred)
		      ;; Check if the graph is irreducible.
		      (when (and (back-edge? pred node)
				 (not (dominates? node pred)))
			 (return 'irreducible))
		      
		      (when (dominates? node pred)
			 (return #t)))
	    (dom_tree_node-preds node))
	 #f))
   
   (define (dfs node)
      (bind-exit (return)
	 (with-trace 'relooper "compute-loop-headers.dfs"
	    (trace-item "node=" (block-label (dom_tree_node-block node)))
	    (let ((loop? (is-loop-header? node)))
	       (cond
		  ((eq? loop? 'irreducible)
		   (return #f))
		  (loop?
		   (trace-item "is a loop header")
		   (dom_tree_node-is_loop_header-set! node #t))))
	    (for-each dfs (dom_tree_node-children node))
	    #t)))
   
   (with-trace 'relooper "compute-loop-headers"
      (dfs tree)))

;*---------------------------------------------------------------------*/
;*    compute-merge-nodes ...                                          */
;*    -------------------------------------------------------------    */
;*    Updates the nodes of the given DOM tree with merge nodes         */
;*    information. That is, marks the merge nodes.                     */
;*---------------------------------------------------------------------*/
(define (compute-merge-nodes tree)
   
   (define (is-merge-node? node)
      ;; A node is a merge node if it has at least two incoming forward 
      ;; edges (backward edges do not count).
      (let ((forward-iedge-count 0))
	 (for-each (lambda (pred)
		      (unless (back-edge? pred node)
			 (set! forward-iedge-count
			    (+fx forward-iedge-count 1))))
	    (dom_tree_node-preds node))
	 (trace-item "forward-iedge-count=" forward-iedge-count)
	 (>=fx forward-iedge-count 2)))
   
   (define (dfs node)
      (with-trace 'relooper "compute-merge-nodes.dfs"
	 (trace-item "node=" (block-label (dom_tree_node-block node)))
	 (when (is-merge-node? node)
	    (trace-item "is a merge node")
	    (dom_tree_node-is_merge_node-set! node #t))
	 (for-each dfs (dom_tree_node-children node))))
   
   (with-trace 'relooper "compute-merge-nodes"
      (dfs tree)))

;*---------------------------------------------------------------------*/
;*    matches-frame? ...                                               */
;*---------------------------------------------------------------------*/
(define (matches-frame? node frame)
   (match-case frame
      ((block-followed-by ?l) (eq? node l))
      ((loop-headed-by ?l) (eq? node l))
      ((with-fall-through ?l) (eq? node l))
      ((if-then-else ?l) (eq? node l))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    index-of ...                                                     */
;*---------------------------------------------------------------------*/
(define (index-of node context)
   (let loop ((ctx context)
	      (index 0))
      (cond
	 ((null? ctx)
	  (error "relooper"
	     (format "Target ~a label not in evaluation context"
		(dom_tree_node-block-label node))
	     (dump-context context)))
	 ((matches-frame? node (car ctx))
	  index)
	 (else
	  (loop (cdr ctx) (+fx 1 index))))))

;*---------------------------------------------------------------------*/
;*    gen-block ...                                                    */
;*---------------------------------------------------------------------*/
(define (gen-block block)
   
   (define (is-terminal? ins)
      (let ((fun (rtl_ins-fun ins)))
	 (or (rtl_notseq? fun) (rtl_last? fun))))
   
   (filter-map (lambda (ins)
		  (unless (is-terminal? ins)
		     (gen-ins ins)))
      (block-first block)))

;*---------------------------------------------------------------------*/
;*    do-branch ...                                                    */
;*---------------------------------------------------------------------*/
(define (do-branch from to context::pair-nil)
   (with-trace 'relooper "do-branch"
      (trace-item "from=" (dom_tree_node-block-label from))
      (trace-item "to=" (dom_tree_node-block-label to))
      (trace-item "context=" (dump-context context))
      (cond
	 ((back-edge? from to)
	  ;; continue
	  `((br ,(index-of to context))))
	 ((dom_tree_node-is_merge_node to)
	  ;; exit
	  (let ((idx (index-of to context)))
	     (if (=fx idx 0)
		 '()
		 `((br ,idx)))))
	 (else
	  ;; inline target basic block
	  (do-tree to context)))))

;*---------------------------------------------------------------------*/
;*    node-within ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-within-opt x ys::pair-nil z context::pair-nil)
   
   (define (within x terminator z context)
      (with-trace 'relooper "node-within.within"
	 `(,@(gen-block (dom_tree_node-block x))
	     ,@(match-case terminator
		  ((goto ?l)
		   (do-branch x l context))
		  ((if ?e ?t ?f)
		   ;; handle ifeq and ifne
		   `((if ,(gen-reg e)
			 (then ,@(do-branch x t
				    (cons `(if-then-else ,z) context)))
			 (else ,@(do-branch x f
				    (cons `(if-then-else ,z) context))))))
		  ((switch ?fun ?type ?patterns ?labels ?args)
		   (list (gen-switch fun type patterns labels args 
			    (lambda (to) (do-branch x to context))
			    (lambda (node) (index-of node context)))))
		  ((other ?ins)
		   (list (gen-ins ins)))
		  (else
		   (error "relooper" "Unknown terminator instruction"
		      terminator))))))
   
   (with-trace 'relooper "node-within"
      (trace-item "x=" (dom_tree_node-block-label x))
      (trace-item "ys=" (map dom_tree_node-block-label ys))
      (trace-item "z=" (when z (dom_tree_node-block-label z)))
      (trace-item "context=" (dump-context context))
      (let ((terminator (flow-leaving x)))
	 (trace-item "terminator="
	    `(,(car terminator) ,@(map typeof (cdr terminator)))
	    " "
	    (if (and (pair? (cdr terminator)) (isa? (cadr terminator) rtl_ins))
		(shape (cadr terminator))
		""))
	 (cond
	    ((null? ys)
	     (if (and z (not (eq? (car terminator) 'if)))
		 (let ((context' (cons `(block-followed-by ,z) context)))
		    `((block ,@(node-within x '() #f context'))))
		 (within x terminator z context)))
	    ((not z)
	     (let ((context' (append context `((with-fall-through ,(car ys))))))
		(append (node-within x (cdr ys) (car ys) context')
		   (do-tree (car ys) context))))
	    (else
	     (let ((context' (cons `(block-followed-by ,z) context)))
		`((block ,@(node-within x ys #f context')))))))))

(define (node-within-unopt x ys::pair-nil z context::pair-nil)
   
   (define (within x terminator context)
      (with-trace 'relooper "within"
	 `(,@(gen-block (dom_tree_node-block x))
	     ,@(match-case terminator
		  ((goto ?l)
		   (do-branch x l context))
		  ((if ?e ?t ?f)
		   ;; handle ifeq and ifne
		   `((if ,(gen-reg e)
			 (then ,@(do-branch x t
				    (cons '(if-then-else) context)))
			 (else ,@(do-branch x f
				    (cons '(if-then-else) context))))))
		  ((switch ?fun ?type ?patterns ?labels ?args)
		   (list (gen-switch fun type patterns labels args 
			    (lambda (to) (do-branch x to context))
			    (lambda (node) (index-of node context)))))
		  ((other ?ins)
		   (list (gen-ins ins)))
		  (else
		   (error "relooper" "Unknown terminator instruction"
		      terminator))))))
   
   (with-trace 'relooper "node-within"
      (if (null? ys)
	  (let ((terminator (flow-leaving x)))
	     (within x terminator context))
	  (cons `(block ,@(node-within x (cdr ys) #f
			     (cons `(block-followed-by ,(car ys)) context)))
	     (do-tree (car ys) context)))))

(define (node-within-orig node y z context)
   (define (node-within node context)
      `(,@(gen-block (dom_tree_node-block node))
	  ,@(let ((terminator (flow-leaving node)))
	       (match-case terminator
		  ((goto ?t) (do-branch node t context))
		  ((if ?c ?t ?f) ;; handle ifeq and ifne
		   `((if ,(gen-reg c)
			 (then ,@(do-branch node t (cons '(if-then-else) context)))
			 (else ,@(do-branch node f (cons '(if-then-else) context))))))
		  ((switch ?fun ?type ?patterns ?labels ?args)
		   (list (gen-switch fun type patterns labels args 
			    (lambda (to) (do-branch node to context))
			    (lambda (node) (index-of node context)))))
		  ((other ?ins) (list (gen-ins ins)))
		  (else (error "relooper" "Unknown terminator instruction" terminator))))))

   (if (null? y)
       (node-within node context)
       (append
	  `((block ,@(node-within-orig node (cdr y) z (cons `(block-followed-by ,(car y)) context))))
	  (do-tree (car y) context))))

(define (node-within x ys::pair-nil z context::pair-nil)
   (node-within-opt x ys z context))

;*---------------------------------------------------------------------*/
;*    do-tree ...                                                      */
;*---------------------------------------------------------------------*/
(define (do-tree::pair x context)
   (with-trace 'relooper "do-tree"
      (trace-item "x=" (dom_tree_node-block-label x))
      (trace-item "context=" (dump-context context))
      
      (define (code-for-x context)
	 (let ((ys (if (has-switch-terminator? (dom_tree_node-block x))
		       (dom_tree_node-children x)
		       (filter (lambda (node)
				  (dom_tree_node-is_merge_node node)) 
			  (dom_tree_node-children x)))))
	    (node-within x ys #f context)))
      
      (if (dom_tree_node-is_loop_header x)
	  `((loop ,@(code-for-x (cons `(loop-headed-by ,x) context))))
	  (code-for-x context))))

