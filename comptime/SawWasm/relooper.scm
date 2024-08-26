(module saw_wasm_relooper
  (import
    type_type
    ast_var
    ast_node
    saw_defs
    backend_backend
    backend_cvm
    saw_wasm_code
    backend_wasm)
  (export 
    (reloop global tree)
    (dom_tree blocks)
    (dump_tree tree depth)))

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
;*    compute_spanning_tree ...                                        */
;*---------------------------------------------------------------------*/
(define (compute_spanning_tree entry_block)
  (let ((stack '())
        (visited (make-hashtable)))
    
    (define (dfs block)
      (let ((node (make-dom_tree_node block '() '() #f #f #f '() 0)))
        (hashtable-put! visited block node)
        (for-each (lambda (succ)
            (unless (hashtable-contains? visited succ)
              (dfs succ))) 
          (block-succs block))
        (set! stack (cons node stack))))

    (define (update-succs-and-preds node)
      ;; FIXME: we use filter-map instead of map because basic blocks seems to 
      ;;        have "phantom" predecessors. That is, predecessors that don't exist 
      ;;        (were not visited but also do not show up in the dumped CFG).
      (dom_tree_node-succs-set! node (filter-map (lambda (block) (hashtable-get visited block)) (block-succs (dom_tree_node-block node))))
      (dom_tree_node-preds-set! node (filter-map (lambda (block) (hashtable-get visited block)) (block-preds (dom_tree_node-block node)))))

    ;; Assign node orders in reverse postorder.
    (define (update-orders stack order)
      (unless (null? stack)
        (dom_tree_node-order-set! (car stack) order)
        (update-orders (cdr stack) (+fx order 1))))

    (dfs entry_block)
    (for-each update-succs-and-preds stack)
    (update-orders stack 0)
    stack))

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

  (define (find_path tree node path)
    (if (eq? tree node)
      (reverse path)
      (let ((result (filter-map 
                      (lambda (subtree) 
                        (find_path subtree node (cons tree path))) 
                      (dom_tree_node-children tree))))
        (if (null? result)
          '()
          (car result)))))

  (define (find_before_first_mismatch path1 path2 last_node)
    (cond 
      ((or (null? path1) (null? path2)) last_node)
      ((eq? (car path1) (car path2)) (find_before_first_mismatch (cdr path1) (cdr path2) (car path1)))
      (else last_node)))

  (let ((path1 (find_path tree node1 '()))
        (path2 (find_path tree node2 '())))
    (find_before_first_mismatch path1 path2 tree)))

;*---------------------------------------------------------------------*/
;*    dom_tree ...                                                     */
;*    -------------------------------------------------------------    */
;*    Computes the dominator tree for the given list of                */
;*    RTL basic blocks.                                                */
;*---------------------------------------------------------------------*/
(define (dom_tree blocks)
  ; Algorithm reference:
  ;   K. D. Cooper, T. J. Harvey, and K. Kennedy.
  ;   A simple, fast dominance algorithm.
  ;   Software Practice & Experience, 4:110, 2001.

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

  (let ((nodes (compute_spanning_tree (car blocks))))
    (set! nodes (sort (lambda (u v) (<fx (dom_tree_node-order u) (dom_tree_node-order v))) nodes))
    (let ((entry_node (car nodes)))
      ;; Do not use parent-set! here because we don't want 
      ;; to update the entry_node children list.
      (dom_tree_node-parent-set! entry_node entry_node)
      ;; Discard entry node, it is at start of the list because it always has order 0.
      (set! nodes (cdr nodes))

      (with-trace 'relooper "dom_tree"
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
                ;; Remove the parent of entry_node (it doesn't have one,
                ;; but for the algorithm sake, we considered that it was its
                ;; own parent).
                (dom_tree_node-parent-set! entry_node #f)
                entry_node))))))))

;*---------------------------------------------------------------------*/
;*    dump_tree ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump_tree tree depth)
  ;; Only used for debugging purposes.
  (define (ppindent depth)
    (unless (=fx depth 0)
        (display "  ")
        (ppindent (-fx depth 1))))
  
  (ppindent depth)
  (display* "- Block " (block-label (dom_tree_node-block tree)))
  (display* " (order=" (dom_tree_node-order tree) ")")
  (display* " (is_loop_header=" (dom_tree_node-is_loop_header tree) ")")
  (display* " (is_merge_node=" (dom_tree_node-is_merge_node tree) ")")
  (newline)
  (for-each (lambda (child)
    (dump_tree child (+fx depth 1))) (dom_tree_node-children tree)))

;*---------------------------------------------------------------------*/
;*    find-terminator ...                                              */
;*    -------------------------------------------------------------    */
;*    Finds the terminator instruction of the block (the last          */
;*    instruction), in a form more suitable for the Relooper           */
;*    algorithm. We ignore rtl_last instructions (e.g. return)         */
;*    as they do not have any successors and therefore are not         */
;*    important for Relooper.                                          */
;*---------------------------------------------------------------------*/
(define (find-terminator node)
  (define (find-succ-by-block block)
    (find (lambda (succ) (eq? (dom_tree_node-block succ) block)) (dom_tree_node-succs node)))

  (define (find-other-succ-by-block block)
    (find (lambda (succ) (not (eq? (dom_tree_node-block succ) block))) (dom_tree_node-succs node)))

  (let ((result 
    (filter-map (lambda (ins)
      (let ((fn (rtl_ins-fun ins)))
        (cond
          ((rtl_ifeq? fn)
            `(if ,@(rtl_ins-args ins)
              ,(find-other-succ-by-block (rtl_ifeq-then fn))
              ,(find-succ-by-block (rtl_ifeq-then fn))))
          ((rtl_ifne? fn)
            `(if ,@(rtl_ins-args ins)  
              ,(find-succ-by-block (rtl_ifne-then fn))
              ,(find-other-succ-by-block (rtl_ifne-then fn))))
          ((rtl_switch? fn) 
            (with-access::rtl_switch fn (type patterns labels)
              `(switch
                ,fn
                ,type 
                ,patterns 
                ,(map find-succ-by-block labels) 
                ,(rtl_ins-args ins))))
          ((rtl_go? fn) `(goto ,(car (dom_tree_node-succs node))))
          ((rtl_notseq? fn) (error "relooper" "Unsupported notseq RTL instruction." fn))
          ((rtl_last? fn) `(other ,ins))
          (else #f))))
      (block-first (dom_tree_node-block node)))))
    (if (null? result)
      ;; In many cases, node2rtl generate basic blocks with an implicit goto 
      ;; at the end (fallthrough the next basic block).
      `(goto ,(car (dom_tree_node-succs node)))
      (car result))))

;*---------------------------------------------------------------------*/
;*    has-switch-terminator? ...                                       */
;*---------------------------------------------------------------------*/
(define (has-switch-terminator? block)
  (find (lambda (ins) (rtl_switch? (rtl_ins-fun ins))) (block-first block)))

;*---------------------------------------------------------------------*/
;*    is-back-edge? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-back-edge? u v)
  (>=fx (dom_tree_node-order u) (dom_tree_node-order v)))

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
          (when (and (is-back-edge? pred node) (not (dominates? node pred)))
            (return 'irreducible))

          (when (dominates? node pred)
            (return #t)))
        (dom_tree_node-preds node))
      #f))

  (bind-exit (return)
    (with-trace 'relooper "compute-loop-headers"
      (define (dfs node)
        (with-trace 'relooper "compute-loop-headers.dfs"
          (trace-item "node=" (block-label (dom_tree_node-block node)))
          (let ((loop? (is-loop-header? node)))
            (cond
              ((eq? loop? 'irreducible) (return #f))
              (loop?
                (trace-item "is a loop header")
                (dom_tree_node-is_loop_header-set! node #t))))
          (for-each dfs (dom_tree_node-children node))))
      (dfs tree)
      ;; The graph is reducible.
      #t)))

;*---------------------------------------------------------------------*/
;*    compute-merge-nodes ...                                          */
;*    -------------------------------------------------------------    */
;*    Updates the nodes of the given DOM tree with merge nodes         */
;*    information. That is, marks the merge nodes.                     */
;*---------------------------------------------------------------------*/
(define (compute-merge-nodes tree)
  (define (is-merge-node node)
    ;; A node is a merge node if it has at least two incoming forward 
    ;; edges (backward edges do not count).
    (let ((forward-iedge-count 0))
      (for-each (lambda (pred)
          (unless (is-back-edge? pred node)
            (set! forward-iedge-count (+fx forward-iedge-count 1))))
        (dom_tree_node-preds node))
      (trace-item "forward-iedge-count=" forward-iedge-count)
      (>=fx forward-iedge-count 2)))

  (with-trace 'relooper "compute-merge-nodes"
    (define (dfs node)
      (with-trace 'relooper "compute-merge-nodes.dfs"
        (trace-item "node=" (block-label (dom_tree_node-block node)))
        (when (is-merge-node node)
          (trace-item "is a merge node")
          (dom_tree_node-is_merge_node-set! node #t))
        (for-each dfs (dom_tree_node-children node))))
    (dfs tree)))

;*---------------------------------------------------------------------*/
;*    matches-frame? ...                                               */
;*---------------------------------------------------------------------*/
(define (matches-frame? node frame)
  (match-case frame
    ((block-followed-by ?l) (eq? node l))
    ((loop-headed-by ?l) (eq? node l))
    ((if-then-else ?l) (eq? node l))
    (else #f)))

;*---------------------------------------------------------------------*/
;*    index-of ...                                                     */
;*---------------------------------------------------------------------*/
(define (index-of node context)
  (cond
    ((null? context) (error "relooper" "Target label not in evaluation context" node))
    ((matches-frame? node (car context)) 0)
    (else (+fx 1 (index-of node (cdr context))))))

;*---------------------------------------------------------------------*/
;*    gen-go ...                                                       */
;*---------------------------------------------------------------------*/
(define (gen-go from to context)
  (cond
    ((is-back-edge? from to) ;; continue
      `((br ,(index-of to context))))
    ((dom_tree_node-is_merge_node to) ;; exit
      `((br ,(index-of to context))))
    (else ;; inline target basic block
      (do-tree to context))))

;*---------------------------------------------------------------------*/
;*    gen-block ...                                                    */
;*---------------------------------------------------------------------*/
(define (gen-block block)
  (define (is-terminal? ins)
    (let ((fun (rtl_ins-fun ins)))
      (or (rtl_notseq? fun) (rtl_last? fun))))

  (filter-map 
    (lambda (ins)
      (if (is-terminal? ins)
        #f
        (gen-ins ins)))
    (block-first block)))

;*---------------------------------------------------------------------*/
;*    node-within ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-within node context)
  `(,@(gen-block (dom_tree_node-block node))
    ,@(let ((terminator (find-terminator node)))
      (match-case terminator
        ((goto ?t) (gen-go node t context))
        ((if ?c ?t ?f) ;; handle ifeq and ifne
          `((if ,(gen-reg c)
            (then ,@(gen-go node t (cons '(if-then-else) context)))
            (else ,@(gen-go node f (cons '(if-then-else) context))))))
        ((switch ?fun ?type ?patterns ?labels ?args)
          (list (gen-switch fun type patterns labels args 
            (lambda (to) (gen-go node to context))
            (lambda (node) (index-of node context)))))
        ((other ?ins) (list (gen-ins ins)))
        (else (error "relooper" "Unknown terminator instruction" terminator))))))

;*---------------------------------------------------------------------*/
;*    node-within-with-y ...                                           */
;*---------------------------------------------------------------------*/
(define (node-within-with-y node y context)
  (if (null? y)
    (node-within node context)
    (append
      `((block ,@(node-within-with-y node (cdr y) (cons `(block-followed-by ,(car y)) context))))
      (do-tree (car y) context))))

;*---------------------------------------------------------------------*/
;*    do-tree ...                                                      */
;*---------------------------------------------------------------------*/
(define (do-tree tree context)
  (with-trace 'relooper "do-tree"
    (trace-item "block=" (block-label (dom_tree_node-block tree)))

    (define (code-for-tree context)
      (let ((selected_children
              (if (has-switch-terminator? (dom_tree_node-block tree))
                (dom_tree_node-children tree)
                (filter 
                  (lambda (node) (dom_tree_node-is_merge_node node)) 
                  (dom_tree_node-children tree)))))
        (node-within-with-y tree selected_children context)))

    (if (dom_tree_node-is_loop_header tree)
      `((loop ,@(code-for-tree (cons `(loop-headed-by ,tree) context))))
      (code-for-tree context))))

;*---------------------------------------------------------------------*/
;*    reloop ...                                                       */
;*---------------------------------------------------------------------*/
(define (reloop global tree)
  (with-trace 'relooper "reloop"
    (trace-item "function=" (global-name global))

    (with-handler
      (lambda (e)
        #f)
      (if (compute-loop-headers tree)
        (begin
          (compute-merge-nodes tree)
          (do-tree tree '()))
        ;; Fail, the CFG is irreducible.
        #f))))
