;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Unsafe/ssr.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Olivier Melancon                                  */
;*    Creation    :  Fri Jun 21 15:42:17 2024                          */
;*    Last change :  Thu Jun 27 07:22:29 2024 (serrano)                */
;*    Copyright   :  2024 Olivier Melancon                             */
;*    -------------------------------------------------------------    */
;*    ssr                                                              */
;*    -------------------------------------------------------------    */
;*    Single-Source Reachability algorithm                             */
;*                                                                     */
;*    Shiloach, Yossi and Even, Shimon, (1981),                        */
;*    An On-Line Edge-Deletion Problem,                                */
;*    28(1), Journal of the ACM,                                       */
;*    https://doi.org/10.1145/322234.322235,                           */
;*                                                                     */
;*    Alshammari, M., & Rezgui, A. (2020).                             */
;*    A single-source shortest path algorithm for dynamic graphs.      */
;*    AKCE International Journal of Graphs and Combinatorics,          */
;*    17(3), 1063-1068.                                                */
;*    https://doi.org/10.1016/j.akcej.2020.01.002                      */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ssr
   
   (import  __error
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r5_control_features_6_4
	    __object
	    __rgc)
   
   (use     __type
	    __bigloo
	    __param
	    __tvector
	    __structure
	    __tvector
	    __bit
	    __date
	    __os
	    __bexit
	    __thread
	    __bignum
	    __hash
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __evenv)
   
   (export (ssr-make-graph::vector #!key (source 0))
	   (ssr-add-edge! graph::vector from::long to::long #!key onconnect)
	   (ssr-remove-edge! graph::vector from::long to::long #!key ondisconnect)
	   (ssr-redirect! graph::vector node::long other::long #!key onconnect ondisconnect)
	   (ssr-connected? graph::vector node::long)))

;*---------------------------------------------------------------------*/
;*    compatibility kit                                                */
;*---------------------------------------------------------------------*/
(define-macro (make-table)
   '(create-hashtable :eqtest eq?))
(define-macro (table-set! set key . val)
   (if (pair? val)
       `(hashtable-put! ,set ,key ,(car val))
       `(hashtable-remove! ,set ,key)))
(define-macro (table-ref set key . def)
   (let ((k (gensym 'k)))
      `(let ((,k ,key))
	  (or (hashtable-get ,set ,k)
	      ,(if (pair? def)
		   (car def)
		   `(error "table-ref" "key unbound" ,k))))))
(define-macro (table-length set)
   `(hashtable-size ,set))
(define-macro (table-for-each proc set)
   `(hashtable-for-each ,set ,proc))
(define-macro (table->list set)
   `(hashtable-map ,set cons))
(define-macro (table-search proc set)
   (let ((k (gensym 'k))
	 (v (gensym 'v))
	 (r (gensym 'r))
	 (t (gensym 't)))
   `(bind-exit (,r)
       (hashtable-for-each ,set
	  (lambda (,k ,v)
	     (let ((,t (,proc ,k ,v)))
		(when ,t (,r ,t)))))
       #f)))
(define-macro (list->table lst)
   (let ((t (gensym 'table)))
      `(let ((,t (make-table)))
	  (for-each (lambda (pair)
		       (table-set! ,t (car pair) (cdr pair)))
	     ,lst)
	  ,t)))

(define-macro (list-sort fn lst)
   `(sort ,fn ,lst))

(define-macro (random-integer sz)
   `(random ,sz))
(define-macro (random-real)
   `(randomfl))

(define-macro (make-parameter init)
   `(lambda () ,init))
(define-macro (parameterize bindings body)
   (let ((temps (map (lambda (b) (gensym)) bindings)))
      `(let ,(map (lambda (t b) `(,t ,(car b))) temps bindings)
	  (unwind-protect
	     (begin
		,@(map (lambda (t b) `(set! ,(car b) (lambda () ,(cadr b)))) temps bindings)
		,body)
	     (begin
		,@(map (lambda (t b) `(set! ,(car b) ,t))))))))

;*---------------------------------------------------------------------*/
;*    Orignal unmodified code                                          */
;*---------------------------------------------------------------------*/
;; infinity is used for nodes rank. Using an absurdly hige value
;; instead of a float allows to stick to fixnum arithmetic while
;; remaining valid for any practical usecase
(define infinity +inf.0)

(define (make-queue) (cons '() '()))
(define (queue-empty? queue) (null? (car queue)))
(define (queue-get! queue)
  (let ((x (caar queue)))
    (set-car! queue (cdar queue))
    (if (queue-empty? queue) (set-cdr! queue '()))
    x))
(define (queue-put! queue x)
  (let ((entry (cons x '())))
    (if (queue-empty? queue)
      (set-car! queue entry)
      (set-cdr! (cdr queue) entry))
    (set-cdr! queue entry)
    x))
(define (queue-peek queue) (if (queue-empty? queue) #f (caar queue)))

(define (make-set) (make-table))
(define (set-add! set x) (table-set! set x #t))
(define (set-remove! set x) (table-set! set x))
(define (set-contains? set x) (table-ref set x #f))
(define (set-length set) (table-length set))
(define (set-empty? set) (= (set-length set) 0))
(define (set-for-each f set) (table-for-each (lambda (k _) (f k)) set))
(define (set-search f set) (table-search (lambda (k _) (and (f k) k)) set))
(define (set->list set) (map car (table->list set)))

(define (table-ref-or-set-default! table x)
  (let ((value (table-ref table x #f)))
    (or value
        (let ((default (make-set)))
          (table-set! table x default)
          default))))

(define (ssr-make-graph #!key (source 0))
  (vector
    source                                  ;; source
    (list->table (list (cons source 0)))    ;; ranks table
    (make-table)                            ;; parents table
    (make-table)                            ;; children table
    (make-table)                            ;; friends table
    (make-table)))                          ;; friendlies table

(define (graph-source graph) (vector-ref graph 0))
(define (graph-ranks graph) (vector-ref graph 1))
(define (graph-parents graph) (vector-ref graph 2))
(define (graph-children graph) (vector-ref graph 3))
(define (graph-friends graph) (vector-ref graph 4))
(define (graph-friendlies graph) (vector-ref graph 5))

;;   Parent P of X
;;   P -> X where rank(X) = rank(P) + 1
;;
;;     P
;;     |
;;     |
;;     v
;;     X

;;   Child C of X
;;   C if a child of X if X is a parent of C

;;   Friend F of X
;;   X -> F where rank(X) >= rank(F) - 1
;;
;;  F     
;;  ^     
;;  |     P
;;  |     |
;;  \     v
;;   \--- X

(define (get-rank graph x)
  (table-ref (graph-ranks graph) x infinity))
(define (set-rank! graph x rank)
  (table-set! (graph-ranks graph) x rank))
(define (update-rank! graph x)
  (let* ((parent (get-parent graph x))
         (parent-rank (if parent (get-rank graph parent) infinity))
         (old-rank (get-rank graph x))
         (new-rank (+ parent-rank 1)))
    (if (= new-rank old-rank) #f (begin (set-rank! graph x new-rank) #t))))

;; all access methods are written such that adding an edge will never delete
;; another edge. However, it can make a node dirty
(define (add-friend! graph node friend)
  (set-add! (table-ref-or-set-default! (graph-friends graph) node) friend)
  (set-add! (table-ref-or-set-default! (graph-friendlies graph) friend) node))
(define (remove-friend! graph node friend)
  (let ((friends (table-ref (graph-friends graph) node #f))
        (friendlies (table-ref (graph-friendlies graph) friend #f)))
    (if friends (set-remove! friends friend))
    (if friendlies (set-remove! friendlies node))))
(define (get-parent graph x)
  (table-ref (graph-parents graph) x #f))
(define (set-parent! graph child parent)
  ;; change old parent status to friend
  (let ((old-parent (get-parent graph child)))
    (when old-parent
      (add-friend! graph old-parent child)
      (remove-child! graph old-parent child)))
  ;; if new parent was a friend remove this status
  (remove-friend! graph parent child)
  ;; set parent for child
  (table-set! (graph-parents graph) child parent)
  ;; add child to new parent
  (set-add! (table-ref-or-set-default! (graph-children graph) parent) child))
(define (remove-parent! graph child)
  (let ((parent (get-parent graph child)))
    (if parent (remove-child! graph parent child)))
  (table-set! (graph-parents graph) child))
(define (remove-child! graph parent child)
  (set-remove!
    (table-ref (graph-children graph) parent)
    child))

(define (clean-edge? graph from to)
  (>= (get-rank graph from) (- (get-rank graph to) 1)))
(define (dirty-edge? graph from to)
  (not (clean-edge? graph from to)))
(define (edge-exists? graph from to)
  (or (parent? graph to from) (friend? graph from to)))

(define (children-for-each f graph x)
  (let ((children (table-ref (graph-children graph) x #f)))
    (if children (set-for-each f children))))
(define (friends-for-each f graph x)
  (let ((friends (table-ref (graph-friends graph) x #f)))
    (if friends (set-for-each f friends))))
(define (neighbors-for-each f graph x)
  (friends-for-each f graph x)
  (children-for-each f graph x))
(define (friendlies-for-each f graph x)
  (let ((friendlies (table-ref (graph-friendlies graph) x #f)))
    (if friendlies (set-for-each f friendlies))))
(define (friendlies-search f graph x)
  (let ((friendlies (table-ref (graph-friendlies graph) x #f)))
    (if friendlies (set-search f friendlies) #f)))
(define (friendlies->list graph x)
  (let ((friendlies (table-ref (graph-friendlies graph) x #f)))
    (if friendlies (set->list friendlies) '())))
    
(define (source? graph x)
  (= (graph-source graph) x))
(define (parent? graph x p)
  ;; use eq? to treat case where x's parent is #f
  (eq? p (get-parent graph x)))
(define (friend? graph x f)
  (let ((friends (table-ref (graph-friends graph) x #f)))
    (if friends (set-contains? friends f) #f)))

(define (ssr-add-edge! graph from to #!key onconnect)
  (define queue (make-queue))

  (define (hoist hoister node)
    (when (dirty-edge? graph hoister node)
      (set-parent! graph node hoister)
      (if (and onconnect (= (get-rank graph node) infinity)) (onconnect node))
      (update-rank! graph node)
      (neighbors-for-each
        (lambda (n)
          (queue-put! queue node)
          (queue-put! queue n))
        graph
        node)))

  (when (not (edge-exists? graph from to)) ;; no duplicate edges
    (add-friend! graph from to)
    (hoist from to)
    (do () ((queue-empty? queue))
	(let* ((h (queue-get! queue))
	       (n (queue-get! queue)))
	   (hoist h n)))))

(define (ssr-remove-edge! graph from to #!key ondisconnect)
  (cond
    ((not (parent? graph to from)) ;; edge not in BFS, can be removed safely
      (remove-friend! graph from to))
    (else
      (remove-parent-edge! graph to ondisconnect: ondisconnect))))

(define (remove-parent-edge! graph to #!key ondisconnect)
  (define loose-queue (make-queue))
  (define catch-queue (make-queue))
  (define loose-set (make-set))
  (define anchor-table (make-table))
  (define disconnected (make-set))

  (define (loosen! node)
    (let* ((rank (get-rank graph node))
           (bucket (table-ref anchor-table rank #f)))
      (when bucket
        (set-remove! bucket node)
        (if (set-empty? bucket) (table-set! anchor-table rank)))
      (set-add! disconnected node)
      (set-rank! graph node infinity)))
  (define (loose? node) (= (get-rank graph node) infinity))

  (define (anchor! node)
    (let* ((rank (get-rank graph node))
           (bucket (table-ref-or-set-default! anchor-table rank)))
      (set-add! bucket node)))
  (define (anchored? node)
    (let* ((rank (get-rank graph node))
           (bucket (table-ref anchor-table rank #f)))
      (and bucket (set-contains? bucket node))))

  (define (drop to)
    (define drop-queue (make-queue))

    (queue-put! drop-queue to)
    (queue-put! drop-queue (get-rank graph (get-parent graph to)))

    (remove-parent! graph to)

    (do () ((queue-empty? drop-queue))
      (let* ((node (queue-get! drop-queue))
             (parent-rank (queue-get! drop-queue))
             (adopter
               (friendlies-search
                   (lambda (f) (= (get-rank graph f) parent-rank))
                   graph
                   node)))
        (if adopter
          ;; preemptively choose a safe parent
          ;; since we are doing a BFS for possibly loose nodes, a friendly
          ;; with the same rank as parent cannot be loose
          (set-parent! graph node adopter)
          (let ((rank (get-rank graph node)))
            (loosen! node)

            ;; DFS across subgraph to find loose nodes
            (children-for-each
              (lambda (child)
                (queue-put! drop-queue child)
                (queue-put! drop-queue rank))
              graph
              node)
            (friendlies-for-each
              (lambda (friendly)
                (if (not (loose? friendly)) (anchor! friendly)))
              graph
              node))))))

  (define (catch node)
    (neighbors-for-each
      (lambda (neighbor)
        (when (loose? neighbor)
          (set-parent! graph neighbor node)
          (set-remove! disconnected neighbor)
          (update-rank! graph neighbor)
          (queue-put! catch-queue neighbor)))
      graph
      node))

  (drop to)

  (let* ((cmp (lambda (x y) (< (car x) (car y))))
         (buckets (list-sort cmp (table->list anchor-table))))
    (for-each
      (lambda (bucket)
        (let ((rank (car bucket))
              (head (queue-peek catch-queue)))
          (do ((next head (queue-peek catch-queue)))
              ((or (not next) (> (get-rank graph next) rank)))
              (catch (queue-get! catch-queue))))
        (set-for-each catch (cdr bucket)))
      buckets)
    ;; all buckets caught, catch rest of the queue
    (do () ((queue-empty? catch-queue))
      (catch (queue-get! catch-queue)))
    (if ondisconnect (set-for-each ondisconnect disconnected))))

(define (ssr-redirect! graph node other #!key onconnect ondisconnect)
  (define (find-min-by-rank nodes)
    (let loop ((best (car nodes))
               (rank (get-rank graph (car nodes)))
               (nodes (cdr nodes)))
      (if (null? nodes)
          best
          (let* ((next (car nodes))
                  (next-rank (get-rank graph next)))
            (if (< next-rank rank)
                (loop next next-rank (cdr nodes))
                (loop best rank (cdr nodes)))))))

  ;; remove all incoming edges to node and redirect them toward other
  ;; if there is an edge from node to node itself, it is not redirected
  ;; this procedure chooses the order in which to add and remove edges
  ;; to minimize the work to maintain ranks
  (when (not (= node other)) ;; do nothing for self redirect
    (let ((parent (get-parent graph node))
          (friendlies
            (filter
              (lambda (f) (not (= f node)))
              (friendlies->list graph node))))
      (cond
        ((and (not parent) (null? friendlies)) #f) ;; case 0: nothing to redirect
        ((>= (get-rank graph node) (get-rank graph other)) ;; case 1: other's rank will not change
          ;; removing friendlies never changes rank
          (for-each (lambda (f) (remove-friend! graph f node)) friendlies)
          ;; check parent in case node was entirely disconnected
          (when parent
            (remove-parent-edge! graph node ondisconnect: ondisconnect)
            (if (not (parent? graph other parent))
                (add-friend! graph parent other)))
          ;; all edges can be redirected to other without possibility of affecting rank
          (for-each (lambda (f) (add-friend! graph f other)) friendlies))
        (else ;; case 2: other's rank may decrease 
          ;; order matters, by adding the the lowest ranked edge first
          ;; we ensure only the first edge updates the rank
          (let ((adopter (or parent (find-min-by-rank friendlies))))
            ;; removing friendlies never changes rank
            (for-each (lambda (f) (remove-friend! graph f node)) friendlies)
            (if parent (remove-parent-edge! graph node ondisconnect: ondisconnect))
            (ssr-add-edge! graph adopter other onconnect: onconnect)
            ;; next edges cannot udpate rank
            (for-each (lambda (f) (when (not (= f adopter)) (add-friend! graph f other))) friendlies)))))))

(define (ssr-connected? graph node)
  (not (= (get-rank graph node) infinity)))

;; tests

#;(define (test)
  (define (make-test-graph source)
    (list->table (list (cons source '()) (cons 'source source))))
  (define (test-graph-edges-for-each f graph)
    (table-for-each
      (lambda (from tos)
        (if (not (eq? from 'source)) 
            (for-each (lambda (to) (f from to)) tos))) graph))
  (define (test-graph-add! graph from to)
    (table-set! graph from (cons to (table-ref graph from '()))))
  (define (test-graph-remove! graph from to)
    (table-set! graph from (filter (lambda (x) (not (= x to))) (table-ref graph from '()))))
  (define (test-graph-redirect! graph node other)
    (define incoming '())
    (test-graph-edges-for-each
      (lambda (from to) (if (eq? to node) (set! incoming (cons from incoming))))
      graph)
    (set! incoming (filter (lambda (f) (not (= f node))) incoming)) ;; remove self redirect
    (for-each
      (lambda (friendly) (test-graph-remove! graph friendly node))
      incoming)
    (for-each
      (lambda (friendly) (test-graph-add! graph friendly other))
      incoming))
  (define (test-graph-rank graph target)
    (let ((queue (make-queue))
          (visited '())
          (source (table-ref graph 'source)))
      (queue-put! queue (cons 0 source))
      (set! visited (cons source visited))
      (let loop ()
        (if (queue-empty? queue)
          infinity
          (let* ((rank-node (queue-get! queue))
                 (rank (car rank-node))
                 (node (cdr rank-node))
                 (children (table-ref graph node '())))
            (if (= node target)
                rank
                (begin
                  (for-each
                    (lambda (child)
                      (when (not (memq child visited))
                        (queue-put! queue (cons (+ rank 1) child))
                        (set! visited (cons child visited))))
                    children)
                  (loop))))))))

  (define param-make      (make-parameter #f))
  (define param-add!      (make-parameter #f))
  (define param-delete!   (make-parameter #f))
  (define param-redirect! (make-parameter #f))
  (define param-rank      (make-parameter #f))

  (define (get-expected-result test . args)
    (parameterize ((param-make make-test-graph)
                   (param-add! test-graph-add!)
                   (param-delete! test-graph-remove!)
                   (param-redirect! test-graph-redirect!)
                   (param-rank test-graph-rank))
      (apply test args)))

  (define (run test . args)
    (parameterize ((param-make make-graph)
                   (param-add! add-edge!)
                   (param-delete! remove-edge!)
                   (param-redirect! redirect!)
                   (param-rank get-rank))
      (apply test args)))

  (define (run-one-fuzzy-test graph-size)
    (define delete-density 0.1)
    (define redirect-density 0.05)

    (define (instruction proc repr)
      (cons proc repr))
    (define (instruction-exec instr graph) ((car instr) graph))
    (define (instruction-repr instr) (cdr instr))

    (define (make-random-instruction)
      (let ((edge (list (random-integer graph-size) (random-integer graph-size)))
            (r (random-real)))
        (cond
          ((< r redirect-density)
            (instruction
              (lambda (graph) (apply (param-redirect!) graph edge))
              (cons 'redirect! edge)))
          ((< r (+ redirect-density delete-density))
            (instruction
              (lambda (graph) (apply (param-delete!) graph edge))
              (cons 'delete! edge)))
          (else
            (instruction
              (lambda (graph) (apply (param-add!) graph edge))
              (cons 'add! edge))))))

    (define (make-random-instructions n)
      (if (= n 0)
        '()
        (cons (make-random-instruction) (make-random-instructions (- n 1)))))

    (define (interpret-instructions instructions)
      (define graph ((param-make) 0))
      (for-each
        (lambda (instr) (instruction-exec instr graph))
        instructions)
      (map (lambda (node) ((param-rank) graph node)) (iota graph-size)))

    (define (find-minimal-example init-instructions)
      (let loop ((instructions init-instructions)
                 (minimal init-instructions))
        (if (null? instructions)
          (if (equal? minimal init-instructions)
            (begin
              (pp (list 'MINIMAL-EXAMPLE:))
              (for-each pp (map instruction-repr minimal)))
            (find-minimal-example minimal))
          (let ((without (filter (lambda (i) (not (eq? i (car instructions)))) minimal)))
            (if (equal?
                  (run interpret-instructions without)
                  (get-expected-result interpret-instructions without))
              (loop (cdr instructions) minimal)
              (loop (cdr instructions) without))))))

    (let* ((nb-instructions (* graph-size (+ 1 (random-integer graph-size))))
           (instructions (make-random-instructions nb-instructions))
           (expected-result (get-expected-result interpret-instructions instructions))
           (result (run interpret-instructions instructions)))
      (or (equal? expected-result result)
          (begin
            (pp '(fuzzy-test FAILED))
            (pp 'OUTPUT:)
            (pp result)
            (pp 'EXPECTED:)
            (pp expected-result)
            ;(pp 'INSTRUCTIONS:)
            ;(for-each pp (map instruction-repr instructions))
            (find-minimal-example instructions)
            #f))))

  (define (fuzzy-test size repetitions)
    (let loop ((i 0))
      (if (< i repetitions)
        (if (run-one-fuzzy-test size) (loop (+ i 1)))
        (pp (list 'fuzzy-test 'OK)))))

  (define (test1)
    (define graph ((param-make) 0))
    ((param-add!) graph 1 2)
    ((param-add!) graph 2 3)
    ((param-add!) graph 3 4)
    ((param-add!) graph 2 4)
    ((param-add!) graph 0 1)
    (map (lambda (n) ((param-rank) graph n)) (iota 5)))

  (define (test2)
    (define graph ((param-make) 0))
    ((param-add!) graph 1 2)
    ((param-add!) graph 2 3)
    ((param-add!) graph 3 4)
    ((param-add!) graph 4 0)
    ((param-add!) graph 0 1)
    (map (lambda (n) ((param-rank) graph n)) (iota 5)))

  (define (test3)
    (define graph ((param-make) 0))
    ((param-add!) graph 3 3)
    ((param-add!) graph 2 3)
    ((param-add!) graph 1 2)
    ((param-add!) graph 0 1)
    ((param-delete!) graph 0 1)
    (map (lambda (n) ((param-rank) graph n)) (iota 4)))

  (define (test4)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 1 2)
    ((param-add!) graph 2 1)
    ((param-delete!) graph 0 1)
    (map (lambda (n) ((param-rank) graph n)) (iota 6)))

  (define (test5)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 1 2)
    ((param-add!) graph 1 3)
    ((param-add!) graph 2 4)
    ((param-add!) graph 3 5)
    ((param-add!) graph 5 4)
    ((param-add!) graph 6 4)
    ((param-delete!) graph 2 4)
    (map (lambda (n) ((param-rank) graph n)) (iota 7)))

  (define (test6)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 1 2)
    ((param-add!) graph 1 3)
    ((param-add!) graph 2 4)
    ((param-add!) graph 3 5)
    ((param-add!) graph 4 6)
    ((param-add!) graph 5 4)
    ((param-add!) graph 6 7)
    ((param-delete!) graph 2 4)
    (map (lambda (n) ((param-rank) graph n)) (iota 8)))

  (define (test7)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 1 2)
    ((param-add!) graph 2 3)
    ((param-add!) graph 2 3)
    ((param-delete!) graph 2 3)
    (map (lambda (n) ((param-rank) graph n)) (iota 4)))

  (define (test8)
    (define graph ((param-make) -1))
    ((param-add!) graph -1 0)
    ((param-add!) graph 0 1)
    ((param-add!) graph 1 2)
    ((param-add!) graph 1 2)
    ((param-delete!) graph 1 2)
    (map (lambda (n) ((param-rank) graph n)) (iota 4 -1)))

  (define (test9)
    (define result1 #f)
    (define result2 #f)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 0 6)
    ((param-add!) graph 1 2)
    ((param-add!) graph 1 3)
    ((param-add!) graph 2 4)
    ((param-add!) graph 3 4)
    ((param-add!) graph 4 5)
    ((param-add!) graph 6 2)
    ((param-add!) graph 6 7)
    ((param-add!) graph 7 3)
    ((param-delete!) graph 0 1)
    (set! result1 (map (lambda (n) ((param-rank) graph n)) (iota 8)))
    ((param-add!) graph 0 1)
    (set! result2 (map (lambda (n) ((param-rank) graph n)) (iota 8)))
    (list result1 result2))

  (define (test10)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 0 2)
    ((param-add!) graph 0 3)
    ((param-add!) graph 0 4)
    ((param-add!) graph 1 5)
    ((param-add!) graph 2 5)
    ((param-add!) graph 3 5)
    ((param-add!) graph 4 5)
    ((param-add!) graph 5 6)
    ((param-add!) graph 6 7)
    ((param-add!) graph 7 8)
    ((param-redirect!) graph 5 8)
    (map (lambda (n) ((param-rank) graph n)) (iota 9)))

  (define (test11)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 0 2)
    ((param-add!) graph 0 3)
    ((param-add!) graph 0 4)
    ((param-add!) graph 1 5)
    ((param-add!) graph 2 5)
    ((param-add!) graph 3 5)
    ((param-add!) graph 4 5)
    ((param-add!) graph 5 6)
    ((param-add!) graph 6 7)
    ((param-add!) graph 7 8)
    ((param-add!) graph 5 5)
    ((param-redirect!) graph 5 8)
    (map (lambda (n) ((param-rank) graph n)) (iota 9)))

  (define (test12)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 0 2)
    ((param-add!) graph 0 3)
    ((param-add!) graph 0 4)
    ((param-add!) graph 1 5)
    ((param-add!) graph 2 5)
    ((param-add!) graph 3 5)
    ((param-add!) graph 4 5)
    ((param-add!) graph 5 6)
    ((param-add!) graph 6 7)
    ((param-add!) graph 7 8)
    ((param-add!) graph 5 5)
    ((param-add!) graph 8 8)
    ((param-add!) graph 8 5)
    ((param-redirect!) graph 5 8)
    (map (lambda (n) ((param-rank) graph n)) (iota 9)))

  (define (test13)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 0 2)
    ((param-add!) graph 0 3)
    ((param-add!) graph 0 4)
    ((param-add!) graph 1 5)
    ((param-add!) graph 2 5)
    ((param-add!) graph 3 5)
    ((param-add!) graph 4 5)
    ((param-redirect!) graph 5 6)
    (map (lambda (n) ((param-rank) graph n)) (iota 7)))

  (define (test14)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 0 2)
    ((param-add!) graph 0 3)
    ((param-add!) graph 0 4)
    ((param-add!) graph 1 5)
    ((param-add!) graph 2 5)
    ((param-add!) graph 3 5)
    ((param-add!) graph 4 5)
    ((param-redirect!) graph 5 0)
    (map (lambda (n) ((param-rank) graph n)) (iota 6)))

  (define (test15)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 1 2)
    ((param-redirect!) graph 2 0)
    ((param-redirect!) graph 0 3)
    (map (lambda (n) ((param-rank) graph n)) (iota 4)))

  (define (test16)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-add!) graph 1 2)
    ((param-redirect!) graph 2 2)
    (map (lambda (n) ((param-rank) graph n)) (iota 3)))

  (define (test17)
    (define graph ((param-make) 0))
    ((param-add!) graph 0 1)
    ((param-redirect!) graph 1 2)
    ((param-add!) graph 0 3)
    ((param-redirect!) graph 3 2)
    ((param-delete!) graph 0 2)
    (map (lambda (n) ((param-rank) graph n)) (iota 4)))

  (define (run-all . tests)
    (for-each
      (lambda (test-data)
        (let* ((name (car test-data))
               (test (cadr test-data))
               (args (cddr test-data))
               (expected (apply get-expected-result test args))
              (result (apply run test args)))
          (if (equal? result expected)
              (pp (list name 'OK))
              (begin
                (pp (list name 'FAILED))
                (pp (list 'EXPECT: expected))
                (pp (list 'OUTPUT: result))))))
      tests))

  (pp '(fuzzy-test 40 500)) (fuzzy-test 40 500)
  (let ()
    (define-macro (list-test name . args) `(list (quote ,name) ,name ,@args))
    (run-all
      (list-test test1)
      (list-test test2)
      (list-test test3)
      (list-test test4)
      (list-test test5)
      (list-test test6)
      (list-test test7)
      (list-test test8)
      (list-test test9)
      (list-test test10)
      (list-test test11)
      (list-test test12)
      (list-test test13)
      (list-test test14)
      (list-test test15)
      (list-test test16)
      (list-test test17))))

#;(test)
