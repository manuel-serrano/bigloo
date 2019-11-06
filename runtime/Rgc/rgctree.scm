;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Rgc/rgctree.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep  9 17:51:46 1998                          */
;*    Last change :  Sun Aug 25 09:11:40 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The construction of the tree from the list representation.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc_tree
   
   (import  __rgc_set
	    __rgc_config
	    __error)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __param
	    __object
	    __thread
	    __bexit
	    __bignum
	    __rgc
	    __bit
	    
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
	    __r5_control_features_6_4
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r4_vectors_6_8)

   (include "Rgc/rgc-node.sch")
   
   (export  (regular-tree->node tree)
	    (print-node node)
	    (print-followpos followpos)
	    (reset-tree!)))

;*---------------------------------------------------------------------*/
;*    regular-tree->node ...                                           */
;*---------------------------------------------------------------------*/
(define (regular-tree->node tree)
   ;; the first think to compute is the number of position in that tree.
   ;; that number is simply the number of char we find.
   (set! *position-number* (regular-tree-position-number tree))
   ;; we initialize the position vector
   (init-positions!)
   ;; we initialize the vector that will holds the followpos
   (init-followpos!)
   (let ((tree (tree->node tree)))
      ;; we are done with the tree we return the tree and the followpos
      (values tree *followpos* *positions* *submatches*)))

;*---------------------------------------------------------------------*/
;*    *position-number* & *positions* ...                              */
;*    -------------------------------------------------------------    */
;*    The number of positions the compiled tree may holds and a vector */
;*    making the association between the position and the character at */
;*    that position.                                                   */
;*---------------------------------------------------------------------*/
(define *position-number* #unspecified)
(define *positions* #unspecified)
(define *submatches* #unspecified)

;*---------------------------------------------------------------------*/
;*    regular-tree-position-number ...                                 */
;*---------------------------------------------------------------------*/
(define (regular-tree-position-number tree)
   (let loop ((tree tree)
	      (num  0))
      (cond
	 ((null? tree)
	  num)
	 ((pair? (car tree))
	  (loop (cdr tree) (loop (car tree) num)))
	 ((fixnum? (car tree))
	  (loop (cdr tree) (+fx num 1)))
	 (else
	  (loop (cdr tree) num)))))

;*---------------------------------------------------------------------*/
;*    init-positions! ...                                              */
;*---------------------------------------------------------------------*/
(define (init-positions!)
   (set! *current-position* -1)
   (set! *positions* (make-vector *position-number* -1))
   (set! *submatches* (make-vector *position-number* '())))

;*---------------------------------------------------------------------*/
;*    *current-position* ...                                           */
;*---------------------------------------------------------------------*/
(define *current-position* #unspecified)

;*---------------------------------------------------------------------*/
;*    get-new-position ...                                             */
;*---------------------------------------------------------------------*/
(define (get-new-position char)
   (set! *current-position* (+fx *current-position* 1))
   (vector-set! *positions* *current-position* char)
   *current-position*)

;*---------------------------------------------------------------------*/
;*    tree->node ...                                                   */
;*---------------------------------------------------------------------*/
(define (tree->node tree)
   (cond
      ((fixnum? tree)
       (integer->node tree))
      ((eq? tree 'epsilon)
       (epsilon->node tree))
      ((not (pair? tree))
       (error #f "RGC:Illegal tree" tree))
      (else
       (case (car tree)
	  ((or)       (or->node (cdr tree)))
	  ((sequence) (sequence->node (cdr tree)))
	  ((*)        (*->node (cadr tree)))
	  ((submatch) (submatch->node (cdr tree)))
	  ((bol)      (bol->node (cdr tree)))
	  (else       (error #f "RGC:Unknown function" tree))))))

;*---------------------------------------------------------------------*/
;*    integer->node ...                                                */
;*---------------------------------------------------------------------*/
(define (integer->node tree)
   (let ((position (get-new-position tree))
	 (firstpos (make-rgcset *position-number*))
	 (lastpos  (make-rgcset *position-number*)))
      (rgcset-add! firstpos position)
      (rgcset-add! lastpos position)
      (node firstpos lastpos #f)))

;*---------------------------------------------------------------------*/
;*    epsilon->node ...                                                */
;*---------------------------------------------------------------------*/
(define (epsilon->node tree)
   (let ((firstpos (make-rgcset *position-number*))
	 (lastpos  (make-rgcset *position-number*)))
      (node firstpos lastpos #t)))

;*---------------------------------------------------------------------*/
;*    binary->node ...                                                 */
;*---------------------------------------------------------------------*/
(define (binary->node bin-op ts)
   (if (null? ts)
       (tree->node 'epsilon)
       (let loop ((ts ts))
	  (if (null? (cdr ts))
	      (tree->node (car ts))
	      (bin-op (tree->node (car ts)) (loop (cdr ts)))))))
   
;*---------------------------------------------------------------------*/
;*    or->node ...                                                     */
;*---------------------------------------------------------------------*/
(define (or->node ts)
   (define (or2->node n1 n2)
	 (let* ((firstpos  (rgcset-or (node-firstpos n1) (node-firstpos n2)))
		(lastpos   (rgcset-or (node-lastpos n1) (node-lastpos n2)))
		(nullable? (or (node-nullable? n1) (node-nullable? n2))))
	    (node firstpos lastpos nullable?)))
   (binary->node or2->node ts))

;*---------------------------------------------------------------------*/
;*    sequence->node ...                                               */
;*---------------------------------------------------------------------*/
(define (sequence->node ts)
   (define (sequence2->node n1 n2)
      (let ((firstpos  (if (node-nullable? n1)
			   (rgcset-or (node-firstpos n1) (node-firstpos n2))
			   (node-firstpos n1)))
	    (lastpos   (if (node-nullable? n2)
			   (rgcset-or (node-lastpos n1) (node-lastpos n2))
			   (node-lastpos n2)))
	    (nullable? (and (node-nullable? n2) (node-nullable? n1))))
	 ;; we adjust the followpos
	 (for-each-rgcset (lambda (i)
			     (followpos-add! i (node-firstpos n2)))
			  (node-lastpos n1))
	 ;; we are node done
	 (node firstpos lastpos nullable?)))
   (binary->node sequence2->node ts))
		 
;*---------------------------------------------------------------------*/
;*    *->node ...                                                      */
;*---------------------------------------------------------------------*/
(define (*->node expr)
   (let* ((sub-node (tree->node expr))
	  (firstpos (node-firstpos sub-node))
	  (lastpos  (node-lastpos sub-node)))
      ;; we set the followpos property
      (for-each-rgcset (lambda (i)
			  (followpos-add! i firstpos))
		       lastpos)
      ;; and we return the new node
      (node firstpos lastpos #t)))

;*---------------------------------------------------------------------*/
;*    submatch->node ...                                               */
;*---------------------------------------------------------------------*/
(define (submatch->node expr)
   (match-case expr
      ((?rule ?submatch ?expr)
       (let* ((node      (tree->node expr))
	      (firstpos  (node-firstpos node))
	      (lastpos   (node-lastpos node))
	      (nullable? (node-nullable? node)))
;* 	  (print "submatch: " rule " " submatch " " expr)              */
;* 	  (print "firstpos: " (rgcset->list firstpos))                 */
;* 	  (print "lastpos : " (rgcset->list lastpos))                  */
;* 	  (print "nullable: " nullable?)                               */
	  (for-each-rgcset (lambda (i)
			      (submatch-start-add! i nullable? rule submatch))
			   firstpos)
	  (for-each-rgcset (lambda (i)
			      (submatch-stop-add! i rule submatch))
			   lastpos)
	  node))
      (else
       (error #f "RGC:Unknown function" expr))))

;*---------------------------------------------------------------------*/
;*    bol->node ...                                                    */
;*---------------------------------------------------------------------*/
(define (bol->node expr)
   (let* ((node     (tree->node expr))
	  (firstpos (node-firstpos node))
	  (lastpos  (node-lastpos node)))
      (node firstpos lastpos #f)))
	 
;*---------------------------------------------------------------------*/
;*    init-followpos! ...                                              */
;*---------------------------------------------------------------------*/
(define (init-followpos!)
   (let ((followpos (make-vector *position-number*)))
      (let loop ((i 0))
	 (if (=fx *position-number* i)
	     (set! *followpos* followpos)
	     (begin
		(vector-set! followpos i (make-rgcset *position-number*))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    *followpos* ...                                                  */
;*---------------------------------------------------------------------*/
(define *followpos* #unspecified)

;*---------------------------------------------------------------------*/
;*    followpos-add! ...                                               */
;*---------------------------------------------------------------------*/
(define (followpos-add! i rgcset)
   (rgcset-or! (vector-ref *followpos* i) rgcset))

;*---------------------------------------------------------------------*/
;*    submatch-start-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (submatch-start-add! position nullable? match submatch)
   (let ((cell (vector-ref *submatches* position)))
      (if (not (pair? cell))
	  (vector-set! *submatches*
		       position
		       (cons (list (list nullable? match submatch)) '()))
	  (set-car! cell (cons (list nullable? match submatch) (car cell))))))
		 
;*---------------------------------------------------------------------*/
;*    submatch-stop-add! ...                                           */
;*---------------------------------------------------------------------*/
(define (submatch-stop-add! position match submatch)
   (let ((cell (vector-ref *submatches* position)))
      (if (not (pair? cell))
	  (vector-set! *submatches*
		       position
		       (cons '() (list (cons match submatch))))
	  (set-cdr! cell (cons (cons match submatch) (cdr cell))))))
		 
;*---------------------------------------------------------------------*/
;*    reset-tree! ...                                                  */
;*---------------------------------------------------------------------*/
(define (reset-tree!)
   (set! *followpos* #unspecified)
   (set! *positions* #unspecified)
   (set! *submatches* #unspecified)
   (set! *position-number* #unspecified))

;*---------------------------------------------------------------------*/
;*    print-followpos ...                                              */
;*---------------------------------------------------------------------*/
(define (print-followpos fp)
   (print "========= FOLLOWPOS ==============================")
   (print "number of pos: " (vector-length fp))
   '(let ((sz (vector-length fp)))
      (let loop ((i 0))
	 (if (<fx i sz)
	     (begin
		(print i ": " (reverse (rgcset->list (vector-ref fp i))))
		(loop (+fx i 1))))))
   (print "=================================================="))

;*---------------------------------------------------------------------*/
;*    print-node ...                                                   */
;*---------------------------------------------------------------------*/
(define (print-node node)
   'blop)
   
