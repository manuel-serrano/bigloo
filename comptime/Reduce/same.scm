;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Reduce/same.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul  5 12:54:32 1996                          */
;*    Last change :  Sun Jan 21 03:52:42 2001 (serrano)                */
;*    Copyright   :  1996-2001 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The comparison of two nodes.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module reduce_same
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node)
   (export  (generic same-node?::bool ::node ::node alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (same-node?::bool node1::node node2::node alias)
   #f)

;*---------------------------------------------------------------------*/
;*    same-node? ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::atom node2::node alias)
   (trace (reduce 3) "***cse:same-node?(atom): " #\Newline
	  "   " (shape node) #\Newline
	  "   " (shape node2) #\Newline)
   (and (atom? node2)
	(eq? (node-type node) (node-type node2))
	(equal? (atom-value node) (atom-value node2))))

;*---------------------------------------------------------------------*/
;*    same-node? ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::kwote node2::node alias)
   (trace (reduce 3) "***cse:same-node?(kwote): " #\Newline
	  "   " (shape node) #\Newline
	  "   " (shape node2) #\Newline)
   (and (kwote? node2)
	(eq? (node-type node) (node-type node2))
	(equal? (kwote-value node) (kwote-value node2))))

;*---------------------------------------------------------------------*/
;*    same-node? ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::var node2::node alias)
   (trace (reduce 3) "***cse:same-node?(var): " #\Newline
	  "   " (shape node) #\Newline
	  "   " (shape node2) #\Newline)
   (and (var? node2)
	(eq? (variable-access (var-variable node)) 'read)
	(eq? (node-type node) (node-type node2))
	(or (eq? (var-variable node) (var-variable node2))
	    (aliased? (var-variable node) (var-variable node2) alias))))

;*---------------------------------------------------------------------*/
;*    same-node? ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::sequence node2::node alias)
   (and (sequence? node2)
	(eq? (node-type node) (node-type node2))
	(same-node*? (sequence-nodes node)
		     (sequence-nodes node2)
		     alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::app-ly node2::node alias)
   (and (app-ly? node2)
	(eq? (node-type node) (node-type node2))
	(same-node? (app-ly-fun node) (app-ly-fun node2) alias)
	(same-node? (app-ly-arg node) (app-ly-arg node2) alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::funcall node2::node alias)
   (and (funcall? node2)
	(eq? (node-type node) (node-type node2))
	(same-node? (funcall-fun node) (funcall-fun node2) alias)
	(same-node*? (funcall-args node) (funcall-args node2) alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::app node2::node alias)
   (and (app? node2)
	(eq? (node-type node) (node-type node2))
	(same-node? (app-fun node) (app-fun node2) alias)
	(same-node*? (app-args node) (app-args node2) alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::extern node2::node alias)
   (trace (reduce 3) "***cse:same-node?(extern): " #\Newline
	  "   " (shape node) #\Newline
	  "   " (shape node2) #\Newline)
   (and (extern? node2)
	(eq? (node-type node) (node-type node2))
	(same-node*? (extern-expr* node) (extern-expr* node2) alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::pragma ...                                          */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::pragma node2::node alias)
   (and (pragma? node2)
	(eq? (node-type node) (node-type node2))
	(string=? (pragma-format node) (pragma-format node2))
	(same-node*? (pragma-expr* node) (pragma-expr* node2) alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::getfield ...                                        */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::getfield node2::node alias)
   (and (getfield? node2)
	(string=? (getfield-fname node) (getfield-fname node2))
	(call-next-method)))
	
;*---------------------------------------------------------------------*/
;*    same-node? ::setfield ...                                        */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::setfield node2::node alias)
   (and (setfield? node2)
	(string=? (setfield-fname node) (setfield-fname node2))
	(call-next-method)))

;*---------------------------------------------------------------------*/
;*    same-node? ::valloc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::valloc node2::node alias)
   #f)

;*---------------------------------------------------------------------*/
;*    same-node? ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::cast node2::node alias)
   (and (cast? node2)
	(eq? (node-type node) (node-type node2))
	(same-node? (cast-arg node) (cast-arg node2) alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::conditional node2::node alias)
   (trace (reduce 3) "***cse:same-node?(conditional): " #\Newline
	  "   " (shape node) #\Newline
	  "   " (shape node2) #\Newline)
   (and (conditional? node2)
	(eq? (node-type node) (node-type node2))
	(same-node? (conditional-false node) (conditional-false node2) alias)
	(same-node? (conditional-true node) (conditional-true node2) alias)
	(same-node? (conditional-test node) (conditional-test node2) alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::fail node2::node alias)
   (trace (reduce 3) "***cse:same-node?(fail): " #\Newline
	  "   " (shape node) #\Newline
	  "   " (shape node2) #\Newline)
   (and (fail? node2)
	(eq? (node-type node) (node-type node2))
	(same-node? (fail-msg node) (fail-msg node2) alias)
	(same-node? (fail-proc node) (fail-proc node2) alias)
	(same-node? (fail-obj node) (fail-obj node2) alias)))

;*---------------------------------------------------------------------*/
;*    same-node? ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (same-node? node::let-var node2::node alias)
   (trace (reduce 3) "***cse:same-node?(let-var) : " #\Newline
	  "   node1: " (shape node) #\Newline
	  "   node2: " (shape node2) #\Newline
	  "   alias: " (shape alias) #\Newline)
   (and (let-var? node2)
	(eq? (node-type node) (node-type node2))
	(eq? (length (let-var-bindings node))
	     (length (let-var-bindings node2)))
	(let ((new-alias (append! (map (lambda (b1 b2)
					       (cons (car b1) (car b2)))
					    (let-var-bindings node)
					    (let-var-bindings node2))
				  alias)))
	   (and (same-node? (let-var-body node) (let-var-body node2) new-alias)
		(let loop ((bindings1 (let-var-bindings node))
			   (bindings2 (let-var-bindings node2)))
		   (cond
		      ((null? bindings1)
		       #t)
		      ((not
			(and (eq? (local-access (car (car bindings1))) 'read)
			     (eq? (local-access (car (car bindings2))) 'read)))
		       #f)
		      ((same-node? (cdr (car bindings1))
				   (cdr (car bindings2))
				   alias)
		       (loop (cdr bindings1) (cdr bindings2)))
		      (else
		       #f)))))))

;*---------------------------------------------------------------------*/
;*    same-node*? ...                                                  */
;*---------------------------------------------------------------------*/
(define (same-node*? node1 node2 alias)
   (cond
      ((null? node1)
       (null? node2))
      ((null? node2)
       #f)
      ((same-node? (car node1) (car node2) alias)
       (same-node*? (cdr node1) (cdr node2) alias))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    aliased? ...                                                     */
;*---------------------------------------------------------------------*/
(define (aliased? var1 var2 alias)
   (let loop ((alias alias))
      (cond
	 ((null? alias)
	  #f)
	 ((eq? (car (car alias)) var1)
	  (if (eq? (cdr (car alias)) var2)
	      #t
	      (loop (cdr alias))))
	 ((eq? (car (car alias)) var2)
	  (if (eq? (cdr (car alias)) var1)
	      #t
	      (loop (cdr alias))))
	 (else
	  (loop (cdr alias))))))
