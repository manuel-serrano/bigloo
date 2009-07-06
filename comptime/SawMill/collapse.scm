(module saw_collapse
   (import type_type ast_var ast_node
	   saw_lib
	   saw_defs
	   )
   (export (collapse b::block))
   (static (wide-class collapsed::block last)) )

;;
;; Do real basic blocks where the following property becomes true
;; (=> (= (length b.preds) 1) (!= (length (car b.preds).succs) 1))
;;
(define (collapse b::block) ;()
   (let dfs ( (b b) )
      (with-access::block b (preds succs first)
	 (widen!::collapsed b (last (last-pair first)))
	 (if (and (pair? preds) (null? (cdr preds)))
	     (let* ( (p (car preds)) (ps (block-succs p)) )
		;; (assert (collapsed? p)) 16.61 -> 8.89
		(if (null? (cdr ps))
		    (begin
		       ;(set-cdr! (last-pair (block-first p)) first)
		       (set-cdr! (collapsed-last p) first)
		       (collapsed-last-set! p (collapsed-last b))
		       (block-succs-set! p succs)
		       (for-each (lambda (s)
				    (with-access::block s (preds)
				       (set! preds (subst preds b p)) ))
				 succs )))))
	 (for-each (lambda (succ::block)
		      (if (not (collapsed? succ)) (dfs succ)) )
		   succs )))
   #unspecified )
