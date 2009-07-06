(module saw_rtl
   (import type_type	;; needed for ast_var !!
	   ast_var	;; local/global
	   ast_node	;; node
	   saw_lib
	   saw_defs
    )
   (export

    ;; Exported functions
    (name::symbol ins::rtl_ins)
    (neighbours::pair ins::rtl_ins)
    (unlink! b::block)
    (rtl_dfs ins::block visit visit?))
;    (duplicate!::pair-nil ins::rtl_ins)
   )

;;
;; The symbolic name of an instruction
;;
(define (name::symbol ins::rtl_ins) ;()
   (class-name (object-class (rtl_ins-fun ins))) )

;;
;; A little around an instruction
;;
(define (neighbours::pair ins::rtl_ins) ;()
   (with-access::rtl_ins ins (dest fun args)
      (list '(froms)
	    '->
	    (name ins)
	    '->
	    '(to) )))

;;
;; Unlink a basic block
;;  ** preds and succs of the basic block remain unchanged
;;
(define (unlink! b::block)
   (with-access::block b (preds succs)
      (if (or (null? succs) (pair? (cdr succs)))
	  (error 'unlink! "must have only one successor" b) )
      (let ( (s (car succs)) )
	 (for-each (lambda (p) (with-access::block p (succs)
				     (set! succs (subst succs b s)) ))
		   preds )
	 (block-preds-set! s (subst-append (block-preds s) b preds)) )))

;;
;; Copy an instruction without consideration of edges
;;
(define (copy::rtl_ins ins::rtl_ins) ;()
   (with-access::rtl_ins ins (dest fun args)
      (instantiate::rtl_ins (dest dest) (fun fun) (args args)) ))

;;
;; Duplicate an instruction
;;
;(define (duplicate!::pair-nil ins::rtl_ins) ;()
;   (with-access::rtl_ins ins (prev next)
;      (if (not (block? prev))
;	  (error 'duplicate! "must be the first instruction of a block"
;		 ins ))
;      (define (dup1 p)
;	 (let ( (new (copy ins)) )
;	    ;; substitute ins by new in p's continuations.
;	    (with-access::rtl_ins p (conts)
;	       (set! conts (subst conts ins new)) )
;	    ;; The new instruction have only one predecessor.
;	    (rtl_ins-froms-set! new (cons p '()))
;	    ;; The new instruction have the same successors.
;	    (rtl_ins-conts-set! new conts)
;	    ;; Update the predecessors of the sucessors.
;	    (for-each (lambda (s) (with-access::rtl_ins s (froms)
;				     (set! froms (cons new froms)) ))
;		      conts )
;	    ;; Return the new allocated instruction.
;	    new ))
;      ;; CARE ok ok boys we can save one duplication but this not (yet)
;      ;;      the instant for time compilation optimization.
;      (for-each (lambda (s) (with-access::rtl_ins s (froms)
;			       (set! froms (remq ins froms)) ))
;		conts )
;      (map dup1 froms) ))

;;
;; Depth traveral of the graph
;;
(define (rtl_dfs b::block visit visit?) ;::(block->void)::(block->bool)
   (visit b)
   (for-each (lambda (succ)
		(if (not (visit? succ)) (rtl_dfs succ visit visit?)) )
	     (block-succs b) ))
