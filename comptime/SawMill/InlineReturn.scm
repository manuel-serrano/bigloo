(module saw_inline_return
   (import type_type ast_var ast_node
	   saw_lib saw_defs )
   (export (inline-returns b::block))
   (static (wide-class dfs::block)) )

(define (inline-returns b::block) ;()
   (for-each inline-return (find-returns b)) )

;;
;; Inline returns in predecessors
;;
;; CARE put the level of optimization!!
(define *inline-depth* 2)

(define (inline-return b::block) ;()
   (define (simple-return? l n)
      (let ( (r (cdr l)) )
	 (or (null? r)
	     (and (rtl_mov? (rtl_ins-fun (car l)))
		  (simple-return? r n) )
	     (and (> n 0)
		  (simple-return? r (-fx n 1)) ))))
   (define (realloc l rmap)
      (if (null? l)
	  l
	  (let ( (dest (rtl_ins-dest (car l))) )
	     (if dest (set! rmap (cons (cons dest (copy-reg dest)) rmap)))
	     (cons (rename-ins (car l) rmap)
		   (realloc (cdr l) rmap) ))))
   (define (inline! b::block l s::block)
      (block-first-set! b (realloc (append (block-first b) l) '()))
      (block-succs-set! b '())
      (block-preds-set! s (remq! b (block-preds s)))
      (inline-return b) )
   (let ( (l (block-first b)) )
      (if (simple-return? l *inline-depth*)
	  (for-each (lambda (p) (if (null? (cdr (block-succs p)))
				    (inline! p l b) ))
		    (block-preds b) )) ))

;;
;; Find the return points.
;;
(define (find-returns b::block) ;()
   (let ( (rets '()) )
      (let dfs ( (b b) )
	 (widen!::dfs b)
	 (let ( (fun (rtl_ins-fun (car (last-pair (block-first b))))) )
	    (if (rtl_last? fun) (set! rets (cons b rets))) )
	 (for-each (lambda (s) (if (not (dfs? s)) (dfs s))) (block-succs b)) )
      rets ))

;; SawLib
(define (copy-reg r)
   (with-access::rtl_reg r (type var)
      (instantiate::rtl_reg (type type) (var var)) ))

(define (rename-ins ins rmap)
   (define (rename x al)
      (let ( (slot (assq x al)) )
	 (if slot (cdr slot) x) ))
   (with-access::rtl_ins ins (loc dest fun args)
      (instantiate::rtl_ins (loc loc)
			    (dest (and dest (rename dest rmap)))
			    (fun fun)
			    (args (map (lambda (a) (rename a rmap)) args)) )))
