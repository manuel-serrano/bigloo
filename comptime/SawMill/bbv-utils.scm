;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/bbv-utils.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 27 08:57:51 2017                          */
;*    Last change :  Fri Jul 28 09:56:27 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    BB manipulations                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-utils
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch")
   
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
	    saw_bbv-cache)

   (export  (get-bb-mark)
	    (set-max-label! blocks::pair-nil)
	    (genlabel)
	    (block->block-list regs b::block)
	    (remove-temps! b::block)
	    (normalize-goto! b::block)
	    (simplify-branch! b::block)
	    (remove-nop! b::block)
	    (redirect-block! b::blockS old::blockS new::blockS)))

;*---------------------------------------------------------------------*/
;*    replace ...                                                      */
;*---------------------------------------------------------------------*/
(define (replace lst old new)
   (let loop ((l lst))
      (cond
	 ((null? l) l)
	 ((eq? (car l) old) (cons new (cdr l)))
	 (else (cons (car l) (loop (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    *label* ...                                                      */
;*---------------------------------------------------------------------*/
(define *label* 0)
(define (genlabel)
   (set! *label* (+fx 1 *label*))
   *label*)

;*---------------------------------------------------------------------*/
;*    set-max-label! ...                                               */
;*---------------------------------------------------------------------*/
(define (set-max-label! blocks::pair-nil)
   (set! *label* 0)
   (for-each (lambda (b)
		(with-access::block b (label)
		   (when (>fx label *label*)
		      (set! *label* label))))
      blocks))

;*---------------------------------------------------------------------*/
;*    *bb-mark* ...                                                    */
;*---------------------------------------------------------------------*/
(define *bb-mark* -1)

;*---------------------------------------------------------------------*/
;*    get-bb-mark ...                                                  */
;*---------------------------------------------------------------------*/
(define (get-bb-mark)
   (set! *bb-mark* (+fx 1 *bb-mark*))
   *bb-mark*)

;*---------------------------------------------------------------------*/
;*    bbset ...                                                        */
;*---------------------------------------------------------------------*/
(define-struct bbset mark list)

;*---------------------------------------------------------------------*/
;*    make-empty-bbset ...                                             */
;*---------------------------------------------------------------------*/
(define (make-empty-bbset::struct)
   (bbset (get-bb-mark) '()))

;*---------------------------------------------------------------------*/
;*    bbset-in? ...                                                    */
;*---------------------------------------------------------------------*/
(define (bbset-in?::bool block::blockS set::struct)
   (with-access::blockS block (%mark)
      (eq? %mark (bbset-mark set))))

;*---------------------------------------------------------------------*/
;*    bbset-cons ...                                                   */
;*---------------------------------------------------------------------*/
(define (bbset-cons::struct b::blockS set::struct)
   (let ((m (bbset-mark set)))
      (with-access::blockS b (%mark)
	 (set! %mark m))
      (bbset-list-set! set (cons b (bbset-list set)))
      set))

;*---------------------------------------------------------------------*/
;*    bbset-cons* ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbset-cons*::struct b::blockS . rest)
   (cond
      ((null? (cdr rest))
       (bbset-cons b (car rest)))
      ((null? (cddr rest))
       (bbset-cons b (bbset-cons (car rest) (cadr rest))))
      (else
       (bbset-cons b (apply bbset-cons* rest)))))

;*---------------------------------------------------------------------*/
;*    bbset-append ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbset-append::struct blocks::pair-nil set::struct)
   (let ((m (bbset-mark set)))
      (for-each (lambda (b)
		   (with-access::blockS b (%mark)
		      (set! %mark m)))
	 blocks)
      (bbset-list-set! set (append blocks (bbset-list set)))
      set))

;*---------------------------------------------------------------------*/
;*    block->block-list ...                                            */
;*---------------------------------------------------------------------*/
(define (block->block-list regs b::block)
   
   (define (swap-succ? b)
      (with-access::block b (first)
	 (when (pair? first)
	    (rtl_ins-ifne? (car (last-pair first))))))
   
   (define (goto-block? b)
      ;; is a block explicitly jumping to its successor
      (with-access::block b (first)
	 (when (pair? first)
	    (let ((i (car (last-pair first))))
	       (rtl_ins-go? i)))))
   
   (define (fallthru-block? b)
      ;; is a block implicitly followed by its successor
      (not (goto-block? b)))
   
   (define (branch-block? b)
      ;; is a block implicitly followed by its successor
      (with-access::block b (first)
	 (when (pair? first)
	    (let ((i (car (last-pair first))))
	       (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i))))))
   
   (define (make-go-block bs to)
      (duplicate::blockS bs
	 (label (genlabel))
	 (first (list
		   (instantiate::rtl_ins/bbv
		      (fun (instantiate::rtl_go
			      (to to)))
		      (dest #f)
		      (args '())
		      (def (make-empty-regset regs))
		      (in (make-empty-regset regs))
		      (out (make-empty-regset regs)))))
	 (succs (list to))
	 (preds (list bs))))
   
   (let loop ((bs (list b))
	      (acc (make-empty-bbset)))
      (cond
	 ((null? bs)
	  (reverse! (bbset-list acc)))
	 ((bbset-in? (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (succs)
	     (cond
		((null? succs)
		 (loop (cdr bs) (bbset-cons (car bs) acc)))
		((fallthru-block? (car bs))
		 (let* ((lp (last-pair (block-first (car bs))))
			(last (car lp))
			(succs (block-succs (car bs))))
		    (cond
		       ((rtl_ins-ifne? last)
			(cond
			   ((and (rtl_ins-typecheck? last)
				 (not (bbset-in? (car succs) acc)))
			    ;; change the ifne for an ifeq
			    (with-access::rtl_ins last (fun)
			       (set! fun
				  (instantiate::rtl_ifeq
				     (then (cadr succs)))))
			    (loop bs acc))
			   ((bbset-in? (cadr succs) acc)
			    (let ((hop (make-go-block (car bs) (cadr succs))))
			       (with-access::block (car succs) (preds)
				  (set! preds
				     (replace preds (car bs) hop))
				  (set-car! (cdr succs) hop))
			       (loop (cons (car succs) (cdr bs))
				  (bbset-cons* hop (car bs) acc))))
			   (else
			    (loop (append (reverse succs) (cdr bs))
			       (bbset-cons (car bs) acc)))))
		       ((rtl_ins-ifeq? last)
			(if (bbset-in? (car succs) acc)
			    (let ((hop (make-go-block (car bs) (car succs))))
			       (with-access::block (car succs) (preds)
				  (set! preds
				     (replace preds (car bs) hop))
				  (set-car! succs hop))
			       (loop (cons (cadr succs) (cdr bs))
				  (bbset-cons* hop (car bs) acc)))
			    (loop (append succs (cdr bs))
			       (bbset-cons (car bs) acc))))
		       ((bbset-in? (car succs) acc)
			(with-access::block (car bs) (first)
			   (let ((lp (last-pair first))
				 (go (instantiate::rtl_ins/bbv
					(fun (instantiate::rtl_go
						(to (car succs))))
					(dest #f)
					(args '())
					(def (make-empty-regset regs))
					(in (make-empty-regset regs))
					(out (make-empty-regset regs)))))
			      (if (rtl_ins-nop? (car lp))
				  (set-car! lp go)
				  (set-cdr! lp (list go))))
			   (loop (cdr bs) (bbset-cons (car bs) acc))))
		       (else
			(loop (append succs (cdr bs))
			   (bbset-cons (car bs) acc))))))
		((and (goto-block? (car bs)) (not (bbset-in? (car succs) acc)))
		 (with-access::block (car bs) (first)
		    (set! first
		       (if (null? (cdr first))
			   (list (instantiate::rtl_ins/bbv
				    (fun (instantiate::rtl_nop))
				    (dest #f)
				    (args '())
				    (def (make-empty-regset regs))
				    (in (make-empty-regset regs))
				    (out (make-empty-regset regs))))
			   (reverse (cdr (reverse first)))))
		    (loop (append succs (cdr bs))
		       (bbset-cons (car bs) acc))))
		(else
		 (loop (append succs (cdr bs))
		    (bbset-cons (car bs) acc)))))))))

;*---------------------------------------------------------------------*/
;*    normalize-goto! ...                                              */
;*---------------------------------------------------------------------*/
(define (normalize-goto! b::block)

   (define (collapse-branch b first)
      (let ((next (car (block-succs b))))
	 (when (and (rtl_ins-branch? (car (block-first next)))
		    (null? (cdr (block-first next))))
	    (let ((nsuccs (block-succs next)))
	       (if (rtl_ins-go? (car first))
		   (set-car! first (car (block-first next)))
		   (set-cdr! first (block-first next)))
	       (block-preds-set! next (remq! b (block-preds next)))
	       (block-succs-set! b nsuccs)
	       (for-each (lambda (ns)
			    (block-preds-set! ns
			       (replace (block-preds ns) next b)))
		  nsuccs)))))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (with-access::block (car bs) (succs preds first)
	     (let liip ((first first))
		(cond
		   ((null? first)
		    (loop (append succs (cdr bs)) (cons (car bs) acc)))
		   ((rtl_ins-go? (car first))
		    (collapse-branch (car bs) first)
		    (loop (append succs (cdr bs)) (cons (car bs) acc)))
		   ((null? (cdr first))
		    (when (and (not (rtl_ins-branch? (car first)))
			       (pair? (block-succs (car bs))))
		       (collapse-branch (car bs) first))
		    (loop (append succs (cdr bs)) (cons (car bs) acc)))
		   ((rtl_ins-ifeq? (car first))
		    [assert (first) (rtl_ins-go? (cadr first))]
		    (with-access::rtl_ins (car first) (fun)
		       (with-access::rtl_ifeq fun (then)
			  (let ((nb (instantiate::block
				       (label (genlabel))
				       (first (cdr first))
				       (preds (list (car bs)))
				       (succs (list then)))))
			     (set-car! succs nb)
			     (set-cdr! first '())
			     (with-access::block then (preds)
				(set! preds (replace preds (car bs) nb)))
			     (loop (append (reverse succs) (cdr bs))
				(cons (car bs) acc))))))
		   ((rtl_ins-ifne? (car first))
		    [assert (first) (rtl_ins-go? (cadr first))]
		    (with-access::rtl_ins (car first) (fun)
		       (with-access::rtl_ifne fun (then)
			  (let ((nb (instantiate::block
				       (label (genlabel))
				       (first (cdr first))
				       (preds (list (car bs)))
				       (succs (list then)))))
			     (set-car! (cdr succs) nb)
			     (set-cdr! first '())
			     (with-access::block then (preds)
				(set! preds (replace preds (car bs) nb)))
			     (loop (append (reverse succs) (cdr bs))
				(cons (car bs) acc))))))
		   (else
		    (liip (cdr first))))))))))

;*---------------------------------------------------------------------*/
;*    remove-temps! ...                                                */
;*---------------------------------------------------------------------*/
(define (remove-temps! b::block)

   (define (remove-ins-temps! i)
      (with-access::rtl_ins i (args)
	 (let loop ((args args))
	    (if (null? args)
		i
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
	  (with-access::blockS (car bs) (succs first octxs)
	     ;; remove useless nop instructions
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
			(set! octxs (blockS-octxs s))
			(loop bs acc))
		     (loop (cons (car succs) (cdr bs))
			(bbset-cons (car bs) acc)))
		 (let liip ((ss succs)
			    (nsuccs '()))
		    (if (null? ss)
			(loop (append (reverse succs) (cdr bs))
			   (bbset-cons (car bs) acc))
			(let ((s (car ss)))
			   (if (goto-block? s)
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
;*    redirect-block! ...                                              */
;*---------------------------------------------------------------------*/
(define (redirect-block! b::blockS old::blockS new::blockS)
   (with-trace 'bbv "redirect-block"
      (trace-item "b=" (block-label b))
      (trace-item "old=" (block-label old) " "
	 (map block-label (block-succs old)))
      (trace-item "new="(block-label new) " "
	 (map block-label (block-succs new)))
      (with-access::blockS b (succs first)
	 (set! succs (replace succs old new))
	 (with-access::block old (preds)
	    (set! preds (remq! b preds)))
	 (with-access::block new (preds)
	    (set! preds (cons b preds)))
	 (let ((last (car (last-pair first))))
	    (cond
	       ((rtl_ins-ifeq? last)
		(with-access::rtl_ins last (fun)
		   (with-access::rtl_ifeq fun (then)
		      (when (eq? then old)
			 (set! then new)))))
	       ((rtl_ins-ifne? last)
		(with-access::rtl_ins last (fun)
		   (with-access::rtl_ifne fun (then)
		      (when (eq? then old)
			 (set! then new)))))
	       ((rtl_ins-go? last)
		(with-access::rtl_ins last (fun)
		   (with-access::rtl_go fun (to)
		      (set! to new))))))))
   b)

