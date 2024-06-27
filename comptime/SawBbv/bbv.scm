;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/SawBbv/bbv.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 10:05:41 2017                          */
;*    Last change :  Thu Jun 27 15:08:04 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Basic Blocks Versioning experiment.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv
   
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
	    saw_bbv-optim
	    saw_bbv-debug
	    saw_bbv-profile)

   (export  (bbv::pair-nil ::backend ::global ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    bbv ...                                                          */
;*---------------------------------------------------------------------*/
(define (bbv back global params blocks)
   (if (and *saw-bbv?*
	    (or (null? *saw-bbv-functions*)
		(memq (global-id global) *saw-bbv-functions*))
	    (>=fx *max-block-merge-versions* 1))
       (with-trace 'bbv (global-id global)
	  (start-bbv-cache!)
	  (verbose 2 "        bbv " (global-id global)
	     " (" *max-block-merge-versions*
	     (if *bbv-optim-vlength* " vlen" "")
	     (if *bbv-optim-alias* " alias" "")
	     (if *bbv-blocks-cleanup* " cleanup" "")
	     ")")
	  (when *bbv-dump-cfg*
	     (dump-cfg global params blocks ".plain.cfg"))
	  (set-max-label! blocks)
	  ;; After the reorder-succs! pass, the succs list order is meaningful.
	  ;; The first successor is the target of the last go instruction
	  ;; (sometimes implicit). If any second successor, it is the target
	  ;; of the conditional branch of the instruction.
	  (reorder-succs! blocks)
	  (when *bbv-dump-cfg*
	     (dump-cfg global params blocks ".reorder.cfg"))
	  ;; replace the possible go instruction that follows an error call
	  (fail! (car blocks))
	  (when *bbv-dump-cfg*
	     (dump-cfg global params blocks ".failure.cfg"))
	  ;; there are several form of type checks, normalize them to ease
	  ;; the bbv algorithm
	  (normalize-typecheck! (car blocks))
	  (when *bbv-dump-cfg*
	     (dump-cfg global params blocks ".typecheck.cfg"))
	  (let ((blocks (normalize-goto! (remove-temps! (car blocks)))))
	     (when *bbv-dump-cfg*
		(dump-cfg global params blocks ".goto.cfg"))
	     (let ((blocks (normalize-ifeq! (car blocks))))
		(when *bbv-dump-cfg*
		   (dump-cfg global params blocks ".ifeq.cfg"))
		(let ((blocks (normalize-mov! (car blocks))))
		   (when *bbv-dump-cfg*
		      (dump-cfg global params blocks ".mov.cfg"))
		   (let ((regs (bbv-liveness! back blocks params)))
		      ;; liveness also widen each block into a blockV
		      (mark-merge! (car blocks))
		      (when *bbv-dump-cfg*
			 (dump-cfg global params blocks ".liveness.cfg"))
		      (unwind-protect
			 (if (null? blocks)
			     '()
			     (let* ((s (dump-blocks 1 "specialize" global params regs #f
					  (bbv-root-block (car blocks)
					     (params->ctx params))))
				    (__a (when *bbv-log*
					   (log-blocks global params blocks)))
				    (history (when *bbv-dump-json*
						(log-blocks-history global params blocks)))
				    (pr (dump-blocks 1 "profile" global params regs history
					   (profile! s)))
				    (as (dump-blocks 1 "assert" global params regs history
					   (assert-context! pr)))
				    (b (block->block-list regs
					  (if *bbv-blocks-cleanup*
					      (dump-blocks 1 "simplify2" global params regs history
						 (simplify-branch! global
						    (dump-blocks 1 "nop" global params regs history
						       (remove-nop! global
							  (dump-blocks 1 "goto" global params regs history
							     (remove-goto! global
								(dump-blocks 1 "simplify1" global params regs history
								   (simplify-branch! global
								      (dump-blocks 1 "coalesce" global params regs history
									 (coalesce! global
									    (gc! as)))))))))))
					      as))))
				(verbose 3 " "
				   (length blocks) " -> " (length b))
				(verbose 2 "\n")
				(dump-blocks 1 "bbv" global params regs history s)
				(map! (lambda (b) (shrink! b)) b)
				b))
			 ;; don't shrink, otherwise dump could no longer
			 ;; be used
			 (unless (or (>=fx *compiler-debug* 1)
				     (>=fx *trace-level* 1))
			    (for-each (lambda (r) (shrink! r)) regs))))))))
       blocks))

;*---------------------------------------------------------------------*/
;*    dump-blocks ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-blocks::blockS level::long name global::global params::pair-nil regs::pair-nil history b::blockS)
   (when (and *bbv-dump-cfg* (or (>=fx *trace-level* level) (=fx level 1)))
      (dump-cfg global params
	 (block->block-list regs b) (format ".~a.cfg" name)))
   (when (and *bbv-dump-json* (or (>=fx *trace-level* level) (=fx level 1)))
      (dump-json-cfg global params history
         (block->block-list regs b) (format ".~a.json" name)))
   b)

;*---------------------------------------------------------------------*/
;*    mark-merge! ...                                                  */
;*    -------------------------------------------------------------    */
;*    Mark the BB where merge is allowed. These blocks are the         */
;*    loop heads.                                                      */
;*---------------------------------------------------------------------*/
(define (mark-merge! block::blockV)
   (with-trace 'bbv "mark-merge!"
      (let loop ((block block)
		 (stack '()))
	 (with-access::blockV block (merge label succs)
	    (cond
	       ((not (eq? merge #unspecified))
		#unspecified)
	       ((memq block stack)
		(trace-item "mark merge " label)
		(set! merge #t))
	       (else
		(let ((nstack (cons block stack)))
		   (for-each (lambda (n)
				(loop n nstack))
		      succs)
		   (unless (eq? merge #t)
		      (set! merge #f)))))))))

;*---------------------------------------------------------------------*/
;*    reorder-succs! ...                                               */
;*---------------------------------------------------------------------*/
(define (reorder-succs! blocks)
   (with-trace 'bbv "reorder-succss!"
      (when (and (pair? blocks) (pair? (cdr blocks)))
	 (let loop ((bs blocks))
	    (when (pair? (cdr bs))
	       (let ((b (car bs))
		     (n (cadr bs)))
		  (with-access::block b (succs first)
		     (when (pair? succs)
			(unless (or (rtl_ins-go? (car (last-pair first)))
				    (rtl_ins-switch? (car (last-pair first))))
			   (set! succs (cons n (remq! n succs))))))
		  (loop (cdr bs))))))))

;*---------------------------------------------------------------------*/
;*    gc! ...                                                          */
;*    -------------------------------------------------------------    */
;*    Coalesce requires dangling blocks to be removed from             */
;*    parent.versions. This pass implements a simple mark&sweep        */
;*    collectors for basic blocks.                                     */
;*---------------------------------------------------------------------*/
(define (gc! b::blockS)
   
   (define (mark b)
      (let loop ((bs (list b))
		 (set (make-empty-bbset)))
	 (cond
	    ((null? bs)
	     set)
	    ((bbset-in? (car bs) set)
	     (loop (cdr bs) set))
	    (else
	     (with-access::blockS (car bs) (succs)
		(when *bbv-debug*
		   (assert-block (car bs) "gc!"))
		(loop (append succs (cdr bs))
		   (bbset-cons (car bs) set)))))))
   
   (define (collect! b setm)
      (let loop ((bs (list b))
		 (set '()))
	 (cond
	    ((null? bs)
	     set)
	    ((memq (car bs) set)
	     (loop (cdr bs) set))
	    (else
	     (with-access::blockS (car bs) (succs parent preds)
		(set! preds (filter (lambda (b) (bbset-in? b setm)) preds))
		(with-access::blockV parent (versions)
		   (set! versions
		      (filter (lambda (b) (bbset-in? b setm)) versions)))
		(loop (append succs (cdr bs))
		   (cons (car bs) set)))))))

   (when (or (eq? *bbv-blocks-cleanup* #t)
	     (and (string? *bbv-blocks-cleanup*)
		  (or (string-contains *bbv-blocks-cleanup* "coalesce")
		      (string-contains *bbv-blocks-cleanup* "gc"))))
      (collect! b (mark b)))
   b)

;*---------------------------------------------------------------------*/
;*    normalize-ifeq! ...                                              */
;*    -------------------------------------------------------------    */
;*    Replace rtl_ifeq with rtl_ifne. After this pass, no rtl_ifeq     */
;*    remain.                                                          */
;*---------------------------------------------------------------------*/
(define (normalize-ifeq!::pair b::block)

   (define (side-effect-free? ins)
      (with-access::rtl_ins ins (args)
	 (when (and (isa? (car args) rtl_ins) (rtl_ins-call? (car args)))
	    (when (rtl_call-predicate (car args))
	       (let ((args (rtl_ins-args* ins)))
		  (and (pair? args) (null? (cdr args)) (rtl_reg? (car args))))))))
   
   (define (normalize-block! b::block)
      (with-access::block b (succs first)
	 (when (pair? succs)
	    ;; the block ends with either if_eq or if_ne
	    (let loop ((ins first))
	       (cond
		  ((null? ins)
		   (when (null? first)
		       (error "normalize-ifeq!" "bad block (no instruction)"
			  (shape b))))
		  ((rtl_ins-ifne? (car ins))
		   (if (and (null? (cdr succs)) (side-effect-free? (car ins)))
		       (with-access::rtl_ins (car ins) (fun args)
			  (set! fun (instantiate::rtl_nop))
			  (set! args '()))
		       #unspecified))
		  ((rtl_ins-ifeq? (car ins))
		   ;; replace ifeq with ifne using the following goto
		   (with-access::rtl_ins (car ins) ((ifeq fun) args)
		      (cond
			 ((or (null? (cdr ins)) (not (rtl_ins-go? (cadr ins))))
			  (error "normalize-ifeq!" "bad block" (shape b)))
			 ((and (null? (cdr succs)) (side-effect-free? (car ins)))
			  (set! ifeq (instantiate::rtl_nop))
			  (set! args '()))
			 (else
			  (with-access::rtl_ifeq ifeq (then loc)
			     (with-access::rtl_ins (cadr ins) ((go fun))
				(with-access::rtl_go go (to)
				   (set! ifeq
				      (instantiate::rtl_ifne
					 (loc loc)
					 (then to)))
				   (set! to then)
				   (set! succs (reverse! succs)))))))))
		  (else
		   (loop (cdr ins))))))))

   (with-trace 'bbv "normalize-ifeq!"
      (let loop ((bs (list b))
		 (acc '()))
	 (cond
	    ((null? bs)
	     (reverse acc))
	    ((memq (car bs) acc)
	     (loop (cdr bs) acc))
	    (else
	     (normalize-block! (car bs))
	     (with-access::block (car bs) (succs)
		(loop (append succs (cdr bs)) (cons (car bs) acc))))))))

;*---------------------------------------------------------------------*/
;*    normalize-mov! ...                                               */
;*    -------------------------------------------------------------    */
;*    Remove nested moves. That is, rewrite                            */
;*      r1 <- r2 <- expr                                               */
;*    into                                                             */
;*      r1 <- expr                                                     */
;*---------------------------------------------------------------------*/
(define (normalize-mov!::pair b::block)

   (define (remove-inner-mov! o)
      (if (rtl_ins? o)
	  (if (rtl_ins-mov? o)
	      (with-access::rtl_ins o (args)
		 (remove-inner-mov! (car args)))
	      (with-access::rtl_ins o (args)
		 (set! args (map! remove-inner-mov! args))
		 o))
	  o))
      
   (define (normalize-ins! i::rtl_ins)
      (if (rtl_ins-mov? i)
	  (with-access::rtl_ins i (args dest fun)
	     (let ((ni (remove-inner-mov! (car args))))
		(when (rtl_ins? ni)
		   (with-access::rtl_ins ni ((tmpa args) (tmpf fun))
		      (set! fun tmpf)
		      (set! args tmpa)))
		i))
	  (with-access::rtl_ins i (args)
	     (set! args (map! remove-inner-mov! args))
	     i)))
   
   (define (normalize-block! b::block)
      (with-access::block b (succs first)
	 (set! first (map! normalize-ins! first))))

   (with-trace 'bbv "normalize-mov!"
      (let loop ((bs (list b))
		 (acc '()))
	 (cond
	    ((null? bs)
	     (reverse acc))
	    ((memq (car bs) acc)
	     (loop (cdr bs) acc))
	    (else
	     (normalize-block! (car bs))
	     (with-access::block (car bs) (succs)
		(loop (append succs (cdr bs)) (cons (car bs) acc))))))))

;*---------------------------------------------------------------------*/
;*    fail! ...                                                        */
;*    -------------------------------------------------------------    */
;*    Replace the possible last go instruction of blocks invoking      */
;*    the ERROR function.                                              */
;*---------------------------------------------------------------------*/
(define (fail!::pair b::block)

   (define (fail-block! b::block)
      (with-access::block b (succs first)
	 (when (and (pair? succs) (rtl_ins-error? (car first)))
	    (let* ((lp (last-pair first))
		   (last (car lp)))
	       (let ((fail (instantiate::rtl_ins
			      (dest #f)
			      (fun (instantiate::rtl_pragma
				      (srfi0 'bigloo-c)
				      (loc (rtl_ins-loc last))
				      (format "exit(0)")))
			      (args '()))))
		  (when (null? (cdr succs))
		     (set! succs '()))
		  (set-cdr! first (list fail)))))))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (fail-block! (car bs))
	  (with-access::block (car bs) (succs)
	     (loop (append succs (cdr bs)) (cons (car bs) acc)))))))
   
;*---------------------------------------------------------------------*/
;*    normalize-typecheck! ...                                         */
;*    -------------------------------------------------------------    */
;*    Replace the possible last go instruction of blocks invoking      */
;*    the ERROR function.                                              */
;*---------------------------------------------------------------------*/
(define (normalize-typecheck!::pair b::block)

   (define (rtl_ins-typecheck? i::rtl_ins)
      (when (rtl_ins-ifne? i)
	 (with-access::rtl_ins i (args)
	    (when (and (isa? (car args) rtl_ins) (rtl_ins-mov? (car args)))
	       (let loop ((j (car args)))
		  (cond
		     ((not (isa? j rtl_ins))
		      #f)
		     ((rtl_ins-mov? j)
		      (with-access::rtl_ins j (args)
			 (loop (car args))))
		     ((rtl_ins-call? j)
		      (when (rtl_call-predicate j)
			 (let ((args (rtl_ins-args* i)))
			    (when (and (pair? args) (null? (cdr args)) (rtl_reg? (car args)))
			       j))))
		     (else
		      #f)))))))
   
   (define (normalize-ins! i::rtl_ins)
      (let ((call (rtl_ins-typecheck? i)))
	 (when call
	    (with-access::rtl_ins i (args dest)
	       (with-access::rtl_ins call ((cdest dest))
		  (set! cdest dest)
		  (set-car! args call))))))
	     
   (define (normalize-block! b::block)
      (with-access::block b (succs first)
	 (for-each normalize-ins! first)))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (normalize-block! (car bs))
	  (with-access::block (car bs) (succs)
	     (loop (append succs (cdr bs)) (cons (car bs) acc)))))))
   
;*---------------------------------------------------------------------*/
;*    normalize-goto! ...                                              */
;*    -------------------------------------------------------------    */
;*    returns a list of blocks (those that follow* the argument).      */
;*    -------------------------------------------------------------    */
;*    Add an explicit goto to all BB so that they all end with         */
;*    a return/fail/exit or a goto.                                    */
;*---------------------------------------------------------------------*/
(define (normalize-goto!::pair b::block)

   (define (normalize-block! b::block)
      (with-access::block b (succs first)
	 (when (pair? succs)
	    (let* ((lp (last-pair first))
		   (last (car lp)))
	       (unless (or (rtl_ins-go? last) (rtl_ins-fail? last))
		  (let ((go (instantiate::rtl_ins
			       (dest #f)
			       (fun (instantiate::rtl_go
				       (loc (rtl_ins-loc last))
				       (to (car succs))))
			       (args '()))))
		     (set-cdr! lp (list go))))))))

   (let loop ((bs (list b))
	      (acc '()))
      (cond
	 ((null? bs)
	  (reverse acc))
	 ((memq (car bs) acc)
	  (loop (cdr bs) acc))
	 (else
	  (normalize-block! (car bs))
	  (with-access::block (car bs) (succs)
	     (loop (append succs (cdr bs)) (cons (car bs) acc)))))))

;*---------------------------------------------------------------------*/
;*    remove-temps! ...                                                */
;*---------------------------------------------------------------------*/
(define (remove-temps! b::block)

   (define (remove-ins-temps! ins::rtl_ins)
      (with-access::rtl_ins ins (args)
	 (let loop ((args args))
	    (if (null? args)
		ins
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


