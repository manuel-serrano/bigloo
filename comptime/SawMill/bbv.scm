;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/SawMill/bbv.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 10:05:41 2017                          */
;*    Last change :  Wed Jul 13 07:54:01 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Basic Blocks versioning experiment.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv
   
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
	    saw_bbv-cache
	    saw_bbv-utils
	    saw_bbv-widening)

   (export  (bbv::pair-nil ::backend ::global ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    *cleanup* ...                                                    */
;*---------------------------------------------------------------------*/
(define *cleanup*
   (let ((e (getenv "BIGLOOBBVCLEANUP")))
      (not e)))

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
;*    bbv ...                                                          */
;*---------------------------------------------------------------------*/
(define (bbv back global params blocks)
   (if (and *saw-bbv?*
	    (or (null? *saw-bbv-functions*)
		(memq (global-id global) *saw-bbv-functions*)))
       (with-trace 'bbv (global-id global)
	  (start-bbv-cache!)
	  (verbose 2 "        bbv " (global-id global))
	  (when (>=fx *trace-level* 1)
	     (dump-blocks global params blocks ".plain.cfg"))
	  (set-max-label! blocks)
	  (reorder-succs! blocks)
	  (when (>=fx *trace-level* 2)
	     (dump-blocks global params blocks ".reorder.cfg"))
	  (let ((blocks (normalize-goto! (remove-temps! (car blocks)))))
	     (when (>=fx *trace-level* 2)
		(dump-blocks global params blocks ".goto.cfg"))
	     (let ((blocks (normalize-ifeq! (car blocks))))
		(when (>=fx *trace-level* 2)
		   (dump-blocks global params blocks ".ifeq.cfg"))
		(let ((regs (liveness! back blocks params)))
		   ;; liveness also widen each block into a blockV
		   (mark-widener! (car blocks))
		   (when (>=fx *trace-level* 2)
		      (dump-blocks global params blocks ".liveness.cfg"))
		   (unwind-protect
		      (if (null? blocks)
			  '()
			  (let* ((s (bbv-block (car blocks)
				       (params->ctx params)))
				 (b (block->block-list regs
				       (if *cleanup*
					   (remove-nop!
					      (remove-goto!
						 (simplify-branch!
						    (merge! (get-bb-mark) s))))
					   s))))
			     (verbose 3 " " (length blocks) " -> " (length b))
			     (verbose 2 "\n")
			     (when (>=fx *trace-level* 1)
				(dump-blocks global params
				   (block->block-list regs s) ".bbv.cfg"))
			     (map! (lambda (b) (shrink! b)) b)
			     b))
		      ;; don't shrink, otherwise dump could no longer be used
		      (unless (or (>=fx *compiler-debug* 1)
				  (>=fx *trace-level* 1))
			 (for-each (lambda (r) (shrink! r)) regs)))))))
       blocks))

;*---------------------------------------------------------------------*/
;*    dump-blocks ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-blocks global params blocks suffix)

   (define oname
      (if (string? *dest*)
	  *dest*
	  (if (and (pair? *src-files*) (string? (car *src-files*)))
	      (prefix (car *src-files*))
	      "./a.out")))
   (define filename (format "~a-~a~a" oname (global-id global) suffix))
   (define name (prefix filename))
      
   (define (dump-blocks port)
      (let* ((id (global-id global)))
	 (fprint port ";; -*- mode: bee -*-")
	 (fprint port ";; *** " id ":")
	 (fprint port ";; " (map shape params))
	 (fprintf port ";; bglcfg '~a' > '~a.dot' && dot '~a.dot' -Tpdf > ~a.pdf\n"
	    filename name name name)
	 (for-each (lambda (b)
		      (dump b port 0)
		      (newline port))
	    blocks)
	 id))
   
   (if oname
       (call-with-output-file filename dump-blocks)
       (dump-blocks (current-error-port))))

;*---------------------------------------------------------------------*/
;*    reorder-succs! ...                                               */
;*---------------------------------------------------------------------*/
(define (reorder-succs! blocks)
   (when (and (pair? blocks) (pair? (cdr blocks)))
      (let loop ((bs blocks))
	 (when (pair? (cdr bs))
	    (let ((b (car bs))
		  (n (cadr bs)))
	       (with-access::block b (succs first)
		  (when (pair? succs)
		     (unless (rtl_ins-go? (car (last-pair first)))
			(set! succs (cons n (remq! n succs))))))
	       (loop (cdr bs)))))))

;*---------------------------------------------------------------------*/
;*    widen-bbv! ...                                                   */
;*    -------------------------------------------------------------    */
;*    Widen the blocks and instructions for preparing the register     */
;*    allocation.                                                      */
;*---------------------------------------------------------------------*/
(define (widen-bbv! o regs::pair-nil)
   
   (define (get-args o)
      (filter rtl_reg/ra? (rtl_ins-args* o)))
   
   (define (args-widen-bbv! o)
      (when (rtl_ins? o)
	 (with-access::rtl_ins o (args fun)
	    (widen!::rtl_ins/bbv o
	       (def (make-empty-regset regs))
	       (in (list->regset (get-args o) regs))
	       (out (make-empty-regset regs)))
	    (for-each args-widen-bbv! args))))
   
   (define (ins-widen-bbv! o)
      (with-access::rtl_ins o (dest args fun)
	 (widen!::rtl_ins/bbv o
	    (def (if (or (not dest) (rtl_reg-onexpr? dest))
		     (make-empty-regset regs)
		     (list->regset (list dest) regs)))
	    (in (list->regset (get-args o) regs))
	    (out (make-empty-regset regs)))
	 (for-each args-widen-bbv! args)))
   
   (define (block-widen-bbv! o)
      (widen!::blockV o)
      (with-access::block o (first)
	 (for-each ins-widen-bbv! first)))

   (for-each block-widen-bbv! o))

;*---------------------------------------------------------------------*/
;*    liveness! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Computes the liveness of a list of blocks. Returns the           */
;*    list of used registers.                                          */
;*    -------------------------------------------------------------    */
;*    This function implements a fix point interation to find the      */
;*    maximal solution of the equations:                               */
;*       in[ n ] = use[ n ] U (out[ n ] - def[ n ])                    */
;*      out[ n ] = Union(succ[ n ]) in[ s ]                            */
;*---------------------------------------------------------------------*/
(define (liveness! back blocks params)
   
   (define (rtl_ins/bbv-in i)
      (with-access::rtl_ins/bbv i (in)
	 in))
   
   (define (liveness-block! block)
      (with-access::block block (succs first)
	 (let loop ((inss (reverse first))
		    (succ (map (lambda (b) (car (block-first b))) succs))
		    (t #f))
	    (if (pair? inss)
		(with-access::rtl_ins/bbv (car inss) (out fun in def)
		   (let ((u (cond
			       ((rtl_ins? succ)
				(regset-union! out (rtl_ins/bbv-in succ)))
			       ((pair? succ)
				(regset-union*! out (map rtl_ins/bbv-in succ)))
			       (else
				#f))))
		      (regset-for-each
			 (lambda (r)
			    (when (not (regset-member? r def))
			       (set! u (or (regset-add! in r) u))))
			 out)
		      (loop (cdr inss) (car inss) (or t u))))
		t))))
   
   
   (let* ((cregs (collect-registers! blocks))
	  (hregs (append-map! collect-register! (backend-registers back)))
	  (pregs (filter rtl_reg/ra? params))
	  (regs (append hregs cregs)))
      ;; pre widen the instructions
      (widen-bbv! blocks regs)
      ;; add the argument of the function to the IN set of the
      ;; first instruction of the first block
      (when (pair? blocks)
	 (let ((inss (block-first (car blocks))))
	    (when (pair? inss)
	       (let ((ins (car inss)))
		  (with-access::rtl_ins/bbv ins (in)
		     (for-each (lambda (a) (regset-add! in a)) pregs))))))
      ;; fix-point iteration
      (let loop ((i 0))
	 (let liip ((bs blocks)
		    (t #f))
	    (if (null? bs)
		(if t
		    (loop (+fx i 1))
		    regs)
		(liip (cdr bs) (or (liveness-block! (car bs)) t)))))))
      
