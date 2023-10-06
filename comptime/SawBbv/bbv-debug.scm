;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-debug.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  6 09:30:19 2023                          */
;*    Last change :  Fri Oct  6 09:32:39 2023 (serrano)                */
;*    Copyright   :  2023 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    bbv debugging tools                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-debug
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
	    "SawMill/bbset.sch")
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    type_typeof
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
	    saw_bbv-range)

   (export  (dump-blocks ::global ::pair-nil ::pair-nil ::bstring)
	    (assert-block ::blockS ::obj)
	    (assert-blocks ::blockS ::obj)
	    (assert-context! b::block)))

;*---------------------------------------------------------------------*/
;*    assert-block ...                                                 */
;*    -------------------------------------------------------------    */
;*    A debug fonction that tests the consistency of the preds,        */
;*    succs, and branch instructions.                                  */
;*---------------------------------------------------------------------*/
(define (assert-block b::blockS stage)
   (with-access::blockS b (preds succs first label parent)
      ;; check that b in the preds.succs
      (let ((l (filter (lambda (p)
			  (with-access::blockS p (succs)
			     (not (memq b succs))))
		  preds)))
	 (when (pair? l)
	    (tprint (shape b))
	    (tprint "preds...")
	    (for-each (lambda (b) (tprint (shape b))) l)
	    (error stage 
	       (format "predecessors not pointing to ~a" label)
	       (map block-label l))))
      ;; check that b in the succs.preds
      (let ((l (filter (lambda (p)
			  (with-access::blockS p (preds)
			     (not (memq b preds))))
		  succs)))
	 (when (pair? l)
	    (tprint (shape b))
	    (tprint "succs...")
	    (for-each (lambda (b) (tprint (shape b))) l)
	    (error stage
	       (format "successors not pointing to ~a" label)
	       (map block-label l))))
      ;; check that the instructions are in the succs
      (let ((l '()))
	 (for-each (lambda (ins)
		      (cond
			 ((rtl_ins-ifeq? ins)
			  (set! l (cons ins l)))
			 ((rtl_ins-ifne? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_ifne fun (then)
				(unless (memq then succs)
				   (set! l (cons ins l))))))
			 ((rtl_ins-go? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_go fun (to)
				(unless (memq to succs)
				   (set! l (cons ins l))))))
			 ((rtl_ins-switch? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_switch fun (labels)
				(for-each (lambda (lbl)
					     (unless (memq lbl succs)
						(set! l (cons ins l))))
				   labels))))))
	    first)
	 (when (pair? l)
	    (tprint "wrong block: " (shape b))
	    (tprint "parent block: " (shape parent))
	    (error stage
	       (format "instruction target not in succs of " label)
	       (map shape l))))))

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
   
   (define filename
      (string-replace (format "~a-~a~a" oname (global-id global) suffix)
	 #\/ #\_))
   
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
;*    assert-blocks ...                                                */
;*    -------------------------------------------------------------    */
;*    Coalesce requires dangling blocks to be removed from             */
;*    parent.versions. This pass implements a simple mark&sweep        */
;*    collectors for basic blocks.                                     */
;*---------------------------------------------------------------------*/
(define (assert-blocks b::blockS lbl)
   (when *bbv-debug*
      (let loop ((bs (list b))
		 (set (make-empty-bbset)))
	 (cond
	    ((null? bs)
	     set)
	    ((bbset-in? (car bs) set)
	     (loop (cdr bs) set))
	    (else
	     (with-access::blockS (car bs) (succs)
		(assert-block (car bs) lbl)
		(loop (append succs (cdr bs))
		   (bbset-cons (car bs) set))))))))

;*---------------------------------------------------------------------*/
;*    assert-context! ...                                              */
;*---------------------------------------------------------------------*/
(define (assert-context! b::block)

   (define (assert-ins i::rtl_ins/bbv)
      (with-access::rtl_ins/bbv i (ctx)
	 (tprint "CTX=" (shape ctx))
	 (instantiate::rtl_nop)))
   
   (assert-blocks b "before assert!")
   (if *bbv-assert*
       (let loop ((bs (list b))
		  (acc (make-empty-bbset)))
	  (cond
	     ((null? bs)
	      b)
	     ((bbset-in? (car bs) acc)
	      (loop (cdr bs) acc))
	     (else
	      (with-access::block (car bs) (first succs preds label)
		 (when *bbv-debug* (assert-block (car bs) "assert-context!"))
		 (let loop ((first first)
			    (acc '()))
		    (if (null? first)
			(set! first (reverse! acc))
			(loop (cdr first)
			   (cons* (car first)
			      (assert-ins (car first))
			      acc))))
		 (loop (append succs (cdr bs)) (bbset-cons (car bs) acc))))))
       b))
