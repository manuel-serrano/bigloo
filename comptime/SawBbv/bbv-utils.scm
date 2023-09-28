;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-utils.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 27 08:57:51 2017                          */
;*    Last change :  Thu Sep 28 11:16:51 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BB manipulations                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-utils
   
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

   (export  (type-in?::bool ::type ::pair-nil)
	    (get-bb-mark)
	    (set-max-label! blocks::pair-nil)
	    (genlabel)
	    (replace ::pair-nil ::obj ::obj)
	    (block->block-list regs b::block)
	    (redirect-block! b::blockS old::blockS new::blockS)
	    (replace-block! ::blockS ::blockS #!key debug)
	    (assert-block ::blockS ::obj)
	    (bbv-ctx-filter-live-in-regs::bbv-ctx ::bbv-ctx ::rtl_ins/bbv)
	    (bbv-ctx-extend-live-out-regs::bbv-ctx ::bbv-ctx ::rtl_ins/bbv)
	    (live-versions::pair-nil ::pair-nil)
	    (dump-blocks ::global ::pair-nil ::pair-nil ::bstring)))

;*---------------------------------------------------------------------*/
;*    type-in? ...                                                     */
;*---------------------------------------------------------------------*/
(define (type-in? type types)
   
   (define (type-eq? x y)
      (cond
	 ((eq? x y) #t)
	 ((eq? x *bint*) (eq? y *long*))
	 ((eq? x *long*) (eq? y *bint*))
	 (else (is-subtype? x y))))
   
   (any (lambda (t) (type-eq? t type)) types))

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
;*    replace ...                                                      */
;*---------------------------------------------------------------------*/
(define (replace lst old new)
   (let loop ((l lst))
      (cond
	 ((null? l) l)
	 ((eq? (car l) old) (cons new (cdr l)))
	 (else (cons (car l) (loop (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    block->block-list ...                                            */
;*---------------------------------------------------------------------*/
(define (block->block-list regs b::block)
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
		(else
		 (loop (append succs (cdr bs))
		    (bbset-cons (car bs) acc)))))))))

;*---------------------------------------------------------------------*/
;*    redirect-block! ...                                              */
;*---------------------------------------------------------------------*/
(define (redirect-block! b::blockS old::blockS new::blockS)
   (with-trace 'bbv-utils
	 (format "redirect-block! ~a" (block-label b))
      (trace-item "old=" (block-label old) " "
	 (map block-label (block-succs old)))
      (trace-item "new="(block-label new) " "
	 (map block-label (block-succs new)))
      (with-access::blockS b (succs first)
	 (set! succs (replace succs old new))
	 (with-access::blockS old (preds)
	    (set! preds (remq! b preds)))
	 (with-access::block new (preds)
	    (unless (memq b preds)
	       (set! preds (cons b preds))))
	 (for-each (lambda (ins)
		      (cond
			 ((rtl_ins-ifeq? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_ifeq fun (then)
				(when (eq? then old)
				   (set! then new)))))
			 ((rtl_ins-ifne? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_ifne fun (then)
				(when (eq? then old)
				   (set! then new)))))
			 ((rtl_ins-go? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_go fun (to)
				(when (eq? to old)
				   (set! to new)))))
			 ((rtl_ins-switch? ins)
			  (with-access::rtl_ins ins (fun)
			     (with-access::rtl_switch fun (labels)
				(set! labels (replace labels old new)))))))
	    first)))
   b)

;*---------------------------------------------------------------------*/
;*    replace-block! ...                                               */
;*    -------------------------------------------------------------    */
;*    Replace one "old" block with a "new" one. Reconnect the preds    */
;*    the old block but disconnect the succs of the old block.         */
;*---------------------------------------------------------------------*/
(define (replace-block! old::blockS new::blockS #!key debug)
   (with-trace 'bbv-utils "replace-block!"
      (trace-item "old=" (block-label old) " "
	 (map block-label (block-succs old)))
      (trace-item "new="(block-label new) " "
	 (map block-label (block-succs new)))
      (unless (eq? old new)
	 ;; debugging
	 (when (and debug *bbv-debug*)
	    (with-access::blockS old ((octx ctx))
	       (with-access::blockS new ((nctx ctx))
		  (unless (ctx>=? nctx octx)
		     (tprint "REPLACE-BLOCK-ERROR! " (block-label old)
			" -> " (block-label new))
		     (tprint " octx=" (shape octx))
		     (tprint " nctx=" (shape nctx))
		     (error "replace-block!"
			(format "Wrong block replacement ~a" (block-label old))
			(block-label new))))))
	 (with-access::blockS old (succs preds mblock)
	    ;; mark the replacement
	    (set! mblock new)
	    (for-each (lambda (b)
			 (with-access::blockS b (preds)
			    (set! preds
			       (filter! (lambda (n) (not (eq? n old))) preds))))
	       succs)
	    (for-each (lambda (b)
			 (with-access::blockS b (succs first)
			    (set! succs (replace succs old new))
			    (for-each (lambda (ins)
					 (cond
					    ((rtl_ins-ifeq? ins)
					     (with-access::rtl_ins ins (fun)
						(with-access::rtl_ifeq fun (then)
						   (when (eq? then old)
						      (set! then new)))))
					    ((rtl_ins-ifne? ins)
					     (with-access::rtl_ins ins (fun)
						(with-access::rtl_ifne fun (then)
						   (when (eq? then old)
						      (set! then new)))))
					    ((rtl_ins-go? ins)
					     (with-access::rtl_ins ins (fun)
						(with-access::rtl_go fun (to)
						   (when (eq? to old)
						      (set! to new)))))
					    ((rtl_ins-switch? ins)
					     (with-access::rtl_ins ins (fun)
						(with-access::rtl_switch fun (labels)
						   (set! labels (replace labels old new)))))))
			       first)))
	       preds)
	    (with-access::blockS new ((npreds preds))
	       (set! npreds (delete-duplicates! (append preds npreds) eq?)))
	    (assert-block new "replace-block!")
	    new))))

;*---------------------------------------------------------------------*/
;*    ctx>=? ...                                                       */
;*---------------------------------------------------------------------*/
(define (ctx>=? x::bbv-ctx y::bbv-ctx)
   
   (define (bbv-ctxentry>=? x y)
      (with-access::bbv-ctxentry x ((xtypes types)
				    (xvalue value)
				    (xpolarity polarity))
	 (with-access::bbv-ctxentry y ((ytypes types)
				       (yvalue value)
				       (ypolarity polarity))
	    (cond
	       ((memq *obj* xtypes)
		#t)
	       ((memq *obj* ytypes)
		#f)
	       ((not (eq? xpolarity ypolarity))
		#f)
	       ((not (every (lambda (t) (memq t ytypes)) xtypes))
		#f)
	       ((isa? xvalue bbv-range)
		(when (isa? yvalue bbv-range)
		   (with-access::bbv-range xvalue ((xlo lo) (xup up))
		      (with-access::bbv-range yvalue ((ylo lo) (yup up))
			 (let ((r (and (<=fx xlo ylo) (>= xup yup))))
			    (unless r
			       (tprint "ERROR ctx>=? " (shape x) " " (shape y)))
			    r)))))
	       (else #t)))))
	       
   (with-access::bbv-ctx x (entries)
      (every (lambda (ex)
		(with-access::bbv-ctxentry ex (reg)
		   (let ((ey (bbv-ctx-get y reg)))
		      (bbv-ctxentry>=? ex ey))))
	 entries)))

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
;*    bbv-ctx-filter-live-in-regs ...                                  */
;*    -------------------------------------------------------------    */
;*    Filter out non-live registers from the environment.              */
;*---------------------------------------------------------------------*/
(define (bbv-ctx-filter-live-in-regs ctx ins::rtl_ins/bbv)

   (define filtered #f)
   
   (define (filter-entries ctx ins)
      (with-access::rtl_ins/bbv ins (in)
	 (filter-map (lambda (e)
			(let ((reg (bbv-ctxentry-reg e)))
			   (if (or (not (isa? reg rtl_reg/ra))
				   (regset-member? reg in))
			       (let ((aliases (filter (lambda (reg)
							 (regset-member? reg in))
						 (bbv-ctxentry-aliases e))))
				  (if (=fx (length aliases)
					 (length (bbv-ctxentry-aliases e)))
				      e
				      (begin
					 (set! filtered #t)
					 (duplicate::bbv-ctxentry e
					    (aliases aliases)))))
			       (begin
				  (set! filtered #t)
				  #f))))
	    (bbv-ctx-entries ctx))))

   (let ((entries (filter-entries ctx ins)))
      (if filtered
	  (duplicate::bbv-ctx ctx
	     (entries entries))
	  ctx)))
   
;*---------------------------------------------------------------------*/
;*    bbv-ctx-extend-live-out-regs ...                                 */
;*---------------------------------------------------------------------*/
(define (bbv-ctx-extend-live-out-regs ctx ins::rtl_ins/bbv)
   (with-access::rtl_ins/bbv ins (out)
      (let ((entries (filter (lambda (e)
				(regset-member? (bbv-ctxentry-reg e) out))
			(bbv-ctx-entries ctx))))
	 (let ((nctx (if (=fx (length entries) (length (bbv-ctx-entries ctx)))
			 ctx
			 (duplicate::bbv-ctx ctx
			    (entries entries)))))
	    (regset-for-each (lambda (r)
				(unless (bbv-ctx-get nctx r)
				   (with-access::rtl_reg r (type)
				      (set! nctx (extend-ctx nctx r
						    (list type) #t)))))
	       out)
	    nctx))))

;*---------------------------------------------------------------------*/
;*    live-versions ...                                                */
;*---------------------------------------------------------------------*/
(define (live-versions versions)
   (filter (lambda (v)
	      (with-access::blockS (cdr v) (mblock preds)
		 (not mblock)))
      versions))

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

