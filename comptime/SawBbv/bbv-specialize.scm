;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawBbv/bbv-specialize.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 07:42:00 2017                          */
;*    Last change :  Mon Sep  5 14:01:24 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BBV instruction specialization                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-specialize
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
	    "SawBbv/bbv-types.sch")
   
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
	    saw_bbv-cache
	    saw_bbv-utils
	    saw_bbv-range
	    saw_bbv-merge
	    saw_bbv-config
	    saw_bbv-cost)

   (export (bbv-block::blockS ::blockV ::bbv-ctx)
	   (bbv-block-specialize!::blockS ::blockV ::bbv-ctx)))

;*---------------------------------------------------------------------*/
;*    configuration                                                    */
;*---------------------------------------------------------------------*/
(define *max-block-version* 3)

;*---------------------------------------------------------------------*/
;*    bbv-block ::blockV ...                                           */
;*---------------------------------------------------------------------*/
(define (bbv-block::blockS bv::blockV ctx::bbv-ctx)
   
   (define (generic-block bv::blockV)
      ;; generate the most generic version (i.e., all variables are
      ;; approximated with obj type) 
      (let ((gctx (with-access::bbv-ctx ctx (entries)
		     (params->ctx
			(map (lambda (e)
				(with-access::bbv-ctxentry e (reg)
				   reg))
			   entries)))))
	 (bbv-block-specialize! bv gctx)))
   
   (with-access::blockV bv (label versions succs preds first merge)
      (with-trace 'bbv-block (format "bbv-block ~a" label)
	 (let ((ctx (bbv-ctx-filter-live-in-regs ctx (car first))))
	    (trace-item "succs=" (map block-label succs))
	    (trace-item "preds=" (map block-label preds))
	    (trace-item "ctx=" (shape ctx))
	    (trace-item "versions=" (map ctx->string (map car (live-versions versions))))
	    (let* ((bs (bbv-block-specialize! bv ctx))
		   (lvs (live-versions versions)))
	       (trace-item "nversions= " (length lvs) "/" (length versions) " "
		  (map ctx->string (map car lvs)))
	       (if (and merge (>fx (length lvs) *max-block-version*))
		   (begin
		      (block-merge-contexts! bv)
		      (live-block bs))
		   bs))))))

;*---------------------------------------------------------------------*/
;*    bbv-block-specialize! ...                                        */
;*---------------------------------------------------------------------*/
(define (bbv-block-specialize!::blockS b::blockV ctx::bbv-ctx)

   (define (find-block-version ctx::bbv-ctx versions::pair-nil)
      (let ((c (bbv-ctx-assoc ctx versions)))
	 (when (pair? c)
	    (cdr c))))
   
   (define (block-specialize!::blockS b::blockV ctx::bbv-ctx)
      (with-access::blockV b (first label succs versions)
	 (let* ((lbl (genlabel))
		(sb (instantiate::blockS
		       (parent b)
		       (label lbl)
		       (first '()))))
	    (set! versions (cons (cons ctx sb) versions))
	    (trace-item "new-label=" lbl)
	    (let ((nfirst (bbv-ins first sb ctx)))
	       (with-access::blockS sb (first succs)
		  (set! first nfirst)
		  sb)))))
   
   (with-access::blockV b (label versions succs preds first merge)
      (with-trace 'bbv-block (format "bbv-block-specialize! ~a" label)
	 (let ((ctx (bbv-ctx-filter-live-in-regs ctx (car first))))
	    (trace-item "ctx=" (shape ctx))
	    (let ((ob (find-block-version ctx versions)))
	       (trace-item "old=" (when ob (block-label ob)))
	       (if ob
		   (live-block ob)
		   (block-specialize! b ctx)))))))

;*---------------------------------------------------------------------*/
;*    live-block ...                                                   */
;*---------------------------------------------------------------------*/
(define (live-block b::blockS)
   (with-access::blockS b (mblock)
      (if mblock
	  (live-block mblock)
	  b)))

;*---------------------------------------------------------------------*/
;*    bbv-ins ...                                                      */
;*---------------------------------------------------------------------*/
(define (bbv-ins first sb::blockS ctx::bbv-ctx)
   
   (define (connect! s::blockS ins::rtl_ins)
      (cond
	 ((rtl_ins-ifne? ins)
	  (with-access::rtl_ins ins (fun)
	     (let ((n (rtl_ifne-then fun)))
		(block-succs-set! s (list #unspecified n))
		(block-preds-set! n (cons s (block-preds n))))))
	 ((rtl_ins-go? ins)
	  (with-access::rtl_ins ins (fun)
	     (let ((n (rtl_go-to fun)))
		(if (pair? (block-succs s))
		    (set-car! (block-succs s) n)
		    (block-succs-set! s (list n)))
		(block-preds-set! n (cons s (block-preds n))))))))
   
   (with-trace 'bbv-ins "bbv-ins"
      (let loop ((oins first)
		 (nins '())
		 (ctx ctx))
	 (when (pair? oins)
	    (trace-item "ins=" (shape (car oins)) " " (length (cdr oins))
	       " " (with-access::rtl_ins (car oins) (fun) (typeof fun)))
	    (trace-item "ctx=" (shape ctx)))
	 (cond
	    ((null? oins)
	     (reverse! nins))
	    ((rtl_ins-specializer (car oins))
	     =>
	     (lambda (specialize)
		;; instruction specialization
		(multiple-value-bind (ins nctx)
		   (specialize (car oins) ctx)
		   (with-access::rtl_ins ins (args)
		      (set! args (map (lambda (i)
					 (if (isa? i rtl_ins)
					     (car (bbv-ins (list i) sb ctx))
					     i))
				    args))
		      (let ((ctx (bbv-ctx-extend-live-out-regs nctx (car oins))))
			 (cond
			    ((rtl_ins-br? ins)
			     (connect! sb ins)
			     (loop (cdr oins) (cons ins nins) ctx))
			    ((rtl_ins-go? ins)
			     (connect! sb ins)
			     (loop '() (cons ins nins) ctx))
			    (else
			     (loop (cdr oins) (cons ins nins) ctx))))))))
	    ((rtl_ins-last? (car oins))
	     ;; a return, fail, ...
	     (with-access::rtl_ins (car oins) (args)
		(let ((args (map (lambda (i)
				    (if (isa? i rtl_ins)
					(car (bbv-ins (list i) sb ctx))
					i))
			       args)))
		   (loop '()
		      (cons (duplicate-ins/args (car oins) args ctx) nins)
		      ctx))))
	    ((rtl_ins-go? (car oins))
	     (with-access::rtl_ins (car oins) (fun)
		(with-access::rtl_go fun (to)
		   (let* ((n (bbv-block to ctx))
			  (ins (duplicate::rtl_ins/bbv (car oins)
				  (ctx ctx)
				  (fun (duplicate::rtl_go fun
					  (to n))))))
		      (connect! sb ins)
		      (loop '() (cons ins nins) ctx)))))
	    ((rtl_ins-ifne? (car oins))
	     (with-access::rtl_ins (car oins) (fun)
		(with-access::rtl_ifne fun (then)
		   (let* ((n (bbv-block then ctx))
			  (ins (duplicate::rtl_ins/bbv (car oins)
				  (ctx ctx)
				  (fun (duplicate::rtl_ifne fun
					  (then n))))))
		      (connect! sb ins)
		      (loop (cdr oins) (cons ins nins) ctx)))))
	    (else
	     (with-access::rtl_ins (car oins) (args)
		(let ((args (map (lambda (i)
				    (if (isa? i rtl_ins)
					(car (bbv-ins (list i) sb ctx))
					i))
			       args)))
		   (loop (cdr oins)
		      (cons (duplicate-ins/args (car oins) args ctx) nins)
		      (bbv-ctx-extend-live-out-regs ctx (car oins))))))))))

;*---------------------------------------------------------------------*/
;*    duplicate-ins/args ...                                           */
;*---------------------------------------------------------------------*/
(define (duplicate-ins/args ins args ctx)
   (duplicate::rtl_ins/bbv ins
      (args args)
      (ctx ctx)))

;*---------------------------------------------------------------------*/
;*    duplicate-ins ...                                                */
;*---------------------------------------------------------------------*/
(define (duplicate-ins ins ctx)
   (duplicate::rtl_ins/bbv ins
      (ctx ctx)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specializer ...                                          */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specializer i::rtl_ins)
   (cond
      ((rtl_ins-fxovop? i) rtl_ins-specialize-fxovop)
      ((rtl_ins-fxcmp? i) rtl_ins-specialize-fxcmp)
      ((rtl_ins-fxop? i) rtl_ins-specialize-fxop)
      ((rtl_ins-typecheck? i) rtl_ins-specialize-typecheck)
      ((rtl_ins-mov? i) rtl_ins-specialize-mov)
      ((rtl_ins-loadi? i) rtl_ins-specialize-loadi)
      ((rtl_ins-call-specialize? i) rtl_ins-specialize-call)
      ((rtl_ins-return-specialize? i) rtl_ins-specialize-return)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-typecheck? ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns the type checked by the instruction or false.            */
;*---------------------------------------------------------------------*/
(define (rtl_ins-typecheck? i::rtl_ins)
   (when (rtl_ins-ifne? i)
      (with-access::rtl_ins i (args)
	 (when (and (isa? (car args) rtl_ins) (rtl_ins-call? (car args)))
	    (when (rtl_call-predicate (car args))
	       (let ((args (rtl_ins-args* i)))
		  (and (pair? args) (null? (cdr args)) (rtl_reg? (car args)))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-typecheck ...                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-typecheck i::rtl_ins ctx)

   (define (min-value x y)
      (cond
	 ((eq? x '_) y)
	 ((eq? y '_) x)
	 ((and (isa? x bbv-range) (isa? y bbv-range)) (bbv-range-intersection x y))
	 ((isa? x bbv-range) x)
	 ((isa? y bbv-range) y)
	 (else '_)))
   
   (with-trace 'bbv-ins "rtl_ins-specialize-typecheck"
      (multiple-value-bind (reg type polarity value)
	 (rtl_ins-typecheck i)
	 (let ((e (bbv-ctx-get ctx reg)))
	    (with-access::rtl_ins i (fun)
	       (trace-item "ins=" (shape i))
	       (trace-item "typ=" (shape type) " polarity=" polarity)
	       (trace-item "value=" (shape value))
	       (trace-item "e=" (shape e))
	       (cond
		  ((and (type-in? type (bbv-ctxentry-types e))
			(bbv-ctxentry-polarity e))
		   ;; positive type simplification
		   (let ((pctx (extend-ctx ctx reg (list type) #t
				  :value (min-value value (bbv-ctxentry-value e)))))
		      (with-access::rtl_ins/bbv i (fun)
			 (with-access::rtl_ifne fun (then)
			    (let ((s (duplicate::rtl_ins/bbv i
					(ctx ctx)
					(fun (instantiate::rtl_go
						(to (bbv-block then pctx))))
					(dest #f)
					(args '()))))
			       ;; the next ctx will be ignored...
			       (values s (instantiate::bbv-ctx)))))))
		  ((and (type-in? type (bbv-ctxentry-types e))
			(not (bbv-ctxentry-polarity e)))
		   ;; negative type simplification
		   (with-access::rtl_ins/bbv i (fun)
		      (let ((s (duplicate::rtl_ins/bbv i
				  (ctx ctx)
				  (fun (instantiate::rtl_nop))
				  (dest #f)
				  (args '()))))
			 (values s ctx))))
		  ((isa? fun rtl_ifne)
		   (with-access::bbv-ctxentry e (aliases)
		      (let ((regs (cons reg aliases)))
			 (with-access::rtl_ifne fun (then)
			    (let* ((pctx (extend-ctx* ctx regs (list type) #t
					    :value (min-value value (bbv-ctxentry-value e))))
				   (nctx (if (bbv-ctxentry-polarity e)
					     (extend-ctx* ctx regs
						(list type) #f)
					     (extend-ctx* ctx regs
						(cons type (bbv-ctxentry-types e)) #f)))
				   (s (duplicate::rtl_ins/bbv i
					 (ctx ctx)
					 (fun (duplicate::rtl_ifne fun
						 (then (bbv-block then pctx)))))))
			       (values s nctx))))))
		  (else
		   (error "rtl_ins-specialize-typecheck"
		      "should not be here"
		      (shape i)))))))))

;*---------------------------------------------------------------------*/
;*    range->loadi ...                                                 */
;*---------------------------------------------------------------------*/
(define (range->loadi i::rtl_ins dest value types)
   (with-access::bbv-range value (max)
      (let* ((atom (instantiate::literal
		      (type *long*)
		      (value max)))
	     (loadi (instantiate::rtl_loadi
		       (constant atom))))
	 (if (memq *long* types)
	     (duplicate::rtl_ins/bbv i
		(dest dest)
		(args '())
		(fun loadi))
	     (duplicate::rtl_ins/bbv i
		(dest dest)
		(args (list (duplicate::rtl_ins/bbv i
			       (dest dest)
			       (args '())
			       (fun loadi))))
		(fun (instantiate::rtl_call 
			(var *long->bint*))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-mov ...                                       */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-mov i::rtl_ins ctx)
   (with-trace 'bbv-ins "rtl_ins-specialize-mov"
      (with-access::rtl_ins i (dest args fun)
	 (let ((ctx (unalias-ctx ctx dest)))
	    (cond
	       ((and (pair? args) (null? (cdr args)) (rtl_reg/ra? (car args)))
		(let ((e (bbv-ctx-get ctx (car args))))
		   (with-access::bbv-ctxentry e (types value)
		      (if (and (bbv-singleton? value))
			  (values (range->loadi i dest value types)
			     (extend-ctx ctx dest types #t :value value))
			  (values (duplicate-ins i ctx)
			     (alias-ctx
				(extend-ctx ctx dest types #t :value value)
				dest (car args)))))))
	       ((and *type-call* (pair? args) (rtl_ins-call? (car args)))
		(with-access::rtl_ins (car args) (fun args)
		   (with-access::rtl_call fun (var)
		      (with-access::global var (value type)
			 (if (and (eq? var *long->bint*) (rtl_ins-loadi? (car args)))
			     (with-access::rtl_ins (car args) (fun)
				(with-access::rtl_loadi fun (constant)
				   (with-access::atom constant (value type)
				      (values (duplicate-ins i ctx)
					 (extend-ctx ctx dest (list *bint*) #t
					    :value (if (fixnum? value) (fixnum->range value) '_))))))
			     (values (duplicate-ins i ctx)
				(extend-ctx ctx dest (list type) #t)))))))
	       ((and *type-loadi* (pair? args) (rtl_ins-loadi? (car args)))
		(with-access::rtl_ins (car args) (fun)
		   (with-access::rtl_loadi fun (constant)
		      (with-access::atom constant (value type)
			 (values (duplicate-ins i ctx)
			    (extend-ctx ctx dest (list type) #t :value
			       (if (fixnum? value) (fixnum->range value) '_)))))))
	       (else
		(values (duplicate-ins i ctx) ctx)))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-loadi ...                                     */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-loadi i::rtl_ins ctx)
   (with-trace 'bbv-ins "rtl_ins-specialize-loadi"
      (with-access::rtl_ins i (dest args fun)
	 (with-access::rtl_loadi fun (constant)
	    (with-access::atom constant (value type)
	       (let ((s (duplicate::rtl_ins/bbv i
			   (ctx ctx)
			   (fun (duplicate::rtl_loadi fun))))
		     (v (if (fixnum? value) (fixnum->range value) '_)))
		  (values s
		     (extend-ctx ctx dest (list type) #t :value v))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-call-specialize? ...                                     */
;*---------------------------------------------------------------------*/
(define (rtl_ins-call-specialize? i::rtl_ins)
   (when (rtl_ins-call? i)
      (with-access::rtl_ins i (dest)
	 dest)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-call ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-call i::rtl_ins ctx)
   
   (define (new-value-loadi i)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_loadi fun (constant)
	    (with-access::atom constant (value type)
	       (if (fixnum? value) (fixnum->range value) '_)))))
   
   (define (new-value-call i)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (with-access::global var (value type)
	       (cond
		  ((eq? var *bint->long*)
		   (cond
		      ((rtl_reg? (car args))
		       (let ((e (bbv-ctx-get ctx (car args))))
			  (if (and e (bbv-range? (bbv-ctxentry-value e)))
			      (bbv-ctxentry-value e)
			      '_)))
		      ((rtl_ins-loadi? (car args))
		       (new-value-loadi (car args)))
		      (else
		       '_)))
		  ((eq? var *long->bint*)
		   (cond
		      ((rtl_reg? (car args))
		       (let ((e (bbv-ctx-get ctx (car args))))
			  (if (and e (bbv-range? (bbv-ctxentry-value e)))
			      (bbv-ctxentry-value e)
			      '_)))
		      ((rtl_ins-loadi? (car args))
		       (new-value-loadi (car args)))
		      (else
		       '_)))
		  (else
		   '_))))))
   
   (define (new-value i)
      (cond
	 ((rtl_ins-call? i) (new-value-call i))
	 ((rtl_ins-loadi? i) (new-value-loadi i))
	 (else '_)))
   
   (with-trace 'bbv-ins "rtl_ins-specialize-call"
      (with-access::rtl_ins i (dest fun)
	 (with-access::rtl_call fun (var)
	    (with-access::global var (value type)
	       (let* ((s (duplicate::rtl_ins/bbv i
			    (ctx ctx)
			    (fun (duplicate::rtl_call fun))))
		      (nv (new-value i)))
		  (values s
		     (extend-ctx ctx dest (list (if (fun? value) type *obj*)) #t
			:value nv))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-return-specialize? ...                                   */
;*---------------------------------------------------------------------*/
(define (rtl_ins-return-specialize? i::rtl_ins)
   (with-access::rtl_ins i (fun args)
      (when (isa? fun rtl_return)
	 (rtl_reg? (car args)))))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-return ...                                    */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-return i::rtl_ins ctx)
   (with-access::rtl_ins i (args dest)
      (let ((e (bbv-ctx-get ctx (car args))))
	 (with-access::bbv-ctxentry e (types value)
	    (if (bbv-singleton? value)
		(let ((loadi (duplicate::rtl_ins/bbv i
				(args (list (range->loadi i
					       (car args) value types)))
				(fun (instantiate::rtl_mov))
				(dest (car args)))))
		   (values (duplicate::rtl_ins i
			      (args (cons loadi (cdr args))))
		      ctx))
		(values i ctx))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-fxcmp? ...                                               */
;*---------------------------------------------------------------------*/
(define (rtl_ins-fxcmp? i)
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (rtl_reg? dest))))))

   (define (rtl_call-fxcmp? i)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (and (=fx (length args) 2)
		 (or (eq? var *<fx*) (eq? var *<=fx*)
		     (eq? var *>fx*) (eq? var *>=fx*)
		     (eq? var *=fx*))
		 (or (reg? (car args)) (rtl_ins-loadi? (car args)))
		 (or (reg? (cadr args)) (rtl_ins-loadi? (cadr args)))))))
   
   (with-access::rtl_ins i (dest fun args)
      (cond
	 ((isa? fun rtl_call) (rtl_call-fxcmp? i))
	 ((isa? fun rtl_ifne) (rtl_call-fxcmp? (car args)))
	 (else #f))))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-fxcmp ...                                     */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-fxcmp i::rtl_ins ctx::bbv-ctx)
   
   (define (true)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #t)))))
   
   (define (false)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #f)))))

   (define (rtl_bint->long? a)
      (when (isa? a rtl_ins)
	 (with-access::rtl_ins a (fun)
	    (when (isa? fun rtl_call)
	       (with-access::rtl_call fun (var)
		  (eq? var *bint->long*))))))
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (with-access::rtl_call fun (var)
			(if (eq? var *bint->long*)
			    (rtl_reg? (car args))
			    (rtl_reg? dest))))))))
   
   (define (reg a)
      (cond
	 ((rtl_reg? a) a)
	 ((rtl_bint->long? a) (car (rtl_ins-args a)))
	 (else (rtl_ins-dest a))))
   
   (define (fxcmp-op i)
      (with-access::rtl_ins i (fun)
	 (with-access::rtl_call fun (var)
	    (cond
	       ((eq? var *<fx*) '<)
	       ((eq? var *<=fx*) '<=)
	       ((eq? var *>fx*) '>)
	       ((eq? var *>=fx*) '>=)
	       ((eq? var *=fx*) '=)))))
   
   (define (inv-op op)
      (case op
	 ((<) '>=)
	 ((<=) '>)
	 ((>) '<=)
	 ((>=) '<)
	 (else op)))
   
   (define (resolve/op i op intl intr)
      (case op
	 ((<) (bbv-range<? intl intr))
	 ((<=) (bbv-range<? intl intr))
	 ((>) (bbv-range>? intl intr))
	 ((>=) (bbv-range>=? intl intr))
	 ((=) (bbv-range=? intl intr))
	 (else #f)))
   
   (define (test-ctxs-ref reg intl intr op ctx::bbv-ctx)
      (if (or (not (bbv-range? intl)) (not (bbv-range? intr)))
	  (values ctx ctx)
	  (let* ((e (bbv-ctx-get ctx reg))
		 (types (bbv-ctxentry-types e)))
	     (case op
		((<)
		 (let ((intrt (bbv-range-lt intl intr))
		       (intro (bbv-range-gte intl intr)))
		    (values (extend-ctx ctx reg types #t :value intrt)
		       (extend-ctx ctx reg types #t :value intro))))
		((<=)
		 (let ((intrt (bbv-range-lte intl intr))
		       (intro (bbv-range-gt intl intr)))
		    (values (extend-ctx ctx reg types #t :value intrt)
		       (extend-ctx ctx reg types #t :value intro))))
		((>)
		 (let ((intrt (bbv-range-gt intl intr))
		       (intro (bbv-range-lte intl intr)))
		    (values (extend-ctx ctx reg types #t :value intrt)
		       (extend-ctx ctx reg types #t :value intro))))
		((>=)
		 (let ((intrt (bbv-range-gte intl intr))
		       (intro (bbv-range-lt intl intr)))
		    (values (extend-ctx ctx reg types #t :value intrt)
		       (extend-ctx ctx reg types #t :value intro))))
		((== ===)
		 (let ((ieq (bbv-range-eq intl intr)))
		    (values (if (bbv-range? ieq)
				(extend-ctx ctx reg types #t :value ieq)
				ctx)
		       ctx)))
		((!= !==)
		 (let ((ieq (bbv-range-eq intl intr)))
		    (values ctx
		       (if (bbv-range? ieq)
			   (extend-ctx ctx reg types #t :value ieq)
			   ctx))))
		(else
		 (values ctx ctx))))))
   
   (define (specialize/op op lhs intl rhs intr ctx::bbv-ctx)
      (cond
	 ((and (reg? lhs) (reg? rhs))
	  (multiple-value-bind (lctxt lctxo)
	     (test-ctxs-ref (reg lhs) intl intr op ctx)
	     (multiple-value-bind (rctxt _)
		(test-ctxs-ref (reg rhs) intr intl (inv-op op) lctxt)
		(multiple-value-bind (_ rctxo)
		   (test-ctxs-ref (reg rhs) intr intl (inv-op op) lctxo)
		   (values rctxt rctxo)))))
	 ((reg? lhs)
	  (test-ctxs-ref (reg lhs) intl intr op ctx))
	 ((reg? rhs)
	  (test-ctxs-ref (reg rhs) intr intl (inv-op op) ctx))
	 (else
	  (values ctx ctx))))
   
   (define (specialize-call i::rtl_ins ctx::bbv-ctx)
      (with-access::rtl_ins i (fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (intl (rtl-range lhs ctx))
		   (intr (rtl-range rhs ctx))
		   (op (fxcmp-op i)))
	       (cond
		  ((not (and (bbv-range? intl) (bbv-range? intr)))
		   (multiple-value-bind (pctx nctx)
		      (specialize/op op lhs (or intl (fixnum-range))
			 rhs (or intr (fixnum-range)) ctx)
		      (values (duplicate::rtl_ins/bbv i) pctx nctx)))
		  ((resolve/op i op intl intr)
		   =>
		   (lambda (val)
		      (with-trace 'bbv-ins "resolve/op"
			 (trace-item "val=" val)
			 (case val
			    ((true)
			     (multiple-value-bind (pctx nctx)
				(specialize/op op lhs (or intl (fixnum-range))
				   rhs (or intr (fixnum-range)) ctx)
				(values (duplicate::rtl_ins/bbv i (fun (true)))
				   pctx nctx)))
			    ((false)
			     (multiple-value-bind (pctx nctx)
				(specialize/op op lhs (or intl (fixnum-range))
				   rhs (or intr (fixnum-range)) ctx)
				(values (duplicate::rtl_ins/bbv i (fun (false)))
				   pctx nctx)))
			    (else
			     (multiple-value-bind (pctx nctx)
				(specialize/op op lhs intl rhs intr ctx)
				(values (duplicate::rtl_ins/bbv i) pctx nctx)))))))
		  (else
		   (multiple-value-bind (pctx nctx)
		      (specialize/op op lhs intl rhs intr ctx)
		      (values (duplicate::rtl_ins/bbv i) pctx nctx))))))))

   (with-trace 'bbv-ins "rtl_ins-specialize-fxcmp"
      (with-access::rtl_ins i (dest fun args)
	 (cond
	    ((isa? fun rtl_ifne)
	     (with-access::rtl_ifne fun (then)
		(multiple-value-bind (ins pctx nctx)
		   (specialize-call (car args) ctx)
		   (cond
		      ((rtl_ins-true? ins)
		       (with-access::rtl_ifne fun (then)
			  (let* ((fun (duplicate::rtl_go fun
					 (to (bbv-block then ctx))))
				 (ni (duplicate::rtl_ins/bbv i
					(ctx ctx)
					(args '())
					(fun fun))))
			     (values ni pctx))))
		      ((rtl_ins-false? ins)
		       (with-access::rtl_ifne fun (then)
			  (let* ((fun (duplicate::rtl_nop fun))
				 (ni (duplicate::rtl_ins/bbv i
					(ctx ctx)
					(args '())
					(fun fun))))
			     (values ni nctx))))
		      (else
		       (let ((fun (duplicate::rtl_ifne fun
				     (then (bbv-block then pctx)))))
			  (values (duplicate::rtl_ins/bbv i
				     (ctx ctx)
				     (args (list ins))
				     (fun fun))
			     nctx)))))))
	    ((isa? fun rtl_call)
	     (multiple-value-bind (ins pctx nctx)
		(specialize-call i ctx)
		(cond
		   ((rtl_ins-true? ins) (values ins pctx))
		   ((rtl_ins-false? ins) (values ins nctx))
		   (else (values ins ctx)))))
	    (else
	     (error "rtl_ins-specialize-fxcmp"
		"Should not be here" (shape i)))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-fxop? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-fxop? i)
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (rtl_reg? dest))))))

   (define (rtl_call-fxop? i)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (and (=fx (length args) 2)
		 (or (eq? var *+fx*)
		     (eq? var *-fx-safe*)
		     (eq? var *+fx-safe*)
		     (eq? var *2+*))
		 (or (reg? (car args)) (rtl_ins-loadi? (car args)))
		 (or (reg? (cadr args)) (rtl_ins-loadi? (cadr args)))))))
   
   (with-access::rtl_ins i (dest fun args)
      (cond
	 ((isa? fun rtl_call)
	  (rtl_call-fxop? i))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-fxovop? ...                                              */
;*---------------------------------------------------------------------*/
(define (rtl_ins-fxovop? i)
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (rtl_reg? dest))))))

   (define (rtl_call-fxovop? i)
      (when (rtl_ins-call? i)
	 (with-access::rtl_ins i (dest fun args)
	    (with-access::rtl_call fun (var)
	       (and (=fx (length args) 3)
		    (or (eq? var *$-fx/ov*)
			(eq? var *$+fx/ov*)
			(eq? var *$*fx/ov*))
		    (or (reg? (car args)) (rtl_ins-loadi? (car args)))
		    (or (reg? (cadr args)) (rtl_ins-loadi? (cadr args)))
		    (reg? (caddr args)))))))

   (when (rtl_ins-ifne? i)
      (with-access::rtl_ins i (fun args)
	 (when (and (pair? args) (null? (cdr args)))
	    (rtl_call-fxovop? (car args))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-fxovop ...                                    */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-fxovop i::rtl_ins ctx::bbv-ctx)
   
   (define (fxovop-ctx ctx reg i::rtl_ins)
      (with-access::rtl_ins i (fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (intl (rtl-range lhs ctx))
		   (intr (rtl-range rhs ctx))
		   (intv (cond
			    ((not (and (bbv-range? intl) (bbv-range? intr)))
			     (fixnum-range))
			    ((eq? var *$-fx/ov*)
			     (bbv-range-sub intl intr))
			    ((eq? var *$+fx/ov*)
			     (bbv-range-add intl intr))
			    ((eq? var *$*fx/ov*)
			     (bbv-range-mul intl intr))
			    (else
			     (error "fxovop-ctx" "wrong operator" (shape fun))))))
	       (with-access::bbv-range intv ((i min) (a max))
		  (let ((intx (instantiate::bbv-range
				 (min (max i (bbv-min-fixnum)))
				 (max (min a (bbv-max-fixnum))))))
		     		     (extend-ctx ctx reg (list *bint*) #t
			:value intx)))))))
   
   (with-trace 'bbv-ins "rtl_ins-specialize-fxovop"
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_ifne fun (then)
	    (let ((call (car args)))
	       (with-access::rtl_ins (car args) (args)
		  (let ((reg (caddr args)))
		     (let* ((pctx (extend-ctx ctx reg (list *bint*) #t
				     :value (fixnum-range)))
			    (nctx (fxovop-ctx ctx reg call))
			    (s (duplicate::rtl_ins/bbv i
				  (ctx ctx)
				  (fun (duplicate::rtl_ifne fun
					  (then (bbv-block then pctx)))))))
			(values s nctx)))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-fxop ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-fxop i::rtl_ins ctx)
   
   (define (fx-safe->fx var i ctx nctx)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (subcall (duplicate::rtl_call fun
			       (var (if (eq? var *-fx-safe*) *-fx* *+fx*))))
		   (castres (duplicate::rtl_call fun
			       (var *long->bint*))))
	       (values (duplicate::rtl_ins/bbv i
			  (fun castres)
			  (args (list (duplicate::rtl_ins/bbv i
					 (fun subcall)
					 (args (list lhs rhs))
					 (ctx ctx))))
			  (ctx nctx))
		  nctx)))))

   (define (2->fx var i ctx nctx)
      (with-access::rtl_ins i (dest fun args)
	 (with-access::rtl_call fun (var)
	    (let* ((lhs (car args))
		   (rhs (cadr args))
		   (castlhs (duplicate::rtl_ins/bbv i
			       (fun (duplicate::rtl_call fun
				       (var *bint->long*)))
			       (args (list lhs))))
		   (castrhs (duplicate::rtl_ins/bbv i
			       (fun (duplicate::rtl_call fun
				       (var *bint->long*)))
			       (args (list rhs))))
		   (subcall (duplicate::rtl_call fun
			       (var (if (eq? var *2-*) *-fx* *+fx*))))
		   (castres (duplicate::rtl_call fun
			       (var *long->bint*))))
	       (values (duplicate::rtl_ins/bbv i
			  (fun castres)
			  (args (list (duplicate::rtl_ins/bbv i
					 (fun subcall)
					 (args (list castlhs castrhs))
					 (ctx ctx))))
			  (ctx nctx))
		  nctx)))))
   
   (with-access::rtl_ins i (dest fun args)
      (with-access::rtl_call fun (var)
	 (let* ((lhs (car args))
		(rhs (cadr args))
		(intl (rtl-range lhs ctx))
		(intr (rtl-range rhs ctx)))
	    (cond
	       ((not (and (bbv-range? intl) (bbv-range? intr)))
		(values (duplicate::rtl_ins/bbv i
			   (ctx ctx))
		   ctx))
	       ((eq? var *-fx*)
		(let ((range (bbv-range-sub intl intr)))
		   (if (bbv-range-fixnum? range)
		       (let ((nctx (extend-ctx ctx dest (list *long*) #t
				      :value range)))
			  (values i nctx))
		       (values (duplicate::rtl_ins/bbv i
				  (ctx ctx))
			  ctx))))
	       ((eq? var *-fx-safe*)
		(let ((range (bbv-range-sub intl intr)))
		   (if (bbv-range-fixnum? range)
		       (let ((nctx (extend-ctx ctx dest (list *bint*) #t
				      :value range)))
			  (values (fx-safe->fx var i ctx nctx)
			     nctx))
		       (values (duplicate::rtl_ins/bbv i
				  (ctx ctx))
			  ctx))))
	       ((eq? var *+fx*)
		(let ((range (bbv-range-add intl intr)))
		   (if (bbv-range-fixnum? range)
		       (let ((nctx (extend-ctx ctx dest (list *long*) #t
				      :value range)))
			  (values i nctx))
		       (values (duplicate::rtl_ins/bbv i
				  (ctx ctx))
			  ctx))))
	       ((eq? var *+fx-safe*)
		(let ((range (bbv-range-add intl intr)))
		   (if (bbv-range-fixnum? range)
		       (let ((nctx (extend-ctx ctx dest (list *bint*) #t
				      :value range)))
			  (values (fx-safe->fx var i ctx nctx)
			     nctx))
		       (values (duplicate::rtl_ins/bbv i
				  (ctx ctx))
			  ctx))))
	       ((eq? var *2-*)
		(let ((range (bbv-range-sub intl intr)))
		   (if (bbv-range-fixnum? range)
		       (let ((nctx (extend-ctx ctx dest (list *bint*) #t
				      :value range)))
			  (values (2->fx var i ctx nctx)
			     nctx))
		       (values (duplicate::rtl_ins/bbv i
				  (ctx ctx))
			  ctx))))
	       ((eq? var *2+*)
		(let ((range (bbv-range-add intl intr)))
		   (if (bbv-range-fixnum? range)
		       (let ((nctx (extend-ctx ctx dest (list *bint*) #t
				      :value range)))
			  (values (2->fx var i ctx nctx)
			     nctx))
		       (values (duplicate::rtl_ins/bbv i
				  (ctx ctx))
			  ctx))))
	       (else
		(values (duplicate::rtl_ins/bbv i
			   (ctx ctx))
		   ctx)))))))
   
;* {*---------------------------------------------------------------------*} */
;* {*    rtl_ins-specialize ...                                           *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    Specialize an instruction according to the typing context.       *} */
;* {*    Returns the new instruction and the new context.                 *} */
;* {*---------------------------------------------------------------------*} */
;* (define (rtl_ins-specialize-TBR i::rtl_ins ctx::pair-nil)           */
;*    (with-trace 'bbv-ins "rtl_ins-specialize"                        */
;*       (trace-item "ins=" (shape i))                                 */
;*       (trace-item "ctx=" (ctx->string ctx))                         */
;*       (cond                                                         */
;* {* 	 ((rtl_ins-last? i)                                            *} */
;* {* 	  (tprint "SHOULD NOT...")                                     *} */
;* {* 	  (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t)))       *} */
;* {* 	 ((rtl_ins-typecheck? i)                                       *} */
;* {* 	  (tprint "SHOULD NOT....")                                    *} */
;* {* 	  (rtl_ins-specialize-typecheck-old i ctx))                    *} */
;* {* 	 ((rtl_ins-vector-bound-check? i)                              *} */
;* {* 	  (rtl_ins-specialize-vector-bound-check i ctx))               *} */
;* {*  	 ((rtl_ins-bool? i)                                            *} */
;* {* 	  (rtl_ins-specialize-bool i ctx))                             *} */
;* 	 ((rtl_ins-go? i)                                              */
;* 	  ;; have to duplicate the instruction to break "to" sharing   */
;* 	  (tprint "SHOULD NOT....")                                    */
;* 	  (with-access::rtl_ins i (fun)                                */
;* 	     (let ((s (duplicate::rtl_ins/bbv i                        */
;* 			 (fun (duplicate::rtl_go fun)))))              */
;* 		(values s ctx))))                                      */
;* 	 ((not (rtl_reg? (rtl_ins-dest i)))                            */
;* 	  (tprint "SHOULD NOT....")                                    */
;* 	  (values i ctx))                                              */
;* 	 ((rtl_ins-mov? i)                                             */
;* 	  (tprint "SHOULD NOT....")                                    */
;* 	  (with-access::rtl_ins i (dest args fun)                      */
;* 	     (cond                                                     */
;* 		((and (pair? args) (null? (cdr args)) (rtl_reg/ra? (car args))) */
;* 		 (values i (alias-ctx ctx dest (car args))))           */
;* 		((and *type-call* (pair? args) (rtl_ins-call? (car args))) */
;* 		 (with-access::rtl_ins (car args) (fun)                */
;* 		    (with-access::rtl_call fun (var)                   */
;* 		       (with-access::global var (value type)           */
;* 			  (values i (extend-ctx ctx dest type #t)))))) */
;* 		(else                                                  */
;* 		 (values i (extend-ctx ctx dest *obj* #t))))))         */
;* 	 ((rtl_ins-ifne? i)                                            */
;* 	  ;; have to duplicate the instruction to break "to" sharing   */
;* 	  (case (bool-value (car (rtl_ins-args i)) ctx)                */
;* 	     ((true)                                                   */
;* 	      (tprint "ifne.TRUE...")                                  */
;* 	      (with-access::rtl_ins i (fun)                            */
;* 		 (with-access::rtl_ifne fun (then)                     */
;* 		    (let ((s (duplicate::rtl_ins/bbv i                 */
;* 				(fun (instantiate::rtl_go              */
;* 					(to then))))))                 */
;* 		       (values s ctx)))))                              */
;* 	     ((false)                                                  */
;* 	      (tprint "ifne.FALSE...")                                 */
;* 	      (let ((s (duplicate::rtl_ins/bbv i                       */
;* 			  (fun (instantiate::rtl_nop)))))              */
;* 		 (values s ctx)))                                      */
;* 	     (else                                                     */
;* 	      (with-access::rtl_ins i (fun)                            */
;* 		 (let ((s (duplicate::rtl_ins/bbv i                    */
;* 			     (fun (duplicate::rtl_ifne fun)))))        */
;* 		    (values s ctx))))))                                */
;* 	                                                               */
;* 	 ((and *type-call* (rtl_ins-call? i))                          */
;* 	  (with-access::rtl_ins i (dest fun)                           */
;* 	     (with-access::rtl_call fun (var)                          */
;* 		(with-access::global var (value type)                  */
;* 		   (if (fun? value)                                    */
;* 		       (values i (extend-ctx ctx dest type #t))        */
;* 		       (values i (extend-ctx ctx dest *obj* #t)))))))  */
;* {* 	 ((rtl_ins-vlen? i)                                            *} */
;* {* 	  (rtl_ins-specialize-vlength i ctx))                          *} */
;* 	 (else                                                         */
;* 	  (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t))))))    */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    rtl_ins-specialize-bool ...                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define (rtl_ins-specialize-bool i ctx)                             */
;*                                                                     */
;*    (define (true)                                                   */
;*       (instantiate::rtl_loadi                                       */
;* 	 (constant (instantiate::literal (type *bool*) (value #t)))))  */
;*                                                                     */
;*    (define (false)                                                  */
;*       (instantiate::rtl_loadi                                       */
;* 	 (constant (instantiate::literal (type *bool*) (value #f)))))  */
;*                                                                     */
;*    (with-access::rtl_ins i (dest fun args)                          */
;*       (with-access::rtl_call fun (var)                              */
;* 	 (cond                                                         */
;* 	    ((and (eq? var *<fx*) (=fx (length args) 2))               */
;* 	     (let ((left (bbv-range-value (car args) ctx))              */
;* 		   (right (bbv-range-value (cadr args) ctx)))           */
;* 		(cond                                                  */
;* 		   ((not (and (bbv-range? left) (bbv-range? right)))     */
;* 		    (values i (extend-ctx ctx dest *bool* #t)))        */
;* 		   ((bbv-range<? left right)                            */
;* 		    (let ((ni (duplicate::rtl_ins/bbv i                */
;* 				 (fun (true)))))                       */
;* 		       (values i (extend-ctx ctx dest *bool* #t :value #t)))) */
;* 		   ((bbv-range>=? left right)                           */
;* 		    (let ((ni (duplicate::rtl_ins/bbv i                */
;* 				 (fun (false)))))                      */
;* 		       (values i (extend-ctx ctx dest *bool* #t :value #f)))) */
;* 		   (else                                               */
;* 		    (values i (extend-ctx ctx dest *bool* #t))))))     */
;* 	    (else                                                      */
;* 	     (with-access::rtl_ins i (dest)                            */
;* 		(values i (extend-ctx ctx dest *bool* #t))))))))       */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    interval-value ...                                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define (interval-value i ctx)                                      */
;*    (cond                                                            */
;*       ((isa? i rtl_reg)                                             */
;*        (let ((e (bbv-ctx-get ctx i)))                                   */
;* 	  (when (and e (eq? (bbv-ctxentry-typ e) *int*))               */
;* 	     (let ((v (bbv-ctxentry-value e)))                         */
;* 		(cond                                                  */
;* 		   ((interval? v) v)                                   */
;* 		   ((vector? v) *length-intv*))))))                    */
;*       ((rtl_ins-mov? i)                                             */
;*        (interval-value (car (rtl_ins-args i)) ctx))                 */
;*       ((rtl_ins-call? i)                                            */
;*        (with-access::rtl_ins i (fun)                                */
;* 	  (with-access::rtl_call fun (var)                             */
;* 	     (cond                                                     */
;* 		((eq? var *int->long*)                                 */
;* 		 (interval-value (car (rtl_ins-args i)) ctx))          */
;* 		(else                                                  */
;* 		 #f)))))                                               */
;*       (else                                                         */
;*        #f)))                                                        */

;* {*---------------------------------------------------------------------*} */
;* {*    bool-value ...                                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define (bool-value i ctx)                                          */
;*    (cond                                                            */
;*       ((isa? i rtl_reg)                                             */
;*        (let ((e (bbv-ctx-get ctx i)))                                   */
;* 	  (cond                                                        */
;* 	     ((or (not e) (not (eq? (bbv-ctxentry-typ e) *bool*))) '_) */
;* 	     ((eq? (bbv-ctxentry-value e) #t) 'true)                   */
;* 	     ((eq? (bbv-ctxentry-value e) #f) 'false)                  */
;* 	     (else '_))))                                              */
;*       ((rtl_ins-mov? i)                                             */
;*        (bool-value (car (rtl_ins-args i)) ctx))                     */
;*       ((rtl_ins-loadi? i)                                           */
;*        (with-access::rtl_ins i (fun)                                */
;* 	  (with-access::rtl_loadi fun (constant)                       */
;* 	     (cond                                                     */
;* 		((not (isa? constant literal)) '_)                     */
;* 		((eq? (literal-value constant) #t) 'true)              */
;* 		((eq? (literal-value constant) #f) 'false)             */
;* 		(else '_)))))                                          */
;*       (else                                                         */
;*        '_)))                                                        */

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-vlength ...                                   */
;*---------------------------------------------------------------------*/
;* (define (rtl_ins-specialize-vlength i ctx)                          */
;*    (with-access::rtl_ins i (dest args)                              */
;*       (cond                                                         */
;* 	 ((not (rtl_reg? (car args)))                                  */
;* 	  (values i (extend-ctx ctx dest *int* #t :value *length-intv*))) */
;* 	 ((find (lambda (e)                                            */
;* 		   (with-access::bbv-ctxentry e (typ value)            */
;* 		      (and (eq? type *int*)                            */
;* 			   (vector? value)                             */
;* 			   (eq? (vector-ref value 0) (car args)))))    */
;* 	     ctx)                                                      */
;* 	  =>                                                           */
;* 	  (lambda (e)                                                  */
;* 	     (values (duplicate::rtl_ins i                             */
;* 			(fun (instantiate::rtl_mov)))                  */
;* 		(alias-ctx ctx dest (car args)))))                     */
;* 	 (else                                                         */
;* 	  (values i (extend-ctx ctx dest *int* #t :value (vector (car args)))))))) */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    rtl_ins-vector-bound-check? ...                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define (rtl_ins-vector-bound-check? i::rtl_ins)                    */
;*    (when (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i))                   */
;*       (with-access::rtl_ins i (args fun)                            */
;* 	 (tprint "YAP" (shape i) " " (typeof fun))                     */
;* 	 (when (isa? fun rtl_call)                                     */
;* 	    (with-access::rtl_call fun (var)                           */
;* 	       (when (eq? var *vector-bound-check*)                    */
;* 		  (tprint "YIP")                                       */
;* 		  (let ((args (rtl_ins-args* i)))                      */
;* 		     (and (rtl_reg? (car args)) (rtl_reg? (cadr args)))))))))) */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    rtl_ins-specialize-vector-bound-check ...                        *} */
;* {*---------------------------------------------------------------------*} */
;* (define (rtl_ins-specialize-vector-bound-check i::rtl_ins ctx)      */
;*    (tprint "VECTOR-BOUND-CHECK" (shape i))                          */
;*    (tprint "                  " (ctx->string ctx))                  */
;*    (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t)))           */
