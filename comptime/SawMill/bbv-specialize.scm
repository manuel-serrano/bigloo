;*=====================================================================*/
;*    .../bigloo/bigloo/comptime/SawMill/bbv-specialize.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 07:42:00 2017                          */
;*    Last change :  Tue Jul  5 12:43:21 2022 (serrano)                */
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
	    "SawMill/bbv-types.sch"
	    "SawMill/bbv-interval.sch")
   
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
	    saw_bbv-utils)
   
   (export (bbv-block::blockS b::blockV ctx::pair-nil)
	   (rtl_ins-specializer ::rtl_ins)
;* 	   (rtl_ins-specialize ::rtl_ins ::pair-nil)                   */
	   (rtl_ins-typecheck?::bool ::rtl_ins)
	   (rtl_ins-intcmp?::bool ::rtl_ins)
	   (rtl_ins-specialize-intcmp i ctx)))

;*---------------------------------------------------------------------*/
;*    basic-block versionning configuration                            */
;*---------------------------------------------------------------------*/
(define *type-call* #t)
(define *type-loadi* #t)

;*---------------------------------------------------------------------*/
;*    intervals                                                        */
;*---------------------------------------------------------------------*/
(define *infinity-intv* (interval *-inf.0* *+inf.0*))
(define *length-intv* (interval #l0 *max-length*))
(define *fixnum-intv* (interval *min-fixnum* *max-fixnum*))

;*---------------------------------------------------------------------*/
;*    bbv-block ::blockV ...                                           */
;*---------------------------------------------------------------------*/
(define (bbv-block::blockS b::blockV ctx::pair-nil)
   (with-access::blockV b (label versions succs preds first)
      (with-trace 'bbv (format "bbv-block ~a" label)
	 (let ((ctx (filter-live-in-regs (car first) ctx)))
	    (trace-item "succs=" (map block-label succs))
	    (trace-item "preds=" (map block-label preds))
	    (trace-item "ctx=" (ctx->string ctx))
	    (trace-item "versions= #" (length versions) " "
	       (map ctx->string (map car versions)))
	    (let ((old (assoc ctx versions)))
	       (if (pair? old)
		   (cdr old)
		   (specialize-block! b ctx)))))))
   
;*---------------------------------------------------------------------*/
;*    specialize-block! ...                                            */
;*---------------------------------------------------------------------*/
(define (specialize-block!::blockS b::blockV ctx)
   
   (define (connect! s::blockS ins::rtl_ins)
      (cond
	 ((rtl_ins-ifeq? ins)
	  (with-access::rtl_ins ins (fun)
	     (let ((n (rtl_ifeq-then fun)))
		(block-succs-set! s (list #unspecified n))
		(block-preds-set! n (cons s (block-preds n))))))
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

   (define (duplicate-ins ins ctx)
      (duplicate::rtl_ins/bbv ins
	 (ctx ctx)))
   
   (with-access::blockV b (first label succs versions)
      (with-trace 'bbv-block (format "specialize-block! ~a" label)
	 (let* ((lbl (genlabel))
		(s (instantiate::blockS
		      (%parent b)
		      (label lbl)
		      (first '()))))
	    (set! versions (cons (cons ctx s) versions))
	    (trace-item "new-label=" lbl)
	    (trace-item "ctx=" (shape ctx))
	    (let loop ((oins first)
		       (nins '())
		       (ctx ctx))
	       (cond
		  ((null? oins)
		   (with-access::blockS s (first)
		      (set! first (reverse! nins))
		      s))
		  ((rtl_ins-specializer (car oins))
		   =>
		   (lambda (specialize)
		      ;; instruction specialization
		      (multiple-value-bind (ins ctx)
			 (specialize (car oins) ctx)
			 (let ((ctx (extend-live-out-regs (car oins) ctx)))
			    (cond
			       ((rtl_ins-br? ins)
				(connect! s ins)
				(loop (cdr oins) (cons ins nins) ctx))
			       ((rtl_ins-go? ins) (connect! s ins)
				(connect! s ins)
				(loop '() (cons ins nins) ctx))
			       (else
				(loop (cdr oins) (cons ins nins) ctx)))))))
		  ((rtl_ins-last? (car oins))
		   ;; a return, fail, ...
		   (loop '() (cons (duplicate-ins (car oins) ctx) nins) '()))
		  ((rtl_ins-go? (car oins))
		   (with-access::rtl_ins (car oins) (fun)
		      (with-access::rtl_go fun (to)
			 (let* ((n (bbv-block to ctx))
				(ins (duplicate::rtl_ins/bbv (car oins)
					(ctx ctx)
					(fun (duplicate::rtl_go fun
						(to n))))))
			    (connect! s ins)
			    (loop '() (cons ins nins) ctx)))))
		  ((rtl_ins-ifeq? (car oins))
		   (with-access::rtl_ins (car oins) (fun)
		      (with-access::rtl_ifeq fun (then)
			 (let* ((n (bbv-block then ctx))
				(ins (duplicate::rtl_ins/bbv (car oins)
					(ctx ctx)
					(fun (duplicate::rtl_ifeq fun
						    (then n))))))
			    (connect! s ins)
			    (loop (cdr oins) (cons ins nins) ctx)))))
		  ((rtl_ins-ifne? (car oins))
		   (with-access::rtl_ins (car oins) (fun)
		      (with-access::rtl_ifne fun (then)
			 (let* ((n (bbv-block then ctx))
				(ins (duplicate::rtl_ins/bbv (car oins)
					(ctx ctx)
					(fun (duplicate::rtl_ifne fun
						    (then n))))))
			    (connect! s ins)
			    (loop (cdr oins) (cons ins nins) ctx)))))
;* 		  ((not (rtl_reg? (rtl_ins-dest (car oins))))          */
;* 		   (loop (cdr oins)                                    */
;* 		      (cons (duplicate-ins (car oins) ctx) nins) ctx)) */
		  (else
		   (loop (cdr oins) (cons (duplicate-ins (car oins) ctx) nins)
		      (extend-live-out-regs (car oins) ctx)))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specializer ...                                          */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specializer i::rtl_ins)
   (cond
      ((rtl_ins-typecheck? i) rtl_ins-specialize-typecheck)
      ((rtl_ins-mov? i) rtl_ins-specialize-mov)
      ((rtl_ins-loadi? i) rtl_ins-specialize-loadi)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-typecheck ...                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-typecheck i::rtl_ins ctx)
   (with-trace 'bbv-ins "rtl_ins-specialize-typecheck"
      (multiple-value-bind (reg type flag)
	 (rtl_ins-typecheck i)
	 (let ((e (ctx-get ctx reg)))
	    (with-access::rtl_ins i (fun)
	       (trace-item "ins=" (shape i))
	       (trace-item "typ=" (shape type) " flag=" flag)
	       (trace-item "e=" (shape e))
	       (cond
		  ((and (eq? (bbv-ctxentry-typ e) type)
			(bbv-ctxentry-flag e))
		   ;; positive type simplification
		   (let ((pctx (extend-ctx ctx reg type #t)))
		      (with-access::rtl_ins/bbv i (fun)
			 (if (isa? fun rtl_ifeq)
			     (let ((s (duplicate::rtl_ins/bbv i
					 (ctx ctx)
					 (fun (instantiate::rtl_nop))
					 (dest #f)
					 (args '()))))
				(values s pctx))
			     (with-access::rtl_ifne fun (then)
				(let ((s (duplicate::rtl_ins/bbv i
					    (ctx ctx)
					    (fun (instantiate::rtl_go
						    (to (bbv-block then pctx))))
					    (dest #f)
					    (args '()))))
				   (values s (extend-ctx ctx reg type #f))))))))
		  ((and (eq? (bbv-ctxentry-typ e) type)
			(not (bbv-ctxentry-flag e)))
		   ;; negative type simplification
		   (let ((nctx (extend-ctx ctx reg type #f)))
		      (with-access::rtl_ins/bbv i (fun)
			 (if (isa? fun rtl_ifne)
			     (let ((s (duplicate::rtl_ins/bbv i
					 (ctx ctx)
					 (fun (instantiate::rtl_nop))
					 (dest #f)
					 (args '()))))
				(values s nctx))
			     (with-access::rtl_ifeq fun (then)
				(let ((s (duplicate::rtl_ins/bbv i
					    (ctx ctx)
					    (fun (instantiate::rtl_go
						    (to (bbv-block then nctx))))
					    (dest #f)
					    (args '()))))
				   (values s (extend-ctx ctx reg type #t))))))))
		  ((isa? fun rtl_ifne)
		   (with-access::rtl_ifne fun (then)
		      (let* ((n (bbv-block then
				   (extend-ctx ctx reg type flag)))
			     (s (duplicate::rtl_ins/bbv i
				   (ctx ctx)
				   (fun (duplicate::rtl_ifne fun
					   (then n))))))
			 (values s (extend-ctx ctx reg type (not flag))))))
		  ((isa? fun rtl_ifeq)
		   (with-access::rtl_ifeq fun (then)
		      (let* ((n (bbv-block then
				   (extend-ctx ctx reg type (not flag))))
			     (s (duplicate::rtl_ins/bbv i
				   (fun (duplicate::rtl_ifeq fun
					   (then n))))))
		      (values s (extend-ctx ctx reg type flag)))))
		  (else
		   (error "rtl_ins-specialize-typecheck"
		      "should not be here"
		      (shape i)))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-mov ...                                       */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-mov i::rtl_ins ctx)
   (with-trace 'bbv-ins "rtl_ins-specialize-mov"
      (with-access::rtl_ins i (dest args fun)
	 (cond
	    ((and (pair? args) (null? (cdr args)) (rtl_reg/ra? (car args)))
	     (with-access::rtl_reg (car args) (type)
		(let ((e (ctx-get ctx (car args))))
		   (values i (extend-ctx ctx dest type #t)))))
	    ((and *type-call* (pair? args) (rtl_ins-call? (car args)))
	     (with-access::rtl_ins (car args) (fun)
		(with-access::rtl_call fun (var)
		   (with-access::global var (value type)
		      (values i (extend-ctx ctx dest type #t))))))
	    ((and *type-loadi* (pair? args) (rtl_ins-loadi? (car args)))
	     (with-access::rtl_ins (car args) (fun)
		(with-access::rtl_loadi fun (constant)
		   (with-access::atom constant (type)
		      (values i (extend-ctx ctx dest type #t))))))
	    (else
	     (values i ctx))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-loadi ...                                     */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-loadi i::rtl_ins ctx)
   (with-trace 'bbv-ins "rtl_ins-specialize-loadi"
      (with-access::rtl_ins i (dest args fun)
	 (with-access::rtl_loadi fun (constant)
	    (with-access::atom constant (type)
	       (values i (extend-ctx ctx dest type #t)))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize ...                                           */
;*    -------------------------------------------------------------    */
;*    Specialize an instruction according to the typing context.       */
;*    Returns the new instruction and the new context.                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-TBR i::rtl_ins ctx::pair-nil)
   (with-trace 'bbv-ins "rtl_ins-specialize"
      (trace-item "ins=" (shape i))
      (trace-item "ctx=" (ctx->string ctx))
      (cond
;* 	 ((rtl_ins-last? i)                                            */
;* 	  (tprint "SHOULD NOT...")                                     */
;* 	  (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t)))       */
;* 	 ((rtl_ins-typecheck? i)                                       */
;* 	  (tprint "SHOULD NOT....")                                    */
;* 	  (rtl_ins-specialize-typecheck-old i ctx))                    */
;* 	 ((rtl_ins-vector-bound-check? i)                              */
;* 	  (rtl_ins-specialize-vector-bound-check i ctx))               */
;*  	 ((rtl_ins-bool? i)                                            */
;* 	  (rtl_ins-specialize-bool i ctx))                             */
	 ((rtl_ins-go? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (tprint "SHOULD NOT....")
	  (with-access::rtl_ins i (fun)
	     (let ((s (duplicate::rtl_ins/bbv i
			 (fun (duplicate::rtl_go fun)))))
		(values s ctx))))
	 ((not (rtl_reg? (rtl_ins-dest i)))
	  (tprint "SHOULD NOT....")
	  (values i ctx))
	 ((rtl_ins-mov? i)
	  (tprint "SHOULD NOT....")
	  (with-access::rtl_ins i (dest args fun)
	     (cond
		((and (pair? args) (null? (cdr args)) (rtl_reg/ra? (car args)))
		 (values i (alias-ctx ctx dest (car args))))
		((and *type-call* (pair? args) (rtl_ins-call? (car args)))
		 (with-access::rtl_ins (car args) (fun)
		    (with-access::rtl_call fun (var)
		       (with-access::global var (value type)
			  (values i (extend-normalize-ctx ctx dest type #t))))))
		(else
		 (values i (extend-ctx ctx dest *obj* #t))))))
	 ((rtl_ins-ifeq? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (case (bool-value (car (rtl_ins-args i)) ctx)
	     ((true)
	      (tprint "ifeq.TRUE...")
	      (let ((s (duplicate::rtl_ins/bbv i
			 (fun (instantiate::rtl_nop)))))
		 (values s ctx)))
	     ((false)
	      (tprint "ifeq.FALSE...")
	      (with-access::rtl_ins i (fun)
		 (with-access::rtl_ifeq fun (then)
		    (let ((s (duplicate::rtl_ins/bbv i
				(fun (instantiate::rtl_go
					(to then))))))
		       (values s ctx)))))
	     (else
	      (with-access::rtl_ins i (fun)
		 (let ((s (duplicate::rtl_ins/bbv i
			     (fun (duplicate-iffun fun)))))
		    (values s ctx))))))
	 ((rtl_ins-ifne? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (case (bool-value (car (rtl_ins-args i)) ctx)
	     ((true)
	      (tprint "ifne.TRUE...")
	      (with-access::rtl_ins i (fun)
		 (with-access::rtl_ifeq fun (then)
		    (let ((s (duplicate::rtl_ins/bbv i
				(fun (instantiate::rtl_go
					(to then))))))
		       (values s ctx)))))
	     ((false)
	      (tprint "ifne.FALSE...")
	      (let ((s (duplicate::rtl_ins/bbv i
			  (fun (instantiate::rtl_nop)))))
		 (values s ctx)))
	     (else
	      (with-access::rtl_ins i (fun)
		 (let ((s (duplicate::rtl_ins/bbv i
			     (fun (duplicate::rtl_ifne fun)))))
		    (values s ctx))))))
	 
	 ((and *type-call* (rtl_ins-call? i))
	  (with-access::rtl_ins i (dest fun)
	     (with-access::rtl_call fun (var)
		(with-access::global var (value type)
		   (if (fun? value)
		       (values i (extend-normalize-ctx ctx dest type #t))
		       (values i (extend-ctx ctx dest *obj* #t)))))))
	 ((rtl_ins-vlen? i)
	  (rtl_ins-specialize-vlength i ctx))
	 (else
	  (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-typecheck? ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns the type checked by the instruction or false.            */
;*---------------------------------------------------------------------*/
(define (rtl_ins-typecheck? i::rtl_ins)
   (when (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i))
      (with-access::rtl_ins i (args)
	 (when (and (isa? (car args) rtl_ins) (rtl_ins-call? (car args)))
	    (when (rtl_call-predicate (car args))
	       (let ((args (rtl_ins-args* i)))
		  (and (pair? args) (null? (cdr args)) (rtl_reg? (car args)))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-bool ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-bool i ctx)
   
   (define (true)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #t)))))
   
   (define (false)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #f)))))
   
   (with-access::rtl_ins i (dest fun args)
      (with-access::rtl_call fun (var)
	 (cond
	    ((and (eq? var *<fx*) (=fx (length args) 2))
	     (let ((left (interval-value (car args) ctx))
		   (right (interval-value (cadr args) ctx)))
		(cond
		   ((not (and (interval? left) (interval? right)))
		    (values i (extend-ctx ctx dest *bool* #t)))
		   ((interval<? left right)
		    (let ((ni (duplicate::rtl_ins/bbv i
				 (fun (true)))))
		       (values i (extend-ctx ctx dest *bool* #t #t))))
		   ((interval>=? left right)
		    (let ((ni (duplicate::rtl_ins/bbv i
				 (fun (false)))))
		       (values i (extend-ctx ctx dest *bool* #t #f))))
		   (else
		    (values i (extend-ctx ctx dest *bool* #t))))))
	    (else
	     (with-access::rtl_ins i (dest)
		(values i (extend-ctx ctx dest *bool* #t))))))))
   
;*---------------------------------------------------------------------*/
;*    interval-value ...                                               */
;*---------------------------------------------------------------------*/
(define (interval-value i ctx)
   (cond
      ((isa? i rtl_reg)
       (let ((e (ctx-get ctx i)))
	  (when (and e (eq? (bbv-ctxentry-typ e) *int*))
	     (let ((v (bbv-ctxentry-value e)))
		(cond
		   ((interval? v) v)
		   ((vector? v) *length-intv*))))))
      ((rtl_ins-mov? i)
       (interval-value (car (rtl_ins-args i)) ctx))
      ((rtl_ins-call? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_call fun (var)
	     (cond
		((eq? var *int->long*)
		 (interval-value (car (rtl_ins-args i)) ctx))
		(else
		 #f)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    bool-value ...                                                   */
;*---------------------------------------------------------------------*/
(define (bool-value i ctx)
   (cond
      ((isa? i rtl_reg)
       (let ((e (ctx-get ctx i)))
	  (cond
	     ((or (not e) (not (eq? (bbv-ctxentry-typ e) *bool*))) '_)
	     ((eq? (bbv-ctxentry-value e) #t) 'true)
	     ((eq? (bbv-ctxentry-value e) #f) 'false)
	     (else '_))))
      ((rtl_ins-mov? i)
       (bool-value (car (rtl_ins-args i)) ctx))
      ((rtl_ins-loadi? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_loadi fun (constant)
	     (cond
		((not (isa? constant literal)) '_)
		((eq? (literal-value constant) #t) 'true)
		((eq? (literal-value constant) #f) 'false)
		(else '_)))))
      (else
       '_)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-vlength ...                                   */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-vlength i ctx)
   (with-access::rtl_ins i (dest args)
      (cond
	 ((not (rtl_reg? (car args)))
	  (values i (extend-ctx ctx dest *int* #t *length-intv*)))
	 ((find (lambda (e)
		   (with-access::bbv-ctxentry e (typ value)
		      (and (eq? type *int*)
			   (vector? value)
			   (eq? (vector-ref value 0) (car args)))))
	     ctx)
	  =>
	  (lambda (e)
	     (values (duplicate::rtl_ins i
			(fun (instantiate::rtl_mov)))
		(alias-ctx ctx dest (car args)))))
	 (else
	  (values i (extend-ctx ctx dest *int* #t (vector (car args))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-intcmp? ...                                              */
;*---------------------------------------------------------------------*/
(define (rtl_ins-intcmp? i)

   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (rtl_reg? dest))))))
   
   (with-access::rtl_ins i (dest fun args)
      (when (isa? fun rtl_call)
	 (with-access::rtl_call fun (var)
	    (and (=fx (length args) 2)
		 (or (eq? var *<fx*) (eq? var *<=fx*)
		     (eq? var *>fx*) (eq? var *>=fx*)
		     (eq? var *=fx*))
		 (or (reg? (car args)) (rtl_ins-loadi? (car args)))
		 (or (reg? (cadr args)) (rtl_ins-loadi? (cadr args))))))))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-intcmp ...                                    */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-intcmp i ctx)
   
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

   (define (intcmp-op i)
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
	 ((<) '>)
	 ((<=) '>=)
	 ((>) '<)
	 ((>=) '<=)
	 (else op)))
   
   (define (resolve/op i op intl intr)
      (case op
	 ((<)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval<? intl intr) (true) (false)))))
	 ((<=)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval<=? intl intr) (true) (false)))))
	 ((>=)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval>=? intl intr) (true) (false)))))
	 ((>)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval>? intl intr) (true) (false)))))
	 ((=)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval=? intl intr) (true) (false)))))
	 (else
	  #f)))

   (define (test-ctxs-ref reg intl intr op ctx)
      (if (or (not (interval? intl)) (not (interval? intr)))
	  (values ctx ctx)
	  (case op
	     ((<)
	      (let ((intrt (interval-lt intl intr))
		    (intro (interval-gte intl intr)))
		 (values (extend-ctx ctx reg *int* #t intrt)
		    (extend-ctx ctx reg *int* #t intro))))
	     ((<=)
	      (let ((intrt (interval-lte intl intr))
		    (intro (interval-gt intl intr)))
		 (values (extend-ctx ctx reg *int* #t intrt)
		    (extend-ctx ctx reg *int* #t intro))))
	     ((>)
	      (let ((intrt (interval-gt intl intr))
		    (intro (interval-lte intl intr)))
		 (values (extend-ctx ctx reg *int* #t intrt)
		    (extend-ctx ctx reg *int* #t intro))))
	     ((>=)
	      (let ((intrt (interval-gte intl intr))
		    (intro (interval-lt intl intr)))
		 (values (extend-ctx ctx reg *int* #t intrt)
		    (extend-ctx ctx reg *int* #t intro))))
	     ((== ===)
	      (let ((ieq (interval-eq intl intr)))
		 (values (if (interval? ieq)
			     (extend-ctx ctx reg *int* #t ieq)
			     ctx)
		    ctx)))
	     ((!= !==)
	      (let ((ieq (interval-eq intl intr)))
		 (values
		    ctx
		    (if (interval? ieq)
			(extend-ctx ctx reg *int* #t ieq)
			ctx))))
	     (else
	      (values ctx ctx)))))
   
   (define (specialize/op op lhs intl rhs intr sctx)
      (cond
	 ((and (reg? lhs) (reg? rhs))
	  (multiple-value-bind (lctxt lctxo)
	     (test-ctxs-ref (reg lhs) intl intr op sctx)
	     (multiple-value-bind (rctxt rctxo)
		(test-ctxs-ref (reg rhs) intr intl (inv-op op) '())
		(values (append rctxt lctxt) (append rctxo lctxo)))))
	 ((reg? lhs)
	  (test-ctxs-ref (reg lhs) intl intr op sctx))
	 ((reg? rhs)
	  (test-ctxs-ref (reg rhs) intr intl (inv-op op) sctx))
	 (else
	  (values #f #f))))
   
   (with-access::rtl_ins i (dest fun args)
      (with-access::rtl_call fun (var)
	 (let* ((lhs (car args))
		(rhs (cadr args))
		(intl (interval-value lhs ctx))
		(intr (interval-value rhs ctx))
		(sctx (extend-ctx ctx dest *bool* #t))
		(op (intcmp-op i)))
	    (cond
	       ((not (and (interval? intl) (interval? intr)))
		(multiple-value-bind (ctxt ctxo)
		   (specialize/op op lhs (or intl *fixnum-intv*)
		      rhs (or intr *fixnum-intv*) sctx)
		   (values i sctx ctxt ctxo)))
	       ((resolve/op i op intl intr)
		=>
		(lambda (ni) (values ni sctx #f #f)))
	       (else
		(multiple-value-bind (ctxt ctxo)
		   (specialize/op op lhs intl rhs intr sctx)
		   (values i sctx ctxt ctxo))))))))

;*---------------------------------------------------------------------*/
;*    interval<? ...                                                   */
;*---------------------------------------------------------------------*/
(define (interval<? left right)
   (<fx (interval-max left) (interval-min right)))

;*---------------------------------------------------------------------*/
;*    interval<=? ...                                                  */
;*---------------------------------------------------------------------*/
(define (interval<=? left right)
   (<=fx (interval-max left) (interval-min right)))

;*---------------------------------------------------------------------*/
;*    interval>? ...                                                   */
;*---------------------------------------------------------------------*/
(define (interval>? left right)
   (>fx (interval-min left) (interval-max right)))

;*---------------------------------------------------------------------*/
;*    interval>=? ...                                                  */
;*---------------------------------------------------------------------*/
(define (interval>=? left right)
   (>=fx (interval-min left) (interval-max right)))

;*---------------------------------------------------------------------*/
;*    interval=? ...                                                   */
;*---------------------------------------------------------------------*/
(define (interval=? left right)
   (and (=fx (interval-min left) (interval-min right))
	(=fx (interval-max left) (interval-max right))))

;*---------------------------------------------------------------------*/
;*    interval-lts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-lts left::struct right::struct shift::int)
   (let ((ra (- (interval-max right) shift)))
      (if (< ra (interval-max left))
	  (if (>= ra (interval-min left))
	      (interval (min (interval-min left) ra) ra)
	      (interval ra ra))
	  left)))

(define (interval-lt left right)
   (interval-lts left right 1))

(define (interval-lte left right)
   (interval-lts left right 0))

;*---------------------------------------------------------------------*/
;*    interval-gts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-gts left::struct right::struct shift::int)
   (let ((ri (+ (interval-min right) shift)))
      (if (> ri (interval-min left))
	  (if (<= ri (interval-max left))
	      (interval ri (max (interval-max left) ri))
	      (interval ri ri))
	  left)))

(define (interval-gt left right)
   (interval-gts left right 1))

(define (interval-gte left right)
   (interval-gts left right 0))

;*---------------------------------------------------------------------*/
;*    interval-eq ...                                                  */
;*---------------------------------------------------------------------*/
(define (interval-eq left::struct right::struct)
   (let* ((ri (interval-min right))
	  (ra (interval-max right))
	  (li (interval-min left))
	  (la (interval-max left))
	  (oi (max ri li))
	  (oa (min ra la)))
      (if (<= oi oa)
	  (interval oi oa)
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-vector-bound-check? ...                                  */
;*---------------------------------------------------------------------*/
(define (rtl_ins-vector-bound-check? i::rtl_ins)
   (when (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i))
      (with-access::rtl_ins i (args fun)
	 (tprint "YAP" (shape i) " " (typeof fun))
	 (when (isa? fun rtl_call)
	    (with-access::rtl_call fun (var)
	       (when (eq? var *vector-bound-check*)
		  (tprint "YIP")
		  (let ((args (rtl_ins-args* i)))
		     (and (rtl_reg? (car args)) (rtl_reg? (cadr args))))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-vector-bound-check ...                        */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-vector-bound-check i::rtl_ins ctx)
   (tprint "VECTOR-BOUND-CHECK" (shape i))
   (tprint "                  " (ctx->string ctx))
   (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t)))
   
;*---------------------------------------------------------------------*/
;*    duplicate-iffun ...                                              */
;*---------------------------------------------------------------------*/
(define (duplicate-iffun fun)
   (if (isa? fun rtl_ifeq)
       (duplicate::rtl_ifeq fun)
       (duplicate::rtl_ifne fun)))
