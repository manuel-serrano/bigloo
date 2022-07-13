;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-types.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 07:05:22 2017                          */
;*    Last change :  Wed Jul 13 07:55:40 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BBV specific types                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-types
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
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
	    saw_bbv-cache
	    saw_bbv-range)

   (export  (wide-class blockV::block
	       (versions::pair-nil (default '()))
	       (%mark::long (default -1))
	       (widener::bool (default #f)))
	    (wide-class blockS::block
	       (%mark::long (default -1))
	       (%hash::obj (default #f))
	       (%blacklist::obj (default '()))
	       (parent::obj read-only (default #unspecified)))
	    (wide-class rtl_ins/bbv::rtl_ins
	       (def (default #unspecified))
	       (out (default #unspecified))
	       (in (default #unspecified))
	       (ctx::pair-nil (default '()))
	       (%hash::obj (default #f)))

	    (class bbv-ctxentry
	       (reg::rtl_reg read-only)
	       (typ::type read-only (default *obj*))
	       (flag::bool read-only)
	       (value read-only (default '_))
	       (aliases::pair-nil (default '())))

	    *max-length*
	    *max-index*
	    *+inf.0*
	    *-inf.0*
	    
	    (ctx->string ::pair-nil)
	    (params->ctx ::pair-nil)
	    (ctx-get ::pair-nil ::rtl_reg)
	    (extend-ctx::pair-nil ::pair-nil ::rtl_reg ::obj ::bool
	       #!key (value '_))
	    (extend-ctx* ctx::pair-nil regs::pair type flag::bool
	       #!key (value '_))
	    (extend-normalize-ctx::pair-nil ::pair-nil ::obj ::obj ::obj
	       #!key (value '_))
	    (refine-ctx::pair-nil ::pair-nil ::obj ::obj ::obj
	       #!key (value '_))
	    (alias-ctx::pair-nil ::pair-nil ::rtl_reg ::rtl_reg)
	    (unalias-ctx::pair-nil ::pair-nil ::rtl_reg)
	    
	    (generic bbv-hash ::obj)
	    (generic bbv-equal?::bool ::obj ::obj)

	    (rtl_ins-last?::bool i::rtl_ins)
	    (rtl_ins-nop?::bool i::rtl_ins)
	    (rtl_ins-mov?::bool i::rtl_ins)
	    (rtl_ins-go?::bool i::rtl_ins)
	    (rtl_ins-br?::bool i::rtl_ins)
	    (rtl_ins-ifeq?::bool i::rtl_ins)
	    (rtl_ins-ifne?::bool i::rtl_ins)
	    (rtl_ins-call?::bool i::rtl_ins)
	    (rtl_ins-vlen?::bool i::rtl_ins)
	    (rtl_ins-loadi?::bool i::rtl_ins)
	    (rtl_ins-bool?::bool i::rtl_ins)
	    (rtl_ins-true?::bool i::rtl_ins)
	    (rtl_ins-false?::bool i::rtl_ins)
	    (rtl_ins-return? i::rtl_ins)
	    
	    (rtl_ins-typecheck i::rtl_ins)
	    (rtl_call-predicate i::rtl_ins)
	    (rtl_call-values i::rtl_ins)))

;*---------------------------------------------------------------------*/
;*    integer boundaries ...                                           */
;*---------------------------------------------------------------------*/
(define *max-fixnum* (bit-lshllong #l1 (-fx (bigloo-config 'int-size) 2)))
(define *min-fixnum* (-llong (negllong *max-fixnum*) #l1))

(define *max-length* *max-fixnum*)
(define *max-index* (-llong *max-length* #l1))

(define *+inf.0* (+llong *max-fixnum* #l1))
(define *-inf.0* (-llong *min-fixnum* #l1))

;*---------------------------------------------------------------------*/
;*    bbv-ctxentry-reg ...                                             */
;*---------------------------------------------------------------------*/
(define (bbv-ctxentry-reg e)
   (with-access::bbv-ctxentry e (reg)
      reg))

;*---------------------------------------------------------------------*/
;*    shape ::bbv-ctxentry ...                                         */
;*---------------------------------------------------------------------*/
(define-method (shape e::bbv-ctxentry)
   (with-access::bbv-ctxentry e (reg typ flag value aliases)
      (vector (shape reg)
	 (if flag
	     (shape typ)
	     (string-append "!" (shape typ)))
	 (format "~s" (shape value))
	 (map shape aliases))))
   
;*---------------------------------------------------------------------*/
;*    ctx->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (ctx->string ctx)
   (call-with-output-string
      (lambda (op)
	 (fprintf op "~s" (map shape ctx)))))

;*---------------------------------------------------------------------*/
;*    params->ctx ...                                                  */
;*---------------------------------------------------------------------*/
(define (params->ctx params)
   ;; a context is an alist of:
   ;;   (reg . (type . #t|#f))
   (sort (lambda (x y)
	    (<=fx (rtl_reg/ra-num (bbv-ctxentry-reg x))
	       (rtl_reg/ra-num (bbv-ctxentry-reg y))))
      (filter-map (lambda (p)
		     (when (isa? p rtl_reg/ra)
			(with-access::rtl_reg p (type)
			   (instantiate::bbv-ctxentry
			      (reg p)
			      (typ type)
			      (flag #t)))))
	 params)))

;*---------------------------------------------------------------------*/
;*    ctx-get ...                                                      */
;*---------------------------------------------------------------------*/
(define (ctx-get ctx reg)
   (let loop ((ctx ctx))
      (when (pair? ctx)
	 (if (eq? (bbv-ctxentry-reg (car ctx)) reg)
	     (car ctx)
	     (loop (cdr ctx))))))

;*---------------------------------------------------------------------*/
;*    extend-ctx/entry ...                                             */
;*    -------------------------------------------------------------    */
;*    Extend CTX with ENTRY. If the register of ENTRY is already in    */
;*    the CTX, the former entry is replaced with the new one.          */
;*---------------------------------------------------------------------*/
(define (extend-ctx/entry ctx::pair-nil entry::bbv-ctxentry)
   (let* ((reg (bbv-ctxentry-reg entry))
	  (rnum (rtl_reg/ra-num reg)))
      (let loop ((ctx ctx))
	 (cond
	    ((null? ctx)
	     (list entry))
	    ((>fx (rtl_reg/ra-num (bbv-ctxentry-reg (car ctx))) rnum)
	     (cons entry ctx))
	    ((eq? (bbv-ctxentry-reg (car ctx)) reg)
	     (cons entry (cdr ctx)))
	    (else
	     (cons (car ctx) (loop (cdr ctx))))))))
	  
;*---------------------------------------------------------------------*/
;*    extend-ctx ...                                                   */
;*    -------------------------------------------------------------    */
;*    Extend the context with a new register assignement.              */
;*---------------------------------------------------------------------*/
(define (extend-ctx ctx::pair-nil reg::rtl_reg type flag::bool
	   #!key (value '_))
   
   (define (new-ctxentry reg::rtl_reg type flag::bool value)
      (instantiate::bbv-ctxentry
	 (reg reg)
	 (typ type)
	 (flag flag)
	 (value value)))
   
   (if (not (isa? reg rtl_reg/ra))
       ctx
       (let ((rnum (rtl_reg/ra-num reg)))
	  (let loop ((ctx ctx))
	     (cond
		((null? ctx)
		 (let ((n (new-ctxentry reg type flag value)))
		    (list n)))
		((>fx (rtl_reg/ra-num (bbv-ctxentry-reg (car ctx))) rnum)
		 (let ((n (new-ctxentry reg type flag value)))
		    (cons n ctx)))
		((eq? (bbv-ctxentry-reg (car ctx)) reg)
		 (let ((n (duplicate::bbv-ctxentry (car ctx)
			     (typ type)
			     (flag flag)
			     (value value))))
		    (cons n (cdr ctx))))
		(else
		 (cons (car ctx) (loop (cdr ctx)))))))))

;*---------------------------------------------------------------------*/
;*    extend-ctx* ...                                                  */
;*    -------------------------------------------------------------    */
;*    Extend the context with a new register assignement.              */
;*---------------------------------------------------------------------*/
(define (extend-ctx* ctx::pair-nil regs::pair type flag::bool
	   #!key (value '_))
   (let loop ((ctx ctx)
	      (regs regs))
      (if (null? regs)
	  ctx
	  (loop (extend-ctx ctx (car regs) type flag :value value)
	     (cdr regs)))))

;*---------------------------------------------------------------------*/
;*    extend-normalize-ctx ...                                         */
;*---------------------------------------------------------------------*/
(define (extend-normalize-ctx ctx reg type flag #!key (value '_))
   (let ((tynorm (assq type *type-norms*)))
      (if (pair? tynorm)
	  (extend-ctx ctx reg (cdr tynorm) flag :value value)
	  (extend-ctx ctx reg type flag :value value))))

;*---------------------------------------------------------------------*/
;*    refine-ctx ...                                                   */
;*    -------------------------------------------------------------    */
;*    Refine the knowledge of an environment, propagating the          */
;*    information to the aliases.                                      */
;*---------------------------------------------------------------------*/
(define (refine-ctx ctx reg type flag #!key (value '_))
   
   (define (new-ctxentry reg type flag value)
      (instantiate::bbv-ctxentry
	 (reg reg)
	 (typ type)
	 (flag flag)
	 (value value)))
   
   (define (refine-one reg ctx)
      (if (not (isa? reg rtl_reg/ra))
	  (cons ctx '())
	  (let ((rnum (rtl_reg/ra-num reg)))
	     (let loop ((ctx ctx))
		(cond
		   ((null? ctx)
		    (let ((n (new-ctxentry reg type flag value)))
		       (values (list n) '())))
		   ((>fx (rtl_reg/ra-num (bbv-ctxentry-reg (car ctx))) rnum)
		    (let ((n (new-ctxentry reg type flag value)))
		       (values (cons n ctx) '())))
		   ((eq? (bbv-ctxentry-reg (car ctx)) reg)
		    (let ((n (duplicate::bbv-ctxentry (car ctx)
				(typ type)
				(flag flag)
				(value value))))
		       (values (cons n (cdr ctx))
			  (with-access::bbv-ctxentry (car ctx) (aliases)
			     aliases))))
		   (else
		    (multiple-value-bind (rctx aliases)
		       (loop (cdr ctx))
		       (values (cons (car ctx) rctx) aliases))))))))
   
   (let loop ((worklist (list reg))
	      (stack '())
	      (ctx ctx))
      (cond
	 ((null? worklist)
	  ctx)
	 ((memq (car worklist) stack)
	  (loop (cdr worklist) stack ctx))
	 (else
	  (multiple-value-bind (nctx aliases)
	     (refine-one (car worklist) ctx)
	     (loop (append aliases (cdr worklist))
		(cons (car worklist) stack) nctx))))))

;*---------------------------------------------------------------------*/
;*    alias-ctx ...                                                    */
;*    -------------------------------------------------------------    */
;*    Create an alias between REG and ALIAS.                           */
;*---------------------------------------------------------------------*/
(define (alias-ctx ctx::pair-nil reg::rtl_reg alias::rtl_reg)
   (let ((re (ctx-get ctx reg))
	 (ae (ctx-get ctx alias)))
      (with-access::bbv-ctxentry re (aliases)
	 (with-access::bbv-ctxentry ae ((aaliases aliases))
	    (let ((all (delete-duplicates
			  (cons alias (append aliases aaliases)))))
	       (let ((nre (duplicate::bbv-ctxentry re
			     (aliases all))))
		  (let loop ((all all)
			     (ctx (extend-ctx/entry ctx nre)))
		     (if (null? all)
			 ctx
			 (let ((ae (ctx-get ctx (car all))))
			    (if ae
				(with-access::bbv-ctxentry ae (aliases)
				   (let ((nae (duplicate::bbv-ctxentry ae
						 (aliases (cons reg aliases)))))
				      (loop (cdr all)
					 (extend-ctx/entry ctx nae))))
				(loop (cdr all) ctx)))))))))))
	  
;*---------------------------------------------------------------------*/
;*    unalias-ctx ...                                                  */
;*    -------------------------------------------------------------    */
;*    Removing all REG aliasings.                                      */
;*---------------------------------------------------------------------*/
(define (unalias-ctx ctx::pair-nil reg::rtl_reg)
   
   (define (unalias ctx::pair-nil reg::rtl_reg alias::rtl_reg)
      (let ((e (ctx-get ctx alias)))
	 (if e
	     (with-access::bbv-ctxentry e (aliases)
		(extend-ctx/entry ctx
		   (duplicate::bbv-ctxentry e
		      (aliases (remq reg aliases)))))
	     ctx)))
   
   (let ((e (ctx-get ctx reg)))
      (if e
	  (with-access::bbv-ctxentry e (aliases typ flag value)
	     (let loop ((aliases aliases)
			(ctx (extend-ctx ctx reg typ flag :value value)))
		(if (null? aliases)
		    ctx
		    (loop (cdr aliases)
		       (unalias ctx reg (car aliases))))))
	  ctx)))

;*---------------------------------------------------------------------*/
;*    shape ::rtl_ins/bbv ...                                          */
;*---------------------------------------------------------------------*/
(define-method (shape o::rtl_ins/bbv)
   (call-with-output-string
      (lambda (p)
	 (dump o p 0))))

;*---------------------------------------------------------------------*/
;*    shape ::block ...                                                */
;*---------------------------------------------------------------------*/
(define-method (shape o::block)
   (call-with-output-string
      (lambda (p)
	 (dump o p 0))))

;*---------------------------------------------------------------------*/
;*    shape ::blockS ...                                               */
;*---------------------------------------------------------------------*/
(define-method (shape o::blockS)
   (call-with-output-string
      (lambda (p)
	 (dump o p 0))))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_ins/bbv ...                                           */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_ins/bbv p m)

   (define (dump-ctx ctx p)
      (display (map shape ctx) p))

   (with-access::rtl_ins/bbv o (%spill fun dest args def in out ctx)
      (with-output-to-port p
	 (lambda ()
	    (display "[" p)
	    (when dest
	       (display "(" p)
	       (dump dest p m)
	       (display " <- " p))
	    (dump-ins-rhs o p m)
	    (when dest
	       (display ")" p))
	    (display " " p)
	    (dump-ctx ctx p)
	    (display "]" p)
	    (display* " ;; def=" (map shape (regset->list def)))
	    (display* " in=" (map shape (regset->list in)))
	    (display* " out=" (map shape (regset->list out)))))))

;*---------------------------------------------------------------------*/
;*    dump ::blockV ...                                                */
;*---------------------------------------------------------------------*/
(define-method (dump o::blockV p m)
   
   (define (lbl n)
      (if (isa? n block) (block-label n) (typeof n)))
   
   (with-access::block o (label first)
      (fprint p "(blockV " label)
      (with-access::blockV o (preds succs widener)
	 (dump-margin p (+fx m 1))
	 (fprint p ":widener " widener)
	 (dump-margin p (+fx m 1))
	 (fprint p ":preds " (map lbl preds))
	 (dump-margin p (+fx m 1))
	 (fprint p ":succs " (map lbl succs)))
      (dump-margin p (+fx m 1))
      (dump* first p (+fx m 1))
      (display "\n )\n" p)))

;*---------------------------------------------------------------------*/
;*    dump ::blockS ...                                                */
;*---------------------------------------------------------------------*/
(define-method (dump o::blockS p m)
   
   (define (lbl n)
      (if (isa? n block) (block-label n) (typeof n)))
   
   (with-access::block o (label first)
      (fprint p "(blockS " label)
      (with-access::blockS o (parent preds succs)
	 (dump-margin p (+fx m 1))
	 (fprint p ":parent " (block-label parent))
	 (dump-margin p (+fx m 1))
	 (fprint p ":widener " (with-access::blockV parent (widener) widener))
	 (dump-margin p (+fx m 1))
	 (fprint p ":preds " (map lbl preds))
	 (dump-margin p (+fx m 1))
	 (fprint p ":succs " (map lbl succs)))
      (dump-margin p (+fx m 1))
      (dump* first p (+fx m 1))
      (display "\n )\n" p)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-last? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-last? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_last)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-nop? ...                                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-nop? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_nop)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-mov? ...                                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-mov? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_mov)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-go? ...                                                  */
;*---------------------------------------------------------------------*/
(define (rtl_ins-go? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_go)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-br? ...                                                  */
;*---------------------------------------------------------------------*/
(define (rtl_ins-br? i::rtl_ins)
   (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-ifeq? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-ifeq? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_ifeq)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-ifne? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-ifne? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_ifne)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-call? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-call? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_call)))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-vlen? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-vlen? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_vlength)))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-loadi? ...                                               */
;*---------------------------------------------------------------------*/
(define (rtl_ins-loadi? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_loadi)))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-loadi-value ...                                          */
;*---------------------------------------------------------------------*/
(define (rtl_ins-boolean? ins bool)
   (with-access::rtl_ins ins (fun)
      (when (isa? fun rtl_loadi)
	 (with-access::rtl_loadi fun (constant)
	    (with-access::literal constant (value)
	       (eq? value bool))))))
   
(define (rtl_ins-true? ins)
   (rtl_ins-boolean? ins #t))
   
(define (rtl_ins-false? ins)
   (rtl_ins-boolean? ins #f))

;*---------------------------------------------------------------------*/
;*    rtl_ins-return? ...                                              */
;*---------------------------------------------------------------------*/
(define (rtl_ins-return? i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (isa? fun rtl_return)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-bool? ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-bool? i::rtl_ins)
   (with-access::rtl_ins i (dest fun)
      (when (and (isa? dest rtl_reg) (isa? fun rtl_call))
	 (with-access::rtl_call fun (var)
	    (with-access::global var (value type)
	       (eq? type *bool*))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-typecheck ...                                            */
;*---------------------------------------------------------------------*/
(define (rtl_ins-typecheck i::rtl_ins)
   (with-access::rtl_ins i (args)
      (let ((typ (rtl_call-predicate (car args)))
	    (val (rtl_call-values (car args))))
	 (let ((args (rtl_ins-args* i)))
	    (values (car args) typ (rtl_ins-ifne? i) val)))))

;*---------------------------------------------------------------------*/
;*    rtl_call-predicate ...                                           */
;*---------------------------------------------------------------------*/
(define (rtl_call-predicate i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (with-access::rtl_call fun (var)
	 (let ((val (variable-value var)))
	    (fun-predicate-of val)))))

;*---------------------------------------------------------------------*/
;*    rtl_call-values ...                                              */
;*---------------------------------------------------------------------*/
(define (rtl_call-values i::rtl_ins)
   (with-access::rtl_ins i (fun)
      (with-access::rtl_call fun (var)
	 (let* ((val (variable-value var))
		(typ (fun-predicate-of val)))
	    (cond
	       ((eq? typ *bint*) (fixnum-range))
	       (else '_))))))

;*---------------------------------------------------------------------*/
;*    bit-xor* ...                                                     */
;*---------------------------------------------------------------------*/
(define (bit-xor* lst)
   (if (null? lst)
       0
       (let loop ((lst (cdr lst))
		  (hash (car lst)))
	  (if (null? lst)
	      hash
	      (loop (cdr lst) (bit-xor (car lst) hash))))))

;*---------------------------------------------------------------------*/
;*    bbv-hash ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (bbv-hash o)
   2345)

;*---------------------------------------------------------------------*/
;*    bbv-hash ::type ...                                              */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::type)
   (with-access::type o (id)
      (symbol-hash-number id)))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::atom ...                                              */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::atom)
   (with-access::atom o (value)
      (get-hashnumber value)))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::blockS ...                                            */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::blockS)
   
   (define (hash-instructions ins)
      (if (null? ins) 0 (bit-xor* (map bbv-hash ins))))
   
   (with-access::blockS o (%hash first)
      (unless %hash (set! %hash (hash-instructions first)))
      %hash))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_ins/bbv ...                                       */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_ins/bbv)
   (with-access::rtl_ins/bbv o (%hash fun dest args)
      (unless %hash
	 (set! %hash
	    (bit-xor
	       (bbv-hash dest)
	       (bit-xor
		  (bit-xor* (map bbv-hash args))
		  (bbv-hash fun)))))
      %hash))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_reg ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_reg)
   (with-access::rtl_reg o (name key type)
      (bit-xor (bbv-hash type)
	 (bit-xor (symbol-hash-number name) (symbol-hash-number key)))))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_fun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_fun)
   (symbol-hash-number (class-name (object-class o))))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_return ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_return)
   (with-access::rtl_return o (type)
      (bit-xor (call-next-method) (bbv-hash type))))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_loadi ...                                         */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_loadi)
   (with-access::rtl_loadi o (constant)
      (bit-xor (call-next-method) (bbv-hash constant))))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_loadg ...                                         */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_loadg)
   (with-access::rtl_loadg o (var)
      (bit-xor (call-next-method) (bbv-hash var))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_loadfun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_loadfun)
   (with-access::rtl_loadfun o (var)
      (bit-xor (call-next-method) (bbv-hash var))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_globalref ...                                     */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_globalref)
   (with-access::rtl_globalref o (var)
      (bit-xor (call-next-method) (bbv-hash var))))

;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_getfield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_getfield)
   (bit-xor (call-next-method)
      (with-access::rtl_getfield o (name objtype type)
	 (bit-xor (bbv-hash name)
	    (bit-xor (bbv-hash objtype) (bbv-hash type))))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_valloc ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_valloc)
   (bit-xor (call-next-method)
      (bit-xor (bbv-hash (rtl_valloc-type o)) (bbv-hash (rtl_valloc-vtype o)))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_vref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_vref)
   (bit-xor (call-next-method)
      (bit-xor (bbv-hash (rtl_vref-type o)) (bbv-hash (rtl_vref-vtype o)))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_vlength ...                                       */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_vlength)
   (bit-xor (call-next-method)
      (bit-xor (bbv-hash (rtl_vlength-type o)) (bbv-hash (rtl_vlength-vtype o)))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_instanceof ...                                    */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_instanceof)
   (bit-xor (call-next-method)
      (bbv-hash (rtl_instanceof-type o))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_storeg ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_storeg)
   (bit-xor (call-next-method)
      (with-access::rtl_storeg o (var)
	 (with-access::global var (id module)
	    (bit-xor (bbv-hash module) (bbv-hash id))))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_setfield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_setfield)
   (bit-xor (call-next-method)
      (with-access::rtl_setfield o (name objtype type)
	 (bit-xor (bbv-hash name)
	    (bit-xor (bbv-hash objtype) (bbv-hash type))))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_vset ...                                          */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_vset)
   (bit-xor (call-next-method)
      (bit-xor (bbv-hash (rtl_vset-type o)) (bbv-hash (rtl_vset-vtype o)))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_new ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_new)
   (bit-xor (call-next-method)
      (with-access::rtl_new o (constr type)
	 (bit-xor (bbv-hash type)
	    (bit-xor* (map bbv-hash constr))))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_call ...                                          */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_call)
   (with-access::rtl_call o (var)
      (bit-xor (call-next-method) (bbv-hash var))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_lightfuncall ...                                  */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_lightfuncall)
   (bit-xor (call-next-method)
      (with-access::rtl_lightfuncall o (name funs rettype)
	 (bit-xor (bbv-hash name)
	    (bit-xor (bbv-hash rettype)
	       (bit-xor* (map bbv-hash funs)))))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_pragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_pragma)
   (bit-xor (call-next-method)
      (bbv-hash (rtl_pragma-format o))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_cast)
   (bit-xor (call-next-method)
      (bit-xor (bbv-hash (rtl_cast-fromtype o)) (bbv-hash (rtl_cast-totype o)))))
   
;*---------------------------------------------------------------------*/
;*    bbv-hash ::rtl_cast_null ...                                     */
;*---------------------------------------------------------------------*/
(define-method (bbv-hash o::rtl_cast_null)
   (bit-xor (call-next-method) (bbv-hash (rtl_cast_null-type o))))
   
;*---------------------------------------------------------------------*/
;*    bbv-equal? ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (bbv-equal? x y)
   (eq? x y))
   
;*---------------------------------------------------------------------*/
;*    bbv-equal? ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::atom y)
   (when (isa? y atom)
      (equal? (atom-value x) (atom-value y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::blockS ...                                          */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::blockS y)
   (when (isa? y blockS)
      (when (=fx (length (block-first x)) (length (block-first y)))
	 (every bbv-equal? (block-first x) (block-first y)))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_ins/bbv ...                                     */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_ins/bbv y)
   (and (bbv-equal? (rtl_ins-dest x) (rtl_ins-dest y))
	(=fx (length (rtl_ins-args x)) (length (rtl_ins-args y)))
	(every bbv-equal? (rtl_ins-args x) (rtl_ins-args y))
	(bbv-equal? (rtl_ins-fun x) (rtl_ins-fun y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_reg ...                                         */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_reg y)
   (and (bbv-equal? (rtl_reg-type x) (rtl_reg-type y))
	(bbv-equal? (rtl_reg-var x) (rtl_reg-var y))
	(bbv-equal? (rtl_reg-name x) (rtl_reg-name y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_fun y)
   (eq? (object-class x) (object-class y)))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_select ...                                      */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_select y)
   (when (isa? y rtl_select)
      (and (eq? (rtl_select-type x) (rtl_select-type x))
	   (=fx (length (rtl_select-patterns x)) (length (rtl_select-patterns y)))
	   (every bbv-equal? (rtl_select-patterns x) (rtl_select-patterns y)))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_switch ...                                      */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_switch y)
   (when (isa? y rtl_switch)
      (and (=fx (length (rtl_switch-labels x)) (length (rtl_switch-labels y)))
	   (every bbv-equal? (rtl_switch-labels x) (rtl_switch-labels y)))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_loadi ...                                       */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_loadi y)
   (when (isa? y rtl_loadi)
      (bbv-equal? (rtl_loadi-constant x) (rtl_loadi-constant y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_loadg ...                                       */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_loadg y)
   (when (isa? y rtl_loadg)
      (bbv-equal? (rtl_loadg-var x) (rtl_loadg-var y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_loadfun ...                                     */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_loadfun y)
   (when (isa? y rtl_loadfun)
      (bbv-equal? (rtl_loadfun-var x) (rtl_loadfun-var y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_globalref ...                                   */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_globalref y)
   (when (isa? y rtl_globalref)
      (bbv-equal? (rtl_globalref-var x) (rtl_globalref-var y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_getfield ...                                    */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_getfield y)
   (when (isa? y rtl_getfield)
      (and (string=? (rtl_getfield-name x) (rtl_getfield-name y))
	   (bbv-equal? (rtl_getfield-objtype x) (rtl_getfield-objtype y))
	   (bbv-equal? (rtl_getfield-type x) (rtl_getfield-type y)))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::valloc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_valloc y)
   (when (isa? y rtl_valloc)
      (bbv-equal? (rtl_valloc-type x) (rtl_valloc-type y))
      (bbv-equal? (rtl_valloc-vtype x) (rtl_valloc-vtype y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_vref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_vref y)
   (when (isa? y rtl_vref)
      (bbv-equal? (rtl_vref-type x) (rtl_vref-type y))
      (bbv-equal? (rtl_vref-vtype x) (rtl_vref-vtype y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::vlength ...                                         */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_vlength y)
   (when (isa? y rtl_vlength)
      (bbv-equal? (rtl_vlength-type x) (rtl_vlength-type y))
      (bbv-equal? (rtl_vlength-vtype x) (rtl_vlength-vtype y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_instanceof ...                                  */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_instanceof y)
   (when (isa? y rtl_instanceof)
      (bbv-equal? (rtl_instanceof-type x) (rtl_instanceof-type y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_storeg ...                                      */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_storeg y)
   (when (isa? y rtl_storeg)
      (bbv-equal? (rtl_storeg-var x) (rtl_storeg-var y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_setfield ...                                    */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_setfield y)
   (when (isa? y rtl_setfield)
      (and (string=? (rtl_setfield-name x) (rtl_setfield-name y))
	   (bbv-equal? (rtl_setfield-objtype x) (rtl_setfield-objtype y))
	   (bbv-equal? (rtl_setfield-type x) (rtl_setfield-type y)))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_vset ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_vset y)
   (when (isa? y rtl_vset)
      (and (bbv-equal? (rtl_vset-type x) (rtl_vset-type y))
	   (bbv-equal? (rtl_vset-vtype x) (rtl_vset-vtype y)))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_new ...                                         */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_new y)
   (error "bbv-equal?" "not implemented" x))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_call ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_call y)
   (when (isa? y rtl_call)
      (bbv-equal? (rtl_call-var x) (rtl_call-var y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_lightfuncall ...                                */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_lightfuncall y)
   (error "bbv-equal?" "not implemented" x))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_pragma ...                                      */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_pragma y)
   (when (isa? y rtl_pragma)
      (string=? (rtl_pragma-format x) (rtl_pragma-format y))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_cast ...                                        */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_cast y)
   (when (isa? y rtl_cast)
      (and (bbv-equal? (rtl_cast-totype x) (rtl_cast-totype y))
	   (bbv-equal? (rtl_cast-fromtype x) (rtl_cast-fromtype y)))))

;*---------------------------------------------------------------------*/
;*    bbv-equal? ::rtl_cast_null ...                                   */
;*---------------------------------------------------------------------*/
(define-method (bbv-equal? x::rtl_cast_null y)
   (when (isa? y rtl_cast_null)
      (bbv-equal? (rtl_cast_null-type x) (rtl_cast_null-type y))))

   
