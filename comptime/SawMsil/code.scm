(module msil_code
   (import engine_param
	   type_type 
	   type_env
	   ast_var
	   ast_node
	   module_module
	   object_class
	   object_slots
	   read_jvm
	   backend_backend
	   backend_bvm
	   backend_dotnet_class
	   backend_lib
	   backend_cplib
	   saw_defs
	   msil_names
	   msil_out
	   msil_maxstack
	   msil_funcall
	   msil_inline
	   )
   (export (module-code me::dotnet v::global params l)
	   (gen-expr me::dotnet ins))
   (static (wide-class lreg::rtl_reg index param?)
	   ;; Better to have final classes for rtl_*call ?
	   (class tail_call::rtl_call)
	   (class tail_lightfuncall::rtl_lightfuncall)
	   (class tail_funcall::rtl_funcall)
	   (class tail_apply::rtl_apply)
	   ) )

(define *comment* #f)
(define *hasprotect* #f)
(define *protectresult* #f)
(define *bexitreg* #f)
(define *bugmonoreg* #f)
(define *switchreg* #f)

;;
;; Main function
;;
(define (module-code me v::global params l)
   (declare-locals me (sort (get-locals params l)
			    (lambda (r1 r2) (<fx (lreg-index r1)
						 (lreg-index r2) ))))
   (print-info me (string-append " -> " (global-name v)))
   (for-each (lambda (b)
		(label me "L" (block-label b))
		(for-each (lambda (i) (gen-ins me i)) (block-first b)) )
	     l )
   ;; Special code for protected functions
   (when *hasprotect*
      (label me "" "endprotect")
      (open-catch me "bigloo.bexception" "fin")
      (load-var me (lreg-index *bugmonoreg*))
      (brfalse me "" "bug_mono")
      (rethrow me)
      (label me "" "bug_mono")
      (push-int me 1)
      (store-var me (lreg-index *bugmonoreg*))
      (load-var me (lreg-index *bexitreg*))
      (castclass me (find-type 'exit))
      (libcall me 'obj 'bigloo.foreign 'debug_handler '(bexception exit))
      (store-var me (lreg-index *protectresult*))
      (open-catch me "System.Exception" "fin")
      (load-var me (lreg-index *bexitreg*))
      (castclass me (find-type 'exit))
      (libcall me 'obj 'bigloo.foreign 'debug_dot_handler '(exception exit))
      (store-var me (lreg-index *protectresult*))
      (close-try me "fin")
      (load-var me (lreg-index *protectresult*))
      (return me) ))

;;
;; Scan the code to retrieve local variable
;; Update all reg to lreg and return all regs not in params.
;;
(define (get-locals params l) ;()
   (set! *hasprotect* #f)
   (set! *protectresult* #f)
   (let ( (n 0) (regs '()) )
      (define (reg->lreg r)
	 ;; Update a register.
	 (if (not (lreg? r))
	     (begin (widen!::lreg r (index n) (param? #f))
		    (set! n (+fx n 1))
		    (set! regs (cons r regs)) )))
      (define (nr t)
	 ;; Create a new register for specific use.
	 (let ( (r (instantiate::rtl_reg
		      (type t)
		      (var #f)
		      (name (gensym)))) )
	    (reg->lreg r)
	    r ))
      (define (check_fun e)
	 ;; Check if something must be done for specific instructions.
	 (let ( (fun (rtl_ins-fun e)) )
	    (cond
	       ((rtl_protect? fun)
		(set! *hasprotect* #t)
		(set! *bugmonoreg* (nr (find-type 'bool)))
		(set! *bexitreg* (rtl_ins-dest e))
		(set! *protectresult* (nr (find-type 'obj))) )
	       ((rtl_switch? fun)
		(set! *switchreg* (nr (find-type 'int))) ))))
      (define (expr e)
	 (if (rtl_reg? e)
	     (reg->lreg e)
	     (begin (check_fun e)
		    (for-each expr (rtl_ins-args e)) )))
      (define (instruction e)
	 (let ( (dest (rtl_ins-dest e)) )
	    (if dest (reg->lreg dest))
	    (check_fun e)
	    (for-each expr (rtl_ins-args e)) ))
      (let loop ( (l params) (n 0) )
	 (unless (null? l)
	    (widen!::lreg (car l) (index n) (param? #t))
	    (loop (cdr l) (+fx n 1)) ))
      (for-each (lambda (b) (for-each instruction (block-first b))) l)
      regs ))

;;
;; Instruction generation
;;
(define (gen-ins me ins)
   (if *comment* (dmp me ins))
   (with-access::rtl_ins ins (dest fun args)
      ;; Call the generic method and check for existing value on stack
      (if (eq? (gen-args-gen-fun fun me args) 'no-value)
	  (when dest
	     (push-constant me "unspecified")
	     (store me dest) )
	  (if dest
	      (store me dest)
	      (let ( (n (size-dest-fun fun)) )
		 (when (>fx n 0)
		    (pop me) ))))))

(define (gen-expr me::dotnet ins)
   (if (rtl_reg? ins)
       (_load me ins)
       (with-access::rtl_ins ins (fun args)
	  (when (eq? (gen-args-gen-fun fun me args) 'no-value)
	     (push-constant me "unspecified")) )))

(define (_load me r)
   (let ( (index (lreg-index r)) )
      (if (lreg-param? r) (load-par me index) (load-var me index)) ))

(define (store me r)
   (let ( (index (lreg-index r)) )
      (if (lreg-param? r) (store-par me index) (store-var me index)) ))
   
(define (dmp me ins)
   (comment me "DUMP" (get-expr ins)) )

(define (get-expr ins)
   (if (not (rtl_reg? ins))
       (with-access::rtl_ins ins (fun args)
	  (cons (class-name (object-class fun))
		(map get-expr args) ))
       (let ( (index (lreg-index ins)) )
	  (string-append (if (lreg-param? ins) "P" "V")
			 (integer->string index) ))))

;;
;; Main entry for instruction generation (argument not generated)
;; Specific for return, predicates and some inlined functions
;;
(define (gen-args-gen-fun-default fun::rtl_fun me args);
   ;; Default case : generate arguments and call another generic method.
   (for-each (lambda (a) (gen-expr me a)) args)
   (gen-fun-with-args fun me args) )
   
(define-generic (gen-args-gen-fun fun::rtl_fun me args);
   (gen-args-gen-fun-default fun me args) )

(define-method (gen-args-gen-fun fun::rtl_return me args);
   (when (and *dotnet-tail* (not *hasprotect*))
      (patch-call (car args)) )
   (gen-args-gen-fun-default fun me args) )

;(let ( (arg (car args)) )
;      (if (or (rtl_reg? arg) (not *dotnet-tail*))
;	  (gen-args-gen-fun-default fun me args)
;	  (let ( (call? (rtl_ins-fun arg)) )
;	     (if (not (rtl_call? call?))
;		 (gen-args-gen-fun-default fun me args)
;		 (let ( (f (rtl_call-var call?)) )
;		    (if (and (or (eq? (global-module f) *module*)
;				 *dotnet-tail-across-modules*)
;			     (not *hasprotect*) )
;			(begin
;			   (for-each (lambda (a) (gen-expr me a))
;				     (rtl_ins-args arg) )
;			   (tail-call-global me f)
;			   (return me) )
;			(gen-args-gen-fun-default fun me args) )))))))

(define (patch-call ins);
   (unless (rtl_reg? ins)
      (let ( (fun (rtl_ins-fun ins)) )
	 (cond
	    ((rtl_mov? fun) (patch-call (car (rtl_ins-args ins))))
	    ((rtl_call? fun)
	     (let ( (f (rtl_call-var fun)) )
		(rtl_ins-fun-set! ins (instantiate::tail_call (var f))) ))
	    ((rtl_lightfuncall? fun)
		(rtl_ins-fun-set! ins (instantiate::tail_lightfuncall)) )
	    ((rtl_funcall? fun)
		(rtl_ins-fun-set! ins (instantiate::tail_funcall)) )
	    ((rtl_apply? fun)
		(rtl_ins-fun-set! ins (instantiate::tail_apply)) )
	    (else #unspecified) ))))


(define-method (gen-args-gen-fun fun::rtl_call me args);
   (let ( (r (inline-call-with-args? me (rtl_call-var fun) args)) )
      (if (eq? r  'not-inlined)
	  (gen-args-gen-fun-default fun me args)
	  r )))

(define-method (gen-args-gen-fun fun::rtl_ifeq me args);
   (let ( (arg (car args)) (lab (block-label (rtl_ifeq-then fun))) )
      (if (rtl_reg? arg)
	  (begin (gen-expr me arg)
		 (brfalse me "L" lab) )
	  (with-access::rtl_ins arg (fun args)
	     (gen-args-gen-predicate fun me args #f lab) ))))

(define-method (gen-args-gen-fun fun::rtl_ifne me args);
   (let ( (arg (car args)) (lab (block-label (rtl_ifne-then fun))) )
      (if (rtl_reg? arg)
	  (begin (gen-expr me arg)
		 (brtrue me "L" lab) )
	  (with-access::rtl_ins arg (fun args)
	     (gen-args-gen-predicate fun me args #t lab) ))))

;;
;; Entry for instruction who needs the (already generated) argument.
;; Specific for instruction with no specific arity
;;
(define-generic (gen-fun-with-args fun::rtl_fun me args);
   ;; Default case: forget arguments and call another generic method.
   (gen-fun fun me) )

(define-method (gen-fun-with-args fun::rtl_lightfuncall me args);
   (gen-funcall me args #f) )

(define-method (gen-fun-with-args fun::tail_lightfuncall me args);
   (gen-funcall me args #t) )

(define-method (gen-fun-with-args fun::rtl_funcall me args);
   (gen-funcall me args #f) )

(define-method (gen-fun-with-args fun::tail_funcall me args);
   (gen-funcall me args #t) )

(define (gen-funcall me args tail?)
   (let ( (n (-fx (length args) 1)) )
      (if (<fx n 5)
	  (let ( (f "bigloo.procedure::funcall") )
	     (when (and tail? *dotnet-tail-funcall*) (declare-tail me))
	     (callvirt me 'obj (string-append f (integer->string n))
		       (make-list n 'obj) ))
	  (begin (push-constant me "nil")
		 (for-each (lambda (a) (newobj me "bigloo.pair" '(obj obj)))
			   (cdr args) )
		 (when (and tail? *dotnet-tail-funcall*)
		    (declare-tail me) )
		 (callvirt me 'obj "bigloo.procedure::apply" '(obj)) ))))

(define-method (gen-fun-with-args fun::rtl_new me args);
   (newobj me (type-name (rtl_new-type fun)) (rtl_new-constr fun)) )

;;
;; Last entry for generating instructions
;;
(define-generic (gen-fun fun::rtl_fun me));

;; Simple one
(define-method (gen-fun fun::rtl_nop me);
   'no-value )

(define-method (gen-fun fun::rtl_mov me);
   'ok )

;; Constants
(define-method (gen-fun fun::rtl_loadi me);
   (let ( (constant (rtl_loadi-constant fun)) )
      (let ( (value (atom-value constant)) )
	 (cond
	    ((number? value)
	     (push-num me value (type-id (node-type constant))) )
	    ((null? value)
	     (push-constant me "nil") )
	    ((boolean? value)
	     (push-int me (if value 1 0)) )
	    ((char? value)
	     (push-int me (char->integer value)) )
	    ((string? value)
	     (push-string me value)
	     ;; CARE do the creation at clinit time
	     (libcall me 'bstring 'bigloo.foreign 'getbytes '(fstring)) )
	    ((eof-object? value)
	     (push-constant me "eof") )
	    ((eq? value '#!optional)
	     (push-constant me "optional") )
	    ((eq? value '#!rest)
	     (push-constant me "rest") )
	    ((eq? value '#!key)
	     (push-constant me "key") )
	    ((eq? value #unspecified)
	     (push-constant me "unspecified") )
	    ((ucs2? value)
	     ;; CARE do the creation at clinit time
	     (push-int me (ucs2->integer value))
	     (newobj me "bigloo.bucs2" '(ucs2)) )
	    (else (error "loadi" "bad atom value" value)) ))))

;; Accessing globals
(define-method (gen-fun fun::rtl_loadg me);
   (load-global me (rtl_loadg-var fun)) )

(define-method (gen-fun fun::rtl_storeg me);
   (store-global me (rtl_storeg-var fun)) )

(define-method (gen-fun fun::rtl_globalref me);
   (let ( (var (rtl_globalref-var fun)) )
      (push-int me (indexed-index var))
      (push-int me '0)
      (newobj me (dotnet-qname me) '(int int)) ))

;; Vectors
(define-method (gen-fun fun::rtl_vref me);
   (load-vector me (rtl_vref-type fun)) )

(define-method (gen-fun fun::rtl_vset me);
   (store-vector me (rtl_vset-type fun)) )

(define-method (gen-fun fun::rtl_vlength me);
   (load-vector-length me) )

(define-method (gen-fun fun::rtl_valloc me);
   (newarray me (rtl_valloc-type fun)) )

;; Objects
(define-method (gen-fun fun::rtl_getfield me);
   (with-access::rtl_getfield fun (name objtype type)
      ;; CARE MANU
      (if (wide-class? objtype) (set! objtype (wide->chunk objtype)))
      (load-field me type objtype name) ))

(define-method (gen-fun fun::rtl_setfield me);
   (with-access::rtl_setfield fun (name objtype type)
      ;; CARE MANU
      (if (wide-class? objtype) (set! objtype (wide->chunk objtype)))
      (store-field me type objtype name) ))

(define-method (gen-fun fun::rtl_isa me);
   (isinst me (rtl_isa-type fun))
   (let ( (l1 (gensym "I")) (l2 (gensym "I")) )
      (brfalse me "" l1)
      (push-int me 1)
      (br me "" l2)
      (label me "" l1)
      (push-int me 0)
      (label me "" l2) ))

(define-method (gen-fun fun::rtl_cast me);
   (when *purify*
      (let ( (type (rtl_cast-type fun)) )
	 (unless (string=? (type-name type) "class System.Object")
	     (castclass me type) ))))

(define-method (gen-fun fun::rtl_cast_null me);
   (let ( (type (rtl_cast_null-type fun)) )
      (if (eq? type 'float)
	  (push-num me 0 'float)
	  (ldnull me)) ))

;; Box
(define-method (gen-fun fun::rtl_makebox me);
   (newobj me "bigloo.cell" '(obj)) )

(define-method (gen-fun fun::rtl_boxref me);
   (load-field me 'obj "bigloo.cell" 'car) )

(define-method (gen-fun fun::rtl_boxset me);
   (store-field me 'obj "bigloo.cell" 'car) )


;; Call/Return
(define-method (gen-fun fun::rtl_call me);
   (let ( (var (rtl_call-var fun)) )
      (let ( (r (inline-call? me var)) )
	 (if (eq? r  'not-inlined)
	     (begin
		(when (and (tail_call? fun)
			   (or (eq? (global-module var) *module*)
			       *dotnet-tail-across-modules* ))
		   (declare-tail me) )
		(call-global me var) )
	     r ))))

(define-method (gen-fun fun::rtl_loadfun me);
   (let ( (var (rtl_loadfun-var fun)) )
      (push-int me (indexed-index var)) ))

(define-method (gen-fun fun::rtl_apply me);
   (callvirt me 'obj "bigloo.procedure::apply" '(obj)) )

(define-method (gen-fun fun::tail_apply me);
   (when *dotnet-tail-funcall* (declare-tail me))
   (callvirt me 'obj "bigloo.procedure::apply" '(obj)) )

(define-method (gen-fun fun::rtl_return me);
   (if *hasprotect*
       (begin (store-var me (lreg-index *protectresult*))
	      (br me "" "endprotect") )
       (return me) ))

;; Control
(define-method (gen-fun fun::rtl_go me);
   (br me "L" (block-label (rtl_go-to fun))) )

(define-method (gen-fun fun::rtl_switch me);
   (with-access::rtl_switch fun (type patterns labels)
      ;; CARE do we have to make a coercion from "type" to int ??
      (let ( (ldef #unspecified) (num2lab '()) )
	 (define (add n lab) (set! num2lab (cons (cons n lab) num2lab)))
	 (for-each (lambda (pat lab)
		      (if (eq? pat 'else)
			  (set! ldef lab)
			  (for-each (lambda (n) (add n lab)) pat) ))
		   patterns
		   (map block-label labels) )
	 (cond
	    ((null? (cdr num2lab))
	     (push-int me (caar num2lab))
	     (bne me "L" ldef)
	     (br me "L" (cdar num2lab)) )
	    (else
	     (set! num2lab (sort num2lab (lambda (x y) (<fx (car x) (car y)))))
	     (let* ( (nums (map car num2lab))
		     (min (car nums))
		     (max (car (last-pair nums)))
		     (n (length nums)) )
		(if (< (/ n (+fx 1 (-fx max min))) 0.75)
		    (_lookup me ldef num2lab *switchreg*)
		    (_table me ldef min (flat num2lab ldef)) )))))))

(define (flat al ldef)
   (define (walk al i r)
      (cond
	 ((null? al) (reverse! r))
	 ((=fx i (caar al)) (walk (cdr al) (+fx i 1) (cons (cdar al) r)))
	 ((>fx i (caar al)) (walk (cdr al) i r))
	 (else (walk al (+fx i 1) (cons ldef r))) ))
   (walk al (caar al) '()) )

(define (_lookup me ldef num2lab reg)
   (store me reg)
   (for-each (lambda (s)
		(_load me reg)
		(push-int me (car s))
		(beq me "L" (cdr s)) )
	     num2lab )
   (br me "L" ldef) )

(define (_table me def beg table)
   (if (and (>= beg 0) (<fx beg 5))
       (let walk ( )
	  (if (not (=fx beg 0))
	      (begin (set! beg (-fx beg 1))
		     (set! table (cons def table))
		     (walk) ))))
   (unless (=fx beg 0)
      (push-int me beg)
      (sub me) )
   (switch me (map (lambda (n) (string-append "L" (integer->string n))) table))
   (br me "L" def) )

;; Exceptions
(define-method (gen-fun fun::rtl_jumpexit me);
   (libcall me 'obj 'bigloo.foreign 'jumpexit '(obj obj))
   ;; Dead code !!
   (castclass me (find-type 'exit))
   (throw me) )

(define-method (gen-fun fun::rtl_fail me);
   (libcall me 'exception 'bigloo.foreign 'fail '(obj obj obj))
   (throw me) )

(define-method (gen-fun fun::rtl_protect me);
   (push-int me 0)
   (store-var me (lreg-index *bugmonoreg*))
   (libcall me 'obj 'bigloo.foreign 'setexit '())
   (store-var me (lreg-index *bexitreg*))
   (open-try me)
   (load-var me (lreg-index *bexitreg*))
   (castclass me (find-type 'exit)) )

(define-method (gen-fun fun::rtl_protected me);
   ;; CARE Strange nothing to do...
   'ok )

;;
;; Main entry for predicate generation (arguments generated)
;; Drop args and call another generic function.
;; Currently not used by inlined functions
;;
(define-generic (gen-args-gen-predicate fun::rtl_fun me args on? lab);
   ;; Default case
   (for-each (lambda (a) (gen-expr me a)) args)
   (gen-predicate fun me on? lab) )

;;
;; Last entry for generating predicate
;;
(define-generic (gen-predicate fun::rtl_fun me on? lab);
   (default-gen-predicate fun me on? lab) )

(define (default-gen-predicate fun me on? lab);
   ;; Default case
   (gen-fun fun me)
   (if on?
       (brtrue me "L" lab)
       (brfalse me "L" lab) ))

;; Call
(define-method (gen-predicate fun::rtl_call me on? lab);
   (let ( (r (inline-predicate? me (rtl_call-var fun) on? lab)) )
      (if (eq? r  'not-inlined)
	  (default-gen-predicate fun me on? lab)
	  r )))

;;CARE isa

