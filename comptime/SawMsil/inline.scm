(module msil_inline
   (import type_type 
	   type_env
	   ast_var
	   ast_node
	   object_class
	   object_slots
	   module_module
	   cnst_alloc
	   read_jvm
	   backend_backend
	   backend_bvm
	   backend_dotnet_class
	   saw_defs
	   msil_out
	   msil_code
	   ast_env)
   (export (check-msil-inlines)
	   (inline-call? me::dotnet var::global)
	   (inline-predicate? me::dotnet var::global on? lab)
	   (inline-call-with-args? me::dotnet var::global args)) )

(define *msil-inlines* '())

(define (check-msil-inlines)
   (let ((unresolved (filter (lambda (i) (not (find-global i)))
			     *msil-inlines*)))
      (when (pair? unresolved)
	 (error 'SawMsil "Cannot find inlined functions" unresolved))))

(define-macro (declare-inline! name)
   (when (> *compiler-debug* 0)
      `(set! *msil-inlines* (cons ,name *msil-inlines*))))
       
;;
(define (inline-call? me::dotnet var::global)
   (let ( (fun (getprop (global-id var) 'msil_inline_function)) )
      (if fun
	  (fun me)
	  'not-inlined )))

(define-macro (define-inline-call name . body)
   `(begin
       (declare-inline! ',name)
       (putprop! ',name 'msil_inline_function
		 (lambda (me) ,@body) )))

;;
(define (inline-call-with-args? me::dotnet var::global args)
   (let ( (fun (getprop (global-id var) 'msil_inline_function_args)) )
      (if fun
	  (fun me args)
	  'not-inlined )))

(define-macro (define-inline-call-args name . body)
   `(begin
       (declare-inline! ',name)
       (putprop! ',name 'msil_inline_function_args
		 (lambda (me args) ,@body) )))
;;
(define (inline-predicate? me::dotnet var::global on? lab)
   (let ( (fun (getprop (global-id var) 'msil_inline_predicate)) )
      (if fun
	  (fun me on? lab)
	  'not-inlined )))

(define-macro (define-inline-predicate name . body)
   `(begin
       (declare-inline! ',name)
       (putprop! ',name 'msil_inline_predicate
		 (lambda (me on? lab) ,@body) )))

;;
(define (skip-mov arg)
   (if (rtl_ins? arg)
       (let ( (fun (rtl_ins-fun arg)) )
	  (if (rtl_mov? fun)
	      (skip-mov (car (rtl_ins-args arg)))
	      fun ))
       arg ))

;;;
;; Mandatory
;;;
(define-inline-call-args cnst-table-ref
   (load-global me (get-cnst-table))
   (gen-expr me (car args))
   (load-vector me 'obj) )

(define-inline-call-args cnst-table-set!
   (load-global me (get-cnst-table))
   (gen-expr me (car args))
   (gen-expr me (cadr args))
   (store-vector me 'obj)
   'no-value )

(define-inline-call make-fx-procedure
   (newobj me (dotnet-qname me) '(int int int)) )

(define-inline-call make-va-procedure
   (newobj me (dotnet-qname me) '(int int int)) )

(define-inline-call make-l-procedure
   (dup me)
   (newobj me (dotnet-qname me) '(int int int)) )

(define-inline-call make-el-procedure
   (newarray me 'obj) )

(define-inline-call procedure-el-ref
   (load-vector me 'obj) )

(define-inline-call procedure-el-set!
   (store-vector me 'obj)
   'no-value )

(define-inline-call-args make-el-procedure-1
   (ldnull me) )

(define-inline-call procedure-1-el-ref
   (pop me) )

;(define-inline-call procedure-1-el-set!) done in node2rtl

;;;
;; Generate 0 instructions (identity)
;;;
(define-inline-call $long->int
   #unspecified )

(define-inline-call $int->long
   #unspecified )

;;;
;; Generate 1 instructions
;;;
(define-inline-call $bint->int
   (load-field me 'int "bigloo.bint" 'value) )

(define-inline-call $bint->long
   (load-field me 'int "bigloo.bint" 'value) )

;(define-inline-call c-fixnum? "INTEGERP")
;(method static c-elong?::bool (::obj) "ELONGP")
;(method static c-llong?::bool (::obj) "LLONGP")

(define-inline-call c-=fx
   (ceq me) )

(define-inline-predicate c-=fx
   (if on? (beq me "L" lab) (bne me "L" lab)) )

(define-inline-call c-=elong
   (ceq me) )

(define-inline-predicate c-=elong
   (if on? (beq me "L" lab) (bne me "L" lab)) )

(define-inline-call c-=llong
   (ceq me) )

(define-inline-predicate c-=llong
   (if on? (beq me "L" lab) (bne me "L" lab)) )

(define-inline-call c-<fx
   (clt me) )

(define-inline-call c->fx
   (cgt me) )

(define-inline-predicate c-<fx
   (if on? (blt me "L" lab) (bge me "L" lab)) )

(define-inline-predicate c->fx
   (if on? (bgt me "L" lab) (ble me "L" lab)) )

(define-inline-predicate c-<=fx
   (if on? (ble me "L" lab) (bgt me "L" lab)) )

(define-inline-predicate c->=fx
   (if on? (bge me "L" lab) (blt me "L" lab)) )

;(define-inline-call c-evenfx?::bool (::long))
;(define-inline-call c-oddfx?::bool (::long) "ODDP_FX")

(define-inline-call c-+fx
   (add me) )

(define-inline-call c--fx
   (sub me) )

(define-inline-call c-*fx
   (mul me) )

(define-inline-call c-/fx
   (div me) )

(define-inline-call c-negfx
   (neg me) )

(define-inline-call c-quotientfx
   (div me) )

(define-inline-call c-remainderfx
   (rem me) )

;;;
;; Generate 3 instructions
;;;
(define-inline-call c-<=fx
   (cgt me)
   (push-int me 1)
   (bitxor me) )

(define-inline-call c->=fx
   (clt me)
   (push-int me 1)
   (bitxor me) )

;;;
;; BAGUE
;;;
(define-inline-call-args $bool->bbool
   (let ( (fun (skip-mov (car args))) )
      (if (rtl_loadi? fun)
	  (if (atom-value (rtl_loadi-constant fun))
	      (push-constant2 me "bbool" "vrai")
	      (push-constant2 me "bbool" "faux") )
	  (let ( (l1 (gensym "I")) (l2 (gensym "I")) )
	     (gen-expr me (car args))
	     (brfalse me "" l1)
	     (push-constant2 me "bbool" "vrai")
	     (br me "" l2)
	     (label me "" l1)
	     (push-constant2 me "bbool" "faux")
	     (label me "" l2) ))))

(define-inline-call $obj->bool
   (push-constant2 me "bbool" "faux")
   (ceq me)
   (push-int me 1)
   (bitxor me) )

(define-inline-predicate $obj->bool
   (push-constant2 me "bbool" "faux")
   (if on? (bne me "L" lab) (beq me "L" lab)) )

(define-inline-call c-cons
   (newobj me "bigloo.pair" '(obj obj)) )

(define-inline-predicate c-pair?
   (isinst me (find-type 'pair))
   (if on? (brtrue me "L" lab) (brfalse me "L" lab)) )

(define-inline-call c-car
   (load-field me 'obj "bigloo.pair" 'car) )

(define-inline-call c-cdr
   (load-field me 'obj "bigloo.pair" 'cdr) )

(define-inline-predicate c-fixnum?
   (isinst me (find-type 'bint))
   (if on? (brtrue me "L" lab) (brfalse me "L" lab)) )

(define-inline-call $string-bound-check?
   (clt me) )

(define-inline-predicate $string-bound-check?
   (if on? (blt me "L" lab) (bge me "L" lab)) )

(define-inline-call $vector-bound-check?
   (clt me) )

(define-inline-predicate $vector-bound-check?
   (if on? (blt me "L" lab) (bge me "L" lab)) )

;;;
;; BEVAL
;;;
(define-inline-predicate c-procedure?
   (isinst me (find-type 'procedure))
   (if on? (brtrue me "L" lab) (brfalse me "L" lab)) )

(define-inline-call-args procedure-ref
   (gen-expr me (car args))
   (load-field me 'vector "bigloo.procedure" 'env)
   (gen-expr me (cadr args))
   (load-vector me 'obj) )

(define-inline-call-args procedure-set!
   (gen-expr me (car args))
   (load-field me 'vector "bigloo.procedure" 'env)
   (gen-expr me (cadr args))
   (gen-expr me (caddr args))
   (store-vector me 'obj)
   'no-value )

(define-inline-predicate c-boxed-eq?
   (if on? (beq me "L" lab) (bne me "L" lab)) )

(define-inline-call c-set-car!
   (store-field me 'obj "bigloo.pair" 'car)
   'no-value )

(define-inline-call c-set-cdr!
   (store-field me 'obj "bigloo.pair" 'cdr)
   'no-value )

(define-inline-predicate c-null?
   (push-constant me "nil")
   (if on? (beq me "L" lab) (bne me "L" lab)) )

(define-inline-predicate c-symbol?
   (isinst me (find-type 'symbol))
   (if on? (brtrue me "L" lab) (brfalse me "L" lab)) )

;; Check if better than double check (as in foreign.cs)
;(define-inline-predicate c-boolean?
;   (isinst me (find-type 'bbool))
;   (if on? (brtrue me "L" lab) (brfalse me "L" lab)) )

;; Still a bug in mono for VECTORP
;(define-inline-predicate $vector?
;   (isinst me (find-type 'vector))
;   (if on? (brtrue me "L" lab) (brfalse me "L" lab)) )


;BCHAR int -> bchar

;;
;; CONFORM
;;
(define-inline-call $string-length
   (load-vector-length me) )

(define-inline-call c-symbol->string
   (load-field me 'bstring "bigloo.symbol" 'pname) )

;;
;; FFT
;;
(define-inline-predicate c-flonum?
   (isinst me (find-type 'real))
   (if on? (brtrue me "L" lab) (brfalse me "L" lab)) )

(define-inline-call $double->real
   (newobj me "bigloo.real" '(double)) )
   
(define-inline-call $real->double
   (load-field me 'double "bigloo.real" 'value) )

(define-inline-predicate c-=fl
   (if on? (beq me "L" lab) (bne me "L" lab)) )

(define-inline-predicate c-<fl
   (if on? (blt me "L" lab) (bge me "L" lab)) )

(define-inline-predicate c->fl
   (if on? (bgt me "L" lab) (ble me "L" lab)) )

(define-inline-predicate c-<=fl
   (if on? (ble me "L" lab) (bgt me "L" lab)) )

(define-inline-predicate c->=fl
   (if on? (bge me "L" lab) (blt me "L" lab)) )

;CARE mono add a conv.r8 after operation
(define-inline-call c-+fl
   (add me) )

(define-inline-call c--fl
   (sub me) )

(define-inline-call c-*fl
   (mul me) )

(define-inline-call c-/fl
   (div me) )

(define-inline-call c-negfl
   (neg me) )

;;
;; MAZE
;;
(define-inline-call c-bitor
   (bitor me) )

(define-inline-call c-bitand
   (bitand me) )

(define-inline-call c-bitxor
   (bitxor me) )

(define-inline-call c-bitnot
   (bitnot me) )

;(method static c-bitrsh::long  (::long ::int)   "BITRSH")
;(method static c-bitursh::ulong (::ulong ::int) "BITURSH")
;(method static c-bitlsh::long  (::long ::int)   "BITLS
