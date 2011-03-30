(module saw_jvm_inline
   (import type_type ast_var ast_node
	   engine_param
	   type_env
	   object_class
	   object_slots
	   module_module
	   cnst_alloc
	   read_jvm
	   backend_backend
	   backend_bvm
	   backend_jvm_class
	   saw_defs
	   saw_jvm_out
	   saw_jvm_code
	   ast_env)
   (export (check-jvm-inlines)
	   (inline-call? me::jvm var::global)
	   (inline-predicate? me::jvm var::global on? lab)
	   (inline-call-with-args? me::jvm var::global args)) )

(define *jvm-inlines* '())

(define (check-jvm-inlines)
   (let ((unresolved (filter (lambda (i) (not (find-global i)))
			     *jvm-inlines*)))
      (when (pair? unresolved)
	 (error 'SawJvm "Cannot find inlined functions" unresolved))))

(define-macro (declare-inline! name)
   (when (> *compiler-debug* 0)
      `(set! *jvm-inlines* (cons ,name *jvm-inlines*))))

;;
(define (inline-call? me::jvm var::global)
   (when (jvm-inline me)
      (let ( (fun (getprop (global-id var) 'saw_jvm_inline_function)) )
	 (if fun
	     (fun me)
	     (let ( (name (global-name var)) (id (global-id var)) )
		'not-inlined )))))

(define *too-hard* '(%exit long->bint $cons c-write-char))

(define-macro (define-inline-call name . body)
   `(begin
       (declare-inline! ',name)
       (putprop! ',name 'saw_jvm_inline_function
		 (lambda (me) ,@body) )))

;;
(define (inline-call-with-args? me::jvm var::global args)
   (when (jvm-inline me)
      (let ( (fun (getprop (global-id var) 'saw_jvm_inline_function_args)) )
	 (if fun
	     (fun me args)
	     'not-inlined ))))

(define-macro (define-inline-call-args name . body)
   `(begin
       (declare-inline! ',name)
       (putprop! ',name 'saw_jvm_inline_function_args
		 (lambda (me args) ,@body) )))

;;
(define (inline-predicate? me::jvm var::global on? lab)
   (when (jvm-inline me)
      (let ( (fun (getprop (global-id var) 'saw_jvm_inline_predicate)) )
	 (if fun
	     (fun me on? lab)
	     'not-inlined ))))

(define-macro (define-inline-predicate name . body)
   `(begin
       (declare-inline! ',name)
       (putprop! ',name 'saw_jvm_inline_predicate
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
   (code! me `(getstatic ,(declare-global me (get-cnst-table))))
   (gen-expr me (car args))
   (code! me '(aaload)) )

(define-inline-call-args cnst-table-set!
   (code! me `(getstatic ,(declare-global me (get-cnst-table))))
   (gen-expr me (car args))
   (gen-expr me (cadr args))
   (code! me '(aastore))
   'no-value )

(define-inline-call-args make-fx-procedure
   (code! me '(new me))
   (code! me '(dup))
   (code! me '(invokespecial init))
   (code! me '(dup))
   (gen-expr me (car args))
   (code! me '(putfield procindex))
   (code! me '(dup))
   (gen-expr me (cadr args))
   (code! me '(putfield procarity))
   (code! me '(dup))
   (gen-expr me (caddr args))
   (code! me '(anewarray obj))
   (code! me '(putfield procenv)) )

(define-inline-call-args make-va-procedure
   (code! me '(new me))
   (code! me '(dup))
   (code! me '(invokespecial init))
   (code! me '(dup))
   (gen-expr me (car args))
   (code! me '(putfield procindex))
   (code! me '(dup))
   (gen-expr me (cadr args))
   (code! me '(putfield procarity))
   (code! me '(dup))
   (gen-expr me (caddr args))
   (code! me '(anewarray obj))
   (code! me '(putfield procenv)) )

(define-inline-call-args make-l-procedure
   (code! me '(new me))
   (code! me '(dup))
   (code! me '(invokespecial init))
   (code! me '(dup))
   (gen-expr me (car args))
   (code! me '(putfield procindex))
   (code! me '(dup))
   (gen-expr me (cadr args))
   (code! me '(anewarray obj))
   (code! me '(putfield procenv)) )

(define-inline-call make-el-procedure
   (code! me '(anewarray obj)) )

(define-inline-call procedure-el-ref
   (code! me '(aaload)) )

(define-inline-call procedure-el-set!
   (code! me '(aastore))
   'no-value )

;
;;;
;;; BOOLEAN
;;;
(define-inline-call c-boolean? ; "INTEGERP"
   (code! me '(instanceof bbool)) )

(define-inline-predicate $obj->bool ; "CBOOL"
   (code! me '(getstatic faux))
   (branch me (if on? 'if_acmpne 'if_acmpeq) lab) )

(define-inline-call-args $bool->bbool ; "BBOOL"
   (let ( (fun (skip-mov (car args))) )
      (if (rtl_loadi? fun)
	  (if (atom-value (rtl_loadi-constant fun))
	      (code! me '(getstatic vrai))
	      (code! me '(getstatic faux)) )
	  (let ( (l1 (gensym "I")) (l2 (gensym "I")) )
	     (gen-expr me (car args))
	     (code! me `(ifeq ,l1))
	     (code! me '(getstatic vrai))
	     (code! me `(goto ,l2))
	     (code! me l1)
	     (code! me '(getstatic faux))
	     (code! me l2) ))))

(define-inline-predicate c-boxed-eq? ; "BOXED_EQ"
   (branch me (if on? 'if_acmpeq 'if_acmpne) lab) )

(define-inline-predicate c-eq? ; "EQ"
   (if *optim-jvm-fasteq*
       (branch me (if on? 'if_acmpeq 'if_acmpne) lab)
       'not-inlined ))

;;;
;;; CHARACTER
;;;
(define-inline-call c-char? ; "CHARP"
   (code! me '(instanceof bchar)) )
;
(define-inline-call-args $uchar->bchar ; "BCHAR"
   (code! me `(getstatic bchar_allocated))
   (gen-expr me (car args))
   (code! me '(sipush 255))
   (code! me '(iand))
   (code! me '(aaload)) )

(define-inline-call $bchar->char ; "CCHAR"
   (code! me '(getfield bchar_value)) )

(define-inline-call $bchar->uchar ; "BCHAR_TO_UCHAR"
   (code! me `(getfield bchar_value))
   (code! me '(sipush 255))
   (code! me '(iand)) )

(define-inline-call $uchar->char ; "UCHAR_TO_CHAR"
   (code! me '(i2b)) )

(define-inline-predicate c-char=? ; "CHAR_EQ"
   (branch me (if on? 'if_icmpeq 'if_icmpne) lab) )

(define-inline-predicate c-char<? ; "CHAR_LT"
   (branch me (if on? 'if_icmplt 'if_icmpge) lab) )

(define-inline-predicate c-char<=? ; "CHAR_LE"
   (branch me (if on? 'if_icmple 'if_icmpgt) lab) )

(define-inline-predicate c-char>? ; "CHAR_GT"
   (branch me (if on? 'if_icmpgt 'if_icmple) lab) )

(define-inline-predicate c-char>=? ; "CHAR_GE"
   (branch me (if on? 'if_icmpge 'if_icmplt) lab) )

;;;
;;; INTEGER
;;;
(define-inline-call c-fixnum? ; "INTEGERP"
   (code! me '(instanceof bint)) )

(define-inline-call c-elong? ; "ELONGP"
   (code! me '(instanceof belong)) )

(define-inline-call char->integer ; "CHAR_TO_INT"
   (code! me '(sipush 255))
   (code! me '(iand)) )

(define-inline-call $int->short ; "INT_TO_SHORT"
   (code! me '(i2s)) )

(define-inline-call $int->byte ; "INT_TO_BYTE"
   (code! me '(i2b)) )

(define-inline-call $short->int ; "SHORT_TO_INT"
   'ok )

(define-inline-call $bint->long ; "BINT_TO_LONG"
   (code! me '(getfield bint_value)) )

;(define-inline-call ("BINT_TO_ULONG" env)
;   (bint-value env) )

(define-inline-call $int->long ; "INT_TO_LONG"
   'ok )

(define-inline-call $long->int ; "LONG_TO_INT"
   'ok )

(define-inline-call $bint->int ; "CINT"
   (code! me '(getfield bint_value)) )

(define (compute-boolean me cop)
   ;; CARE check if inversing the predicate may generate better code.
   (let ( (l1 (gensym "I")) (l2 (gensym "I")) )
      (code! me `(,cop ,l1))
      (code! me '(iconst_0))
      (code! me `(goto ,l2))
      (code! me l1)
      (code! me '(iconst_1))
      (code! me l2) ))

(define-inline-call c-=fx ; "EQ_FX"
   (compute-boolean me 'if_icmpeq) )

(define-inline-call c-<fx ; "LT_FX"
   (compute-boolean me 'if_icmplt) )

(define-inline-call c-<=fx ; "LE_FX"
   (compute-boolean me 'if_icmple) )

(define-inline-call c->fx ; "GT_FX"
   (compute-boolean me 'if_icmpgt) )

(define-inline-call c->=fx ; "GE_FX"
   (compute-boolean me 'if_icmpge) )

(define-inline-predicate c-=fx ; "EQ_FX"
   (branch me (if on? 'if_icmpeq 'if_icmpne) lab) )

(define-inline-predicate c-<fx ; "LT_FX"
   (branch me (if on? 'if_icmplt 'if_icmpge) lab) )

(define-inline-predicate c-<=fx ; "LE_FX"
   (branch me (if on? 'if_icmple 'if_icmpgt) lab) )

(define-inline-predicate c->fx ; "GT_FX"
   (branch me (if on? 'if_icmpgt 'if_icmple) lab) )

(define-inline-predicate c->=fx ; "GE_FX"
   (branch me (if on? 'if_icmpge 'if_icmplt) lab) )

(define-inline-predicate c-evenfx? ; "EVENP_FX"
   (code! me '(iconst_1))
   (code! me '(iand))
   (branch me (if on? 'ifeq 'ifne) lab) )

(define-inline-predicate c-oddfx? ; "ODDP_FX"
   (code! me '(iconst_1))
   (code! me '(iand))
   (branch me (if on? 'ifne 'ifeq) lab) )

(define-inline-call c-+fx (code! me '(iadd))) ; "PLUS_FX"
(define-inline-call c--fx (code! me '(isub))) ; "MINUS_FX"
(define-inline-call c-*fx (code! me '(imul))) ; "MUL_FX"
(define-inline-call c-/fx (code! me '(idiv))) ; "DIV_FX"
(define-inline-call c-quotientfx (code! me '(idiv))) ; "QUOTIENT_FX"
(define-inline-call c-remainderfx (code! me '(irem))) ; "REMAINDER_FX"
(define-inline-call c-bitor (code! me '(ior))) ; "BITOR"
(define-inline-call c-bitand (code! me '(iand))) ;"BITAND"
(define-inline-call c-bitxor (code! me '(ixor))) ; "BITXOR"
(define-inline-call c-bitnot ; "BITNOT"
   (code! me '(iconst_1))
   (code! me '(ineg))
   (code! me '(ixor)) )

;;;
;;; FLOAT
;;;
(define-inline-call c-flonum? ; "REALP"
   (code! me '(instanceof real)) )

(define-inline-call $real->double ; "REAL_TO_DOUBLE"
   (code! me '(getfield real_value)) )

(define-inline-call $fixnum->flonum ; "FIXNUM_TO_FLONUM"
   (code! me '(i2d)) )

(define-inline-call $flonum->fixnum ; "FLONUM_TO_FIXNUM"
   (code! me '(d2i)) )

(define-inline-call  $elong->flonum; "ELONG_TO_FLONUM"
   (code! me '(l2d)) )

(define-inline-call  $flonum->elong ; "FLONUM_TO_ELONG"
   (code! me '(d2l)) )

(define-inline-call  $llong->flonum; "LLONG_TO_FLONUM"
   (code! me '(l2d)) )

(define-inline-call  $flonum->llong ; "FLONUM_TO_LLONG"
   (code! me '(d2l)) )

(define-inline-predicate c-=fl ; "EQ_FL"
   (code! me '(dcmpg))
   (branch me (if on? 'ifeq 'ifne) lab) )

(define-inline-predicate c-<fl ; "LT_FL"
   (code! me '(dcmpl))
   (branch me (if on? 'iflt 'ifge) lab) )

(define-inline-predicate c-<=fl ; "LE_FL"
   (code! me '(dcmpl))
   (branch me (if on? 'ifle 'ifgt) lab) )

(define-inline-predicate c->fl ; "GT_FL"
   (code! me '(dcmpg))
   (branch me (if on? 'ifgt 'ifle) lab) )

(define-inline-predicate c->=fl ; "GE_FL"
   (code! me '(dcmpg))
   (branch me (if on? 'ifge 'iflt) lab) )

(define-inline-call c-+fl (code! me '(dadd))) ; "PLUS_FL"
(define-inline-call c--fl (code! me '(dsub))) ; "MINUS_FL"
(define-inline-call c-*fl (code! me '(dmul))) ; "MUL_FL"
(define-inline-call c-/fl (code! me '(ddiv))) ; "DIV_FL"
(define-inline-call c-negfl (code! me '(dneg))) ; "NEG_FL"

;;;
;;; CONSTANTS
;;;
;(define-inline-call ("EOF_OBJECTP" env)
;   (_instanceof env (jlib-declare env 'j_eof)) )

(define-inline-call $null? ; "NULLP"
   (code! me '(getstatic *nil*))
   (compute-boolean me 'if_acmpeq) )

(define-inline-predicate $null? ; "NULLP"
   (code! me '(getstatic *nil*))
   (branch me (if on? 'if_acmpeq 'if_acmpne) lab) )

;;;
;;; Unicode characters
;;;
;
;;; Delayed until write corrected and C ucs2->integer fixed
;
;;;
;;; Unicode strings
;;;
;
;;; Also delayed
;
;;;
;;; PROCESS
;;;
;
;;; Delayed
;
;;;
;;; STRING
;;;
(define-inline-call $string? ; "STRINGP"
   (code! me '(instanceof (vector byte))) )

(define-inline-call $string->bstring ; "string_to_bstring"
   'ok )

(define-inline-call $bstring->string ; "BSTRING_TO_STRING"
   'ok )

(define-inline-call $string-ref ; "STRING_REF"
   (code! me '(baload))
   (code! me '(sipush 255))
   (code! me '(iand)) )

(define-inline-call $string-set! ; "STRING_SET"
   (code! me '(bastore))
   'no-value )

(define-inline-call $string-length ; "STRING_LENGTH"
   (code! me '(arraylength)) )

(define-inline-call $make-string/wo-fill ; "make_string_sans_fill"
   (code! me '(newarray byte)) )

;;;
;;; KEYWORD
;;;
;(define-inline-call ("KEYWORDP" env)
;   (_instanceof env (jlib-declare env 'j_keyword)) )
;
;(define-inline-call ("KEYWORD_TO_STRING" env)
;   (_getfield env 'ad (jlib-declare env 'key_string)) )
;
;;;public static KEYWORD string_to_keyword(byte[] s) {
;;;	return(KEYWORD.make_keyword(s));
;;;    }
;
;;;
;;; SYMBOL
;;;
(define-inline-call c-symbol? ; "SYMBOLP"
   (code! me '(instanceof symbol)) )

(define-inline-call c-symbol->string ; "SYMBOL_TO_STRING"
   (code! me '(getfield symbol_string)) )

;;;
;;; CELL
;;;
(define-inline-call cell-set! ; "CELL_SET"
   (code! me '(putfield ccar))
   'no-value )

(define-inline-call cell-ref ; "CELL_REF"
   (code! me '(getfield ccar)) )

;;;
;;; FOREIGN
;;;
;;; Delayed
;
;;;
;;; CUSTOM
;;;
;;; Delayed
;
;;;
;;; PAIR
;;;
(define-inline-call $pair? ; "PAIRP"
   (code! me '(instanceof pair)) )

(define-inline-call $car ; "CAR"
   (code! me '(getfield car)) )

(define-inline-call $cdr ; "CDR"
   (code! me '(getfield cdr)) )

(define-inline-call $set-car! ; "SET_CAR"
   (code! me '(putfield car))
   'no-value )

(define-inline-call $set-cdr! ; "SET_CDR"
   (code! me '(putfield cdr))
   'no-value )

;;;
;;; EXTENDED PAIR
;;;
(define-inline-call $epair? ; "EXTENDED_PAIRP"
   (code! me '(instanceof extended_pair)) )

(define-inline-call $cer ; "CER"
   (code! me '(getfield cer)) )

(define-inline-call $set-cer! ; "SET_CER"
   (code! me '(putfield cer))
   'no-value )

;;;
;;; VECTOR
;;;
(define-inline-call $vector? ; "VECTORP"
   (code! me '(instanceof (vector obj))) )

;;;
;;; TVECTOR
;;;
;
;;;
;;; STRUCT
;;;

;;;
;;; OBJECT
;;;

;;;
;;; PROCEDURE
;;;
(define-inline-call c-procedure? ; "PROCEDUREP"
   (code! me '(instanceof procedure)) )

(define-inline-call-args procedure-set! ; "PROCEDURE_SET"
   (gen-expr me (car args))
   (code! me '(getfield procenv))
   (gen-expr me (cadr args))
   (gen-expr me (caddr args))
   (code! me '(aastore))
   'no-value )

(define-inline-call-args procedure-ref ; "PROCEDURE_REF"
   (gen-expr me (car args))
   (code! me '(getfield procenv))
   (gen-expr me (cadr args))
   (code! me '(aaload)) )

;;;
;;; EXCEPTION
;;;

;;;
;;; EVAL
;;;

;;;
;;; FILE/SYSTEM/OS
;;; 
;
;;;
;;; SOCKET
;;;
;
;;;
;;; INPUT
;;;

;;;
;;; mutex
;;;

;;;
;;; condition-variable
;;;

