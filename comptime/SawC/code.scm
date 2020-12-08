(module saw_c_code
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   (import type_type		; type
	   ast_var		; local/global
	   ast_node		; atom
	   module_module	; *module*
	   engine_param		; *stdc* ...

	   type_tools		; for emit-atom-value/make-typed-declaration
	   type_cache		; for emit-atom-value
	   type_typeof
	   cnst_alloc
	   tools_shape
	   backend_backend
	   backend_cvm
	   backend_c_emit
	   saw_defs
	   saw_woodcutter
	   saw_node2rtl
	   saw_expr
	   saw_regset
	   saw_register-allocation
	   saw_bbv)
   (export (saw-cheader)
	   (saw-cgen b::cvm v::global)
	   (saw-cepilogue))
   (cond-expand ((not bigloo-class-generate) (include "SawC/code.sch")))
   (static (wide-class SawCIreg::rtl_reg index)) )

(define *comment* #f)
(define *trace* #f)
(define *count* #f)
(define *inline-simple-macros* #t)
(define *counter* 0)
(define *hasprotect* #f)
(define *haspushexit* #f)
(define *haspushbefore* #f)
(define *pushtraceemmited* 0)

;;
(define (saw-cheader) ;()
   (with-output-to-port *c-port*
      (lambda () (header)) ))

(define (header) ;()
   (if *count*
       (begin (display "extern int bbcount[];\n")
	      (display "extern char *bbname[];\n")
	      (display "extern obj_t PRINT_TRACE(obj_t);\n\n") ))

   (display "\n") )

(define (saw-cepilogue) ;()
   (with-output-to-port *c-port*
      (lambda () (cepilogue)) ))

(define (cepilogue)
   (display "\n")
   (if *count*
       (begin (display* "int bbcount[" *counter* "];\n")
	      (display* "char *bbname[" *counter* "];\n")
	      (display "obj_t PRINT_TRACE(obj_t r) {\n")
	      (display "\tint i;\n")
	      (display* "\tfor(i=0;i<" *counter* ";i++)\n")
	      (display "\t\tprintf(\"%d\t%d\\n\",bbcount[i],i);\n")
	      (display "\treturn(BIGLOO_EXIT(r));\n")
	      (display "}\n") )))

(define (saw-cgen b::cvm v::global) ;()
   (let ( (l (global->blocks b v)) )
      (set! *hasprotect* #f)
      (set! *haspushexit* #f)
      (set! *haspushbefore* #f)
      (set! *pushtraceemmited* 0)
      (with-output-to-port *c-port*
	 (lambda () (genfun b v l)) )))

(define (genfun b::cvm v::global l) ;(list block)
   (with-access::global v (id name value type)
      (with-access::sfun value (loc args)
	 (let ( (params (map local->reg args)) )
	    (build-tree b params l)
	    (set! l (register-allocation b v params l))
	    (set! l (bbv b v params l))
	    (on-trace (cgen 2) (dump-basic-blocks 'genfun v params l))
	    (let ( (locals (get-locals params l)) )
	       (if *comment* (display* "/* " id " */\n"))
	       (let ( (ins (car (block-first (car l)))) )
		  (if (and *c-debug-lines-info*
			   (not *bdb-debug-no-line-directives?*))
		      (or (print-location? loc)
			  (print-location? (find-location ins)) )))
	       (gen-type type)
	       (display* " " (gname v))
	       (gen-type-regs params)
	       (display " {\n")
	       (if *hasprotect* (display " jmp_buf_t jmpbuf;\n"))
	       (if *haspushexit* (display " struct exitd exitd;\n"))
	       (if *haspushbefore* (display " struct befored befored;\n"))
	       (declare-regs locals)
	       (if *trace* (display* "printf(\"" id "=" name "\\n\");\n")) ))))
   (genbody l)
   (display (make-string *pushtraceemmited* #\}))
   (display "\n}\n\n") )

(define (get-locals params l) ;()
   ;; update all reg to ireg and  return all regs not in params.
   (let ( (n 0) (regs '()) )
      (define (check_fun fun)
	 (if (rtl_protect? fun) (set! *hasprotect* #t))
	 (if (rtl_call? fun)
	     (let ( (id (global-id (rtl_call-var fun))) )
		(if (eq? id 'push-exit!) (set! *haspushexit* #t))
		(if (eq? id 'push-before!) (set! *haspushbefore* #t)) )))
      (define (expr->ireg e)
	 (cond
	    ((isa? e SawCIreg))
	    ((rtl_reg? e) (widen!::SawCIreg e (index n))
			  (set! n (+fx n 1))
			  (set! regs (cons e regs)) )
	    (else (check_fun (rtl_ins-fun e))
		  (map expr->ireg (rtl_ins-args e)) )))
      (define (visit b::block)
	 (for-each
	  (lambda (ins)
	     (with-access::rtl_ins ins (dest fun args)
		(check_fun fun)
		(if dest (expr->ireg dest))
		(for-each expr->ireg args) ))
	  (block-first b) ))
      (for-each expr->ireg params)
      (set! regs '())
      (for-each visit l)
      regs ))

(define (gen-type-regs l) ;()
   (display "(")
   (if (not (null? l))
       (begin
	  (gen-type-reg (car l))
	  (for-each (lambda (arg) (display ", ") (gen-type-reg arg))
		    (cdr l) )))
   (display ")") )

(define (gen-type-reg reg) ;()
   (display (make-typed-declaration (rtl_reg-type reg) (reg_name reg))) )

(define (reg_name reg) ;()
   (string-append (if (SawCIreg-var reg) "V" "R")
		  (integer->string (SawCIreg-index reg)) ))

(define (declare-regs l) ;()
   (for-each (lambda (r)
		(display " ")
		(gen-type-reg r)
		(if *comment*
 		    (let ( (type (rtl_reg-type r)) )
		       (with-access::type type (id name)
			  (display* "; /* " id "=" name " */\n") ))
		    (display ";\n") ))
	     l ))

(define (genbody l) ;(list block)
   (for-each (lambda (b)
		(out-label (block-label b))
		(let ( (l (block-first b)) )
		   (if (location? (find-location (car l))) (print ""))
		   (for-each gen-ins l) ))
	     l ))

(define (out-label label)
   (display* "L" label ":")
   (if *count*
       (begin (display* "\tbbcount[" *counter* "]++;")
	      (set! *counter* (+ 1 *counter*)) ))
   (if *trace* (display* "\tprintf(\"" label "\\n\"); ")) )

(define (print-location? loc)
   (if (location? loc)
       (begin
	  (print "#line " (location-lnum loc) " \"" (location-fname loc) "\"")
	  #t )
       #f ))

(define (find-location ins::rtl_ins)
   (define (walk* l)
      (and (not (null? l))
	   (or (walk (car l))
	       (walk* (cdr l)) )))
   (define (walk ins)
      (if (not (rtl_ins? ins))
	  #f
	  (let ( (loc (rtl_ins-loc ins)) )
	     (if (location? loc)
		 loc
		 (walk* (rtl_ins-args ins)) ))))
   (and *c-debug-lines-info*
	(not *bdb-debug-no-line-directives?*)
	(walk ins) ))

;;;
;;; Instruction Generation
;;;
(define (gen-ins ins::rtl_ins) ;()
   (with-access::rtl_ins ins (dest fun args)
      (print-location? (find-location ins))
      (display "\t")
      (if dest
	  (begin (gen-reg/dest dest)
		 (display " = ") ))
      (gen-expr fun args)
      (display ";\n") ))

(define-generic (gen-expr fun::rtl_fun args);
   ;; Default case
   (gen-fun-name fun)
   (display "(")
   (gen-prefix fun)
   (gen-args args)
   (display ")") )

(define (gen-args args) ;()
   (if (not (null? args))
       (begin (gen-reg (car args))
	      (for-each (lambda (r) (display ", ") (gen-reg r))
			(cdr args) ))))

(define (gen-reg reg) ;()
   (if (isa? reg SawCIreg)
       (gen-reg/dest reg)
       (gen-expr (rtl_ins-fun reg) (rtl_ins-args reg)) ))

(define (gen-reg/dest reg) ;()
   (display (if (SawCIreg-var reg) "V" "R"))
   (display (SawCIreg-index reg)) )

;;
;; Special cases of gen-expr
;;
(define-method (gen-expr fun::rtl_lightfuncall args);
   (gen-Xfuncall "L_" args #f) )

(define-method (gen-expr fun::rtl_getfield args);
   (display "((")
   (gen-prefix fun)
   (gen-reg (car args))
   (display ")->")
   (display (rtl_getfield-name fun))
   (display ")") )

(define-method (gen-expr fun::rtl_setfield args);
   (display "((")
   (gen-prefix fun)
   (gen-reg (car args))
   (display ")->")
   (display (rtl_setfield-name fun))
   (display "=")
   (gen-reg (cadr args))
   (display ")") )


(define-method (gen-expr fun::rtl_funcall args);
   (if *stdc*
       (begin (display "(VA_PROCEDUREP(")
	      (gen-reg (car args))
	      (display ") ? ")
	      (gen-Xfuncall "" args #t)
	      (display " : ")
	      (gen-Xfuncall "" args #f)
	      (display ")") )
       (gen-Xfuncall "" args #t) ) )

(define (gen-Xfuncall type args eoa?);()
   (display* "PROCEDURE_" type "ENTRY(")
   (gen-reg (car args))
   (display ")(")
   (gen-args args)
   (if eoa? (display ", BEOA"))
   (display ")") )

;;
(define-method (gen-expr fun::rtl_protect args);
   ;; we have a pending "Rxx = "
   (display "(obj_t) jmpbuf;\n\t")
   (display "{BGL_STORE_TRACE();")
   (display "if(SETJMP(jmpbuf)) {BGL_RESTORE_TRACE(); return(BGL_EXIT_VALUE());}}\n")
   (display "#if( SIGSETJMP_SAVESIGS == 0 )\n" *c-port*)
   (display "  bgl_restore_signal_handlers();\n" *c-port*)
   (display "#endif\n" *c-port*) )

;;
(define-method (gen-expr fun::rtl_pragma args);
   (emit-pragma (rtl_pragma-format fun) args)
   (display "") )

;;
(define-method (gen-expr fun::rtl_switch args);
   (let ( (pats (rtl_switch-patterns fun)) )
      (display "switch(")
      (gen-reg (car args))
      (display ") {")
      (for-each (lambda (pat lab)
		   (if (eq? pat 'else)
		       (display* "\n\t default: ")
		       (for-each (lambda (n)
				    (display "\n\t case ")
				    (emit-atom-value n)
				    (display ":") )
				 pat ))
		   (display* " goto L" (block-label lab) ";") )
		pats
		(rtl_switch-labels fun) )
      (display "\n\t}") ))

;;
(define-method (gen-expr fun::rtl_call args);

   (define (rtl-reserved? var name)
      (not (eq? (global-name var) name)) )
   
   (define (gen-expr-infix name args)
      (if (null? args)
	  (display* "(" name ")")
	  (begin
	     (display "(")
	     (gen-reg (car args))
	     (display name)
	     (let loop ( (args (cdr args)) )
		(when (pair? args)
		   (gen-reg (car args))
		   (display name)
		   (loop (cdr args)) ))
	     (display ")") )))
   
   (define (gen-expr-prefix name args)
      (display* name "(")
      (if (or (string=? name "make_fx_procedure")
	      (string=? name "MAKE_FX_PROCEDURE")
	      (string=? name "make_va_procedure")
	      (string=? name "MAKE_VA_PROCEDURE")
	      (string=? name "MAKE_L_PROCEDURE") )
	  (display "(function_t) ") )
      (gen-args args)
      (display ")") )
   
   (let* ( (var (rtl_call-var fun))
	   (name (gname var)) )
      (if (and (global? var)
	       (isa? (global-value var) cfun)
	       (cfun-infix? (global-value var))
	       (not (rtl-reserved? var name)) )
	  (gen-expr-infix name args)
	  (gen-expr-prefix name args) )))

;;
;; Name of operators.
;;
(define-generic (gen-fun-name fun::rtl_fun);
   ;; Default case
   (gen-upcase fun) )

(define (gen-upcase fun::rtl_fun) ;()
   (display* "BGL_"
	     (string-upcase (symbol->string (class-name (object-class fun))))) )

(define (vfun-name f::bstring type::type) ; ()
   (display* "BGL_RTL_" (if (basic-type? type) "T" "") f) )
   
(define-method (gen-fun-name fun::rtl_valloc) ;()
   (vfun-name "VALLOC" (rtl_valloc-type fun)) )

(define-method (gen-fun-name fun::rtl_vref) ;()
   (vfun-name "VREF" (rtl_vref-type fun)) )

(define-method (gen-fun-name fun::rtl_vset) ;()
   (vfun-name "VSET" (rtl_vset-type fun)) )

(define-method (gen-fun-name fun::rtl_vlength) ;()
   (vfun-name "VLENGTH" (rtl_vlength-type fun)) )

; To add some readability on generated code
(define (no-name fun::rtl_fun);()
   (if (not *inline-simple-macros*) (gen-upcase fun)) )

(define-method (gen-fun-name fun::rtl_mov) (no-name fun));()
(define-method (gen-fun-name fun::rtl_loadi) (no-name fun));()
(define-method (gen-fun-name fun::rtl_loadg) (no-name fun));()

(define-method (gen-fun-name fun::rtl_return);
   (if *inline-simple-macros* (display "return") (gen-upcase fun)) )

;;
;; Extra arguments
;;
(define-generic (gen-prefix fun::rtl_fun) ;()
   "" )

(define-method (gen-prefix fun::rtl_loadi) ;()
   (emit-atom-value (atom-value (rtl_loadi-constant fun))) )

(define-method (gen-prefix fun::rtl_loadg) ;()
   (display (gname (rtl_loadg-var fun))) )

(define-method (gen-prefix fun::rtl_loadfun) ;()
   (display* "(obj_t) " (gname (rtl_loadfun-var fun))) )

(define-method (gen-prefix fun::rtl_storeg) ;()
   (display* (gname (rtl_storeg-var fun)) ", ") )

(define-method (gen-prefix fun::rtl_globalref) ;()
   (display* (gname (rtl_globalref-var fun))) )

(define-method (gen-prefix fun::rtl_go) ;()
   (display* "L" (block-label (rtl_go-to fun))) )

(define-method (gen-prefix fun::rtl_ifeq) ;()
   (display* "L" (block-label (rtl_ifeq-then fun)) ", ") )

(define-method (gen-prefix fun::rtl_ifne) ;()
   (display* "L" (block-label (rtl_ifne-then fun)) ", ") )

(define (vextra type::type) ; ()
   (if (basic-type? type)
       (let ((tn (type-name type)))
	  (display* (bigloo-mangle tn) ", " tn ", ")) ))
   
(define-method (gen-prefix fun::rtl_valloc) ;()
   (vextra (rtl_valloc-type fun)) )

(define-method (gen-prefix fun::rtl_vref) ;()
   (vextra (rtl_vref-type fun)) )

(define-method (gen-prefix fun::rtl_vset) ;()
   (vextra (rtl_vset-type fun)) )

(define-method (gen-prefix fun::rtl_vlength) ;()
   (vextra (rtl_vlength-type fun)) )

(define-method (gen-prefix fun::rtl_cast) ;()
   (display (make-typed-declaration (rtl_cast-totype fun) ""))
   (display ", ") )

(define-method (gen-prefix fun::rtl_cast_null) ;()
   (display (make-typed-declaration (rtl_cast_null-type fun) "")) )

;;
;; Type
;;
(define (gen-type type::type) ;()
   (display* (type-name type)) )

(define (basic-type? type::type) ;()
   (with-access::type type (id)
      (memq id '(double
		 llong ullong
		 elong uelong
		 long ulong
		 int uint
		 char uchar
		 byte ubyte
		 bool string
		 int8 uint8
		 int16 uint16
		 int32 uint32
		 int64 uint64)) ))

;;
(define (gname var::global) ;()
   (inl-op (global-name var)) )

(define (inl-op s) ;()
   (cond
      ((string=? s "==") "BGL_RTL_EQ")
      ((string=? s ">=") "BGL_RTL_GE")
      ((string=? s "<=") "BGL_RTL_LE")
      ((string=? s ">") "BGL_RTL_GT")
      ((string=? s "<") "BGL_RTL_LT")
      ((string=? s "|") "BGL_RTL_OR")
      ((string=? s "&") "BGL_RTL_AND")
      ((string=? s "+") "BGL_RTL_ADD")
      ((string=? s "-") "BGL_RTL_SUB")
      ((string=? s "*") "BGL_RTL_MUL")
      ((string=? s "/") "BGL_RTL_DIV")
      ((string=? s "%") "BGL_RTL_REM")
      ((string=? s " | ") "BGL_RTL_OR")
      ((string=? s " & ") "BGL_RTL_AND")
      ((string=? s " ^ ") "BGL_RTL_XOR")
      ((string=? s " >> ") "BGL_RTL_RSH")
      ((string=? s " << ") "BGL_RTL_LSH")
      ((string=? s "PUSH_EXIT") "BGL_RTL_PUSH_EXIT")
      ((string=? s "PUSH_BEFORE") "BGL_RTL_PUSH_BEFORE")
      ((string=? s "PUSH_TRACE")
       (set! *pushtraceemmited* (+fx 1 *pushtraceemmited*))
       "{PUSH_TRACE" )
      ((string=? s "BGL_ENV_PUSH_TRACE")
       (set! *pushtraceemmited* (+fx 1 *pushtraceemmited*))
       "{BGL_ENV_PUSH_TRACE" )
      ((string=? s "BIGLOO_EXIT")
       (if *count* "PRINT_TRACE" s) )
      ;; MANU
      ((string=? s "STRING_REF") "BGL_RTL_STRING_REF")
      (else s) ))

;;
;; Specific methods for expression folding
;;
(define-method (accept-folding? b::cvm ins tree)
   (or (deep-mov? tree)
       (not (multiple-evaluation ins tree)) ))

(define (deep-mov? ins)
   (or (rtl_reg? ins)
       (and (rtl_mov? (rtl_ins-fun ins))
	    (deep-mov? (car (rtl_ins-args ins))) )))

(define (multiple-evaluation ins tree)
   (let ( (fun (rtl_ins-fun ins)) )
          ; A verifier plus profondement
      (or (rtl_valloc? fun)
	  (and (rtl_lightfuncall? fun)
	       (eq? (car (rtl_ins-args ins))
		    (rtl_ins-dest tree) ))
	  (and (rtl_funcall? fun)
	       (eq? (car (rtl_ins-args ins))
		    (rtl_ins-dest tree) ))
	  (and (rtl_call? fun)
	       (multiple-evaluation2 (rtl_call-var fun)) ))))

(define *bad-macros*
   '(
     ;; I select only those which disturb the mbrot bench...
     "WRITE_CHAR"
     ;; And all I cant figure which one disturb beval
     "BOOLEANP" "INTEGERP" "NULLP" "PAIRP" "SYMBOLP" "VECTORP"
     ))

(define (multiple-evaluation2 var)
   (not (global-args-safe? var)) )

;;
;; Taken from ../Cgen/emit-cop with slice modifs
;;
(define (emit-pragma format args)
   (if (null? args)
       (display format *c-port*)
       (let* ((sport  (open-input-string format))
	      (args   (list->vector args))
	      (parser (regular-grammar ()
			 ((: #\$ (+ (in (#\0 #\9))))
			  (let* ((str   (the-string))
				 (len   (the-length))
				 (index (string->number
					 (substring str 1 len))))
			     (gen-reg (vector-ref args (-fx index 1)))
			     (ignore)))
			 ((+ (out #\$))
			  (display (the-string) *c-port*)
			  (ignore))
			 ("$$"
			  (display "$" *c-port*)
			  (ignore))
			 (else
			  (the-failure)))))
	  (read/rp parser sport)
	  #t)))
