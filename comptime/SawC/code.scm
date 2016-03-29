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
	   saw_register-allocation)
   (export (saw-cheader)
	   (saw-cgen b::cvm v::global)
	   (saw-cepilogue))
   (cond-expand ((not bigloo-class-generate) (include "SawC/code.sch")))
   (static (wide-class SawCIreg::rtl_reg index defs uses framed)
	   (wide-class liveblk::block in out in_onf in_onr out_onf out_onr)
	   (wide-class liveins::rtl_ins after) ))

(define *comment* #f)
(define *trace* #f)
(define *count* #f)
(define *inline-simple-macros* #t)
(define *counter* 0)
(define *hasprotect* #f)
(define *haspushexit* #f)
(define *haspushbefore* #f)
(define *pushtraceemmited* 0)
(define *trace_used* #f)
(define *trace_nb_frames* #f)

(define *hasframe* #f)
(define *pointer-in-a-frame* #t)
(define *ms-spil* #f)

;;
(define (saw-cheader) ;()
   (with-output-to-port *c-port*
      (lambda () (header)) ))

(define (header) ;()
   (when *trace_used*
      (display "extern long bps_time;\n")
      (display "extern int bps_used();\n") )
   (when *trace_nb_frames*
      (display "extern long bps_time;\n")
      (display "extern int bps_nbFrames();\n") )
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
      (set! *hasframe* #f)
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
	    (if *saw-register-allocation?*
		(set! l (register-allocation b v params l)) )
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
	       (display " {AN_OBJECT\n")
	       (if *hasprotect* (display " jmp_buf_t jmpbuf;\n"))
	       (if *haspushexit* (display " struct exitd exitd;\n"))
	       (if *haspushbefore* (display " struct befored befored;\n"))
	       (when *ms-spil* (ms_spil_init l))
	       (declare-variables name params locals)
	       (if *trace* (display* "printf(\"" id "=" name "\\n\");\n")) )))
      (genbody name l)
      (display (make-string *pushtraceemmited* #\}))
      (display "}\n") ))

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
	    ((rtl_reg? e)
	     (widen!::SawCIreg e
		(index n)
		(defs '())
		(uses '())
		(framed (and *pointer-in-a-frame*
			     (not (basic-type? (rtl_reg-type e))) )) )
	     (set! n (+fx n 1))
	     (set! regs (cons e regs)) )
	    (else (check_fun (rtl_ins-fun e))
		  (for-each expr->ireg (rtl_ins-args e)) )))
      (define (add-def r l)
	 (with-access::SawCIreg r (defs) (set! defs (cons l defs))) )
      (define (add-use e ins)
	 (if (isa? e SawCIreg)
	     (with-access::SawCIreg e (uses) (set! uses (cons ins uses)))
	     (for-each (lambda (r) (add-use r e)) (rtl_ins-args e)) ))
      (define (visit b::block)
	 (let rec ( (l (block-first b)) )
	    (unless (null? l)
	       (let ( (ins (car l)) )
		  (with-access::rtl_ins ins (dest fun args)
		     (check_fun fun)
		     (if dest (expr->ireg dest))
		     (for-each expr->ireg args)
		     (when *pointer-in-a-frame*
			(if dest (add-def dest l))
			(for-each (lambda (r) (add-use r ins)) args) )))
	       (rec (cdr l)) )))
      (for-each expr->ireg params)
      (set! regs '())
      (for-each visit l)
      regs ))

;; formal parameters declaration
(define (gen-type-regs l) ;()
   (display "(")
   (if (not (null? l))
       (begin
	  (gen-type-reg (car l))
	  (for-each (lambda (arg) (display ", ") (gen-type-reg arg))
		    (cdr l) )))
   (display ")") )

(define (reg_standard_name reg)
   (string-append (if (SawCIreg-var reg) "V" "R")
		  (integer->string (SawCIreg-index reg)) ))

(define (gen-type-reg reg) ;()
   (display (make-typed-declaration (rtl_reg-type reg) (reg_standard_name reg))) )

;; Special case for return where we can omit to put a var in the frame
(define (check-special-case-of-return r)
   (with-access::SawCIreg r (framed defs uses)
      (when (and framed
		 (pair? defs)
		 (null? (cdr defs))
		 (pair? uses)
		 (null? (cdr uses))
		 (rtl_return? (rtl_ins-fun (car uses)))
		 (eq? (cadar defs) (car uses)) )
	 (set! framed #f) )))

;; Local variables declaration
(define (declare-variables name params locals)
   (for-each check-special-case-of-return locals)
   (check-need-frame params locals)
   (declare-regs locals)
   (declare-frame name params locals) )

(define (check-need-frame params locals)
   (define (f? r) (SawCIreg-framed r))
   (set! *hasframe*
	 (and (or *pointer-in-a-frame* *ms-spil*)
	      (or *hasprotect* (any f? params) (any f? locals)) )))

(define (declare-regs l) ;()
   (for-each (lambda (r)
		(unless (and (SawCIreg-framed r) (not *ms-spil*))
		   (display "\t")
		   (gen-type-reg r)
		   (display ";")
		   (when *comment*
		      (let ( (type (rtl_reg-type r)) )
			 (with-access::type type (id name)
			    (display* " /* " id "=" name " */") )))
		   (display "\n") ))
	     l ))

(define (declare-frame name params locals)
   (when *hasframe*
      (let ( (count 0) )
	 (define (gen-frame-field r)
	    (when (SawCIreg-framed r)
	       (set! count (+fx count 1))
	       (display "\t\t")
	       (gen-type-reg r)
	       (display ";\n") ))
	 (define (gen-move-to-frame r)
	    (when (SawCIreg-framed r)
	       (display "\t")
	       (gen-reg-frame r)
	       (display " = ")
	       (gen-reg-standard r)
	       (display ";\n") ))
	 (define (gen-init-frame r)
	    (when (SawCIreg-framed r)
	       (display "\t")
	       (gen-reg-frame r)
	       (display " = 0;\n") ))
	 (print "\tstruct {")
	 (print "\t\t bgl_saw_frame_header_t header;")
	 (for-each gen-frame-field params)
	 (for-each gen-frame-field locals)
	 (print "\t} lpf;")
	 (print "\tlpf.header.size = " count ";")
	 (print "\tlpf.header.link = BGL_ENV_SAW_SP(BGL_CURRENT_DYNAMIC_ENV());")
	 (print "\tBGL_ENV_SAW_SP_SET(BGL_CURRENT_DYNAMIC_ENV(), &(lpf.header));")
	 (print "\tlpf.header.name = \"" name "\";")
	 (for-each gen-init-frame locals)
	 (for-each gen-move-to-frame params) )))

;;
;; Code generation
;;
(define (genbody name l) ;(list block)
   (for-each (lambda (b)
		(out-label name (block-label b))
		(let ( (l (block-first b)) )
		   (if (location? (find-location (car l))) (print ""))
		   (if *ms-spil*
		       (gen-bb b)
		       (for-each gen-ins l) )))
	     l ))

(define (gen-bb b)
   (define (rs l) (map reg_standard_name l))
   (with-access::liveblk b (succs first in out in_onf in_onr out_onf out_onr)
      (when *comment*
	 (print "/* live=" (rs in) " fp=" (rs in_onf) " r=" (rs in_onr) " */") )
      (let rec ( (l first) (onf in_onf) (onr in_onr) )
	 (let ( (onfr (gen-ins-prelude (car l) onf onr)) )
	    (if (null? (cdr l))
		(gen-last-inst (car l) succs out_onf out_onr)
		(begin (gen-ins (car l))
		       (gen-ins-postlude (car l) (car onfr) (cdr onfr))
		       (rec (cdr l) (car onfr) (cdr onfr)) ))))
      ))

(define *ugly_onf* '())
(define *ugly_onr* '())
(define (gen-last-inst inst succs f r)
   (define (restore r)
      (display "\t")
      (gen-reg-standard r)
      (display " = ")
      (gen-reg-frame r)
      (display ";\n") )
   (define (save r)
      (display "\t")
      (gen-reg-frame r)
      (display " = ")
      (gen-reg-standard r)
      (display ";\n") )
   (define (unify f r b)
      (with-access::liveblk b (in_onf in_onr)
	 (for-each save (set_sub in_onf f))
	 (for-each restore (set_sub in_onr r)) ))
   (define (then fun)
      (if (rtl_ifne? fun) (rtl_ifne-then fun) (rtl_ifeq-then fun)) )
   (with-access::rtl_ins inst (dest fun args)
      (cond
	 ((null? succs) ; a.k.a. (rtl_last? fun) for return and co
	  (gen-ins inst) )
	 ((null? (cdr succs)) ; goto or implicit goto
	  (unless (rtl_go? fun) (gen-ins inst))
	  (unify f r (car succs))
	  (when (rtl_go? fun) (gen-ins inst)) )
	 ((or (rtl_ifne? fun) (rtl_ifeq? fun))
	  (let ( (then (then fun)) )
	     (display "\tif(")
	     (when (rtl_ifeq? fun) (display "!"))
	     (gen-reg (car args))
	     (display ") {\n" )
	     (unify f r then)
	     (print "\t\tgoto L" (block-label then))
	     (print ";\t}")
	     (unify f r (car (set_sub succs (cons then '())))) ))
	 ((rtl_switch? fun)
	  ;; hacked directly in the specific gen-expr
	  (set! *ugly_onf* f)
	  (set! *ugly_onr* r)
	  (gen-ins inst) )
	 (else (error 'explicit_spit "can manage this node"
		      (symbol->string (class-name (object-class fun))) ))))
   (gen-ins-postlude inst f r) )

(define (gen-ins-postlude ins onf onr)
   (define (rs l) (map reg_standard_name l))
   (when *comment*
      (with-access::liveins ins (after)
	 (print "/* live=" (rs after) " fp=" (rs onf) " r=" (rs onr) " */") )))


(define (out-label name label)
   (display* "L" label ":")
   (when *trace_used*
      (print "\tif(bps_time++ % 10000 == 0)\n")
      (print "\t\tfprintf(stderr, \"%ld %d\\n\", bps_time++, bps_used());") )
   (when *trace_nb_frames*
      (print "\tif(bps_time++ % 10000 == 0)\n")
      (print "\t\tfprintf(stderr, \"%ld %d\\n\", bps_time++, bps_nbFrames());") )
   (if *count*
       (begin (display* "\tbbcount[" *counter* "]++;")
	      (set! *counter* (+ 1 *counter*)) ))
   (if *trace* (display* "\tprintf(\"" name "@" label "\\n\"); ")) )

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
   (if (and (SawCIreg-framed reg) (not *ms-spil*))
       (gen-reg-frame reg)
       (gen-reg-standard reg) ))

(define (gen-reg-frame reg) ;()
   (display "lpf.")
   (gen-reg-standard reg) )

(define (gen-reg-standard reg) ;()
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
   (display "BGL_ENV_SAW_SP_SET(BGL_CURRENT_DYNAMIC_ENV(), &(lpf.header));\n")
   (display "#if( SIGSETJMP_SAVESIGS == 0 )\n" *c-port*)
   (display "  bgl_restore_signal_handlers();\n" *c-port*)
   (display "#endif\n" *c-port*) )

;;
(define-method (gen-expr fun::rtl_pragma args);
   (emit-pragma (rtl_pragma-format fun) args)
   (display "") )

;;
(define-method (gen-expr fun::rtl_switch args);
   (define (restore r)
      (display "\t")
      (gen-reg-standard r)
      (display " = ")
      (gen-reg-frame r)
      (display ";\n") )
   (define (save r)
      (display "\t")
      (gen-reg-frame r)
      (display " = ")
      (gen-reg-standard r)
      (display ";\n") )
   (define (unify f r b)
      (with-access::liveblk b (in_onf in_onr)
	 (for-each save (set_sub in_onf f))
	 (for-each restore (set_sub in_onr r)) ))
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
		   (when *ms-spil* (unify *ugly_onf* *ugly_onr* lab))
		   (display* " goto L" (block-label lab) ";") )
		pats
		(rtl_switch-labels fun) )
      (display "\n\t}") ))

;;
(define-method (gen-expr fun::rtl_call args);
   (let ( (name (gname (rtl_call-var fun))) )
      (display* name "(")
      (if (or (string=? name "make_fx_procedure")
	      (string=? name "MAKE_FX_PROCEDURE")
	      (string=? name "make_va_procedure")
	      (string=? name "MAKE_VA_PROCEDURE")
	      (string=? name "MAKE_L_PROCEDURE") )
	  (display "(function_t) ") )
      (gen-args args)
      (display ")") ))

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

(define-method (gen-fun-name fun::rtl_storeg) ;()
   (vfun-name "STOREG" (variable-type (rtl_storeg-var fun))) )

; To add some readability on generated code
(define (no-name fun::rtl_fun);()
   (if (not *inline-simple-macros*) (gen-upcase fun)) )

(define-method (gen-fun-name fun::rtl_mov) (no-name fun));()
(define-method (gen-fun-name fun::rtl_loadi) (no-name fun));()
(define-method (gen-fun-name fun::rtl_loadg) (no-name fun));()

(define-method (gen-fun-name fun::rtl_return);
   (when *hasframe*
      (display "BGL_ENV_SAW_SP_SET(BGL_CURRENT_DYNAMIC_ENV(), lpf.header.link);\n\t") )
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
   ;; CARE In most (all?) cases boolean and unboxed values (bint) could
   ;; be considered as basic types
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
		 int64 uint64

		 bbool bint
		 )) ))

;;
(define (gname var::global) ;()
   (patch (global-name var)) )

(define (patch s) ;()
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
      ((string=? s "0L") "BGL_RTL_0L")
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
       (and (not (multiple-evaluation ins tree))
	    (accept_folding_with_frame? ins tree) )))

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

(define (multiple-evaluation2 var)
   (not (global-args-safe? var)) )

;;
;; Special case for not loosing a root .
;;
(define (accept_folding_with_frame? ins tree)
   (cond (*pointer-in-a-frame*
	  (and (not (rtl_return? (rtl_ins-fun ins)))
	       (fold_in_frame ins tree) ))
	 (*ms-spil*
	  (and (not (rtl_return? (rtl_ins-fun ins)))
	       (fold_with_spil ins tree) ))
	 (else #t) ))

(define (fold_in_frame ins tree)
   ;; ins:(f .. x ..) tree:x=E, can we replace to (f .. E ..)
   ;; the general contract for (f .. Ei ..) to be valid is that if
   ;; one Ei do allocation, then all others Ej must return only
   ;; basic types. 
   (define (bt? x)
      (if (rtl_reg? x)
	  (basic-type? (rtl_reg-type x))
	  (bt? (rtl_ins-dest x)) ))
   (define (all-basic-values-but ins tree)
      (every (lambda (x) (or (eq? x (rtl_ins-dest tree)) (bt? x)))
	     (rtl_ins-args ins) ))
   (if (no-alloc-args ins)
       (or (no-alloc-in tree) (all-basic-values-but ins tree))
       ;; Here it means that some A in (f .. x .. A ..) do allocation
       ;; therefore by induction x is a basic type and it's ok
       (begin (unless (bt? tree) (print "/* ERROR fold */"))
	      #t )))

(define (fold_with_spil ins tree)
   ;; The contract is stronger for (f .. Ei ..) to be valid if
   ;; one Ei do allocation, then all others Ej must also use only
   ;; basic type registers
   (define (bt? x)
      (if (rtl_reg? x)
	  (basic-type? (rtl_reg-type x))
	  (bt? (rtl_ins-dest x)) ))
   (define (basic-regs x)
      (if (rtl_reg? x)
	  (bt? x)
	  (every basic-regs (rtl_ins-args x)) ))
   (define (all-basic-but ins tree)
      (every (lambda (x) (or (eq? x (rtl_ins-dest tree))
			     (and (bt? x) (basic-regs x)) ))
	     (rtl_ins-args ins) ))
   (if (no-alloc-args ins)
       (or (no-alloc-in tree) (all-basic-but ins tree))
       ;; Here it means that some A in (f .. x .. A ..) do allocation
       ;; therefore by induction x is a basic type and it's ok
       (begin (unless (bt? tree) (print "/* ERROR fold */"))
	      (basic-regs tree) )))

(define (no-alloc-args ins)
   (every no-allocation-arg (rtl_ins-args ins)) )

(define (no-alloc-in tree)
   ;; return #t when the evaluation of tree doesn't allocate
   (and (no-allocation-fun (rtl_ins-fun tree))
	(no-alloc-args tree) ))

(define (no-allocation-arg a)
   (if (rtl_ins? a)
       (no-alloc-in a)
       #t ))

(define-generic (no-allocation-fun fun::rtl_fun) #t)
(define-method (no-allocation-fun fun::rtl_return) #f) ; this one is special
(define-method (no-allocation-fun fun::rtl_makebox) #f)
(define-method (no-allocation-fun fun::rtl_new) #f)
(define-method (no-allocation-fun fun::rtl_apply) #f)
(define-method (no-allocation-fun fun::rtl_lightfuncall) #f)
(define-method (no-allocation-fun fun::rtl_funcall) #f)
(define-method (no-allocation-fun fun::rtl_call)
   (memq 'no-alloc (global-pragma (rtl_call-var fun))) )
;; CARE MANU...
(define-method (no-allocation-fun fun::rtl_pragma) #t)




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

;;
;; Manuel's stategy
;;
(define (ms_spil_init l)
   (for-each live-init l)
   ;; CARE check time with (reverse l)
   (for-each live-fix l)
   ;(in-reg-backward l)
   ;(in-reg-forward l)
   (fix-in-reg-backward l)
   (fix-in-reg-forward l)
   (default-in-frame l)
   (just_to_be_sure l)
   (set_framed l) )

(define (just_to_be_sure l)
   (define (bb-check b)
      (with-access::liveblk b (in in_onf in_onr out out_onf out_onr)
	 (unless (and (set_include (union in_onf in_onr) in)
		      (set_include in (union in_onf in_onr)) )
	    (print "/* ERROR IN " (block-label b) " */") )
	 (unless (and (set_include (union out_onf out_onr) out)
		      (set_include out (union out_onf out_onr)) )
	    (print "/* ERROR OUT " (block-label b) " */") )))
   (for-each bb-check l) )

(define (gen-ins-prelude ins::liveins onf onr);
   ;; <onr> are the registers saved on frame
   ;; <onf> are the registers directly available
   ;; must return the new values of <onr> and <onf>
   (define (restore r)
      (display "\t")
      (gen-reg-standard r)
      (display " = ")
      (gen-reg-frame r)
      (display ";\n") )
   (define (save r)
      (display "\t")
      (gen-reg-frame r)
      (display " = ")
      (gen-reg-standard r)
      (display ";\n") )
   (with-access::liveins ins (dest args after)
      (let ( (l (leaves args)) )
	 (for-each restore (set_sub (intersection l onf) onr))
	 (if (no-alloc-in ins)
	     (cons (set_sub1? onf dest) (set_add1? dest (union onr l)))
	     (begin (for-each save (set_sub1? (set_sub after onf) dest))
		    (cons (set_sub1? after dest) (set_add1? dest '())) )))))

;; all registers in an instruction
(define (leave e)
   (if (rtl_reg? e)
       (cons e '())
       (leaves (rtl_ins-args e)) ))

(define (leaves l)
   (if (null? l) '() (union (leave (car l)) (leaves (cdr l)))) )

;; init
(define (set_framed l)
   ;; set the "framed" flag of registers
   (define (framed r) (with-access::SawCIreg r (framed) (set! framed #t)))
   (define (ins-set-framed ins onf onr)
      (with-access::liveins ins (dest args after)
	 (let ( (l (leaves args)) )
	    ;; may be obs.. (done via save)
	    (for-each framed (set_sub (intersection l onf) onr))
	    (if (no-alloc-in ins)
		(cons (set_sub1? onf dest) (set_add1? dest (union onr l)))
		(begin (for-each framed (set_sub1? (set_sub after onf) dest))
		       (cons (set_sub1? after dest) (set_add1? dest '())) )))))
   (define (bb-set-framed b)
      (with-access::liveblk b (first in_onf in_onr)
	 (let rec ( (l first) (onf in_onf) (onr in_onr) )
	    (unless (null? l)
	       (let ( (onfr (ins-set-framed (car l) onf onr)) )
		  (rec (cdr l) (car onfr) (cdr onfr)) )))))
   (define (unify f r b)
      (with-access::liveblk b (in_onf in_onr)
	 (for-each framed (set_sub in_onf f))
	 (for-each framed (set_sub in_onr r)) ))
   (define (bb-control b)
      (with-access::liveblk b (out_onf out_onr succs)
	 (for-each (lambda (b) (unify out_onf out_onr b)) succs) ))
   (for-each bb-control l)
   (for-each bb-set-framed l) )


(define (in-reg-backward l)
   ;; set the "in_onr" and "in_onf" fields of all basic block
   (define (inst-in-reg ins s)
      (with-access::rtl_ins ins (dest args)
	 (let ( (regs (leaves args)) )
	    (if (no-alloc-in ins)
		(union (set_sub1? s dest) regs)
		regs ))))
   (define (walk l s)
      (if (null? l)
	  s
	  (inst-in-reg (car l) (walk (cdr l) s)) ))
   (define (bb-in-reg-backward b)
      (with-access::liveblk b (in_onr in_onf out first)
	 (set! in_onr (walk first '()))
	 (set! in_onf (set_sub out in_onr)) ))
   (for-each bb-in-reg-backward l) )

(define (in-reg-forward l)
   ;; compute "out_onr" and "out_onf" fields from "in_*"
   (define (walk_onf1 ins in)
      (with-access::liveins ins (dest after)
	 (set_sub1? (if (no-alloc-in ins) in after) dest) ))
   (define (walk_onr1 ins in)
      (with-access::liveins ins (dest args)
	 (let ( (regs (leaves args)) )
	    (if (no-alloc-in ins)
		(union in (set_add1? dest regs))
		(if dest (cons dest '()) '()) ))))
   (define (walk_onf l in)
      (if (null? l) in (walk_onf (cdr l) (walk_onf1 (car l) in))) )
   (define (walk_onr l in)
      (if (null? l) in (walk_onr (cdr l) (walk_onr1 (car l) in))) )
   (define (bb-onx-forward b)
      (with-access::liveblk b (in_onr in_onf out_onr out_onf first)
	 (set! out_onr (walk_onr first in_onr))
	 (set! out_onf (walk_onf first in_onf)) ))
   (define (inter_r l r)
      (if (null? l)
	  r
	  (inter_r (cdr l) (intersection r (liveblk-out_onr (car l)))) ))
   (define (inter_f l r)
      (if (null? l)
	  r
	  (inter_r (cdr l) (intersection r (liveblk-out_onf (car l)))) ))
   (define (contr b)
      (with-access::liveblk b (preds in_onr)
	 (unless (null? preds)
	    (let ( (i (inter_r (cdr preds) (liveblk-out_onr (car preds)))) )
		(unless (set_include i in_onr)
		   (set! in_onr (union in_onr i))
		   #t )))))
   (define (contf b)
      (with-access::liveblk b (preds in_onf)
	 (unless (null? preds)
	     (let ( (i (inter_f (cdr preds) (liveblk-out_onf (car preds)))) )
		(unless (set_include i in_onf)
		   (set! in_onf (union in_onf i))
		   #t )))))
   (define (fixpoint l)
      (for-each bb-onx-forward l)
      (when (or (any contr l) (any contf l)) (fixpoint l)) )
   (fixpoint l) )

(define (fix-in-reg-backward l)
   (define (init b)
      (with-access::liveblk b (out_onr out_onf)
	 (set! out_onr '())
	 (set! out_onf '()) ))
   (define (back_inst ins f r)
      (with-access::liveins ins (dest args after)
	 (let ( (regs (leaves args)) )
	    (if (no-alloc-in ins)
		(values (set_sub1? f dest)
			(union (set_sub1? r dest) regs) )
		;; assume f included in after ?
		(values (set_sub1? (union f after) dest)
			regs ) ))))
   (define (back_instrs l f r)
      (if (null? l)
	  (values f r)
	  (multiple-value-bind (f r) (back_instrs (cdr l) f r)
				(back_inst (car l) f r) )))
   (define (back b)
      (with-access::liveblk b (in out_onf out_onr in_onf in_onr first)
	 (multiple-value-bind (f r) (back_instrs first out_onf out_onr)
			       (set! in_onf (intersection in f))
			       (set! in_onr (intersection in r)) )))
   (define (all_inter l out)
      (if (null? l)
	  (values '() '())
	  (let rec ( (l l) (f out) (r out) )
	     (if (null? l)
		 (values f r)
		 (with-access::liveblk (car l) (in_onf in_onr)
		    (rec (cdr l)
			 (intersection f in_onf)
			 (intersection r in_onr) ))))))
   (define (check_in b f r)
      (with-access::liveblk b (preds in in_onf in_onr)
	 (let ( (r (intersection r in)) (f (intersection f in)) )
	    (unless (and (set_include f in_onf) (set_include r in_onr))
	       (set! in_onf f)
	       (set! in_onr r)
	       (map fixpoint preds) ))))
   (define (check_out b f r)
      (with-access::liveblk b (first out_onf out_onr)
	 (unless (and (set_include f out_onf) (set_include r out_onr))
	    (set! out_onf f)
	    (set! out_onr r)
	    (multiple-value-bind (f r) (back_instrs first f r)
				 (check_in b f r) ))))
   (define (fixpoint b)
      (with-access::liveblk b (out succs)
	 (multiple-value-bind (f r) (all_inter succs out) (check_out b f r)) ))
   (for-each init l)
   (for-each back l)
   (for-each fixpoint l) )

(define (fix-in-reg-forward l)
   (define (forw_inst ins f r)
      (with-access::liveins ins (dest args after)
	 (let ( (regs (leaves args)) )
	    (if (no-alloc-in ins)
		(values (intersection after (set_sub1? f dest))
			(intersection after (union (set_add1? dest r) regs)) )
		(values (intersection after (set_sub1? after dest))
			(intersection after (set_add1? dest '())) )))))
   (define (forw_instrs l f r)
      (if (null? l)
	  (values f r)
	  (multiple-value-bind (f r) (forw_inst (car l) f r)
			       (forw_instrs (cdr l) f r) )))
				
   (define (forw b)
      (with-access::liveblk b (out_onf out_onr in_onf in_onr first)
	 (multiple-value-bind (f r) (forw_instrs first in_onf in_onr)
			       (set! out_onf f)
			       (set! out_onr r) )))
   (define (all_inter l in)
      (if (null? l)
	  ;; Here we are at L0
	  (values '() '())
	  (let rec ( (l l) (f in) (r in) )
	     (if (null? l)
		 (values f r)
		 (with-access::liveblk (car l) (out_onf out_onr)
		    (rec (cdr l)
			 (intersection f out_onf)
			 (intersection r out_onr) ))))))
   (define (check_in b f r)
      (with-access::liveblk b (first in_onf in_onr)
	 (unless (and (set_include f in_onf) (set_include r in_onr))
	    (set! in_onf (union f in_onf))
	    (set! in_onr (union r in_onr))
	    (multiple-value-bind (f r) (forw_instrs first f r)
				 (check_out b f r) ))))
   (define (check_out b f r)
      (with-access::liveblk b (succs out_onf out_onr)
	 (unless (and (set_include f out_onf) (set_include r out_onr))
	    (set! out_onf (union f out_onf))
	    (set! out_onr (union r out_onr))
	    (map fixpoint succs) )))
   (define (fixpoint b)
      (with-access::liveblk b (preds in)
	 (multiple-value-bind (f r) (all_inter preds in) (check_in b f r)) ))
   (for-each forw l)
   (for-each fixpoint l) )

(define (default-in-frame l)
   (define (forw_inst ins f r)
      (with-access::liveins ins (dest args after)
	 (let ( (regs (leaves args)) )
	    (if (no-alloc-in ins)
		(values (intersection after (set_sub1? f dest))
			(intersection after (union (set_add1? dest r) regs)) )
		(values (intersection after (set_sub1? after dest))
			(intersection after (set_add1? dest '())) )))))
   (define (forw_instrs l f r)
      (if (null? l)
	  (values f r)
	  (multiple-value-bind (f r) (forw_inst (car l) f r)
			       (forw_instrs (cdr l) f r) )))
				
   (define (forw b)
      (with-access::liveblk b (out_onf out_onr in_onf in_onr first)
	 (multiple-value-bind (f r) (forw_instrs first in_onf in_onr)
			       (set! out_onf f)
			       (set! out_onr r) )))
   (define (dif b)
      (with-access::liveblk b (in in_onf in_onr)
	 (let ( (rem (set_sub in (union in_onf in_onr))) )
	    (unless (null? rem)
	       (set! in_onf (union rem in_onf))
	       (forw b) ))))
   (for-each dif l) )

;; Liveness (taken from SawJvm/code.scm) updated for only basic-type
(define (live-init b)
   (widen!::liveblk b (in '()) (out '()) (in_onf '()) (in_onr '())
		      (out_onf '()) (out_onr '()) )
   (for-each (lambda (ins) (widen!::liveins ins (after '())))
	     (block-first b) ))

(define (live-fix b)
   (define (live-arg a regs)
      (cond
	 ((not (rtl_reg? a))
	  (live-args (rtl_ins-args a) regs) )
	 ((or (basic-type? (rtl_reg-type a)) (memq a regs))
	  regs )
	 (else (cons a regs)) ))
   (define (live-args args regs)
      (for-each (lambda (a) (set! regs (live-arg a regs))) args)
      regs )
   (define (live-instr ins regs)
      (with-access::liveins ins (dest fun args after)
	 (set! after regs)
	 (live-args args (if dest (remq dest regs) regs)) ))
   (define (live-instrs l regs)
      (if (null? l)
	  regs
	  (live-instr (car l) (live-instrs (cdr l) regs)) ))
   (define (fixpoint b live)
      (with-access::liveblk b (in out preds first)
	 (set! out live)
	 (let ( (nlive (live-instrs first live)) )
	    (unless (set_include nlive in)
	       (set! in nlive)
	       (for-each (lambda (p)
			    (let ( (o (liveblk-out p)) )
			       (unless (set_include nlive o)
				  (fixpoint p (union nlive o)) )))
			 preds )))))
   (fixpoint b (liveblk-out b)) )

;;
;; Set library
;;
(define (set_add1 x l)
   (cond ((null? l) (cons x '()))
	 ((eq? (car l) x) l)
	 (else (cons (car l) (set_add1 x (cdr l)))) ))

(define (set_add1? x l)
   (if x (set_add1 x l) l) )

(define (set_sub1 l x)
   (cond ((null? l) '())
	 ((eq? (car l) x) (cdr l))
	 (else (cons (car l) (set_sub1 (cdr l) x))) ))

(define (set_sub1? l x)
   (if x (set_sub1 l x) l) )

(define (set_sub l1 l2)
   (cond ((null? l1) '())
	 ((memq (car l1) l2) (set_sub (cdr l1) l2))
	 (else (cons (car l1) (set_sub (cdr l1) l2))) ))

(define (set_include l1 l2)
   (cond ((null? l1) #t)
	 ((memq (car l1) l2) (set_include (cdr l1) l2))
	 (else #f) ))

(define (intersection l1 l2)
   (cond ((null? l1) '())
	 ((memq (car l1) l2) (cons (car l1) (intersection (cdr l1) l2)))
	 (else (intersection (cdr l1) l2)) ))

(define (union l1 l2)
   (cond ((null? l1) l2)
	 ((memq (car l1) l2) (union (cdr l1) l2))
	 (else (union (cdr l1) (cons (car l1) l2))) ))

