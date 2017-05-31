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
   (static
    (class gen-info
       maxlabel
       (params (default '()))
       (locals (default '()))
       (frame-size (default 0))
       (comment-body (default #f))
       (comment-label (default #f))
       (comment (default #f))
       )
    (wide-class SawCIreg::rtl_reg index findex)
    (wide-class liveblk::block (mark (default #f))
       ;; annotation on control flow edges
       (annots (default '()))
       ;; for tarjan's algo of connex component
       (index (default #f)) (lowlink (default #f)) (onStack (default #f))
       ;; for the variable mapping
       (in (default '())) (out (default '()))
       (in_onf (default '())) (in_onr (default '()))
       (out_onf (default '())) (out_onr (default '()))
       ;; for min-cut
       (wsuccs (default '()))
       ;; for the tree cover
       (frame (default '())) (room (default #f)) (onvisit (default #f)) )
    (class room in sons up dframe)
    (class annot)
    (class spill::annot r::SawCIreg at)
    (class spill0::annot at)
    (class fill::annot r::SawCIreg at)
    (class flink::annot)
    (class funlink::annot)
    (class fsize::annot size)

    (wide-class liveins::rtl_ins
       (annots (default '()))
       (postannots (default '()))
       (after (default '())) )

    (class vertex
       (label (default "unamed"))
       (succs (default '()))
       (mark (default #f)) )

    ))

;; see make-info for switching some general flags

(define *trace* #f)

(define *count* #f)
(define *counter* 0)

(define *count-memory-access* #f)
(define *read-register* 0)
(define *write-register* 0)
(define *read-frame* 0)
(define *write-frame* 0)

(define *inline-simple-macros* #t)
(define *hasprotect* #f)
(define *haspushexit* #f)
(define *haspushbefore* #f)
(define *pushtraceemmited* 0)
(define *trace_used* #f)
(define *trace_nb_frames* #f)


(define *systematic-frame-access* #f)
(define *systematic-spill-fill* #f)
(define *generic-unique-frame* #t)
(define *generic-meet-frame* #f)
(define *generic-tree-frame* #f)
(define *ondemand-unique-frame* #f)

;; *saw-spill* defined outside

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
  (when (or *count* *count-memory-access*)
     (when *count*
	(display "extern int bbcount[];\n")
	(display "extern char *bbname[];\n") )
     (when *count-memory-access*
	(display "long __read_register__ = 0;\n")
	(display "long __read_frame__ = 0;\n")
	(display "long __write_register__ = 0;\n")
	(display "long __write_frame__ = 0;\n") )
     (display "extern obj_t PRINT_TRACE(obj_t);\n\n") )
   (display "\n") )

(define (saw-cepilogue) ;()
   (with-output-to-port *c-port*
      (lambda () (cepilogue)) ))

(define (cepilogue)
   (display "\n")
   (when (or *count* *count-memory-access*)
      (when *count*
	 (display* "int bbcount[" *counter* "];\n")
	 (display* "char *bbname[" *counter* "];\n") )
      (display "obj_t PRINT_TRACE(obj_t r) {\n")
      (when *count*
	 (display "\tint i;\n")
	 (display* "\tfor(i=0;i<" *counter* ";i++)\n")
	 (display "\t\tprintf(\"%d\t%d\\n\",bbcount[i],i);\n") )
      (when *count-memory-access*
	 (display "\tprintf(\" read: reg=%ld frame=%ld, write: reg %ld frame %ld\\n\", __read_register__, __read_frame__, __write_register__, __write_frame__);\n") )
      (display "\treturn(BIGLOO_EXIT(r));\n")
      (display "}\n") ))

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
	 (let ( (params (map local->reg args)) (info (make-info l)) )
	    (set! l (pre-process b info v params (clean-first-block info l)))
	    (on-trace (cgen 2) (dump-basic-blocks 'genfun v params l))
	    (when (gen-info-comment info) (display* "/* " id " */\n"))
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
	    (declare-variables info)
	    (if *trace* (display* "printf(\"" id "=" name "\\n\");\n"))
	    (genbody info name l)
	    (display (make-string *pushtraceemmited* #\}))
	    (display "}\n") ))))

(define (make-info l)
   (define (maxlabel l)
      (cond ((null? l) 0)
	    (else (max (block-label (car l)) (maxlabel (cdr l)))) ))
   (instantiate::gen-info
      (comment #f)
      (comment-body #f)
      (comment-label #t)
      (maxlabel (+fx 1 (maxlabel l)))) )

(define (clean-first-block info l)
   ;; sometimes the first block is also a recursive point
   (let ( (b0 (car l)) )
      (if (and #f (null? (block-preds b0)))
	  l
	  (let ( (goto (instantiate::rtl_ins
			  (fun (instantiate::rtl_go (to b0)))
			  (args '()) ))
		 (n (gen-info-maxlabel info)) )
	     (let ( (nbb (instantiate::block (label n)
					     (preds '())
					     (succs (cons b0 '()))
					     (first (cons goto '())) )) )
		(block-preds-set! b0 (cons nbb (block-preds b0)))
		(gen-info-maxlabel-set! info (+fx n 1))
		(cons nbb l) )))))


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

;; Local variables declaration
(define (declare-variables info)
   (for-each (lambda (v) (declare-reg info v)) (gen-info-locals info))
   (declare-frame (gen-info-frame-size info)) )

(define (declare-reg info r)
   (display "\t")
   (gen-type-reg r)
   (display ";")
   (when (gen-info-comment info)
      (let ( (type (rtl_reg-type r)) )
	 (with-access::type type (id name)
	    (display* " /* " id "=" name " */") )))
   (display "\n") )

(define (declare-frame size)
   (when (> size 0)
      (print "\tstruct {")
      (print "\t\t bgl_saw_frame_header_t header;")
      (let rec ( (i 0) )
	 (when (< i size)
	    (print "\t\tobj_t F" i ";")
	    (rec (+fx i 1)) ))
      (print "\t} lpf;") ))


;;
;; Code generation
;;
(define (genbody info name l) ;(list block)
   (when (gen-info-comment-body info) (comment-body l))
   (for-each (lambda (b)
		(out-label name (block-label b))
		(when (gen-info-comment-label info) (comment-label b))
		(let ( (l (block-first b)) )
		   (if (location? (find-location (car l))) (print ""))
		   (for-each gen-ins l) ))
	     l ))

(define (rs l) (map SawCIreg-index l))
(define (bs l) (map block-label l))
(define (sr l) (map (lambda (r) (bs (room-in r))) l))
(define (rl r) (block-label (car (room-in r))))

(define (comment-body l)
   (print " /* CC= " (map bs (tarjan l)) " */ ")
   (print " /* TC=" (room2list (liveblk-room (car l))) " */")
   (print " /* FC=" (r2frame (liveblk-room (car l))) " */")
   (print " /* FD=" (r2dframe (liveblk-room (car l))) " */") )

(define (room2list r)
   (cons (map block-label (room-in r))
	 (map room2list (room-sons r)) ))

(define (r2frame r)
   (cons (rs (room-framed r)) (map r2frame (room-sons r))) )

(define (r2dframe r)
   (cons (rs (room-dframe r)) (map r2dframe (room-sons r))) )

(define (comment-label b)
   (with-access::liveblk b (frame preds succs room in in_onf in_onr out out_onf out_onr)
      (print "\t/* cf:" (bs preds) "->" (bs succs)
	     " lfr " (rs in) (rs in_onf) (rs in_onr) "->"
	     (rs out) (rs out_onf) (rs out_onr)
	     " in " (if room (rl room) #f)
	     " framed= " (rs (bb-framed b))
	     " frame=" (rs frame) " */" )
      (when room
	 (let* ( (r (liveblk-room b)) (rin (room-in r)) )
	    (when (and rin (eq? b (car rin)) (not (null? (cdr rin))))
	       (print "\t/* room=" (bs rin)
		      " f=" (rs (room-framed r)) " */" ))))))

(define (out-label name label)
   (display* "L" label ":")
   (when *trace_used*
      (print "\tif(bps_time++ % 10000 == 0)\n")
      (print "\t\tfprintf(stderr, \"%ld %d\\n\", bps_time++, bps_used());") )
   (when *trace_nb_frames*
      (print "\tif(bps_time++ % 10000 == 0)\n")
      (print "\t\tfprintf(stderr, \"%ld %d\\n\", bps_time++, bps_nbFrames());") )
   (when *count*
      (display* "\tbbcount[" *counter* "]++;")
      (set! *counter* (+ 1 *counter*)) )
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
   (with-access::liveins ins (dest fun args annots postannots)
      (print-location? (find-location ins))
      (for-each gen-annot annots)
      (display "\t")
      (if dest
	  (begin (gen-reg/dest dest)
		 (display " = ") ))
      (gen-expr fun args)
      (display ";\n")
      (for-each gen-annot postannots) ))

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
   (gen-reg-standard reg) )

(define (gen-reg-standard reg) ;()
   (display (if (SawCIreg-var reg) "V" "R"))
   (display (SawCIreg-index reg)) )

;;
;; The gen-annot method
;;
(define-generic (gen-annot a::annot);
   (error 'gen-annot "undefined-method" a) )

(define-method (gen-annot a::spill);
   (with-access::spill a (r at)
      (display "\tlpf.F")
      (display at)
      (display " = ")
      (gen-reg-standard r)
      (print ";") ))

(define-method (gen-annot a::spill0);
   (with-access::spill0 a (at)
      (print "\tlpf.F" at " = " 0 ";") ))

(define-method (gen-annot a::fill);
   (with-access::fill a (r at)
      (display "\t")
      (gen-reg-standard r)
      (print " = lpf.F" at ";") ))

(define-method (gen-annot a::flink);
   (print "\tlpf.header.link = BGL_ENV_SAW_SP(BGL_CURRENT_DYNAMIC_ENV());")
   (print "\tBGL_ENV_SAW_SP_SET(BGL_CURRENT_DYNAMIC_ENV(), &(lpf.header));") )

(define-method (gen-annot a::funlink);
   (print "\tBGL_ENV_SAW_SP_SET(BGL_CURRENT_DYNAMIC_ENV(), lpf.header.link);") )

(define-method (gen-annot a::fsize);
   (with-access::fsize a (size)
      (print "\tlpf.header.size = " size ";") ))

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
   (patch-gname (global-name var)) )

(define (patch-gname s) ;()
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
       (if (or *count* *count-memory-access*) "PRINT_TRACE" s) )
      ;; MANU
      ((string=? s "STRING_REF") "BGL_RTL_STRING_REF")
      (else s) ))

;;
;; Specific methods for expression folding
;;
(define-method (accept-folding? b::cvm ins tree)
   (or (deep-mov? tree)
       (and (not (multiple-evaluation ins tree))
	    (cond ((or *systematic-spill-fill*
		       *generic-unique-frame*
		       *generic-meet-frame*
		       *generic-tree-frame*
		       *ondemand-unique-frame*
		       )
		   (and (not (rtl_return? (rtl_ins-fun ins)))
			(fold_with_spil ins tree) ))
		  (*systematic-frame-access*
		   (and (not (rtl_return? (rtl_ins-fun ins)))
			(fold_in_frame ins tree) ))
		  (else #t) ))))

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
;(define-method (no-allocation-fun fun::rtl_return) #f) ; this one is special
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


;;;
;;; Pre processing
;;;
(define (pre-process b::cvm info v params l)
   ;; when possible create expression in arguments of instructions
   (build-tree b params l)
   ;; merge some variables
   (when *saw-register-allocation?*
      (set! l (register-allocation b v params l)) )
   ;; some common initialisation
   (pre-process-init info params l)
   ;; Specific analysis / ust return the new list of basic blocks
   (cond (*systematic-frame-access*
	  (always-frame-access info l) )
	 (*systematic-spill-fill*
	  (always-spill-fill info l) )
	 (*generic-unique-frame*
	  (generic-unique-frame info l) )
	 (*generic-meet-frame*
	  (generic-meet-frame info l) )
	 (*generic-tree-frame*
	  (generic-tree-frame info l) )
	 (*ondemand-unique-frame*
	  (ondemand-unique-frame info l) )
	 (else l) ))

(define (pre-process-init info params l) ;()
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
		(findex #f) )
	     (set! n (+fx n 1))
	     (set! regs (cons e regs)) )
	    (else (check_fun (rtl_ins-fun e))
		  (for-each expr->ireg (rtl_ins-args e)) )))
      (define (visit b::block)
	 (widen!::liveblk b)
	 (let rec ( (l (block-first b)) )
	    (unless (null? l)
	       (let ( (ins (car l)) )
		  (with-access::rtl_ins ins (dest fun args)
		     (widen!::liveins ins)
		     (check_fun fun)
		     (if dest (expr->ireg dest))
		     (for-each expr->ireg args) ))
	       (rec (cdr l)) )))
      (for-each expr->ireg params)
      (set! regs '())
      (for-each visit l)
      (gen-info-locals-set! info regs)
      (gen-info-params-set! info params) ))

;; About variables/instructions/annotations
(define (a-fill r)
   (instantiate::fill (r r) (at (SawCIreg-findex r))) )

(define (a-spill r)
   (instantiate::spill (r r) (at (SawCIreg-findex r))) )

(define (a-spill0 r)
   (instantiate::spill0 (at (SawCIreg-findex r))) )

(define (a-funlink)
   (instantiate::funlink) )

(define (add-annot ins a)
   (liveins-annots-set! ins (cons a (liveins-annots ins))) )

(define (adds-annot ins l)
   (liveins-annots-set! ins (append l (liveins-annots ins)))
   #f )

(define (add-postannot ins a)
   (liveins-postannots-set! ins (cons a (liveins-postannots ins))) )

(define (adds-postannot ins l)
   (liveins-postannots-set! ins (append l (liveins-postannots ins)))
   #f )

(define (i-fill i r)
   (add-annot i (a-fill r)) )

(define (i-spill i r)
   (add-annot i (a-spill r)) )

(define (i-post-fill i r)
   (add-postannot i (a-fill r)) )

(define (i-post-spill i r)
   (add-postannot i (a-spill r)) )

(define (i-funlink i)
   (add-annot i (a-funlink)) )

(define (ret-annot ins)
   (with-access::liveins ins (dest fun args)
      (when (rtl_return? fun)
	 (i-funlink ins) )))


;; edge annotation
(define (add-edge-annot from to a)
   (with-access::liveblk from (annots)
      (let ( (slot (assq to annots)) )
	 (if slot
	     (unless (member a (cdr slot))
		(set-cdr! slot (cons a (cdr slot))) )
	     (set! annots (cons (list to a) annots)) ))))

(define (del-edge-annot from to a)
   (with-access::liveblk from (annots)
      (let ( (slot (assq to annots)) )
	 (when slot
	    (set-cdr! slot (delete a (cdr slot))) ))))

(define (has-edge-annot from to a)
   (member a (cassq to (liveblk-annots from))) )

(define (add-edge-annot-fill from to v)
   (add-edge-annot from to (a-fill v)) )

(define (add-edge-annot-spill from to v)
   (add-edge-annot from to (a-spill v)) )

(define (del-edge-annot-fill from to v)
   (del-edge-annot from to (a-fill v)) )

(define (del-edge-annot-spill from to v)
   (del-edge-annot from to (a-spill v)) )

(define (has-edge-annot-fill from to v)
   (has-edge-annot from to (a-fill v)) )

(define (has-edge-annot-spill from to v)
   (has-edge-annot from to (a-spill v)) )

(define (leave e)
   (if (rtl_reg? e)
       (cons e '())
       (leaves (rtl_ins-args e)) ))

(define (leaves l)
   (if (null? l) '() (union (leave (car l)) (leaves (cdr l)))) )

(define (cut e in)
   (cond ((not (rtl_reg? e)) (cuts (rtl_ins-args e) in))
	 ((memq e in) (cons e '()))
	 (else '()) ))

(define (cuts l in)
   (if (null? l) '() (union (cut (car l) in) (cuts (cdr l) in))) )

;;;
;;; Systematic frame access
;;;
(define (set-frame-index info i l)
   (if (null? l)
       (check-gen-info-frame-size info i)
       (begin (SawCIreg-findex-set! (car l) i)
	      (set-frame-index info (+fx i 1) (cdr l)) )))

(define (check-gen-info-frame-size info i)
   (gen-info-frame-size-set! info (max i (gen-info-frame-size info))) )

(define (always-frame-access info l)
   (define (may-be-root v) (not (basic-type? (rtl_reg-type v))))
   (define (framed l) (filter may-be-root l))
   (with-access::gen-info info (params locals)
      (let* ( (rp (framed params)) (rl (framed locals)) (fv (append rp rl)) )
	 (define (ins-annotation ins)
	    (with-access::liveins ins (dest fun args)
	       (for-each (lambda (r) (i-fill ins r)) (cuts args fv))
	       (when (and dest (may-be-root dest))
		  (i-post-spill ins dest) )))
	 (if (null? fv)
	     l
	     (begin
		;; associate an index to each framed variable
		(unique_frame_index info fv l)
		;; add annot on all instructions
		(for-each (lambda (b) (for-each ins-annotation (block-first b)))
			  l )
		;; add annot for frame link/init/unlink
		(unique_frame_init info fv l)
		l )))))


(define (unique_frame_index info fv l)
   (set-frame-index info 0 fv) )

(define (unique_frame_init info fv l)
   ;; init-all in the first instruction
   (with-access::gen-info info (params locals)
      (liveins-annots-set! (car (block-first (car l)))
         `(,(instantiate::fsize (size (gen-info-frame-size info)))
	   ,(instantiate::flink)
	   ,@(map a-spill  (set_sub fv locals))
	   ,@(map a-spill0 (set_sub fv params)) )))
   ;; clean the frame on all returns.
   (unless (null? fv) (blocks-ret-annot l)) )

(define (blocks-ret-annot l)
   (for-each (lambda (b) (for-each ret-annot (block-first b))) l) )

;;;
;;; Systematic spill/fill
;;;
;;; A variable have to be in a frame when it exists an instruction such that
;;; 1) the instruction may call the gc, i.e. may do some allocation
;;; (no-alloc-in) 2) the variable is live after the instruction (after)
;;; 3) the variable is not the destination of the instruction

(define (instr-framed ins)
   ;; all the variables that "ins" impose to be in a frame
   (with-access::liveins ins (dest after)
      (if (no-alloc-in ins)
	  '()
	  (set_sub1? after dest) )))

(define (bb-framed b)
   (mapunion instr-framed (liveblk-first b)) )

(define (always-spill-fill info l)
   (liveness l)
   (with-access::gen-info info (params locals)
      (let ( (fv (mapunion bb-framed l)) )
	 (define (ins-annotation ins)
	    (with-access::liveins ins (fun)
	       (let ( (cut (instr-framed ins)) )
		  (for-each (lambda (r) (i-spill ins r)) cut)
		  (for-each (lambda (r) (i-post-fill ins r)) cut) )))
	 (if (null? fv)
	     l
	     (begin
		;; associate an index to each framed variable
		(unique_frame_index info fv l)
		;; add annot on all instructions
		(for-each (lambda (b) (for-each ins-annotation (block-first b)))
			  l )
		;; add annot for frame link/init/unlink
		(unique_frame_init info fv l)
		l )))))

;;;
;;; Generic spill/fill
;;;
;;; The systematic spill/fill have the correct set of variables to be put in a
;;; frame. But some spills and/or fills can be removed. For example in
;;; f(); g(); h(a); "a" doesn't have to be spilt two times. For each point of
;;; the program, for each "framed" variable, we have to kown if the value is in
;;; the register (C variable) and/or in the frame (already spilt).

;;; First we use a specific stategy wich compute the fields in_r in_f of the
;;; basic block. Then we use a specific strategy to assign a frame index, if
;;; needed, to each variables. with these two passes, spill/fill annotations
;;; can be done in a generic way.
(define (generic-spill-fill info l inrf findex initf reset-findex)
   (liveness l)
   (let ( (fv (mapunion bb-framed l)) )
      (if (null? fv)
	  l
	  (begin
	     ;; compute fields in_onf and in_onr
	     (inrf info fv l)
	     ;; associate an index to each framed variable
	     (findex info fv l)
	     ;; add annot on all instructions
	     (for-each (lambda (b) (do-spill-annotations reset-findex b fv)) l)
	     ;; init-all in the first instruction
	     (initf info fv l)
	     ;; create basic blocks for edge annotations
	     (manage-edge-annotations info l) ))))

(define (do-spill-annotations reset-findex bb fv)
   (define (ins-annotation ins f r)
      (with-access::liveins ins (dest fun args after)
	 (let ( (l (cuts args fv)) )
	    (for-each (lambda (r) (i-fill ins r)) (set_sub l r))
	    (unless (no-alloc-in ins)
	       (for-each (lambda (r) (i-spill ins r))
			 (set_sub1? (set_sub after f) dest) )))))
   (define (forw-inst ins f r)
      (with-access::liveins ins (dest args after)
	 (let ( (regs (cuts args fv)) )
	    (if (no-alloc-in ins)
		(values (set_sub1? f dest)
			(set_add1? dest (union r regs)) )
		(values (set_sub1? after dest)
			(set_add1? dest '()) )))))
   (define (add-bb-annot f t a)
      (unless (null? a)
	 (let ( (l (liveblk-annots f)) )
	    (liveblk-annots-set! f (cons (cons t a) l)) )))
   (define (unify f r b)
      (with-access::liveblk b (in in_onf in_onr)
	 (add-bb-annot bb b
	    (append (map a-spill (set_sub in_onf f))
		    (map a-fill (set_sub (union in_onr (set_sub in in_onf)) r)) ))))
   (with-access::liveblk bb (succs first in in_onf in_onr)
      (reset-findex bb)
      (let rec ( (l first) (f in_onf) (r (union in_onr (set_sub in in_onf))) )
	 (if (null? l)
	     (for-each (lambda (b) (unify f r b)) succs)
	     (begin (ins-annotation (car l) f r)
		    (multiple-value-bind (f r) (forw-inst (car l) f r)
					 (rec (cdr l) f r) ))))))

;;
;; Create extra basic blocks to manage annotations on edges
;;
(define (manage-edge-annotations info l)
   (append l (all-extra-basic-blocks info l)) )

(define (all-extra-basic-blocks info l)
   (if (null? l)
       '()
       (append (extra-basic-blocks info (car l))
	       (all-extra-basic-blocks info (cdr l)) )))

(define (extra-basic-blocks info bb)
   (with-access::liveblk bb (annots)
      (let ( (r '()) )
	 (for-each (lambda (ba)
		      (let ( (nbb (edge-annot info bb (car ba) (cdr ba))) )
			 (when nbb (set! r (cons nbb  r))) ))
		   annots )
	 r )))

(define (edge-annot info from to annots)
   (if (null? (cdr (block-preds to)))
       (let ( (ins (car (block-first to))) )
	  (liveins-annots-set! ins (append annots (liveins-annots ins)))
	  #f )
       (let ( (ins (car (last-pair (block-first from)))) )
	  (let ( (fun (rtl_ins-fun ins)) )
	     (edge-annotation fun info ins from to annots) ))))

(define (insert-new-bb info from to annots)
   (let ( (goto (instantiate::liveins
		  (fun (instantiate::rtl_go (to to)))
		  (args '())
		  (annots annots)
		  (after #f) ))
	  (n (gen-info-maxlabel info)) )
      (let ( (nbb (instantiate::liveblk (label n)
					(preds (cons from '()))
					(succs (cons to '()))
					(first (cons goto '()))
					(room (liveblk-room from)) )) )
	 (block-succs-set! from (subst (block-succs from) to nbb))
	 (block-preds-set! to (subst (block-preds to) from nbb))
	 (gen-info-maxlabel-set! info (+fx n 1))
	 nbb )))

;;;
;;; The generic spill/fill with a unique frame
;;;
(define (generic-unique-frame info l)
   (generic-spill-fill info l
		       standard_inrf
		       unique_frame_index
		       unique_frame_init
		       (lambda (x) x) ))


;;
;; computation of the fields in_r and in_f of the basic blocks
;;
(define (standard_inrf info fv l)
   (fix-in-reg-backward l fv)
   (fix-in-reg-forward l info fv)
   (insure-in-frame-or-in-register l) )
   

(define (fix-in-reg-backward l fv)
   (define (init b)
      (with-access::liveblk b (out_onr out_onf)
	 (set! out_onr '())
	 (set! out_onf '()) ))
   (define (back_inst ins f r)
      (with-access::liveins ins (dest args after)
	 (let ( (regs (cuts args fv)) )
	    (if (no-alloc-in ins)
		(values (set_sub1? f dest)
			(union (set_sub1? r dest) regs) )
		;; assume f included in after included in fv ?
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

(define (fix-in-reg-forward l info fv)
   (define (forw_inst ins f r)
      (with-access::liveins ins (dest args after)
	 (let ( (regs (cuts args fv)) )
	    ;; CARE Check why intersection and remove dest
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
	  (values '() (gen-info-params info))
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

(define (insure-in-frame-or-in-register l)
   (define (bb_unplaced b)
      (with-access::liveblk b (in in_onf in_onr)
	 (set_sub in (union in_onf in_onr)) ))
   (define (unplaced l) (mapunion bb_unplaced l))
   (let ( (vars (unplaced l)) )
      (unless (null? vars)
	 (print "/* UNPLACED VARS " (rs vars) " */")
	 (for-each (lambda (v) (insure-var-inf-or-inr v l)) vars) )))

(define (insure-var-inf-or-inr v l)
   (define (bb-unplaced? b)
      (with-access::liveblk b (in in_onf in_onr)
	 (memq v (set_sub in (union in_onf in_onr))) ))
   (let ( (ll (filter bb-unplaced? l)) )
      (let ( (rn (instantiate::liveblk (label -1) (first (list 'nop))))
	     (fn (instantiate::liveblk (label -2) (first (list 'nop)))) )
	 (define (add-edge f t)
	    (with-access::liveblk f (wsuccs)
	       (let ( (slot (assq t wsuccs)) )
		  (if slot
		      (set-cdr! slot (+fx 1 (cdr slot)))
		      (set! wsuccs (cons (cons t 1) wsuccs)) ))))
	 (define (add-edge2 f t)
	    (when (and f t)
	       (add-edge f t)
	       (add-edge t f) ))
	 (define (rnode b live f r)
	    (if (bb-unplaced? b)
		b
		(if (memq v f) (unless (memq v r) rn) fn) ))
	 (define (rnode-in b)
	    (with-access::liveblk b (out out_onf out_onr)
	       (rnode b out out_onf out_onr) ))
	 (define (rnode-out b)
	    (with-access::liveblk b (in in_onf in_onr)
	       (rnode b in in_onf in_onr) ))
	 (for-each (lambda (b) (liveblk-wsuccs-set! b '())) ll)
	 (for-each (lambda (b)
		      (for-each (lambda (bb) (add-edge2 (rnode-in bb) b))
				(block-preds b) )
		      (for-each (lambda (bb) (add-edge2 b (rnode-out bb)))
				(block-succs b) ))
		   ll )
	 (let ( (r (min-cut ll rn fn liveblk-label
			    liveblk-mark liveblk-mark-set!
			    liveblk-wsuccs liveblk-wsuccs-set!
			    )) )
	    ; r is the list of bb where v must be in a reg
	    (for-each (lambda (b) (with-access::liveblk b (in_onr)
				     (set! in_onr (cons v in_onr)) ))
		      r )
	    (for-each (lambda (b) (with-access::liveblk b (in_onf)
				     (set! in_onf (cons v in_onf)) ))
		      (set_sub ll r) )))))


(define (insure-in-frame-or-in-register__old l)
   ;; sometimes (prove that can only occur for a strongly connected component)
   ;;;           ^ Wrong (see before)
   ;; it appears that for a basic block, in_onf cup in_onr is not the set of
   ;; framed variables, if "x" is such a variable, we have to decide if for
   ;; basic block, x must considered as in a register or already in the frame.
   ;; We can do some improvments as considering, as here, that  x must be in
   ;; the frame
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
	       ;; Seems useless (no need of out_on?
	       (forw b) ))))
   (for-each dif l) )

;;
;; The edge-annotation method
;;
(define-generic (edge-annotation fun::rtl_fun info ins::liveins from::block to::block annots::pair-nil);
   ;; the default-case is when fun is not a branching instruction, there is
   ;; an implicit goto to the next instruction. Done via postannots
   (adds-postannot ins annots) )

(define-method (edge-annotation fun::rtl_ifeq info ins from to annots);
   (with-access::rtl_ifeq fun (then)
      (if (eq? then to)
	  (let ( (r (insert-new-bb info from to annots)) )
	     (set! then r)
	     r )
	  (adds-postannot ins annots) )))

(define-method (edge-annotation fun::rtl_ifne info ins from to annots);
   (with-access::rtl_ifne fun (then)
      (if (eq? then to)
	  (let ( (r (insert-new-bb info from to annots)) )
	     (set! then r)
	     r )
	  (adds-postannot ins annots) )))
   
(define-method (edge-annotation fun::rtl_go info ins from to annots);
   (adds-annot ins annots) )

(define-method (edge-annotation fun::rtl_switch info ins from to annots);
   (with-access::rtl_switch fun (labels)
      (let ( (r (insert-new-bb info from to annots)) )
	 (set! labels (subst labels to r))
	 r )))

;;;
;;; Based on tree cover, variables have still a unique frame index
;;;
(define (generic-meet-frame info l)
   (generic-spill-fill info l
		       standard_inrf
		       meet_frame_index
		       meet_frame_init
		       (lambda (x) x) ))

(define (meet_frame_index info fv l)
   (define (walk r f)
      (let ( (lf (room-local-frame r f)) )
	 (let ( (nf (append f lf)) )
	    (set-frame-index info (length f) lf)
	    (for-each (lambda (r) (walk r nf)) (room-sons r)) )))
   (build-room-tree l)
   (walk (liveblk-room (car l)) '()) )

(define (room-local-frame r up)
   (let ( (l (room-sons r)) (sure (room-framed r)) )
      (if (null? l)
	  (set_sub sure up)
	  (let rec ( (l (cdr l)) (once (cont-framed (car l))) (sure sure) )
	     (if (null? l)
		 (set_sub sure up)
		 (let ( (once2 (cont-framed (car l))) )
		    (rec (cdr l)
			 (union (set_sub once once2) (set_sub once2 once))
			 (union sure (intersection once once2)) )))))))
	 
(define (room-framed r)
   (mapunion bb-framed (room-in r)) )

(define (cont-framed r)
   (union (room-framed r) (mapunion cont-framed (room-sons r))) )

(define (meet_frame_init info fv l)
   (define (walk up f s)
      (let ( (lf (room-local-frame s f)) )
	 (let ( (nf (append f lf)) )
	    (unless (null? nf) (blocks-ret-annot (room-in s)))
	    (for-each (lambda (from)
			 (for-each (lambda (to)
				      (when (and (eq? (liveblk-room to) s)
						 (not (null? lf)) )
					 (frame-extension info from f to lf) ))
				   (block-succs from) ))
		      (room-in up) )
	    (for-each (lambda (ss) (walk s nf ss)) (room-sons s)) )))
   (let ( (m (liveblk-room (car l))) )
      (for-each (lambda (s) (walk m '() s)) (room-sons m)) ))

(define (frame-extension info from f to slot)
   (if (and #f (null? (cdr (block-preds to))))
       (frame-extension-inside info f to slot)
       (let ( (l (liveblk-annots from))
	      (a (frame-extension-edge info f slot)) )
	  (liveblk-annots-set! from (cons (cons to a) l)) )))

(define (frame-extension-inside info f b slot)
   (error 'notyet "inside propagation" (block-label b)) )

(define (frame-extension-edge info f slot)
   (with-access::gen-info info (params locals)
      `(,(instantiate::fsize (size (+fx (length f) (length slot))))
	   ,@(if (null? f) (list (instantiate::flink)) '())
	   ,@(map a-spill (set_sub slot locals))
	   ,@(map a-spill0 (set_sub slot params)) )))



;;;
;;; Based on tree cover, variables can be in differents places in the frame
;;;
(define (generic-tree-frame info l)
   (generic-spill-fill info l
		       noup_inrf
		       tree_frame_index
		       tree_frame_init
		       reset-tree-findex ))

;; Idea to optimize the computation of in_on? 1) fix backward inside a room where
;; we remove the exiting edges to the subrooms 2) a simple backward
(define (noup_inrf info fv l)
   (fix-in-reg-forward l info fv) )

(define (reset-tree-findex b)
   (let rec ( (l (liveblk-frame b)) (i 0) )
      (unless (null? l)
	 (SawCIreg-findex-set! (car l) i)
	 (rec (cdr l) (+fx i 1)) )))

(define (tree_frame_index info fv l)
   (define (walk r f)
      (let ( (lf (room-local-local-frame r f)) )
	 (let ( (nf (append f lf)) )
	    (check-gen-info-frame-size info (length nf))
	    (for-each (lambda (b) (liveblk-frame-set! b nf)) (room-in r))
	    (for-each (lambda (r) (walk r nf)) (room-sons r)) )))
   (build-room-tree l)
   (walk (liveblk-room (car l)) '()) )

(define (room-local-local-frame r up)
   (set_sub (room-framed r) up) )

(define (tree_frame_init info fv l)
   (define (walk up f s)
      (let ( (lf (room-local-local-frame s f)) )
	 (let ( (nf (append f lf)) )
	    (unless (null? nf) (blocks-ret-annot (room-in s)))
	    (for-each (lambda (from)
			 (for-each (lambda (to)
				      (when (and (eq? (liveblk-room to) s)
						 (not (null? lf)) )
					 (frame-extension info from f to lf) ))
				   (block-succs from) ))
		      (room-in up) )
	    (for-each (lambda (ss) (walk s nf ss)) (room-sons s)) )))
   (let ( (m (liveblk-room (car l))) )
      (for-each (lambda (s) (walk m '() s)) (room-sons m)) ))


	 




;;
;; On demand spill/fill
;;
(define (ondemand-spill-fill info l findex initf)
   (liveness l)
   (let ( (fv (mapunion bb-framed l)) )
      (if (null? fv)
	  l
	  (begin
	     ;; associate an index to each framed variable
	     (findex info fv l)
	     ;; add spill/fill annots on all instructions
	     (for-each (lambda (b)
			  (let rec ( (done '()) (todo (block-first b)) )
			     (unless (null? todo)
				(annot-ins b done (car todo) fv)
				(rec (append done (cons (car todo) '()))
				     (cdr todo) ))))
		       l )
	     ;; init-all in the first instruction
	     (initf info fv l)
	     ;; create basic blocks for edge annotations
	     (manage-edge-annotations info l) ))))
	     

(define (annot-ins b l ins fv)
   (for-each (lambda (v) (unless (filled? l b v '())
			    (do-fill l b v '() i-post-fill) ))
	     (cuts (rtl_ins-args ins) fv) )
   (for-each (lambda (v) (unless (spilled? l b v '())
			    (do-spill l b v '() i-post-spill) ))
	     (instr-framed ins) ))

(define (filled? l b v s)
   (if (null? l)
       (every (lambda (b) (or (memq b s) (filled? (block-first b) b v (cons b s))))
	      (block-preds b) )
       (let ( (ins (car (last-pair l))) )
	  (or (eq? v (rtl_ins-dest ins))
	      (and (no-alloc-in ins)
		   (or (memq v (leaves (rtl_ins-args ins)))
		       (filled? (all-but-last l) b v s) ))))))

(define (do-fill l b v s filler)
   (if (null? l)
       (for-each (lambda (bb)
		    (unless (or (memq bb s)
				(filled? (block-first bb) bb v (cons bb s)) )
		       (do-block-fill bb b v (cons bb s)) ))
		 (block-preds b) )
       (let ( (ins (car (last-pair l))) )
	  (when (eq? v (rtl_ins-dest ins))
	     (error 'do-fill "already in register" (SawCIreg-index v)) )
	  (if (no-alloc-in ins)
	      (unless (memq v (leaves (rtl_ins-args ins)))
		 (do-fill (all-but-last l) b v s i-post-fill) )
	      (filler ins v) ))))

(define (do-block-fill from to v s)
   (add-edge-annot-fill from to v)
   (when (every (lambda (b) (has-edge-annot-fill from b v))
		(liveblk-succs from) )
      (for-each (lambda (b) (del-edge-annot-fill from b v)) (liveblk-succs from))
      (do-fill (block-first from) from v s (last-post-fill from)) ))

(define (last-post-fill b)
   ;; May be better to think that a post annotation of a "ifne" mean a edge
   ;; annotation of the two sucessors instead of a the implicit continuation
   (lambda (ins v)
      (if (accept-post? (rtl_ins-fun ins))
	  (i-post-fill ins v)
	  (for-each (lambda (bb) (add-edge-annot-fill b bb v))
		    (block-succs b) ))))

(define-generic (accept-post? fun::rtl_fun);
   ;; the default-case is when fun is not a branching instruction, there is
   ;; an implicit goto to the next instruction. Done via postannots
   #t )

(define-method (accept-post? fun::rtl_notseq) #f)




(define (spilled? l b v s)
   (if (null? l)
       ;; (every f '())=#t => filled at L0
       (every (lambda (b) (or (memq b s) (spilled? (block-first b) b v (cons b s))))
	      (block-preds b) )
       (let ( (ins (car (last-pair l))) )
	  (and (not (eq? v (rtl_ins-dest ins)))
	       (or (not (no-alloc-in ins))
		   (or (memq v (leaves (rtl_ins-args ins)))
		       (spilled? (all-but-last l) b v s) ))))))

(define (do-spill l b v s spiller)
   (if (null? l)
       (for-each (lambda (bb)
		    (unless (or (memq bb s)
				(spilled? (block-first bb) bb v (cons bb s)) )
		       (do-block-spill bb b v (cons bb s)) ))
		 (block-preds b) )
       (let ( (ins (car (last-pair l))) )
	  (if (eq? v (rtl_ins-dest ins))
	      (spiller ins v)
	      (if (not (no-alloc-in ins))
		  (error 'do-spill "already in frame" (SawCIreg-index v))
		  (do-spill (all-but-last l) b v s i-post-spill) )))))

(define (do-block-spill from to v s)
   (add-edge-annot-spill from to v)
   (when (every (lambda (b) (has-edge-annot-spill from b v))
		(liveblk-succs from) )
      (for-each (lambda (b) (del-edge-annot-spill from b v)) (liveblk-succs from))
      (do-spill (block-first from) from v s (last-post-spill from)) ))

(define (last-post-spill b)
   (lambda (ins v)
      (if (accept-post? ins)
	  (i-post-spill ins v)
	  (for-each (lambda (bb) (add-edge-annot-spill b bb v))
		    (block-succs b) ))))

;;;
;;; The generic spill/fill with a unique frame
;;;
(define (ondemand-unique-frame info l)
   (ondemand-spill-fill info l
			unique_frame_index
			unique_frame_init ))




;;
;; Liveness (taken from SawJvm/code.scm) updated for only basic-type
;;
(define (liveness l)
   ;; CARE check time with (reverse l)
   (for-each live-fix l) )

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
;; Tarjan's computation of strongly connected components
;; en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
;;
(define (tarjan l)
   (let ( (index 0) (S '()) (P '()) )
      (define (strongconnect v)
	 (liveblk-index-set! v index)
	 (liveblk-lowlink-set! v index)
	 (set! index (+fx index 1))
	 (set! S (cons v S))
	 (liveblk-onStack-set! v #t)
	 (for-each
	  (lambda (w)
	     (cond ((eq? (liveblk-index w) #unspecified)
		    (strongconnect w)
		    (liveblk-lowlink-set! v (min (liveblk-lowlink v)
						 (liveblk-lowlink w) )) )
		   ((liveblk-onStack w)
		    (liveblk-lowlink-set! v (min (liveblk-lowlink v)
						 (liveblk-index w) )) )))
	  (liveblk-succs v) )
	 (when (=fx (liveblk-lowlink v) (liveblk-index v))
	    (let rec ( (C '()) (w (car S)) )
	       (set! S (cdr S))
	       (set! C (cons w C))
	       (liveblk-onStack-set! w #f)
	       (if (eq? v w)
		   (set! P (cons C P))
		   (rec C (car S)) ))))
		   
      (for-each (lambda (b) (when (eq? (liveblk-index b) #unspecified)
			       (strongconnect b) ))
		l )
      P ))

;;
;; Extract directly a tree from dfs of a graph
;;
(define (build-room-tree l)
   (tree-extract-root (car l)) )

(define (tree-extract-root b)
   (liveblk-room-set! b (instantiate::room (in (cons b '()))
					   (sons '())
					   (dframe '())
					   (up #f) ))
   (tree-extract b) )

(define (tree-extract b1)
   (define (move-up r n)
      (if (=fx n 0)
	  r
	  (move-up (room-up r) (-fx n 1)) ))
   (define (depth r)
      (if r
	  (+fx 1 (depth (room-up r)))
	  -1 ))
   (define (meet-same-lvl b1 b2)
      (if (eq? b1 b2)
	  b1
	  (meet-same-lvl (room-up b1) (room-up b2)) ))
   (define (meet r1 r2)
      (let ( (n1 (depth r1)) (n2 (depth r2)) )
	 (if (>fx n1 n2)
	     (meet-same-lvl (move-up r1 (-fx n1 n2)) r2)
	     (meet-same-lvl r1 (move-up r2 (-fx n2 n1))) )))
   (define (unify1 rb rm)
      (let ( (in (room-in rm)) (f (room-up rb)) )
	 (room-in-set! rm (append (room-in rb) in))
	 (when f (room-sons-set! f (remq rb (room-sons f))))
	 (for-each (lambda (r)
		      (room-up-set! r rm)
		      (room-sons-set! rm (cons r (room-sons rm))) )
		   (room-sons rb))
	 (for-each (lambda (b) (liveblk-room-set! b rm)) (room-in rb)) ))
   (define (unify-path from to first?)
      (cond ((eq? from to) 'ok)
	    ((not first?) (unify-path (room-up from) to #t))
	    (else (unify1 from to)
		  (unify-path (room-up from) to #t) )))
   (define (unify-room last? r1 r2)
      (let ( (mr (meet r1 r2)) )
	 (unify-path r1 mr #t)
	 (unify-path r2 mr last?) ))
   (liveblk-onvisit-set! b1 #t)
   (for-each (lambda (b2)
		(let ( (r1 (liveblk-room b1)) )
		   (with-access::liveblk b2 (room onvisit)
		      (cond ((eq? room #f)
			     ;; tree edge
			     (let ( (r2 (instantiate::room (in (cons b2 '()))
							   (sons '())
							   (dframe '())
							   (up r1) )) )
				(set! room r2)
				(room-sons-set! r1 (cons r2 (room-sons r1)))
				(tree-extract b2) ))
			    (onvisit ;; back edge
			     (unify-room #t r1 room) )
			    (else ;; forward or cross edge
			     (unify-room #f r1 room) )))))
	     (liveblk-succs b1) )
   (liveblk-onvisit-set! b1 #f) )


;;
;; Graph library
;;
(define (min-cut g s t nname mark mark! succs succs!)
   (define (see-graph g)
      (for-each (lambda (n)
		   (display* "/* " (nname n))
		   (for-each (lambda (e) (display* " " (nname (car e))
						   ":" (cdr e) ))
			     (succs n) )
		   (print " */") )
		g ))
   (define (get-edge n1 n2) (assq n2 (succs n1)))
   (define (rem-edge-succ n l)
      (cond ((null? l) l)
	    ((eq? (caar l) n) (cdr l))
	    (else (cons (car l) (rem-edge-succ n (cdr l)))) ))
   (define (rem-edge n1 n2)
      (succs! n1 (rem-edge-succ n2 (succs n1))) )
   (define (sub-weight! n1 n2 w)
      (let ( (e (get-edge n1 n2)) )
	 (let ( (r (-fx (cdr e) w)) )
	    (cond
	       ((<fx r 0)
		(error 'sub-weight "negative weight" (cons (cdr e) w)) )
	       ((=fx r 0)
		(rem-edge n1 n2) )
	       (else (set-cdr! e r)) ))))
   (define (add-weight! n1 n2 w)
      (let ( (e (get-edge n1 n2)) )
	 (if e
	  (set-cdr! e (+fx (cdr e) w))
	  (succs! n1 (cons (cons n2 w) (succs n1))) )))
   (define (dfs n visit)
      (let rec ( (n n) (p (cons n '())) )
	 (unless (mark n)
	    (mark! n #t)
	    (visit n p)
	    (for-each (lambda (e) (rec (car e) (cons (car e) p))) (succs n)) )))
   (define (get-path g s t) ;; return a path from s to t in g or #f if no
      (for-each (lambda (n) (mark! n #f)) g)
      (bind-exit (find)
	 (dfs s (lambda (n p) (if (eq? n t) (find (reverse! p)))))
	 #f ))
   (see-graph g)
   (print "/* unplaced basic blocks  " (map nname g) " */")
   (let rec ( )
      (let ( (p (get-path g s t)) )
	 (define (min-weight-in-path p)
	    (let rec ( (m (cdr (get-edge (car p) (cadr p))))
		       (p (cdr p)) )
	       (if (null? (cdr p))
		   m
		   (rec (min m (cdr (get-edge (car p) (cadr p))))
			(cdr p) ))))
	 (define (update-path p m)
	    ; (print "MIN " m)
	    (let rec ( (s (car p)) (p (cdr p)) )
	       (unless (null? p)
		  (let ( (u (car p)) )
		     (sub-weight! s u m)
		     (add-weight! u s m)
		     (rec (car p) (cdr p)) ))))
	 (if p
	     (begin ; (print "PATH " (map nname p))
		    (update-path p (min-weight-in-path p))
		    (rec) )
	     (begin (for-each (lambda (n) (mark! n #f)) g)
		    (let ( (r '()) )
		       (dfs (car g) (lambda (n p) (set! r (cons n r))))
		       (print "/*\t selected for regs " (map nname r) " */")
		       r ))))))


;;
;; Set library
;;
(define (subst l x y)
   (cond ((null? l) '())
	 ((eq? (car l) x) (cons y (subst (cdr l) x y)))
	 (else (cons (car l) (subst (cdr l) x y))) ))

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

(define (mapunion f l)
   (if (null? l)
       '()
       (union (f (car l)) (mapunion f (cdr l))) ))


;; lib
(define (all-but-last l)
   (if (null? (cdr l))
       '()
       (cons (car l) (all-but-last (cdr l))) ))

(define (cassq x l)
   (let ( (r (assq x l)) )
      (if r
	  (cdr r)
	  '() )))
