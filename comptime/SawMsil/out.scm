(module msil_out
   (import engine_param
	   type_type ast_var ast_node
	   object_class
	   object_slots
	   backend_backend
	   backend_bvm
	   backend_dotnet_class
	   backend_cplib
	   saw_defs
	   msil_names )
   (export (open-namespace me::dotnet)
	   (close-namespace me::dotnet)
	   (open-class me::dotnet name::bstring super::bstring public?)
	   (close-class me::dotnet)
	   (open-method me::dotnet name modifiers rtype args)
	   (close-method me::dotnet)
	   (declare-entrypoint me::dotnet)
	   (declare-maxstack me::dotnet n)
	   (declare-tail me::dotnet)
	   (open-global-method me::dotnet var::global)
	   (push-num me::dotnet n type)
	   (push-int me::dotnet n)
	   (push-string me::dotnet s::bstring)
	   (push-constant me::dotnet x)
	   (push-constant2 me::dotnet c x)
	   (load-global me::dotnet var::global)
	   (store-global me::dotnet var::global)
	   (load-par me::dotnet n::int)
	   (store-par me::dotnet n::int)
	   (load-var me::dotnet n::int)
	   (store-var me::dotnet n::int)
	   (call me::dotnet name modifiers args)
	   (call-global me::dotnet var::global)
	   (return me::dotnet)
	   (open-try me::dotnet)
	   (open-catch me::dotnet type cont)
	   (close-try me::dotnet cont)
	   (pop me::dotnet)
	   (dup me::dotnet)
	   (newarray me::dotnet type)
	   (load-vector me::dotnet type)
	   (store-vector me::dotnet type)
	   (newobj me::dotnet typename l)
	   (load-field me::dotnet type class field)
	   (switch me::dotnet labs)
	   (castclass me::dotnet type::type)
	   (declare-field me::dotnet field::slot)
	   (declare-global me::dotnet var::global)
	   (declare-locals me::dotnet l)
	   (comment me::dotnet tag::bstring obj)
	   (store-field me::dotnet type class field)
	   (load-vector-length me::dotnet)
	   (label me::dotnet prefix lab)
	   (br me::dotnet prefix::bstring label)
	   (brfalse me::dotnet prefix::bstring label)
	   (brtrue me::dotnet prefix::bstring label)
	   (throw me::dotnet)
	   (rethrow me::dotnet)
	   (isinst me::dotnet type::type)
	   (sub me::dotnet)
	   (beq me::dotnet prefix::bstring label)
	   (bne me::dotnet prefix::bstring label)
	   (leave me::dotnet cont)
	   (callvirt me::dotnet typer name args)
	   (ldnull me::dotnet)
	   (clt me::dotnet)
	   (cgt me::dotnet)
	   (ceq me::dotnet)
	   (blt me::dotnet prefix::bstring label)
	   (bgt me::dotnet prefix::bstring label)
	   (ble me::dotnet prefix::bstring label)
	   (add me::dotnet)
	   (mul me::dotnet)
	   (rem me::dotnet)
	   (div me::dotnet)
	   (neg me::dotnet)
	   (bitand me::dotnet)
	   (bitor me::dotnet)
	   (bitxor me::dotnet)
	   (bitnot me::dotnet)
	   (bge me::dotnet prefix::bstring label)
	   (libcall me::dotnet typer pkgc name args)
	   (open-method-virtual me::dotnet typer name args)
	   (open-method-constructor me::dotnet args)
	   (open-method-class-constructor me::dotnet)
	   (call-super-constructor me::dotnet type args)
	   (call-super me::dotnet typer pkgc name args)
	   (print-info me::dotnet msg) ))

;;
;; Get the real type name
;;
(define (get-type-name t)
   (cond
      ((symbol? t) (std-typename t))
      ((string? t) t)
      ((type? t) (type-name t))
      ((local? t) (type-name (local-type t)))
      (else (error 'call "cannot get the typename" t)) ))

;;
;; display a list comma separated
;;
(define (declare-list out::output-port l arg->name);
   (display "(" out)
   (let walk ( (p "") (l l) )
      (if (null? l)
	  (display ")" out)
	  (begin (display p out)
		 (display (arg->name (car l)) out)
		 (walk "," (cdr l)) ))))

;;
;; Comments
;;
(define (comment me::dotnet tag::bstring obj);
   (fprint (dotnet-out me) "// " tag " " obj) )

;;
;; Name Space aka Package
;;
(define (open-namespace me::dotnet);
   (let ( (name (dotnet-qname me)) )
      (unless (=fx (string-length (suffix name)) 0)
	 (let ( (out (dotnet-out me)) )
	    (fprint out ".namespace '" (prefix name) "' {") ))))

(define (close-namespace me::dotnet);
   (let ( (name (dotnet-qname me)) )
      (unless (=fx (string-length (suffix name)) 0)
	 (let ( (out (dotnet-out me)) )
	    (fprint out "}") ))))


;;
;; Class declaration
;;
(define (open-class me::dotnet name::bstring super::bstring public?);
   (define (unquote name)
      (if (char=? (string-ref name 0) #\')
	  (substring name 1 (-fx (string-length name) 1))
	  name ))
   (define (unqualified name)
      (let ( (suf (suffix name)) )
	 (if (=fx (string-length suf) 0) name suf) ))
   (let ( (out (dotnet-out me)) )
      (fprint out ".class " (if public? "public" "private") " auto ansi '"
	      (unqualified (unquote name)) "' extends " super " {" )))

(define (close-class me::dotnet);
   (fprint (dotnet-out me) "}") )

;;
;; Field declaration
;;
(define (declare-field me::dotnet field::slot);
   (fprint (dotnet-out me) ".field public " (type-name (get-field-type field))
	   " " (slot-name field) ))

(define (declare-global me::dotnet var::global);
   (fprint (dotnet-out me) ".field "
	   (if (eq? (global-import var) 'export) "public" "private")
	   " static "
	   (type-name (global-type var))
	   " "
	   (global-simplename var) ))


;;
;; Method declaration
;;
(define (open-method me::dotnet modifiers typer name args);
   (let ( (out (dotnet-out me)) )
      (display ".method " out)
      (for-each (lambda (m) (display m out) (display " " out)) modifiers)
      (display (get-type-name typer) out)
      (display " " out)
      (display name out)
      (declare-list out args get-type-name)
      (fprint out (if *purify* " cil managed {" " cil unmanaged {")) ))

(define (open-method-virtual me::dotnet typer name args);
   (open-method me '(public virtual hidebysig instance)
		typer name args ))

(define (open-method-constructor me::dotnet args);
   (open-method me '(public hidebysig specialname rtspecialname instance)
		'void ".ctor" args ))

(define (open-method-class-constructor me::dotnet);
  (open-method me '(public static hidebysig specialname rtspecialname)
	       'void ".cctor" '() ))

(define (open-global-method me::dotnet var::global);
   (open-method me
		(if (eq? (global-import var) 'export)
		    '(public static)
		    '(private static) )
		(global-type var)
		(global-simplename var)
		(sfun-args (global-value var)) ))

(define (close-method me::dotnet);
   (fprint (dotnet-out me) "}") )

;;
;; Entry point declaration
;;
(define (declare-entrypoint me::dotnet);
   (fprint (dotnet-out me) "\t.entrypoint") )

;;
;; Maxstack declaration
;;
(define (declare-maxstack me::dotnet n);
   (fprint (dotnet-out me) "\t.maxstack\t" n) )

;;
;; Local variables declaration
;;
(define (declare-locals me::dotnet l);
   (unless (null? l)
      (let ( (out (dotnet-out me)) )
	 (display "\t.locals\t" out)
	 (declare-list out l (lambda (r) (type-name (rtl_reg-type r))))
	 (fprint out "") )))

;;
;; Declare tail annotation
;;
(define (declare-tail me::dotnet);
   (fprint (dotnet-out me) "\ttail.") )

;;
;; Constants
;;
(define (push-num me::dotnet n type);
   (let ( (out (dotnet-out me)) )
      (if (bignum? n)
	  (begin
	     (display "\tldstr\t\"" out)
	     (strout out (bignum->string n))
	     (fprint out "\"")
	     (fprint out "\tcall \tclass 'bigloo.bignum' 'bigloo.foreign'::'bgl_string_to_bignum'(class System.String)"))
	  (begin
	     (case type
		((float)
		 (fprint out "\tldc.r4\t" n))
		((double)
		 (cond
		    ((nanfl? n)
		     (fprint out "\tldc.r8  float64(0xFFF8000000000000)"))
		    ((and (infinitefl? n)
			  (<fl n 0.0))
		     (fprint out "\tldc.r8  float64(0xFFF0000000000000)"))
		    ((infinitefl? n)
		     (fprint out "\tldc.r8  float64(0x7FF0000000000000)"))
		    (else
		     (fprint out "\tldc.r8\t" n))))
		((elong uelong)
		 ;; CARE check if conv.i8 better
		 (fprint out "\tldc.i8\t" (elong->string n))) 
		((llong ullong)
		 ;; CARE check if conv.i8 better
		 (fprint out "\tldc.i8\t" (llong->string n)))
		(else
		 ;;(bool byte char short int long)
		 (display "\tldc.i4" out)
		 (case n
		    ((-1)
		     (fprint out ".m1") )
		    ((0 1 2 3 4 5 6 7 8)
		     (fprint out "." n) )
		    (else
		     (if (and (>fx n 8) (<fx n #x100)) (display ".s" out))
		     (fprint out "\t" n) ))))))))

(define (push-int me::dotnet n);
   (push-num me n 'int) )

(define (push-string me::dotnet s::bstring);
   (let ( (out (dotnet-out me)) )
      (display "\tldstr\t\"" out)
      (strout out s)
      (fprint out "\"") ))

(define (strout out s)
   (define (w n) (write-char (integer->char n) out))
   (let ( (n (string-length s)) )
      (let walk ( (i 0) )
	 (if (not (= i n))
	     (let ( (c (string-ref s i)) )
		(let ( (cn (char->integer c)) )
		   (cond
		      ((char=? c #\")
		       (display "\\\"" out) )
		      ((char=? c #\\)
		       (display "\\\\" out) )
		      ((= cn 0)
		       (w #xC0)
		       (w #x80) )
		      ((< cn #x80)
		       (write-char c out) )
		      ((< cn #x800)
		       (w (bit-or #xC0 (bit-rsh cn 6)))
		       (w (bit-or #x80 (bit-and cn #x3F))) )
		      (else
		       (w (bit-or #xE0 (bit-rsh cn 12)))
		       (w (bit-or #x80 (bit-and (bit-rsh cn 6) #x3F)))
		       (w (bit-or #x80 (bit-and cn #x3F))) ))
		   (walk (+fx i 1)) ))))))

(define (push-constant me::dotnet x);
   (fprint (dotnet-out me)
	   "\tldsfld\tclass 'bigloo." x  "' 'bigloo." x "'::'_" x "'\n" ))

(define (push-constant2 me::dotnet c x);
   (fprint (dotnet-out me)
	   "\tldsfld\tclass 'bigloo." c "' 'bigloo." c "'::'" x "'\n" ))

;;
;; Global variables
;;
(define (load-global me::dotnet var::global);
   (fprint (dotnet-out me) "\tldsfld\t" (global-name var)) )

(define (store-global me::dotnet var::global);
   (fprint (dotnet-out me) "\tstsfld\t" (global-name var)) )

;;
;; Load/Store Parameter/Local variable
;;
(define (load-par me::dotnet n::int);
   (let ( (s (cond ((<fx n 4) ".") ((<fx n #x100) ".s\t") (else "\t"))) )
      (fprint (dotnet-out me) "\tldarg" s n) ))

(define (store-par me::dotnet n::int);
   (let ( (s (cond ((<fx n #x100) ".s\t") (else "\t"))) )
      (fprint (dotnet-out me) "\tstarg" s n) ))

(define (load-var me::dotnet n::int);
   (let ( (s (cond ((<fx n 4) ".") ((<fx n #x100) ".s\t") (else "\t"))) )
      (fprint (dotnet-out me) "\tldloc" s n) ))

(define (store-var me::dotnet n::int);
   (let ( (s (cond ((<fx n 4) ".") ((<fx n #x100) ".s\t") (else "\t"))) )
      (fprint (dotnet-out me) "\tstloc" s n) ))

;;
;; Method call
;;
(define (_call me::dotnet callop typer name args);
   (let ( (out (dotnet-out me)) )
      (display callop out)
      (display (get-type-name typer) out)
      (display " " out)
      (display name out)
      (declare-list out args get-type-name)
      (fprint out "") ))

(define (call-super me::dotnet typer pkgc name args);
   (_call me "\tcall instance\t" typer (string-append pkgc "::'" name "'")
	  args ))

(define (call-super-constructor me::dotnet type args);
   (_call me "\tcall instance\t" 'void (string-append type "::.ctor") args) )

(define (libcall me::dotnet typer pkgc name args);
   (let ( (out (dotnet-out me)) )
      (display "\tcall\t" out)
      (display (get-type-name typer) out)
      (display " '" out)
      (display pkgc out)
      (display "'::'" out)
      (display name out)
      (display "'" out)
      (declare-list out args get-type-name)
      (fprint out "") ))

(define (callvirt me::dotnet typer name args);
   (_call me "\tcallvirt instance\t" typer name args) )

(define (call me::dotnet name modifiers args);
   (let ( (out (dotnet-out me)) )
      (display "\tcall\t" out)
      (display modifiers out)
      (display " " out)
      (display name out)
      (declare-list out args get-type-name)
      (fprint out "") ))

(define (call-global me::dotnet var::global);
   (define (callargs fun)
      (if (cfun? fun)
	  (if (memq 'static (cfun-method fun))
	      (cfun-args-type fun)
	      (cdr (cfun-args-type fun)) )
	  (sfun-args fun) ))
   (define (callop fun)
      (if (cfun? fun)
	  (let ( (modifiers (cfun-method fun)) )
	     (cond
		((memq 'static modifiers) "\tcall\t")
		((memq 'abstract modifiers) "\tcallvirt instance\t")
		((memq 'final modifiers) "\tcall instance\t")
		((memq 'native modifiers) "\tcall instance\t")
		(else "\tcallvirt instance\t") ))
	  "\tcall\t" ))
   (let ( (fun (variable-value var)) (out (dotnet-out me)) )
      (display (callop fun) out)
      (display (global-name var) out)
      (declare-list out (callargs fun) get-type-name)
      (fprint out "") ))

(define (return me::dotnet);
   (fprint (dotnet-out me) "\tret") )

;;
;; Exception
;;
(define (throw me::dotnet);
   (fprint (dotnet-out me) "\tthrow") )

(define (rethrow me::dotnet);
   (fprint (dotnet-out me) "\trethrow") )

(define (open-try me::dotnet)
   (fprint (dotnet-out me) ".try {") )

(define (open-catch me::dotnet type cont);
   (let ( (out (dotnet-out me)) )
      (fprint out "\tleave\t" cont)
      (fprint out " } catch " (get-type-name type) " {") ))

(define (leave me::dotnet cont);
   (fprint (dotnet-out me) "\tleave\t" cont) )
   
(define (close-try me::dotnet cont);
   (let ( (out (dotnet-out me)) )
      (fprint out "\tleave\t" cont)
      (fprint out " }")
      (display cont out)
      (display ":" out) ))

;;
;; Instructions on vectors
;;
(define (newarray me::dotnet type);
   (fprint (dotnet-out me) "\tnewarr\t" (get-type-name type)) )

(define (extension type)
   (if (type? type)
       (case (type-id type)
	  ((bool byte ubyte char) "i1")
	  ((ucs2 short ushort) "i2")
	  ((int long ulong) "i4")
	  ((float) "r4")
	  ((double) "r8")
	  (else "ref") )
       "ref" ))
   
(define (load-vector me::dotnet type);
   (fprint (dotnet-out me) "\tldelem." (extension type)) )

(define (store-vector me::dotnet type);
   (fprint (dotnet-out me) "\tstelem." (extension type)) )
   
(define (load-vector-length me::dotnet);
   (fprint (dotnet-out me) "\tldlen")
   (fprint (dotnet-out me) "\tconv.i4") )

;;
;; Instructions on objects
;;
(define (newobj me::dotnet typename l);
   (define (unquote name)
      (if (char=? (string-ref name 0) #\')
	  (substring name 1 (-fx (string-length name) 1))
	  name ))
   (define (unclass name)
      (if (substring=? name "class " 6)
	 (substring name 6 (string-length name))
	 name ))
   (let ( (out (dotnet-out me)) )
      (display "\tnewobj\tinstance void '" out)
      (display (unquote (unclass typename)) out)
      (display "'::.ctor" out)
      (declare-list out l get-type-name)
      (fprint out "") ))

(define (load-field me::dotnet type class field);
   (fprint (dotnet-out me) "\tldfld\t" (get-type-name type) " "
	   (get-type-name class) "::'" field "'" ))

(define (store-field me::dotnet type class field);
   (fprint (dotnet-out me) "\tstfld\t" (get-type-name type) " "
	   (get-type-name class) "::'" field "'" ))

(define (castclass me::dotnet type);
   (fprint (dotnet-out me) "\tcastclass\t" (type-name type)) )

(define (isinst me::dotnet type::type);
   (fprint (dotnet-out me) "\tisinst\t" (type-name type)) )

;;
;; Branch instructions
;;
(define (label me::dotnet prefix lab);
   (let ( (out (dotnet-out me)) )
      (display prefix out)
      (display lab out)
      (display ":" out) ))

(define (br me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tbr\t" prefix label) )

(define (brfalse me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tbrfalse\t" prefix label) )

(define (brtrue me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tbrtrue\t" prefix label) )

(define (beq me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tbeq\t" prefix label) )

(define (bne me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tbne.un\t" prefix label) )

(define (blt me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tblt\t" prefix label) )

(define (bgt me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tbgt\t" prefix label) )

(define (bge me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tbge\t" prefix label) )

(define (ble me::dotnet prefix::bstring label);
   (fprint (dotnet-out me) "\tble\t" prefix label) )

(define (switch me::dotnet labs);
   (let ( (out (dotnet-out me)) )
      (display "\tswitch\t" out)
      (if *dotnet-pnet-workaround-switch*
	  (declare-list out labs (lambda (x) (string-append "BUGPNET" x)))
	  (declare-list out labs (lambda (x) x)) )
      (fprint out "")
      (when *dotnet-pnet-workaround-switch*
	 (let ( (s (gensym "BUGPNET")) )
	    (fprint out s ":\tbr\t" s "_def")
	    (let ( (done '()) )
	       (for-each (lambda (x)
			    (unless (member x done)
			       (set! done (cons x done))
			       (fprint out "BUGPNET" x ":\tbr\t" x) ))
			 labs ))
	    (fprint out s "_def:") ))))

;;
;; Arithmetics
;;
(define (add me::dotnet);
   (fprint (dotnet-out me) "\tadd") )

(define (sub me::dotnet);
   (fprint (dotnet-out me) "\tsub") )

(define (mul me::dotnet);
   (fprint (dotnet-out me) "\tmul") )

(define (div me::dotnet);
   (fprint (dotnet-out me) "\tdiv") )

(define (neg me::dotnet);
   (fprint (dotnet-out me) "\tneg") )

(define (rem me::dotnet);
   (fprint (dotnet-out me) "\trem") )

;;
;; Bit instructions
;;
(define (bitand me::dotnet);
   (fprint (dotnet-out me) "\tand") )

(define (bitor me::dotnet);
   (fprint (dotnet-out me) "\tor") )

(define (bitxor me::dotnet);
   (fprint (dotnet-out me) "\txor") )

(define (bitnot me::dotnet);
   (fprint (dotnet-out me) "\tnot") )


;;
;; Simple instructions
;;
(define (ldnull me::dotnet)
   (fprint (dotnet-out me) "\tldnull") )

(define (pop me::dotnet);
   (fprint (dotnet-out me) "\tpop") )

(define (dup me::dotnet);
   (fprint (dotnet-out me) "\tdup") )

(define (ceq me::dotnet);
   (fprint (dotnet-out me) "\tceq") )

(define (clt me::dotnet);
   (fprint (dotnet-out me) "\tclt") )

(define (cgt me::dotnet);
   (fprint (dotnet-out me) "\tcgt") )

;;
;; Debug
;;
(define (print-info me::dotnet msg)
   '(let ( (out (dotnet-out me)) )
      (push-string me msg)
      (fprint out "\tcall\t void bigloo.foreign::print(class System.String)")))
   
