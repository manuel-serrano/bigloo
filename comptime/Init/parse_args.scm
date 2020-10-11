;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Init/parse_args.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Aug  7 11:47:46 1994                          */
;*    Last change :  Mon Mar 16 06:02:14 2020 (serrano)                */
;*    Copyright   :  1992-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The command line arguments parsing                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module init_parse-args
   (include "Tools/trace.sch"
	    "Engine/pass.sch"
	    "Init/pass-args-parse.sch")
   (export  (parse-args args))
   (import  engine_configure
	    engine_param
	    init_main
	    init_extend
	    init_setrc
	    init_lib-dir
	    module_module
	    module_alibrary
	    module_eval
	    write_version
	    tools_trace
	    tools_speek
	    tools_license
	    tools_error
	    read_access
	    read_jvm
	    cc_ld
	    jas_as
	    jas_profile
	    jas_peep))

;*---------------------------------------------------------------------*/
;*    Global parse args parameters ...                                 */
;*---------------------------------------------------------------------*/
(define *extended-done?* #f)
(define *libraries* '())
(define *trace-level* 0)
(define *user-load-path* (list "." (bigloo-config 'library-directory)))
(define *force-saw* #unspecified)
(define *error-localization-opt* #unspecified)
(define *user-heap-name* #f)

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   (set! *bigloo-cmd-name* (car args))
   (set! *bigloo-args* args)
   (do-parse-args (cdr args))
   ;; we setup the heap name
   (when *user-heap-name* (set! *heap-name* *user-heap-name*))
   ;; saw or no saw
   (set! *saw* (or (eq? *force-saw* #t)
		   (memq *target-language* '(jvm .net))))
   (let ((pres (if *extended-done?*
		   #t
		   (let ((auto-mode (let loop ((sfiles *src-files*))
				       (if (null? sfiles)
					   #f
					   (let ((cell (and
							(string? (car sfiles))
							(assoc
							   (suffix (car sfiles))
							   *auto-mode*))))
					      (if (pair? cell)
						  (cdr cell)
						  (loop (cdr sfiles))))))))
		      (if auto-mode
			  (begin
			     (set! *src-files* '())
			     (do-parse-args `("-extend"
						,auto-mode
						,@(cdr args))))
			  #t)))))
      ;; we are done with the parsing, we invert all the lists
      (set! *src-files*       (reverse! *src-files*))
      (set! *o-files*         (reverse! *o-files*))
      (set! *c-files*         (reverse! *c-files*))
      (set! *load-path*       (append (reverse! *user-load-path*) *load-path*))
      (set! *bigloo-user-lib* (reverse! *bigloo-user-lib*))
      (set! *rest-args*       (reverse! *rest-args*))
      ;; we clean the optimization flags
      (unless (boolean? *optim-unroll-loop?*)
	 (set! *optim-unroll-loop?* #f))
      (when (>fx *bdb-debug* 0)
	 (set! *inlining?* #f)
	 (when (>fx *optim* 0)
	    (warning "Incompatible options" "-O/-gbdb" " disabling debug")
	    (set! *bdb-debug* 0))
	 (when (and (fixnum? *profile-mode*) (>fx *profile-mode* 0))
	    (warning "Incompatible options" "-p/-gbdb" " disabling debug")
	    (set! *bdb-debug* 0)))
      ;; we always add a jvm package name for FOREIGN thus, a heap that
      ;; is not compiled in -jvm mode can still be used later with -jvm
      ;; option (provided that the heap file does not use any java clause).
      (add-qualified-type! 'foreign *jvm-foreign-class-name*)
      ;; when compiling for the Java back-end, the call/cc mode must
      ;; always be disabled (because call/cc is only supported in the
      ;; dynamic extend of the compilation).
      (when (memq *target-language* '(jvm .net))
	 (set! *additional-heap-names*
	    (delete! "bdb" *additional-heap-names*))
	 (when (or (not (number? *compiler-debug*)) (<= *compiler-debug* 1))
	    (set! *optim-dataflow?* #t)
	    (set! *optim-reduce-beta?* #t)))
      ;; profiling mode disable inlining
      (when (and (fixnum? *profile-mode*)
		 (>fx *profile-mode* 0)
		 (not (memq *target-language* '(jvm .net))))
	 (set! *optim-loop-inlining?* #f)
	 (set! *optim-unroll-loop?* #f)
	 (set! *inlining-kfactor* (lambda (olevel) 0))
	 (when (>=fx *profile-mode* 2)
	    (set! *inlining?* #f)))
      ;; we start the trace if the level is > 0
      (when (>fx *trace-level* 0)
	 (let ((passes (trace get-pass-names)))
	    (if (memq *pass* passes)
		(start-trace *trace-level* *pass*)
		(warning "parse-args" "No trace for this pass -- " *pass*))
	    (for-each (lambda (pass)
			 (if (not (memq pass passes))
			     (warning "parse-args"
				"No trace for this pass -- "
				pass)))
	       *additional-traces*)))
      ;; when the reader is used to construct the constant, it must
      ;; be initialized very soon
      (when (eq? *init-mode* 'read)
	 (set! *early-with-modules* (cons '__reader *early-with-modules*)))
      ;; initialize the libraries
      (for-each use-library! *libraries*)
      ;; we check with back-end we are using for defining the correct
      ;; srfi ressources
      (case *target-language*
	 ((jvm)
	  (register-srfi! 'bigloo-jvm))
	 ((c native)
	  (set! *target-language* (if *saw* 'c-saw 'c))
	  (register-srfi! 'bigloo-c)))
      ;; and we are done for the arguments parsing
      pres))
 
;*---------------------------------------------------------------------*/
;*    do-parse-args ...                                                */
;*---------------------------------------------------------------------*/
(define (do-parse-args args)
   
   (define (environment-usage manual?)
      (print "Shell Variables:")
      (for-each (lambda (var)
		   (if manual?
		       (begin
			  (print "   - " (car var))
			  (print "     " (cdr var)))
		       (print "   - " (car var) ": " (cdr var))))
		(cons `("TMPDIR" . ,(format "temporary directory (default \"~a\")"
					   (os-tmp)))
		      '(("BIGLOOLIB" . "libraries' directory")
			("BIGLOOHEAP" . "the initial heap size in megabytes (4 MB by default)")
			("BIGLOOSTACKDEPTH" . "the error stack depth printing")
			("BIGLOOLIVEPROCESS" . "the maximum number of Bigloo live processes")
			("BIGLOOTRACE" . "list of active traces"))))
      (newline)
      (print "Runtime Command file:")
      (print "   - ~/.bigloorc"))
   (define (usage args-parse-usage level manual?)
      (version)
      (print "usage: bigloo [options] [name.suf]")
      (newline)
      (args-parse-usage #f)
      (newline)
      (environment-usage manual?)
      (newline)
      (newline)
      (print "------------")
      (print " * : only available in developing mode")
      (print " . : option enabled from -O3 mode")
      (newline)
      (if (> level 1)
	  (begin
	     (newline)
	     (print "Bigloo Control Variables:")
	     (bigloo-variables-usage manual?)))
      (if *bigloo-licensing?*
	  (begin
	     (newline)
	     (newline)
	     (print (bigloo-license))))
      (compiler-exit 0))

   (bigloo-warning-set! 2)
   
   (pass-args-parse (lib-dir jvm default) default args
       
;*--- misc ------------------------------------------------------------*/
      (section "Misc")
      ;; preliminary test
      (("-" (help "Read source code on current input channel"))
       (set! *src-files* (cons 'stdin *src-files*)))
      ;; help
      (("?")
       (usage args-parse-usage 1 #f))
      ((("-help" "--help") (help "This help message"))
       (usage args-parse-usage 1 #f))
      (("-help2" (help "The exhaustive help message"))
       (usage args-parse-usage 2 #f))
      (("-help-manual" (help "The help message formatted for the manual"))
       (usage args-parse-usage 2 #t))
      ;; output name
      (("-o" ?file (help "Name the output FILE"))
       (set! *dest* file))
      ;; output to current output port
      (("--to-stdout" (help "Write C code on current output channel"))
       (set! *verbose* -1)
       (set! *dest* '--to-stdout))
      ;; stop after .o production
      (("-c" (help "Suppress linking and produce a .o file"))
       (set! *pass* 'cc))
      ;; generates a shared library
      (("-y" (help "Generate a shared library"))
       (set! *pass* 'so))
      ;; suffixes
      (("-suffix" ?suffix (help "Recognize suffix as Scheme source"))
       (set! *src-suffix* (cons suffix *src-suffix*)))
      ;; access file name
      (("-afile" ?file (help "Name of the access file"))
       (set! *access-files* (append *access-files* (list file))))
      ;; one access
      (("-access" ?module ?file (help "Set access between module and file"))
       (module-add-access! (string->symbol module) (list file) "."))
      ;; package access file name
      (("-jfile" ?file (help "Name of the Jvm package file"))
       (set! *qualified-type-file* file))
      ;; one type addition
      (("-jadd" ?module ?qtype (help "Set JVM qualifed type name for module"))
       (add-qualified-type! (string->symbol module) qtype))
      ;; main function
      (("-main" ?fun (help "Set the main function"))
       (set! *main* (string->symbol fun)))
      ;; with modules
      (("-with" ?module (help "Import addition module"))
       (set! *early-with-modules* (cons (string->symbol module)
					*early-with-modules*)))
      ;; multiple-include
      (("-multiple-inclusion" (help "Enables multiple inclusions of the Bigloo includes"))
       (set! *include-multiple* #t))
      ;; Bigloo libary
      (("-library" ?library (help "Compile/link with additional Bigloo library"))
       (set! *libraries* (cons (string->symbol library) *libraries*)))
      ;; srfi support
      (("-srfi" ?srfi (help "Declares srfi support"))
       (register-srfi! (string->symbol srfi)))
      (("-dload-sym" (help "Emit a Bigloo dynamic loading entry point"))
       (set! *dlopen-init* #t))
      (("-dload-init-sym" ?name (help "Emit a Bigloo dynamic loading entry point, named NAME"))
       (set! *dlopen-init* name))
      (("-dload-init-gc" (help "For GC initialization for dynamic code"))
       (set! *dlopen-init-gc* #t))
      (("-heapsize" ?size (help "Set the initial heap size value (in megabyte)"))
       (set! *user-heap-size* (string->integer size)))
      
;*--- Configuration and version ---------------------------------------*/
      (section "Configuration and path")
      ;; version
      (("-version" (help "The current release"))
       (short-version)
       (compiler-exit 0))
      ;; revision
      (("-revision" (help "The current release (short format)"))
       (revision)
       (compiler-exit 0))
      ;; query
      (("-query" (help "Dump the current configuration"))
       (query))
      ;; -q
      (("-q" (help "Do not load any rc file"))
       'nothing-to-do)
      ;; -eval
      (("-eval" ?string (help "Evaluate STRING before compiling"))
       (let ((port (open-input-string string)))
	  (let laap ((exp (read port)))
	     (if (eof-object? exp)
		 'done
		 (begin
		    (eval exp)
		    (laap (read port)))))))
      ;; -load
      (("-load" ?file (help "Load FILE before compiling"))
       (unless (load-init file)
	  (error "-load" "Cannot find file" file)))
      ;; load path
      (("-I" ?dir (help "Add DIR to the load path"))
       (set! *user-load-path* (cons dir *user-load-path*)))
      ;; library path
      (pass lib-dir
       (("-lib-dir" ?dir (help "Set lib-path to DIR"))
	(process-lib-dir-parameter dir)))
      (("-L" ?name (help "Set additional library path"))
       (set! *lib-dir* (cons name *lib-dir*))
       (bigloo-library-path-set! (cons name (bigloo-library-path))))
      (("-lib-version" ?version (help "Set the Bigloo library version"))
       (library-translation-table-add! 'bigloo "bigloo" version))
      (("-libgc-version" ?version (help "Set the Bigloo GC library version"))
       (library-translation-table-add! 'bigloogc "bigloogc" version))
      (("-libgc" ?gc (help "Use the given GC library"))
       (set! *gc-lib* (string->symbol gc))
       (set! *gc-custom?* #f))
      
;*--- back-end --------------------------------------------------------*/
      (section "Back-end")
      ;; native code generation
      (("-native" (help "Compile module to native object file (via C)"))
       (set! *obj-suffix* (list *c-object-file-extension*))
       (set! *target-language* 'native))
      ;; jvm code generation
      (pass jvm
	    (("-jvm" (help "Compile module to JVM .class files"))
	     (set! *heap-name* *heap-jvm-name*)
	     (set! *obj-suffix* '("class"))
	     (set! *target-language* 'jvm)))
      ;; Bigloo Assembly code generation
      (("-saw" (help "Cut the AST in the saw-mill"))
       (set! *force-saw* #t))
      (("-no-saw" (help "Disable saw back-ends"))
       (set! *force-saw* #f))
      ;; interperter
      (("-i" (help "Interprete module"))
       (set! *interpreter* #t))
      
;*--- Dialect options -------------------------------------------------*/
      (section "Dialect")
      ;; snow
      (("-snow" (help "Compiles a snow source code"))
       (set! *src-suffix* (cons "snow" *src-suffix*))
       (set! the-remaining-args (cons* "-extend" "snow"
				       "-library" "snow"
				       the-remaining-args)))
      ;; scmpkg
      ((("-scmpkg" "-spi") (help "Compiles a ScmPkg source code"))
       (set! *src-suffix* (cons "spi" *src-suffix*))
       (set! the-remaining-args (cons* "-extend" "pkgcomp"
				       "-library" "pkgcomp"
				       the-remaining-args)))
      ;; nil
      (("-nil" (help "Evaluate '() as #f in `if' expression"))
       (set! *nil* #f))
      (("-call/cc" (help "Enable call/cc function"))
       ;; -g3 and -call/cc are incompatible
       (if (>fx *compiler-debug* 1)
	   (set! *compiler-debug* 1))
       (set! *call/cc?* #t))
      (("-hygien" (help "Obsolete (R5rs macros are always supported)"))
       #unspecified)
      (("-fidentifier-syntax" ?syntax (help "Identifiers syntax \"r5rs\" (default) or \"bigloo\""))
       (bigloo-identifier-syntax-set! (string->symbol syntax)))
      ;; reflection
      (("-fno-reflection" (help "Deprecated"))
       #unspecified)
      (("+fno-reflection" (help "Deprecated"))
       #unspecified)
      ;; object-nil
      (("-fclass-nil" (help "Deprecated"))
       #unspecified)
      (("-fno-class-nil" (help "Deprecated"))
       #unspecified)
      ;; arithmetic
      (("-farithmetic" (help "Suppress genericity of arithmetic operators"))
       (set! *arithmetic-genericity* #f))
      ;; arithmetic-overflow
      (("-farithmetic-overflow" (help "Suppress arithmetic overflow checks"))
       (set! *arithmetic-overflow* #f))
      (("-fno-arithmetic-overflow" (help "Enable arithmetic overflow checks"))
       (set! *arithmetic-overflow* #t))
      ;; case sensitivity
      (("-fcase-sensitive" (help "Case sensitive reader (default)"))
       (bigloo-case-sensitivity-set! 'sensitive))
      (("-fcase-insensitive" (help "Case insensitive reader (downcase symbols)"))
       (bigloo-case-sensitivity-set! 'downcase))
      ;; type refefinition
      (("-fallow-type-redefinition" (help "allow type redifinition"))
       (set! *allow-type-redefinition* #t))
;*--- Optimization ----------------------------------------------------*/
      (section "Optimization")
      ;; benchmarking
      (("-Obench" (help "Benchmarking mode"))
       (do-parse-args `("-O6" "-unsafe"
			     "-copt" ,*cflags-optim*
			     "-static-all-bigloo"))) 
      ;; optimization
      (("-O?opt" (help "-O[0..6]" "Optimization modes"))
       (parse-optim-args opt))
      ;; fix arithmetic tagging
      (("-ftagged-fxop" (help "Enable tagged fix-ops optimization"))
       (set! *optim-tagged-fxop?* #t))
      (("-fno-tagged-fxop" (help "Disable tagged fix-ops optimization"))
       (set! *optim-tagged-fxop?* #f))
      ;; cfa optimizations
      (("-fcfa" (help "Enable CFA"))
       (set! *optim-cfa?* #t))
      (("-fno-cfa" (help "Disable CFA"))
       (set! *optim-cfa?* #f))
      (("-fcfa-arithmetic" (help "Enable arithmetic spec. (see -farithmetic-overflow)"))
       (set! *arithmetic-overflow* #f)
       (set! *optim-cfa-fixnum-arithmetic?* #t)
       (set! *optim-cfa-flonum-arithmetic?* #t))
      (("-fno-cfa-arithmetic" (help "Disable arithmetic spec."))
       (set! *optim-cfa-fixnum-arithmetic?* #f)
       (set! *optim-cfa-flonum-arithmetic?* #f))
      (("-fcfa-arithmetic-fixnum" (help "Enable fixnum arithmetic spec."))
       (set! *arithmetic-overflow* #f)
       (set! *optim-cfa-fixnum-arithmetic?* #t))
      (("-fno-cfa-arithmetic-fixnum" (help "Disable fixnum arithmetic spec."))
       (set! *optim-cfa-fixnum-arithmetic?* #f))
      (("-fcfa-arithmetic-flonum" (help "Enable flonum arithmetic spec. (enabled from -O2)"))
       (set! *optim-cfa-flonum-arithmetic?* #t))
      (("-fno-cfa-arithmetic-flonum" (help "Disable flonum arithmetic spec. "))
       (set! *optim-cfa-flonum-arithmetic?* #f))
      (("-fcfa-tracking" (help "Enable CFA tracking (enabled from -O2)"))
       (set! *optim-cfa-free-var-tracking?* #t))
      (("-fno-cfa-tracking" (help "Disable CFA tracking"))
       (set! *optim-cfa-free-var-tracking?* #f))
      (("-fcfa-pair" (help "Enable CFA pairs approximations"))
       (set! *optim-cfa-pair?* #t))
      (("-fno-cfa-pair" (help "Disable CFA pairs approximations"))
       (set! *optim-cfa-pair?* #f))
      (("-fcfa-unbox-closure-args" (help "Enable CFA unboxed closure args (enabled from -O2)"))
       (set! *optim-cfa-unbox-closure-args* #t))
      (("-fno-cfa-unbox-closure-args" (help "Disable CFA unboxed closure args"))
       (set! *optim-cfa-unbox-closure-args* #f))
      (("-fno-cfa-local-function" (help "Disable CFA local function tracking"))
       (set! *optim-cfa-force-loose-local-function?* #t))
      ;; loop unrolling
      (("-funroll-loop" (help "Enable loop unrolling (enabled from -O3)"))
       (set! *optim-unroll-loop?* #t))
      (("-fno-unroll-loop" (help "Disable loop unrolling"))
       (set! *optim-unroll-loop?* #f))
      (("-fno-loop-inlining" (help "Disable loop inlining"))
       (set! *optim-loop-inlining?* #f)) 
      (("-floop-inlining" (help "Enable loop inlining (default)"))
       (set! *optim-loop-inlining?* #t)) 
      (("-fno-inlining" (help "Disable inline optimization"))
       (set! *inlining?* #f))
      (("-fno-user-inlining" (help "Disable user inline optimization"))
       (set! *user-inlining?* #f))
      ;; isa inlining
      (("-fisa" (help "Inline isa? type predicate"))
       (set! *optim-isa?* #t))
      (("-fno-isa" (help "Inline isa? type predicate"))
       (set! *optim-isa?* #f))
      ;; data flow optimization
      (("-fbeta-reduce" (help "Enable simple beta reduction (enabled from -O2)"))
       (set! *optim-reduce-beta?* #t))
      (("-fno-beta-reduce" (help "Disable simple beta reduction"))
       (set! *optim-reduce-beta?* #f))
      (("-fdataflow" (help "Enable dataflow optimizations (enabled from -O)"))
       (set! *optim-dataflow?* #t))
      (("-fno-dataflow" (help "Disable dataflow optimizations"))
       (set! *optim-dataflow?* #f))
      (("-fdataflow-for-errors" (help "Enable dataflow optimizations for improviing type error messages"))
       (set! *optim-dataflow-for-errors?* #t))
      (("-fno-dataflow-for-errors" (help "Disable dataflow optimizations for improviing type error messages"))
       (set! *optim-dataflow-for-errors?* #f))
      (("-fdataflow-types" (help "Enable type dataflow optimizations (enabled from -O2)"))
       (set! *optim-dataflow-types?* #t))
      (("-fno-dataflow-types" (help "Disable type dataflow optimizations"))
       (set! *optim-dataflow-types?* #f))
      (("-finitflow" (help "Enable init flow"))
       (set! *optim-initflow?* #t))
      (("-fno-initflow" (help "Disable init flow"))
       (set! *optim-initflow?* #f))
      (("-fsync-failsafe" (help "Enable failsafe synchronize optimization"))
       (set! *optim-sync-failsafe?* #t))
      (("-fno-sync-failsafe" (help "Disable failsafe synchronize optimization"))
       (set! *optim-sync-failsafe?* #f))
      ;; O macro
      (("-fO-macro" (help "Enable Optimization macro (default)"))
       (set! *optim-O-macro?* #t))
      (("-fno-O-macro" (help "Disable Optimization macro"))
       (set! *optim-O-macro?* #f))
      ;; tailc
      (("-fglobal-tailc" (help "Enable global tail-call optimization"))
       (set! *global-tail-call?* #t))
      (("-fno-global-tailc" (help "Disable global tail-call optimization"))
       (set! *global-tail-call?* #f))
      ;; return
      (("-freturn" (help "Enable set-exit replacement with return"))
       (set! *optim-return?* #t))
      (("-fno-return" (help "Disable set-exit replacement"))
       (set! *optim-return?* #f))
      (("-freturn-goto" (help "Enable local set-exit replacement with return"))
       (set! *optim-return?* #t)
       (set! *optim-return-goto?* #t))
      (("-fno-return-goto" (help "Disable local set-exit replacement"))
       (set! *optim-return-goto?* #f))
      (("-fstackable" (help "Enable stackable optimization"))
       (set! *optim-stackable?* #t))
      (("-fno-stackable" (help "Disable stackable optimization"))
       (set! *optim-stackable?* #f))
      (("-funcell" (help "Enable cell removal"))
       (set! *optim-uncell?* #t))
      (("-fno-uncell" (help "Disable cell removal"))
       (set! *optim-uncell?* #f))
      ;; saw register allocation
      (("-fsaw-realloc" (help "Enable saw register re-allocation"))
       (set! *saw-register-reallocation?* #t))
      (("-fsaw-regalloc" (help "Enable saw register allocation"))
       (set! *saw-register-allocation?* #t))
      (("-fno-saw-regalloc" (help "Disable saw register allocation"))
       (set! *saw-register-allocation?* #f))
      (("-fsaw-bbv" (help "Enable saw basic-blocks versionning"))
       (set! *saw-bbv?* #t))
      (("-fno-saw-bbv" (help "Disable saw basic-blocks versionning"))
       (set! *saw-bbv?* #f))
      (("-fsaw-regalloc-msize" ?size (help "Set the register allocation body size limit"))
       (set! *saw-register-allocation?* #t)
       (set! *saw-register-allocation-max-size* (string->integer size)))
      (("-fsaw-regalloc-fun" ?name (help "Allocate registers on this very function"))
       (set! *saw-register-allocation?* #t)
       (set! *saw-register-allocation-functions*
	     (cons (string->symbol name) *saw-register-allocation-functions*)))
      (("-fno-saw-regalloc-fun" ?name (help "Don't allocate registers on this very function"))
       (set! *saw-register-allocation?* #t)
       (set! *saw-no-register-allocation-functions*
	     (cons (string->symbol name) *saw-no-register-allocation-functions*)))
      (("-fsaw-regalloc-onexpr" (help "Allocate registers on expressions"))
       (set! *saw-register-allocation-onexpression?* #t))
      (("-fno-saw-regalloc-onexpr" (help "Don't allocate registers on expressions"))
       (set! *saw-register-allocation-onexpression?* #f))
      (("-fsaw-spill" (help "Enable saw spill optimization"))
       (set! *saw-spill* #t))
      
;*--- Safety ----------------------------------------------------------*/
      (section "Safety")
      ;; unsafe
      (("-unsafe?opt" (help "-unsafe[atrsvleh]" "Don't check [type/arity/range/struct/version/library/eval/heap]"))
       (parse-safe/unsafe-args opt #t))
      (("-safe?opt" (help "-safe[atrsvle]" "Enforce check [type/arity/range/struct/version/library/eval]"))
       (parse-safe/unsafe-args opt #f))
      
;*--- Debug -----------------------------------------------------------*/
      (section "Debug")
      ;; -g
      (("-glines" (help "Emit # line directives"))
       (set! *c-debug-lines-info* #t))
      (("-gbdb-no-line" (help "Don't emit # line directives"))
       (set! *bdb-debug-no-line-directives?* #t))
      (("-gbdb?opt" (help "-gbdb[23]" "Compile with bdb debug informations"))
       (parse-bdb-args opt))
      (("-gself" (help "Enables self compiler debug options"))
       (bigloo-debug-set! 1)
       (set! *compiler-stack-debug?* #t)
       (set! *compiler-sharing-debug?* #t)
       (set! *compiler-type-debug?* #t))
      (("-gmodule" (help "Debug module initialization"))
       (set! *debug-module* 1)
       (bigloo-debug-module-set! 1))
      (("-gerror-localization" (help "Localize error calls in the source code"))
       (set! *error-localization-opt* #t)
       (set! *error-localization* #t))
      (("-gno-error-localization" (help "Don't localize error calls in the source code"))
       (set! *error-localization-opt* #f)
       (set! *error-localization* #f))
      (("-gjvm" (help "Annote JVM classes for debug"))
       (set! *jvm-debug* #t))
      (("-gtrace?opt" (help "-gtrace[12all]" "Instrument for stack tracing"))
       (set! *compiler-debug-trace*
	  (cond
	     ((=fx (string-length opt) 0) 1)
	     ((string=? opt "all") 10000000)
	     (else (string->integer opt)))))
      (("-g?opt" (help "-g[234]" "Produce Bigloo debug informations"))
       (parse-debug-args opt))
      (("-cg" (help "Compile C files with debug option"))
       (set! *rm-tmp-files* #f)
       (set! *c-debug* #t)
       (set! *strip* #f))
      (("-export-all" (help "Eval export-all all routines"))
       (set! *all-eval?* #t))
      (("-export-exports" (help "Eval export-exports all routines"))
       (set! *all-export-eval?* #t))
      (("-export-mutable" (help "Enables Eval redefinition of all \"::obj\" routines"))
       (set! *all-export-mutable?* #t))
      
;*--- Profiling -------------------------------------------------------*/
      (section "Profiling")
      ;; -pg
      (("-p" (help "-p[2g]" "Compile for cpu profiling"))
       (if (or (not (number? *profile-mode*))
	       (<fx *profile-mode* 1))
	   (begin
	      (set! *strip* #f)
	      (set! *profile-library* #t)
	      (set! *cc-options* (append *cc-options* (list *cflags-prof*)))
	      (set! *jas-profile-mode* 1)
	      (set! *profile-mode* 1)
	      (set! *jas-profile-mode* 1))))
      (("-p2")
       (if (or (not (number? *profile-mode*))
	       (<fx *profile-mode* 2))
	   (begin
	      (set! *strip* #f)
	      (set! *profile-library* #t)
	      (set! *cc-options* (append *cc-options* (list *cflags-prof*)))
	      (set! *profile-mode* 2)
	      (set! *jas-profile-mode* 2)
	      (do-parse-args '("-static-bigloo")))))
      (("-pg")
       (set! *strip* #f)
       (set! *profile-library* #t)
       (set! *cc-options* (append *cc-options* (list *cflags-prof*))))
      (("-pmem?level" (help "-pmem[level]" "Compile for memory profiling"))
       ;;(bigloo-compiler-debug-set! 1)
       ;; (set! *compiler-debug* 1)
       (let ((l (max (string->integer level) 1)))
	  (set! *compiler-debug-trace* (*fx 5 l))
	  (set! *jas-peephole* #f)
	  (set! *bmem-profiling* #t)
	  (bigloo-profile-set! l)))
      (("-psync" (help "Profile synchronize expr (see $exitd-mutex-profile)"))
       (set! *sync-profiling* #t))
      
;*--- verbosity -------------------------------------------------------*/
      (section "Verbosity")
      ;; silence
      (("-s" (help "Be silent and inhibit all warning messages"))
       (set! *verbose* -1)
       (bigloo-warning-set! 0)
       (set! *load-verbose* #f))
      ;; verbose
      (("-v" (help "-v[23]" "Be verbose"))
       (set! *verbose* 1))
      (("-v2")
       (set! *verbose* 2))
      (("-v3")
       (set! *verbose* 3))
      (("-hello" (help "Say hello"))
       (set! *hello* #t))
      (("-no-hello" (help "Dont' say hello even in verbose mode"))
       (set! *hello* #f))
      ;; warning
      (("-w" (help "Inhibit all warning messages"))
       (bigloo-warning-set! 0))
      (("-wslots" (help "Inhibit overriden slots warning messages"))
       (set! *warning-overriden-slots* #f))
      (("-Wvariables" (help "Enable overriden variable warning messages"))
       (set! *warning-overriden-variables* #t))
      (("-Wtypes" (help "Enable type check warning messages"))
       (set! *warning-types* #t))
      (("-Wslot" (help "Enable default slot value warning messages"))
       (set! *warning-default-slot-value* #t))
      (("-Wno-slot" (help "Disable default slot value warning messages"))
       (set! *warning-default-slot-value* #f))
      (("-Wall" (help "Warn about all possible type errors"))
       (set! *warning-overriden-slots* #t)
       (set! *warning-overriden-variables* #t)
       (set! *warning-default-slot-value* #t)
       #;(set! *warning-types* #t)
       (bigloo-warning-set! 2))
      (("-Werror=type" (help "Treat type warnings as error"))
       (set! *warning-type-error* #t))
      
;*--- Compilation modes -----------------------------------------------*/
      (section "Compilation modes")
      ;; remove temporary files
      (("-rm" (help "<-/+>rm" "Don't or force removing .c or .il files"))
       (set! *rm-tmp-files* #f))
      (("+rm")
       (set! *rm-tmp-files* #t))
      ;; Extended compiler
      (("-extend" ?name (help "Extend the compiler"))
       (set! *extended-done?* #t)
       (load-extend name)
       (when (procedure? *extend-entry*)
	  (set! the-remaining-args (*extend-entry* the-remaining-args))))
      (("-extend-module" ?exp (help "Extend the module syntax"))
       (call-with-input-string exp
	  (lambda (in)
	     (let ((proc (eval (read in))))
		(cond
		   ((not (procedure? proc))
		    (error "-extend-module" "Extension not a procedure" exp))
		   ((not (correct-arity? proc 1))
		    (error "-extend-module" "Illegal extension procedure" exp))
		   (else
		    (bigloo-module-extension-handler-set! proc)))))))
      (("-fsharing" (help "Attempt to share constant data"))
       (set! *shared-cnst?* #t))
      (("-fno-sharing" (help "Do not attempt to share constant data"))
       (set! *shared-cnst?* #f))
      (("-fmco" (help "Produce an .mco file"))
       (set! *module-checksum-object?* #t))
      (("-fmco-include-path" ?dir (help "Add dir to mco C include path"))
       (set! *mco-include-path* (cons dir *mco-include-path*)))
      
;*--- Back-end compilation and link -----------------------------------*/
      (section "Native specific options")
      ;; The C compiler
      (("-cc" ?compiler (help "Specify the C compiler"))
       (set! *cc* compiler))
      ;; ISO C
      (("-stdc" (help "Generate strict ISO C code"))
       (set! *stdc* #t))
      ;; cc options
      (("-copt" ?string (help "Invoke cc with STRING"))
       (set! *cc-options* (append *cc-options* (list string))))
      (("-cheader" ?string (help "C header"))
       (set! *c-user-header* (append *c-user-header* (list string))))
      (("-cfoot" ?string (help "C foot"))
       (set! *c-user-foot* (append *c-user-foot* (list string))))
      (("-rpath" ?path (help "Add C runtime-path (rpath)"))
       (set! *cflags-rpath* (append *cflags-rpath* (list path))))
      ;; link options
      (("-ldopt" ?string (help "Invoke ld with STRING"))
       (set! *ld-options* (string-append string " " *ld-options*)))
      (("-ldpostopt" ?string (help "Invoke ld with STRING (end of arguments)"))
       (set! *ld-post-options* (cons string *ld-post-options*)))
      ;; move or -o ?
      (("--force-cc-o" (help "Force the C compiler to use -o instead of mv"))
       (set! *cc-move* #f))
      ;; link absolute or relative
      (("-ld-relative" (help "Link using -l notation for libraries (default)"))
       (set! *ld-relative* #t))
      (("-ld-absolute" (help "Link using absolute path names for libraries"))
       (set! *ld-relative* #f))
      ;; static Bigloo library
      (("-static-bigloo" (help "Link with the static bigloo library"))
       (register-srfi! 'static-bigloo)
       (set! *static-bigloo?* #t))
      ;; static all Bigloo library
      (("-static-all-bigloo"
	(help "Link with static version of all bigloo libraries"))
       (register-srfi! 'static-all-bigloo)
       (register-srfi! 'static-bigloo)
       (set! *static-bigloo?* #t)
       (set! *static-all-bigloo?* #t))
      ;; double libraries inclusions
      (("-ld-libs1"
	(help "Add once user libraries when linking"))
       (set! *double-ld-libs?* #f))
      (("-ld-libs2"
	(help "Add twice user libraries when linking (default)"))
       (set! *double-ld-libs?* #t))
      ;; C library linking
      (("-l?library" (help "Link with host library"))
       (set! *bigloo-user-lib* (cons (string-append "-l" library)
				     *bigloo-user-lib*)))
      ;; main generation
      (("-auto-link-main" (help "Enable main generation when needed for linking"))
       (set! *auto-link-main* #t))
      (("-no-auto-link-main" (help "Disable main generation"))
       (set! *auto-link-main* #f))
      (("--force-gc-roots" (help "Register global variables as GC roots"))
       (set! *gc-force-register-roots?* #t))
      
;*--- Jvm specific options --------------------------------------------*/
      (section "Jvm specific options")
      ;; jvm shell code generation
      (("-jvm-shell" ?shell (help "Shell for JVM scripts (\"sh\", \"msdos\")"))
       (cond
	  ((string=? shell "sh")
	   (set! *jvm-shell* shell))
	  ((string=? shell "msdos")
	   (set! *jvm-shell* shell))
	  (else
	   (error "parse-args" "Illegal `-jvm-shell' argument" shell))))
      ;; jvm bytecode verifier compliance
      (("-jvm-purify" (help "Produce byte code verifier compliant JVM code (default)"))
       (set! *purify* #t))
      (("-no-jvm-purify" (help "Don't care about JVM code verifier"))
       (set! *purify* #f))
      ;; main jvm class
      (("-jvm-mainclass" ?class (help "JVM main class"))
       (set! *jvm-mainclass* class))
      ;; path
      (("-jvm-classpath" ?path (help "JVM application classpath"))
       (set! *jvm-classpath* path))
      (("-jvm-bigloo-classpath" ?p (help "JVM Bigloo rts classpath"))
       (set! *jvm-bigloo-classpath* p))
      (("-jvm-path-separator" ?sep (help "Set the JVM classpath separator"))
       (set! *jvm-path-separator* sep))
      (("-jvm-directory" ?name (help "Directory where to store class files."))
       (set! *jvm-directory* name))
      (("-jvm-catch-errors" (help "Catch internal JVM errors"))
       (set! *jvm-catch* #t))
      (("-no-jvm-catch-errors" (help "Don't catch internal JVM errors"))
       (set! *jvm-catch* #f))
      (("-jvm-jarpath" ?name (help "Set the JVM classpath for the produced jar file"))
       (set! *jvm-jarpath* name))
      ;; misc
      (("-jvm-cinit-module" (help "Enable JVM class constructors to initiliaze bigloo modules"))
       (set! *jvm-cinit-module* #t))
      (("-no-jvm-cinit-module" (help "Disable JVM class constructors to initiliaze bigloo modules"))
       (set! *jvm-cinit-module* #f))
      ;; debug options
      (("-jvm-char-info" (help "Generate char info for the debugger (in addition to line info)"))
       (set! *jvm-char-info* #t))
      (("-no-jvm-char-info" (help "Do not generate char info for the debugger"))
       (set! *jvm-char-info* #f))
      ;; JVM optimization
      (("-fjvm-inlining" (help "Enable JVM back-end inlining"))
       (set! *optim-jvm-inlining* (+fx 1 *optim-jvm-inlining*)))
      (("-fjvm-constr-inlining" (help "Enable JVM back-end inlining for constructors"))
       (set! *optim-jvm-constructor-inlining*
	     (+fx 1 *optim-jvm-constructor-inlining*)))
      (("-fno-jvm-inlining" (help "Disable JVM back-end inlining"))
       (set! *optim-jvm-inlining* 0))
      (("-fno-jvm-constr-inlining" (help "Disable JVM back-end inlining for constructors"))
       (set! *optim-jvm-constructor-inlining* 0))
      (("-fjvm-peephole" (help "Enable JVM back-end peephole"))
       (set! *optim-jvm-peephole* (+fx 1 *optim-jvm-peephole*))
       (set! *jas-peephole* #t))
      (("-fno-jvm-peephole" (help "Disable JVM back-end peephole"))
       (set! *optim-jvm-peephole* 0)
       (set! *jas-peephole* #f))
      (("-fjvm-branch" (help "Enable JVM back-end branch"))
       (set! *optim-jvm-branch* (+fx 1 *optim-jvm-branch*)))
      (("-fno-jvm-branch" (help "Disable JVM back-end branch"))
       (set! *optim-jvm-branch* 0))
      (("-fjvm-fasteq" (help "EQ? no longer works on integers (use =FX)"))
       (set! *optim-jvm-fasteq* #t))
      (("-fno-jvm-fasteq" (help "Disable JVM back-end fasteq transformation"))
       (set! *optim-jvm-fasteq* #f))
      (("-jvm-env" ?var (help "Make the shell variable visible to GETENV"))
       (set! *jvm-env* (cons var *jvm-env*)))
      (("-jvm-jar" (help "Enable JVM jar files generation"))
       (set! *jvm-jar?* #t))
      (("-no-jvm-jar" (help "Disable JVM jar files generation (default)"))
       (set! *jvm-jar?* #f))
      (("-jvm-java" ?file (help "Use FILE as JVM"))
       (set! *jvm-java* file))
      (("-jvm-opt" ?string (help "JVM invocation option"))
       (set! *jvm-options* (string-append *jvm-options* " " string)))
      
;*--- trace options ---------------------------------------------------*/
      (section "Traces")
      ;; traces
      (("-t" (help "-t[2|3|4]" "Generate a trace file (*)"))
       (set! *trace-level* 1))
      (("-t2")
       (set! *trace-level* 2))
      (("-t3")
       (set! *trace-level* 3))
      (("-t4")
       (set! *trace-level* 4))
      (("-t5")
       (set! *trace-level* 5))
      (("+t?pass" (help "Force pass to be traced"))
       (set! *additional-traces* (cons pass *additional-traces*)))
      ;; shape
      (("-shape?opt" (help "-shape[mktTalun]" "Some debugging tools (private)"))
       (parse-shape-args opt))
      
;*--- Compiler stages -------------------------------------------------*/
      (section "Compilation stages")
      (("-mco" (help "Stop after .mco production"))
       (set! *module-checksum-object?* #t)
       (set! *pass* 'mco))
      (("-syntax" (help "Stop after the syntax stage (see -hygiene)"))
       (set! *pass* 'syntax))
      (("-expand" (help "Stop after the preprocessing stage"))
       (set! *pass* 'expand))
      (("-expand-module" (help "Produce the expanded module clause"))
       (set! *pass* 'dump-module))
      (("-ast" (help "Stop after the ast construction stage"))
       (set! *pass* 'ast))
      (("-syntax-check" (help "Stop after checking syntax"))
       (set! *pass* 'syntax-check))
      (("-bdb-spread-obj" (help "Stop after the bdb obj spread stage"))
       (set! *pass* 'bdb-spread-obj))
      (("-trace" (help "Stop after the trace pass"))
       (set! *pass* 'trace))
      (("-callcc" (help "Stop after the callcc pass"))
       (set! *pass* 'callcc))
      (("-bivalue" (help "Stop after the bivaluation stage"))
       (set! *pass* 'bivalue))
      (("-inline" (help "Stop after the inlining stage"))
       (set! *pass* 'inline))
      (("-inline+" (help "Stop after the 2nd inlining stage"))
       (set! *pass* 'inline+))
      (("-beta" (help "Stop after the constant beta reduction stage"))
       (set! *pass* 'beta))
      (("-fail" (help "Stop after the failure replacement stage"))
       (set! *pass* 'fail))
      (("-abound" (help "Stop after the array bound checking stage"))
       (set! *pass* 'abound))
      (("-initflow" (help "Stop after the type initflow stage"))
       (set! *pass* 'initflow))
      (("-narrow" (help "Stop after the scope narrowing stage"))
       (set! *pass* 'narrow))
      (("-tlift" (help "Stop after the type lifting stage"))
       (set! *pass* 'tlift))
      (("-dataflow" (help "Stop after the type dataflow stage"))
       (set! *pass* 'dataflow))
      (("-dataflow+" (help "Stop after the second type dataflow stage"))
       (set! *pass* 'dataflow+))
      (("-dataflow++" (help "Stop after the third type dataflow stage"))
       (set! *pass* 'dataflow++))
      (("-fuse" (help "Stop after the fuse stage"))
       (set! *pass* 'fuse)) 
      (("-user" (help "Stop after the user pass"))
       (set! *pass* 'user))
      (("-fxop" (help "Stop after the fx-ops optimization"))
       (set! *pass* 'fxop))
      (("-coerce" (help "Stop after the type coercing stage"))
       (set! *pass* 'coerce))
      (("-effect" (help "Stop after the effect stage"))
       (set! *pass* 'effect))
      (("-effect+" (help "Stop after the 2nd effect stage"))
       (set! *pass* 'effect+))
      (("-reduce" (help "Stop after the reduction opt. stage"))
       (set! *pass* 'reduce))
      (("-reduce+" (help "Stop after the 2nd reduction opt. stage"))
       (set! *pass* 'reduce+))
      (("-reduce-" (help "Stop after the very first reduction stage"))
       (set! *pass* 'reduce-))
      (("-assert" (help "Stop after the assertions stage"))
       (set! *pass* 'assert))
      (("-cfa" (help "Stop after the cfa stage"))
       (set! *pass* 'cfa))
      (("-stackable" (help "Stop after the stackable stage"))
       (set! *pass* 'stackable))
      (("-closure" (help "Stop after the globalization stage"))
       (set! *pass* 'closure))
      (("-recovery" (help "Stop after the type recovery stage"))
       (set! *pass* 'recovery))
      (("-bdb" (help "Stop after the Bdb code production"))
       (set! *pass* 'bdb))
      (("-cnst" (help "Stop after the constant allocation"))
       (set! *pass* 'cnst))
      (("-integrate" (help "Stop after the integration stage"))
       (set! *pass* 'integrate))
      (("-tailc" (help "Stop after the tailc stage"))
       (set! *pass* 'tailc))
      (("-return" (help "Stop after the return stage"))
       (set! *pass* 'return))
      (("-uncell" (help "Stop after the cell removal stage"))
       (set! *pass* 'uncell))
      (("-isa" (help "Stop after the isa stage"))
       (set! *pass* 'isa))
      (("-init" (help "Stop after the initialization construction stage"))
       (set! *pass* 'init))
      (("-classgen" (help "Produce an include file for class accessors"))
       (set! *pass* 'classgen))
      (("-egen" (help "Produce an include file for effects (requires -saw)"))
       (set! *pass* 'egen))
      (("-hgen" (help "Produce a C header file with class definitions"))
       (set! *pass* 'hgen))
      (("-cgen" (help "Do not C compile and produce a .c file"))
       (set! *pass* 'cgen))
      (("-indent" (help "Produce an indented .c file"))
       (set! *pass* 'cindent))
      (("-jvmas" (help "Produce a JVM .jas file"))
       (set! *target-language* 'jvm)
       (set! *pass* 'jvmas))
      
;*--- Constant initialization -----------------------------------------*/
      (section "Constant initialization")
      (("-init-lib" (help "-init-[lib|read|intern]" "Constants initialization mode"))
       (set! *init-mode* 'lib))
      (("-init-read")
       (set! *init-mode* 'read))
      (("-init-intern")
       (set! *init-mode* 'intern))
      (("-init-object-legacy" (help "-init-object-[legacy|staged]" "Object system initialization"))
       (set! *object-init-mode* 'legacy))
      (("-init-object-staged")
       (set! *object-init-mode* 'staged))
      
;*--- Private options -------------------------------------------------*/
      (section "Bootstrap and setup")
      ;; library construction
      (("-mklib" (help "Compile a library module"))
       (set! *lib-mode* #t)
       (set! *init-mode* 'lib))
      ;; additional library construction
      (("-mkaddlib" (help "Compile an additional library module"))
       (set! *init-mode* 'lib))
      ;; heap compilation
      (("-mkheap" (help "Build an heap file"))
       (set! *pass* 'make-heap))
      ;; heap compilation
      (("-mkaddheap" (help "Build an additional heap file"))
       (set! *pass* 'make-add-heap))
      ;; distribution compilation
      (("-mkdistrib" (help "Compile a main file for a distribution"))
       (set! *pass* 'distrib))
      ;; Bigloo license
      (("--license" (help "Display the Bigloo license and exit"))
       (print (bigloo-license))
       (compiler-exit 0))
      (("-LICENSE" (help "Add the license to the generated C files"))
       (set! *bigloo-licensing?* #t))
      ;; heap name
      (("-heap" ?name (help "Specify an heap file (or #f to not load heap)"))
       (set! *user-heap-name* name))
      ;; heap library
      (("-heap-library" ?lib (help "The library the heap belongs to"))
       (set! *heap-library* (string->symbol lib)))
      ;; heap dumping
      (("-dump-heap" ?name (help "Dump the content of a heap"))
       (set! *heap-dump-names* (cons name *heap-dump-names*)))
      ;; additinal heap name
      (("-addheap" ?name (help "Specify an additional heap file"))
       (set! *additional-heap-name* name))
      ;; the reader option
      (("-fread-internal" (help "Read source from binary interned file"))
       (set! *reader* 'intern))
      (("-fread-internal-src" (help "Read source only from binary interned file"))
       (set! *reader* 'intern-src))
      (("-fread-internal-src-file-name" ?name (help "Set fake source file name"))
       (when (eq? (car *src-files*) 'stdin)
	  (set! *src-files* (cons (string->symbol name) (cdr *src-files*)))))
      (("-fread-plain" (help "Read source from plain text file"))
       (set! *reader* 'plain))
      ;; target
      (("-target" ?lang (help "DON'T USE, (see -native, -jvm)"))
       (case (string->symbol lang)
	  ((c)
	   'nothing)
	  ((jvm)
	   (set! *target-language* 'jvm))
	  ((.net)
	   (set! *target-language* '.net))
	  (else
	   (error "parse-args" "Unknown target" lang))))
      
;*--- Unknown arguments -----------------------------------------------*/
      (("-?dummy")
       (set! *rest-args* (cons (string-append "-" dummy) *rest-args*)))
      
;*--- The source file -------------------------------------------------*/
      (else
       (let ((suf (suffix else)))
	  (cond 
	     ((and (string? suf)
		   (=fx (string-length suf) 0)
		   (null? *src-files*))
	      (set! *src-files* (list else)))
	     ((member suf *src-suffix*)
	      (set! *src-files* (cons else *src-files*)))
	     ((assoc suf *auto-mode*)
	      (set! *src-files* (cons else *src-files*)))
	     (*interpreter*
	      'ignore)
	     ((member suf *c-suffix*)
	      (set! *c-files*  (cons else *c-files*)))
	     ((member suf *obj-suffix*)
	      (set! *o-files*  (cons else *o-files*)))
	     ((and (eq? *target-language* '.net) (member suf *csharp-suffix*))
	      (set! *o-files* (cons else *o-files*)))
	     (else
	      (set! *rest-args* (cons else *rest-args*))))))))

;*---------------------------------------------------------------------*/
;*    parse-shape-args ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-shape-args string)
   (let ((len (string-length string)))
      (if (=fx len 0)
	  (begin
	     (set! *module-shape?*   #t)
	     (set! *key-shape?*      #t)
	     (set! *type-shape?*     #t)
	     (set! *typename-shape?* #t)
	     (set! *access-shape?*   #t)
	     (set! *location-shape?* #t)
	     (set! *user-shape?*     #t)
	     (set! *name-shape?*     #t))
	  (let liip ((i 0))
	     (if (=fx i len)
		 'done
		 (begin
		    (case (string-ref string i)
		       ((#\m)
			(set! *module-shape?* #t))
		       ((#\k)
			(set! *key-shape?* #t))
		       ((#\t)
			(set! *type-shape?* #t))
		       ((#\T)
			(set! *type-shape?* #t)
			(set! *typenode-shape?* #t))
		       ((#\a)
			(set! *access-shape?* #t))
		       ((#\l)
			(set! *location-shape?* #t))
		       ((#\u)
			(set! *user-shape?* #t))
		       ((#\n)
			(set! *typename-shape?* #t)
			(set! *name-shape?* #t))
		       (else
			(error "parse-arg" "Illegal -shape option" string)))
		    (liip (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    parse-safe/unsafe-args ...                                       */
;*    -------------------------------------------------------------    */
;*    The variable @label unsafe-rgc@ is defined in the library. See   */
;*    the __rgc module (@ref ../../runtime/Rgc/rgc.scm:unsafe-rgc@)    */
;*---------------------------------------------------------------------*/
(define (parse-safe/unsafe-args string val)
   (let ((len (string-length string)))
      (if (=fx len 0)
	  (begin
	     (set! *unsafe-rgc* val)
	     (set! *unsafe-library* val)
	     (set! *unsafe-arity* val)
	     (set! *unsafe-type* val)
	     (set! *unsafe-struct* val)
	     (set! *unsafe-range* val)
	     (set! *unsafe-version* val)
	     (set! *unsafe-eval* val))
	  (let liip ((i 0))
	     (if (=fx i len)
		 'done
		 (begin
		    (case (string-ref string i)
		       ((#\r)
			(set! *unsafe-range* val))
		       ((#\a)
			(set! *unsafe-arity* val))
		       ((#\t)
			(set! *unsafe-rgc* val)
			(set! *unsafe-type* val))
		       ((#\s)
			(set! *unsafe-struct* val))
		       ((#\v)
			(set! *unsafe-version* val))
		       ((#\l)
			(set! *unsafe-library* val))
		       ((#\e)
			(set! *unsafe-eval* val))
		       ((#\h)
			;; only change heap is h is specified
			(set! *unsafe-heap* val))
		       (else
			(error "parse-arg" "Illegal -unsafe option" string)))
		    (liip (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    parse-debug-args ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-debug-args string)
   
   (define (-g2!)
      (bigloo-compiler-debug-set! 2)
      (set! *compiler-debug* 2)
      (set! *jas-peephole* #f))
   
   (define (-g3!)
      ;; -g3 and -call/cc are incompatible
      (unless *call/cc?*
	 (bigloo-compiler-debug-set! 3)
	 (set! *compiler-debug-trace* 2)
	 (set! *compiler-debug* 3)))
   
   (define (-g4!)
      (bigloo-compiler-debug-set! 4)
      (set! *compiler-debug* 4))
   
   (bigloo-compiler-debug-set! 1)
   (set! *compiler-debug* 1)
   (set! *purify* #t)
   (set! *jvm-debug* #t)
   (set! *jas-warning* #f)
   (set! *compiler-debug-trace* 1)
   (when (eq? *error-localization-opt* #unspecified)
      (set! *error-localization* #t))
   (if (> (string-length string) 0)
       (case (string-ref string 0)
	  ((#\2)
	   (-g2!))
	  ((#\3)
	   (-g3!))
	  ((#\4)
	   (-g4!))
	  (else
	   (error "parse-arg" "Illegal -g option" string)))))

;*---------------------------------------------------------------------*/
;*    parse-bdb-args ...                                               */
;*---------------------------------------------------------------------*/
(define (parse-bdb-args string)
   
   (define (-gbdb2!)
      (set! *bdb-debug* 2)
      (set! *compiler-debug* 1)
      (set! *user-heap-size* 1)
      (set! *jas-warning* #f))
   
   (define (-gbdb3!)
      (set! *bdb-debug* 3))
   
   (if (bigloo-config 'have-bdb)
       (begin 
	  (set! *libraries* (cons 'bdb *libraries*))
	  
	  (set! *user-heap-size* 1)
	  (set! *bdb-debug* 1)
	  (set! *purify* #t)
	  (set! *jvm-debug* #t)
	  (set! *jas-warning* #f)
	  (set! *c-debug-lines-info*  #t)
	  (if (> (string-length string) 0)
	      (case (string-ref string 0)
		 ((#\2)
		  (-gbdb2!))
		 ((#\3)
		  (-gbdb2!)
		  (-gbdb3!))
		 (else
		  (error "parse-arg" "Illegal -g option" string)))))
       (warning "-gbdb"
		"Bdb not available (see Bigloo configuration option)"
		" ignoring option")))
   
;*---------------------------------------------------------------------*/
;*    parse-optim-args ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-optim-args string)
   
   (define (-O2!)
      (set! *cc-options* (append *cc-options* (list *cflags-optim*)))
      (set! *optim-jvm-inlining* 2)
      (set! *optim-jvm-branch* 3)
      (set! *optim-jvm-fasteq* #t)
      (set! *optim-reduce-beta?* #t)
      (set! *optim-cfa-flonum-arithmetic?* #t)
      (set! *optim-dataflow-types?* #t)
      (set! *optim-initflow?* #t)
      (set! *optim-tagged-fxop?* #t)
      ;; (set! *optim-narrow?* #t)
      ;; (set! *optim-return?* #t)
      (set! *optim-cfa-free-var-tracking?* #t)
      (set! *optim-cfa-unbox-closure-args* #t))
      
   (define (-O3!)
      (-O2!)
      (set! *optim-jvm-inlining* 3)
      (set! *optim-jvm-branch* 5)
      (if (not (boolean? *optim-unroll-loop?*))
	  (set! *optim-unroll-loop?* #t)))
   
   (set! *optim* 1)
   (set! *optim-sync-failsafe?* #t)
   (set! *optim-jvm-inlining* 1)
   (set! *optim-jvm-branch* 1)
   (set! *optim-O-macro?* #t)
   (set! *optim-dataflow?* #t)
   (set! *optim-symbol-case* #t)
   (if (> (string-length string) 0)
       (case (string-ref string 0)
	  ((#\0)
	   (set! *optim* 0))
	  ((#\1)
	   (set! *optim* 1))
	  ((#\2)
	   (-O2!)
	   (set! *optim* 2))
	  ((#\3)
	   (-O3!)
	   (set! *optim* 3))
	  ((#\4 #\5)
	   (-O3!)
	   (set! *optim* (-fx (char->integer (string-ref string 0))
			    (char->integer #\0))))
	  ((#\6)
	   (-O3!)
	   (set! *optim-stackable?* #t)
	   (set! *optim-uncell?* #t)
	   (set! *optim* (-fx (char->integer (string-ref string 0))
			    (char->integer #\0))))
	  (else
	   (error "parse-arg" "Illegal -O option" string)))
       (set! *cc-options* (append *cc-options* (list "-O")))))

;*---------------------------------------------------------------------*/
;*    query ...                                                        */
;*---------------------------------------------------------------------*/
(define (query)
   (version)
   (newline)
   (print "setups:")
   (newline)
   (print "*cc*                   : " *cc*)
   (print "*cc-options*           : " *cc-options*)
   (print "*ld-options*           : " *ld-options*)
   (print "*ld-post-options*      : " *ld-post-options*)
   (print "*bigloo-lib*           : " *bigloo-lib*)
   (print "*bigloo-user-lib*      : " *bigloo-user-lib*)
   (print "*lib-dir*              : " *lib-dir*)
   (print "*include-foreign*      : " *include-foreign*)
   (print "*heap-name*            : " *heap-name*)
   (print "*target-language*      : " *target-language*)
   (print "*jvm-shell*            : " *jvm-shell*)
   (newline)
   (print "Too see all variables enter the interpreter")
   (print "or use the -help2 option.")
   (newline)
   (print "----")
   (print " (*) read-only variables")
   (compiler-exit 0))

;*---------------------------------------------------------------------*/
;*    load-init ...                                                    */
;*---------------------------------------------------------------------*/
(define (load-init file)
   (let ((f (find-file/path file *user-load-path*)))
      (when (string? f)
	 (begin
	    (pass-prelude f)
	    (loadq f)
	    #t))))
