;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/comptime/Engine/compiler.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 08:22:54 1996                          */
;*    Last change :  Mon Oct  6 07:50:31 2025 (serrano)                */
;*    Copyright   :  1996-2025 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compiler driver                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_compiler
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   
   (import  tools_error
	    tools_progn
	    tools_shape
	    engine_pass
	    engine_signals
	    engine_param
	    engine_engine
	    read_src
	    write_unit
	    write_ast
	    read_access
	    read_jvm
	    heap_restore
	    heap_make
	    ast_env
	    ast_check-sharing
	    ast_check-type
	    ast_remove
	    type_type
	    ast_var
	    ast_node
	    ast_build
	    ast_unit
	    ast_check-global-init
	    ast_init
	    ast_lvtype
	    user_user
	    peephole_walk
	    type_env
	    type_cache
	    module_module
	    module_module5
	    module_include
	    module_alibrary
	    module_foreign
	    expand_eps
	    expand_install
	    init_main
	    init_setrc
	    trace_walk
	    beta_walk
	    inline_walk
	    effect_walk
	    stackable_walk
	    callcc_walk
	    fail_walk
	    abound_walk
	    dataflow_walk
	    initflow_walk
	    globalize_walk
	    cfa_walk
	    cfa_tvector
	    cfa_pair
	    integrate_walk
	    tailc_walk
	    fxop_walk
	    flop_walk
	    prebox_walk
	    coerce_walk
	    reduce_walk
	    cnst_walk
	    object_classgen
	    object_class
	    object_slots
	    hgen_walk
	    bdb_setting
	    bdb_spread-obj
	    bdb_walk
	    prof_walk
	    return_walk
	    uncell_walk
	    nums_walk
	    fastfl_walk
	    isa_walk
	    cc_cc
	    cc_ld
	    cc_roots
	    backend_backend
	    backend_walk)

   (with    backend_c
            backend_wasm
	    backend_jvm)
   
   (export  (compiler)))

;*---------------------------------------------------------------------*/
;*    compiler ...                                                     */
;*---------------------------------------------------------------------*/
(define (compiler)
   ;; initialize signal handlers for ending nicely even on compiler error
   (profile signal (install-compiler-signals!))
   
   ;; initialize the user selected compilation backend
   (set-backend! *target-language*)
   
   ;; adjust the compilation parameters according to the backend
   ;; specificities and various options
   (unless (>fx (bigloo-compiler-debug) 0)
      (unless (backend-bound-check (the-backend))
	 (set! *unsafe-range* #t))
      (unless (backend-type-check (the-backend))
	 (set! *unsafe-type* #t)))
   (unless (backend-typed-funcall (the-backend))
      (set! *optim-cfa-unbox-closure-args* #f))
   (when (or (>=fx *compiler-debug-trace* 1)
	     (>=fx *compiler-debug* 1))
      ;; compiler traces being imcompatible with the setjmp/longmp
      ;; optimization, disable it in debug mode
      (set! *optim-return-goto?* #f))
   
   ;; read the source file
   (let ((src (*pre-processor* (profile read (read-src)))))
      ;; if src is false, we launch the interpreter because it means
      ;; that the reader has found a #!... expression instead of a
      ;; module clause
      (cond
	 ((not src) 
	  (set! *interpreter* #t)
	  (compiler-exit (engine)))
	 ((not (pair? src))
	  (if (eq? *reader* 'intern-src)
	      (exit 1)
	      (user-error "Parse error" "Illegal source file" src))))
      
      ;; now (and only now) we can say hello
      (hello-world)
      
      ;; conifgure bdb when debugging
      (if (and (>fx *bdb-debug* 0)
	       (memq 'bdb (backend-debug-support (the-backend))))
	  (profile bdb (bdb-setting!)))
      
      ;; check now if all arguments have been parsed
      (unless (null? *rest-args*)
	 (warning "Don't know what to do with arguments: " *rest-args*))
      
      ;; we read access file
      (profile afile (read-access-files))
      (profile package (read-jfile))
      
      ;; create (or restore) the compilation environment
      (if *lib-mode*
	  (profile env
	     (begin
		(initialize-Genv!)
		(initialize-Tenv!)))
	  (profile heap (restore-heap)))
      
      ;; initialize the type caching system
      (profile itype (install-type-cache!))
      
      ;; when the vector->tvector optimization is enabled the types
      ;; of vector-set! family functions have to be patched
      (profile vect (patch-vector-set!))
      
      ;; when the cfa pair tracking the types of set-cxr! family functions
      ;; have to be patched
      (profile pair (patch-pair-set!))
      
      ;; declare the compilation srfi1 keywords before parsing the module
      (register-srfi! 'bigloo-compile)
      (when (eq? *pass* 'classgen)
	 (register-srfi! 'bigloo-class-generate))
      (when (>=fx (bigloo-compiler-debug) 1)
	 (register-srfi! 'bigloo-debug))
      
      (let ((unsafe 0))
	 (when *unsafe-type*
	    (set! unsafe (+fx 1 unsafe))
	    (register-srfi! 'bigloo-unsafe-type))
	 (when *unsafe-range*
	    (set! unsafe (+fx 1 unsafe))
	    (register-srfi! 'bigloo-unsafe-range))
	 (when *unsafe-arity*
	    (set! unsafe (+fx 1 unsafe))
	    (register-srfi! 'bigloo-unsafe-arity))
	 (when *unsafe-library*
	    (set! unsafe (+fx 1 unsafe))
	    (register-srfi! 'bigloo-unsafe-library))
	 (when *unsafe-eval*
	    (set! unsafe (+fx 1 unsafe))
	    (register-srfi! 'bigloo-unsafe-eval))
	 (when *unsafe-version*
	    (set! unsafe (+fx 1 unsafe))
	    (register-srfi! 'bigloo-unsafe-version))
	 (when (=fx unsafe 6)
	    (register-srfi! 'bigloo-unsafe)))
      
      ;; install the compiler macros ...
      (install-initial-expander)
      
      ;; new bigloo class accessorless
      (register-srfi! 'bigloo-class-sans)
      
      ;; build the program ast
      (let ((ast (src->ast src)))
	 
	 (stop-on-pass 'ast (lambda () (write-ast ast)))
	 (check-sharing "ast" ast)
	 (check-type "ast" ast #f #f)
	 
	 ;; stop after performing syntax check and building ast
	 ;; and write no output. this is usefull for use with
	 ;; Flycheck and Flymake
	 (stop-on-pass 'syntax-check (lambda () #unspecified))
	 
	 ;; early peephole optimizations
	 (when *optim-peephole?*
	    (set! ast (profile peephole (peephole-walk! ast))))
	 (stop-on-pass 'peephole (lambda () (write-ast ast)))
	 (check-sharing "peephole" ast)
	 (check-type "peephole" ast #f #f)
	 
	 ;; compute the global init property
	 (when *optim-initflow?*
	    (set! ast (profile initflow (initflow-walk! ast))))
	 (stop-on-pass 'initflow (lambda () (write-ast ast)))
	 (check-sharing "initflow" ast)
	 (check-type "initflow" ast #f #f)
	 
	 ;; we make a heap on `mkheap' mode
	 (stop-on-pass 'make-heap (lambda () (make-heap)))
	 
	 ;; when compiling for bdb we turn all _ type into Bigloo obj type
	 (when (and (>fx *bdb-debug* 0)
		    (memq 'bdb (backend-debug-support (the-backend))))
	    (bdb-spread-obj! ast))
	 (stop-on-pass 'bdb-spread-obj (lambda () (write-ast ast)))
	 
	 ;; generate debug (and profiling) traces before inlining
	 (when (and (>=fx *compiler-debug-trace* 1)
		    ;;(>=fx (bigloo-compiler-debug) 2)
		    (backend-trace-support (the-backend)))
	    (set! ast (profile trace (trace-walk! ast))))
	 (check-sharing "trace" ast)
	 (check-type "trace" ast #f #f)
	 
	 ;; when we are compiling with call/cc we have to
	 ;; put all written local variables in cells
	 (when (and *call/cc?* (backend-callcc (the-backend)))
	    (set! ast (profile callcc (callcc-walk! ast))))
	 (stop-on-pass 'callcc (lambda () (write-ast ast)))
	 (check-sharing "callcc" ast)
	 (check-type "callcc" ast #f #f)
	 
	 ;; the effect property computation
	 (set! ast (profile effect (effect-walk! ast #f)))
	 (stop-on-pass 'effect (lambda () (write-ast ast)))
	 (check-sharing "effect" ast)
	 (check-type "effect" ast #f #f)
	 
	 ;; the stackable property computation
	 (set! ast (profile stackable (stackable-walk! ast)))
	 (stop-on-pass 'stackable (lambda () (write-ast ast)))
	 (check-sharing "stackable" ast)
	 (check-type "stackable" ast #f #f)
	 
	 ;; isa optimization
	 (set! ast (profile isa (isa-walk! ast)))
	 (stop-on-pass 'isa (lambda () (write-ast ast)))
	 (check-sharing "isa" ast)
	 (check-type "isa" ast #f #f)
	 
	 ;; specialize flonum operations
	 (set! ast (profile glop (flop-walk! ast)))
	 (stop-on-pass 'flop (lambda () (write-ast ast)))
	 (check-sharing "flop" ast)
	 (check-type "flop" ast #f #f)
	 
	 ;; inlining optimization
	 (set! ast (profile inline (inline-walk! "Inline" ast 'all)))
	 (stop-on-pass 'inline (lambda () (write-ast ast)))
	 (check-sharing "inline" ast)
	 (check-type "inline" ast #f #f)
	 
	 ;; we make a heap on `mkheap' mode
	 ;; MS: 10 May 2007
	 ;; This stage has been move back after the inlining in ordre
	 ;; to solve the problem of function calling static inline
	 ;; definitions.
	 ;; Prior to Bigloo3.0a, this stage used to take place right after
	 ;; the make-heap stage.
	 (stop-on-pass 'make-add-heap (lambda () (make-add-heap)))
	 
	 ;; very simple beta reduction of constant
	 (set! ast (profile beta (beta-walk! ast)))
	 (stop-on-pass 'beta (lambda () (write-ast ast)))
	 (check-sharing "beta" ast)
	 (check-type "beta" ast #f #f)
	 
	 ;; replace `failure' invokation by `error/location' when
	 ;; invoked in debug mode (to be performed after the coercion stage)
	 (when (and (>=fx (bigloo-compiler-debug) 1) *error-localization*)
	    (set! ast (profile fail (fail-walk! ast))))
	 (stop-on-pass 'fail  (lambda () (write-ast ast)))
	 (check-sharing "fail" ast)
	 (check-type "fail" ast #f #f)
	 
	 ;; compute type information based on the explicit type tests found
	 ;; in the source code.
	 (when *optim-dataflow-types?*
	    (if *call/cc?*
		(set! *optim-dataflow-types?* #f)
		(begin
		   (set! ast (profile reduce- (reduce-walk! ast "Reduce0" #t)))
		   (stop-on-pass 'reduce0 (lambda () (write-ast ast)))
		   (set! ast (profile dataflow (dataflow-walk! ast "Dataflow"))))))
	 (stop-on-pass 'dataflow (lambda () (write-ast ast)))
	 (check-sharing "dataflow" ast)
	 (check-type "dataflow" ast #f #f)
	 
	 ;; the globalization stage
	 (set! ast (profile glo (globalize-walk! ast 'closure)))
	 (stop-on-pass 'closure (lambda () (write-ast ast)))
	 (check-sharing "closure" ast)
	 (check-type "closure" ast #f #f)
	 
	 ;; the control flow analysis
	 (set! ast (profile cfa (cfa-walk! ast)))
	 (stop-on-pass 'cfa (lambda () (write-ast ast)))
	 (check-sharing "cfa" ast)
	 (check-type "cfa" ast #t #f)
	 
	 ;; Set the default type to obj so that newly created variable
	 ;; will no longer be attached the _ type
	 (set-default-type! *obj*)
	 
	 ;; the CFA has introduced type information that the dataflow
	 ;; analysis can used to improve type checking elimination.
	 (when *optim-dataflow-types?*
	    (set! ast (profile dataflow (dataflow-walk! ast "Dataflow+"))))
	 (stop-on-pass 'dataflow+ (lambda () (write-ast ast)))
	 (check-sharing "dataflow" ast)
	 (check-type "dataflow" ast #t #f)
	 
	 ;; the integration pass
	 (set! ast (profile integ (integrate-walk! ast)))
	 (stop-on-pass 'integrate (lambda () (write-ast ast)))
	 (check-sharing "integrate" ast)
	 (check-type "integrate" ast #t #f)
	 
	 ;; the tail-call pass
	 (when (or *global-tail-call?*
		   (backend-require-tailc (the-backend)))
	    (set! ast (profile integ (tailc-walk! ast)))
	    (check-sharing "tailc" ast)
	    (check-type "tailc" ast #t #f))
	 (stop-on-pass 'tailc (lambda () (write-ast ast)))
	 
	 ;; the reduction transformation for improving error detections
	 (when *optim-dataflow-for-errors?*
	    (set! ast (profile reduce- (reduce-walk! ast "Reduce-" #t)))
	    (check-sharing "reduce-" ast)
	    (check-type "reduce-" ast #t #f))
	 (stop-on-pass 'reduce- (lambda () (write-ast ast)))
	 
	 ;; introduce array (vectors and strings) bound checking
	 ;; this stage has to be executed after the heap has been
	 ;; generated/restored, otherwise the compilation mode of the
	 ;; heap is visible in the compilation of a client module, which
	 ;; is wrong because the heap is shared by all versions of a lib
	 (unless *unsafe-range*
	    (set! ast (profile abound (abound-walk! ast))))
	 (stop-on-pass 'abound (lambda () (write-ast ast)))
	 (check-sharing "abound" ast)
	 (check-type "abound" ast #t #f)
	 
	 ;; fast flonum optimization
	 (when *optim-fastfl?*
	    (set! ast (profile fastfl (fastfl-walk! ast)))
	    (stop-on-pass 'fastfl (lambda () (write-ast ast)))
	    (check-sharing "fastfl" ast)
	    (check-type "fastfl" ast #t #t))
	 
	 (set! ast (profile coerce (coerce-walk! ast)))
	 (stop-on-pass 'coerce (lambda () (write-ast ast)))
	 (check-sharing "coerce" ast)
	 (check-type "coerce" ast #t #t)
	 
	 ;; C tagged arithmetic operations
	 (when (and *optim-tagged-fxop?*
		    (memq (backend-language (the-backend)) '(c c-saw)))
	    (set! ast (profile fxop (fxop-walk! ast)))
	    (stop-on-pass 'fxop (lambda () (write-ast ast)))
	    (check-sharing "fxop" ast)
	    (check-type "fxop" ast #t #f))
	 
	 ;; Preboxing (or box-unbox) optimization
	 (when *optim-prebox?*
	    (set! ast (profile fxop (prebox-walk! ast)))
	    (stop-on-pass 'prebox (lambda () (write-ast ast)))
	    (check-sharing "prebox" ast)
	    (check-type "prebox" ast #t #f))
	 
	 ;; now that type checks have been introduced, we recompute
	 ;; the type dataflow analysis
	 (when *optim-dataflow-types?*
	    (set! ast (profile dataflow (dataflow-walk! ast "Dataflow++"))))
	 (stop-on-pass 'dataflow++ (lambda () (write-ast ast)))
	 (check-sharing "dataflow" ast)
	 (check-type "dataflow" ast #t #t)
	 
	 ;; we re-run the effect computations (for coercion and
	 ;; type checks)
	 (when (or (>=fx *optim* 1) (eq? *pass* 'egen))
	    (set! ast (profile effect (effect-walk! ast #t))))
	 (stop-on-pass 'egen (lambda () (write-effect ast)))
	 (check-sharing "effect" ast)
	 (check-type "effect" ast #t #t)
	 
	 ;; the reduction optimizations
	 (when (>=fx *optim* 1)
	    (set! ast (profile reduce (reduce-walk! ast "Reduce"))))
	 (stop-on-pass 'reduce (lambda () (write-ast ast)))
	 (check-sharing "reduce" ast)
	 (check-type "reduce" ast #t #t)
	 
	 ;; the bdb initialization code
	 (when (and (>fx *bdb-debug* 0)
		    (memq 'bdb (backend-debug-support (the-backend))))
	    (set! ast (profile bdb (bdb-walk! ast))))
	 (stop-on-pass 'bdb (lambda () (write-ast ast)))
	 
	 ;; the constant computation
	 (set! ast (profile cnst (cnst-walk! ast)) )
	 (stop-on-pass 'cnst (lambda () (write-ast ast)))
	 (check-sharing "cnst" ast)
	 (check-type "cnst" ast #t #t)
	 
	 ;; the set-exit=>return transformation pass
	 (when (and *optim-return?* (backend-retblock (the-backend)))
	    (set! ast (profile return (return-walk! ast)))
	    (set! ast (lvtype-ast! ast)))
	 (stop-on-pass 'return (lambda () (write-ast ast)))
	 (check-sharing "return" ast)
	 (check-type "return" ast #t #f)
	 
	 ;; we re-perform the inlining pass in high optimization mode
	 ;; in order to inline all type checkers.
	 (set! ast (profile inline (inline-walk! "Inline+" ast 'reducer)))
	 (set! ast (lvtype-ast! ast))
	 (check-sharing "inline+" ast)
	 (check-type "inline+" ast #t #t)
	 (stop-on-pass 'inline+ (lambda () (write-ast ast)))
	 
	 ;; the code production
	 (let ((ast2 (append-ast (ast-initializers) ast)))
	    (stop-on-pass 'init (lambda () (write-ast ast2)))
	    (check-sharing "init" ast2)
	    (check-type "init" ast2 #t #t)
	    
	    ;; the 2nd reduction optimizations
	    (when (or (>=fx *optim* 2) (backend-effect+ (the-backend)))
	       (set! ast2 (profile effect (effect-walk! ast2 #t)))
	       (stop-on-pass 'effect+ (lambda () (write-ast ast2)))
	       (set! ast2 (profile reduce (reduce-walk! ast2 "Reduce+"))))
	    (stop-on-pass 'reduce+ (lambda () (write-ast ast2)))
	    (check-sharing "reduce+" ast2)
	    (check-type "reduce+" ast2 #t #t)
	    
	    ;; useless cell removal
	    (when *optim-uncell?*
	       (set! ast (profile uncell (uncell-walk! ast)))
	       (stop-on-pass 'uncell (lambda () (write-ast ast)))
	       (check-sharing "uncell" ast)
	       (check-type "uncell" ast #t #f))
	    
	    ;; double fix/flo predicates optimization
	    (when *optim-nums?*
	       (set! ast (profile uncell (nums-walk! ast)))
	       (stop-on-pass 'nums (lambda () (write-ast ast)))
	       (check-sharing "nums" ast)
	       (check-type "nums" ast #t #f))
	    
	    (backend-walk (remove-var 'now ast2)))
	 
	 0)))

;*---------------------------------------------------------------------*/
;*    src->ast ...                                                     */
;*---------------------------------------------------------------------*/
(define (src->ast src)
   (case *module-version*
      ((4)
       (module4->ast src))
      ((5)
       (module5->ast src))
      (else
       (match-case (car src)
	  ((module ?- :version 4 . ?-) (module4->ast src))
	  ((module ?- :version 5 . ?-) (module5->ast src))
	  (else (module4->ast src))))))

;*---------------------------------------------------------------------*/
;*    module5->ast ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5->ast expr)
   (with-trace 'compiler "module5->ast"
      (trace-item "expr=" expr)
      (pass-prelude "Module5")
      
      (set! *module-version* 5)
      (register-srfi! 'bigloo-module5)
      
      (module5-register-plugin! 'pragma module5-plugin-pragma)
      (module5-register-extern-plugin! "C" module5-extern-plugin-c)
      (module5-register-extern-plugin! "java" module5-extern-plugin-java)
      (module5-register-extern-plugin! "wasm" module5-extern-plugin-wasm)

      (module4-register-plugin! 'extern module4-plugin-extern)

      (let* ((expr-mod (car expr))
	     (expr-body (cdr expr))
	     (mod (module5-parse expr (car *src-files*)
		     :lib-path *lib-dir*
		     :expand module5-expand
		     :cache-dir *module-cache-dir*))
	     (tu (unit 'toplevel 100 '() #t #f))
	     (units (list tu))
	     (xenv (create-hashtable :weak 'open-string)))
	 
	 (trace-item "units=" units)
	 (trace-item "body=" expr-body)

	 ;; imported module unit (before processing the module body)
	 (set! units (cons (module5-imported-unit mod comptime-expand) units))
	 
	 (with-access::Module mod (id body checksum main decls imports)

	    (module5-expand-and-resolve! mod xenv
	       :heap-modules (module5-heap4-modules))
	    (module5-checksum! mod)
	    
	    (hashtable-for-each decls
	       (lambda (k d)
		  (with-access::Decl d (scope alias)
		     (when (eq? scope 'import)
			(let ((def (module5-import-def mod d)))
			   (with-access::Def def (expr kind)
			      (when (memq kind '(macro expander))
				 (add-macro-definition! expr alias))))))))
	    
	    (set! *module* id)
	    (set! *module-clause* expr-mod)
	    (set! *module-checksum* checksum)
	    
	    (stop-on-pass 'dump-module (lambda () (dump-module mod)))
	    
	    ;; profiling initilization code
	    (when (>=fx *profile-mode* 1)
	       (set! units (cons (make-prof-unit) units)))
	    
	    ;; produce the mco file on user demand
	    (when *module-checksum-object?*
	       (profile mco (module-checksum-object)))
	    (stop-on-pass 'mco (lambda () 0))
	    
	    ;; complete the module body
	    (let ((code (debug-code body)))
	       (unit-sexp*-add! tu code)
	       (when (>=fx *optim* 2)
		  (unit-sexp*-add-head! tu (optimize-code)))))
	 
	 ;; check for errors that might have already occurred
	 ;; when building the ast but delayed
	 (pass-postlude #unspecified)
	 
	 ;; check if all types are defined
	 (profile ctype (check-types))
	 
	 ;; SCH class accessor generation
	 (stop-on-pass 'classgen classgen-walk)
	 
	 ;; C header generation
	 (stop-on-pass 'hgen hgen-walk)
	 
	 ;; we load the library init files. This must be done after
	 ;; regular macros have been installed in order to enable these
	 ;; macro redefinitions
	 (load-library-init)
	 
	 ;; once library clauses have been parsed
	 ;; we must restore again additional heaps
	 (restore-additional-heaps)
	 (additional-heap-restore-globals!)
	 (unit-sexp*-add-head! tu (get-alibrary-inits))
	 
	 ;; user pass
	 (user-walk tu)
	 (stop-on-pass 'user (lambda () (write-unit units)))
	 
	 ;; object unit
	 (let ((u (module5-object-unit mod)))
	    (when u (set! units (cons u units))))

	 ;; imported inline units
	 (let ((u (module5-imported-inline-unit mod)))
	    (when u (set! units (cons u units))))
	 
	 ;; ... and the global user-defined macro expansion
	 (profile expand (expand-units units))
	 (stop-on-pass 'expand
	    (lambda ()
	       (set! *module-clause* (module5-expand *module-clause*))
	       (write-unit units)))
	 
	 ;; explicit GC roots registration
	 (when (and *gc-force-register-roots?*
		    (backend-force-register-gc-roots (the-backend)))
	    (set! units (cons (make-gc-roots-unit) units)))

	 ;; build the variable and function ast
	 (module5-ast! mod)

	 (let* ((m (module5-main mod))
		(ast (profile ast (build-ast units))))

	    ;; register main declaration
	    (set! *main* m)

	    ;; handle pragma declarations
	    (module5-resolve-pragma! mod)

	    ;; check if inlined functions used by the backend
	    ;; have all been defined
	    (backend-check-inlines (the-backend))

	    ;; collect all the libraries
	    (with-access::Module mod (libraries)
	       (for-each (lambda (l) (use-library! (car l))) libraries))

	    ;; generate a heap5 ondemange
	    (stop-on-pass 'make-add-heap
	       (lambda () (module5-write-heap *additional-heap-name* mod)))
	    
	    ast))))

;*---------------------------------------------------------------------*/
;*    module4->ast ...                                                 */
;*---------------------------------------------------------------------*/
(define (module4->ast src)
   (with-trace 'compiler "module4->ast"
      (trace-item "src=" src)
      (set! *module-version* 4)
      (register-srfi! 'bigloo-module4)
      (let* ((exp0 (comptime-expand/error (car src)))
	     (module (progn-first-expression exp0))
	     (src-code (append (progn-tail-expressions exp0) (cdr src)))
	     (units (profile module (produce-module! module)))
	     (tu (find (lambda (u) (eq? (unit-id u) 'toplevel)) units)))
	 
	 (stop-on-pass 'dump-module (lambda () (dump-module module)))
	 
	 ;; profiling initilization code
	 (when (>=fx *profile-mode* 1)
	    (set! units (cons (make-prof-unit) units)))
	 
	 ;; produce the mco file
	 (when *module-checksum-object?*
	    (profile mco (module-checksum-object)))
	 (stop-on-pass 'mco (lambda () 0))
	 
	 ;; build the module body
	 (let ((code (debug-code src-code)))
	    (unit-sexp*-add! tu code)
	    (when (>=fx *optim* 2)
	       (unit-sexp*-add-head! tu (optimize-code))))
	 
	 ;; check for errors that might have already occurred
	 ;; while building the ast and that could have been delayed
	 (pass-postlude #unspecified)
	 
	 ;; check if all types are defined
	 (profile ctype (check-types))
	 
	 ;; SCH class accessor generation
	 (stop-on-pass 'classgen classgen-walk)
	 
	 ;; C header generation
	 (stop-on-pass 'hgen hgen-walk)
	 
	 ;; we load the library init files. This must be done after
	 ;; regular macros have been installed in order to enable these
	 ;; macro redefinitions
	 (load-library-init)
	 
	 ;; once library clauses have been parsed
	 ;; we must restore again additional heaps
	 (restore-additional-heaps)
	 (additional-heap-restore-globals!)
	 (unit-sexp*-add-head! tu (get-alibrary-inits))
	 
	 ;; we perfom user pass
	 (user-walk tu)
	 (stop-on-pass 'user (lambda () (write-unit units)))
	 
	 ;; ... and we macro expand
	 (profile expand (expand-units units))
	 (stop-on-pass 'expand (lambda () (write-unit units)))
	 
	 ;; check if inlined code correspond to library functions
	 (backend-check-inlines (the-backend))
	 
	 ;; explicit GC roots registration
	 (when (and *gc-force-register-roots?*
		    (backend-force-register-gc-roots (the-backend)))
	    (set! units (cons (make-gc-roots-unit) units)))
	 
	 (profile ast (build-ast units)))))

;*---------------------------------------------------------------------*/
;*    debug-code ...                                                   */
;*---------------------------------------------------------------------*/
(define (debug-code src-code)
   (let ((body (if (null? src-code) '(#unspecified) src-code)))
      (if (and (>=fx *compiler-debug-trace* 1) *main*)
	  (cons `(bigloo-debug-set!
		    ,(max (bigloo-compiler-debug) *compiler-debug-trace*))
	     body)
	  body)))

;*---------------------------------------------------------------------*/
;*    optimize-code ...                                                */
;*---------------------------------------------------------------------*/
(define (optimize-code)
   (list (%append-2-define)))
