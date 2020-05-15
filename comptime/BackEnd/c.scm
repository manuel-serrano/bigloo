;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/BackEnd/c.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  4 14:10:06 2003                          */
;*    Last change :  Wed Dec 11 06:54:13 2019 (serrano)                */
;*    Copyright   :  2003-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The C back-end                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_c
   
   (include "Engine/pass.sch"
	    "Ast/unit.sch"
	    "Tools/trace.sch")
   
   (import  tools_shape
	    tools_error
	    engine_param
	    engine_configure
	    engine_compiler
	    engine_link
	    module_module
	    module_library
	    module_alibrary
	    module_eval
	    type_type
	    type_cache
	    type_env
	    type_typeof
	    ast_var
	    ast_node
	    ast_occur
	    ast_build
	    object_class
	    object_slots
	    bdb_emit
	    bdb_setting
	    prof_emit
	    backend_backend
	    backend_init
	    backend_cvm
	    backend_c_emit
	    backend_c_prototype
	    backend_c_main
	    cc_indent
	    cc_cc
	    cc_ld
	    cc_roots
	    init_setrc
	    read_reader
	    ast_env
	    ast_type-occur)

   (with    cgen_compile
	    saw_c_compile)

   (export  (build-sawc-backend)
	    (build-cgen-backend)
	    (cc-compiler ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    registerd backends ...                                           */
;*---------------------------------------------------------------------*/
(register-backend! 'c build-cgen-backend)
(register-backend! 'c-saw build-sawc-backend)

;*---------------------------------------------------------------------*/
;*    build-sawc-backend ...                                           */
;*---------------------------------------------------------------------*/
(define (build-sawc-backend)
   (instantiate::sawc
      (language 'c-saw)
      (heap-compatible 'c)
      (trace-support #f)
      (srfi0 'bigloo-c)
      (require-tailc #t)))
 
;*---------------------------------------------------------------------*/
;*    build-cgen-backend ...                                           */
;*---------------------------------------------------------------------*/
(define (build-cgen-backend)
   (instantiate::cgen
      (language 'c)
      (srfi0 'bigloo-c)))

;*---------------------------------------------------------------------*/
;*    backend-compile ...                                              */
;*---------------------------------------------------------------------*/
(define-method (backend-compile me::cvm)
   (let ((c-prefix (profile cgen (c-walk me))))
      (stop-on-pass 'cgen (lambda () 'done))
      (stop-on-pass 'distrib (lambda () 'done))
      (when (string? c-prefix)
	 (when (or (eq? *pass* 'cindent) *c-debug*) (indent c-prefix))
	 (stop-on-pass 'cindent (lambda () 'done)))
      c-prefix))

;*---------------------------------------------------------------------*/
;*    backend-link ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (backend-link me::cvm result)
   (when (string? result)
      (cc-compiler result #f)))

;*---------------------------------------------------------------------*/
;*    c-walk ...                                                       */
;*---------------------------------------------------------------------*/
(define (c-walk me::cvm)
   (pass-prelude (if (sawc? me) "C generation (saw)" "C generation (cgen)")
      (lambda () (start-emission! ".c")))
   
   ;; a very little comment 
   (emit-header)
   
   ;; emit the GC selection
   (emit-garbage-collector-selection)

   ;; if we are in debugging mode, we generate a macro
   (if (or (>fx *compiler-debug* 0) *c-debug*)
       (emit-debug-activation))
   
   ;; the include (both Bigloo's and user's ones)
   (emit-include)
   
   ;; emit the C user header
   (when (pair? *c-user-header*)
      (fprint *c-port* "/* User header */")
      (for-each (lambda (h) (fprint *c-port* h)) *c-user-header*))
   
   ;; C++ guard start
   (fprint *c-port* "#ifdef __cplusplus")
   (fprint *c-port* "extern \"C\" {")
   (fprint *c-port* "#endif")
   
   ;; we emit the generated type for the used classes
   (for-each-type!
      (lambda (t) (type-occurrence-set! t 0)))
   (for-each-global!
      (lambda (global)
	 (cond
	    ((and (eq? (global-module global) *module*)
		  (>fx (global-occurrence global) 0))
	     (type-increment-global! global))
	    ((require-prototype? global)
	     (type-increment-global! global)
	     (type-occurrence-increment! (global-type global))
	     (when (sfun? (global-value global))
		(for-each (lambda (a)
			     (cond
				((type? a)
				 (type-occurrence-increment! a))
				((local? a)
				 (type-occurrence-increment! (local-type a)))))
		   (sfun-args (global-value global))))))))
   (let ((fixpoint #f))
      (let loop ()
	 (unless fixpoint
	    (set! fixpoint #t)
	    (for-each (lambda (t::tclass)
			 (when (>fx (type-occurrence t) 0)
			    (with-access::tclass t (slots)
			       (for-each (lambda (t::slot)
					    (with-access::slot t (type)
					       (when (=fx (type-occurrence type) 0)
						  (type-occurrence-increment! type)
						  (set! fixpoint #f))))
				  slots))))
	       (get-class-list)))))
   (let ((classes (filter (lambda (t)
			     (>fx (type-occurrence t) 0))
		     (get-class-list))))
      (emit-class-types classes *c-port*))
   
   ;; we declare prototypes, first, we print the prototype of variables
   (emit-prototypes)
   
   ;; then we emit the constants values
   (emit-cnsts)
   
   ;; emit the GC roots, when demanded
   (when *gc-force-register-roots?*
      (gc-roots-emit *c-port*))
   
   (let ((globals (cvm-functions me)))
      
      ;; when compiling in bdb mode we have to emit the identifier
      ;; translation table.
      (when (>fx *bdb-debug* 0)
	 (emit-bdb-info globals *c-port*))
      
      ;; when compiling for profile we emit identifier translation table
      (when (>fx *profile-mode* 0)
	 (emit-prof-info globals *c-port*))
      
      ;; we print the C main...
      (if (and (or *main* (memq *pass* '(ld distrib)))
	       (not (eq? *main* 'imported)))
	  (emit-main))
      
      ;; we emit the dynamic loading init point
      (let ((mod-init (get-module-init)))
	 (if (and (bigloo-config 'have-dlopen)
		  (global? mod-init))
	     (cond
		((symbol? *dlopen-init*)
		 (emit-dlopen-init mod-init (symbol->string *dlopen-init*)))
		((string? *dlopen-init*)
		 (emit-dlopen-init mod-init *dlopen-init*))
		(*dlopen-init*
		 (emit-dlopen-init mod-init bgl-foreign-dlopen-init))
		(else
		 #unspecified))))
      ;; we now emit the code for all the Scheme functions
      (backend-compile-functions me)
      
      ;; C++ guard end
      (fprint *c-port* "#ifdef __cplusplus")
      (fprint *c-port* "}")
      (fprint *c-port* "#endif")
      
      ;; emit the C user foot
      (for-each (lambda (h) (fprint *c-port* h)) *c-user-foot*)
      
      (stop-emission!)))

;*---------------------------------------------------------------------*/
;*    cc-compiler ...                                                  */
;*---------------------------------------------------------------------*/
(define (cc-compiler c-prefix o-prefix)
   ;; set the target language
   (set-backend! 'c)
   ;; we invoke now the C compiler
   (cc c-prefix o-prefix (not (eq? *pass* 'cc)))
   (stop-on-pass 'cc (lambda () 'done))
   ;; and the linker
   (ld (if (string? o-prefix) o-prefix c-prefix) #f))

;*---------------------------------------------------------------------*/
;*    backend-cnst-table-name ::cvm                                    */
;*---------------------------------------------------------------------*/
(define-method (backend-cnst-table-name me::cvm offset)
   (if (=fx offset 0)
       "*__cnst"
       (string-append "__cnst[ " (number->string offset) " ] ")))

;*---------------------------------------------------------------------*/
;*    make-tmp-file-name ...                                           */
;*---------------------------------------------------------------------*/
(define (make-tmp-file-name)
   (make-file-name *bigloo-tmp*
		   (string-append "main-tmp"
				  "@"
				  (let ((user (getenv "USER")))
				     (if (not (string? user))
					 ""
					 user))
				  "."
				  (car *src-suffix*))))

;*---------------------------------------------------------------------*/
;*    backend-link-objects ::cvm ...                                   */
;*---------------------------------------------------------------------*/
(define-method (backend-link-objects me::cvm sources)
   (when (>fx *bdb-debug* 0) (bdb-setting!))
   (cond
      ((null? sources)
       (let ((first (prefix (car *o-files*))))
	  (warning "link" "No source file found" " -- " *o-files*)
	  (load-library-init)
	  (set! *o-files* (cdr *o-files*))
	  (ld first #f)))
      (else
       (let loop ((sources sources)
		  (clauses '())
		  (main #f)
		  (fmain "")
		  (libraries '()))
	  (if (null? sources)
	      (if (or main (not *auto-link-main*) (eq? *pass* 'so))
		  (let ((first (prefix (car *o-files*))))
		     ;; if libraries are used by some module we add them
		     ;; to the link
		     (for-each (lambda (lib)
				  (match-case lib
				     ((library . ?libs)
				      (for-each use-library! libs))
				     ((eval . ?clauses)
				      (for-each (match-lambda
						   ((library . ?libs)
						    (for-each use-library! libs)
						    (for-each add-eval-library! libs)))
					 clauses))))
			libraries)
		     ;; we load the library init files.
		     (load-library-init)
		     (set! *src-files* (list fmain))
		     (set! *o-files* (cdr *o-files*))
		     (ld first #f))
		  ;; let's generate a main, then link
		  (let ((tmp (make-tmp-file-name)))
		     (make-tmp-main tmp main (gensym 'module) clauses libraries)
		     (set! *src-files* (list tmp))
		     ;; we have to remove extra mco files before compiler
		     ;; otherwise the compiler will warn about that files.
		     (let loop ((ra  *rest-args*)
				(res '()))
			(cond
			   ((null? ra)
			    (set! *rest-args* (reverse! res)))
			   ((member (suffix (car ra)) *mco-suffix*)
			    (loop (cdr ra) res))
			   (else
			    (loop (cdr ra) (cons (car ra) res)))))
		     (unwind-protect
			(compiler)
			;; we load the library init files.
			(begin
			   (load-library-init)
			   (let* ((pre (prefix tmp))
				  (c-file (string-append pre ".c"))
				  (o-file (string-append
					     pre
					     "."
					     *c-object-file-extension*)))
			      (for-each (lambda (f)
					   (when (file-exists? f)
					      (delete-file f)))
				 (list tmp c-file o-file)))))
		     0))
	      (let ((port (open-input-file (caar sources))))
		 (if (not (input-port? port))
		     (error "" "Illegal file" (caar sources))
		     (let ((exp (expand (compiler-read port))))
			(close-input-port port)
			(match-case exp
			   ((module ?name . ?-)
			    (let ((libs (find-libraries (cddr exp)))
				  (nmain (find-main (cddr exp))))
			       (loop (cdr sources)
				  (cons (list name
					   (string-append
					      "\"" (caar sources) "\""))
				     clauses)
				  (or nmain main)
				  (if nmain (caar sources) fmain)
				  (append libs libraries))))
			   (else
			    (loop (cdr sources)
			       clauses
			       main
			       fmain
			       libraries)))))))))))

;*---------------------------------------------------------------------*/
;*    backend-subtype? ::cvm ...                                       */
;*---------------------------------------------------------------------*/
(define-method (backend-subtype? b::cvm t1 t2)
   ;; CARE Agree manuel ? Something better ?
   (or (eq? t1 t2)
       (eq? (type-id t1) (type-id t2))
       (eq? t2 *_*)
       (string=? (type-name t1) (type-name t2)) ))

;*---------------------------------------------------------------------*/
;*    backend-gc-init ::cvm ...                                        */
;*---------------------------------------------------------------------*/
(define-method (backend-gc-init b::cvm)
   '(pragma "BGL_GC_INIT()"))
