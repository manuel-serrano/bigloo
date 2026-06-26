;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/comptime/BackEnd/jvm.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 18 08:31:55 2012                          */
;*    Last change :  Fri Jun 26 07:44:31 2026 (serrano)                */
;*    Copyright   :  2012-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bigloo JVM backend driver                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_jvm
   (include "Engine/pass.sch")
   (import engine_param
	   engine_pass
	   engine_link
	   engine_compiler
	   tools_error
	   ast_var
	   ast_node
	   module_module
	   module_alibrary
	   module_eval
	   type_type
	   type_typeof
	   type_env
	   object_class
	   backend_backend
	   backend_bvm
	   backend_jvm_class
	   backend_c_main
	   read_jvm
	   jas_as
	   jas_peep
	   saw_jvm_compile
	   saw_jvm_ld
	   saw_jvm_inline
	   init_setrc
	   read_reader))

;*---------------------------------------------------------------------*/
;*    The backend                                                      */
;*---------------------------------------------------------------------*/
(register-backend! 'jvm build-jvm-backend)

;*---------------------------------------------------------------------*/
;*    build-jvm-backend ...                                            */
;*---------------------------------------------------------------------*/
(define (build-jvm-backend)
   (instantiate::jvm
      (language 'jvm)
      (name "jvm")
      (heap-suffix "jheap")
      (srfi0 'bigloo-jvm)
      (foreign-clause-support '(java))
      (strict-type-cast #f)
      (debug-support '(jvm))
      (bound-check #t)
      (type-check #t)
      (force-register-gc-roots #f)
      (retblock #f)))

;*---------------------------------------------------------------------*/
;*    backend-initialize! ::wasm ...                                   */
;*---------------------------------------------------------------------*/
(define-method (backend-initialize! o::jvm)
   (call-next-method)
   (unregister-srfi! 'rlimit))

;*---------------------------------------------------------------------*/
;*    backend-select! ::jvm ...                                        */
;*---------------------------------------------------------------------*/
(define-method (backend-select! me::jvm)
   (jvm-qualified-name-set! 'foreign (string->symbol *jvm-foreign-class-name*)))

;*---------------------------------------------------------------------*/
;*    backend-compile ...                                              */
;*---------------------------------------------------------------------*/
(define-method (backend-compile me::jvm)
   
   (define (emit classfile dest)
      (let ((dir *jvm-dir-name*))
	 (if (eq? *pass* 'jvmas)
	     (let ((port (if (not (string? dest))
			     (current-output-port)
			     (open-output-file (string-append dir "/" dest)))))
		(jvmasdump classfile port)
		(if (not (eq? port (current-output-port)))
		    (close-output-port port)))
	     (let* ((cname (if (not (string? dest))
			       (string-append dir "/a.class")
			       (string-append dir "/" dest)))
		    (port (open-output-binary-file cname)))
		(verbose 2 "        class: " cname "\n")
		(if (not (binary-port? port))
		    (error "jvm-dump" "Can't open file for output" cname))
		(with-handler
		   (lambda (e)
		      (when *rm-tmp-files*
			 (delete-file cname))
		      (raise e))
		   (unwind-protect
		      (jvm-as classfile port)
		      (close-binary-port port)))))))
   
   (pass-prelude "Jvm" start-jvm-emission!)
   (verbose 2 "      [module: " *module* " package: "
      (jvm-package-get *module*) "]"#\Newline)
   
   ;; set classes name and check path correctness
   (jvm-qname-set! me (class-qualified-type-name-get *module*))
   
   ;; if we are going to link and we have not found a main yet, we
   ;; have to produce a fake one
   (when (and (not *main*) *auto-link-main* (memq *pass* '(ld distrib)))
      (set! *main* (make-bigloo-main)))
   
   (jvm-check-package *module* *jvm-dir-name*)
   
   (let ((l* (saw_jvm-compile me)))
      (unless (eq? *pass* 'sawmill)
	 (let ((bname (cond
			 ((eq? *pass* 'ld)
			  (if (string? (jvm-qname me))
			      (addsuffix (prefix (basename (jvm-qname me))))
			      "a.class"))
			 ((not (string? *dest*))
			  (if (string? (jvm-qname me))
			      (addsuffix (prefix (basename (jvm-qname me))))
			      #f))
			 (else
			  (addsuffix (prefix (basename *dest*)))))))
	    
	    (emit (car l*) bname)
	    
	    (for-each (lambda (cf) (emit cf (jasname cf))) (cdr l*))
	    
	    (stop-on-pass 'cc (lambda () 'done))
	    (stop-on-pass 'jvmas (lambda () 'done))
	    (stop-on-pass 'jast (lambda () 'done))))))

;*---------------------------------------------------------------------*/
;*    jvm-check-package ...                                            */
;*    -------------------------------------------------------------    */
;*    We check that the class file name is compatible with the         */
;*    JVM qualified type name declared for the class.                  */
;*---------------------------------------------------------------------*/
(define (jvm-check-package module dir)
   
   (define (compare-path? base path)
      (let ((lbase (string-length base))
	    (lpath (string-length path)))
	 (if (< lpath lbase)
	     #f
	     (let loop ((rpath (-fx lpath 1))
			(rbase (-fx lbase 1)))
		(if (=fx rbase -1)
		    #t
		    (let ((cbase (string-ref base rbase))
			  (cpath (string-ref path rpath)))
		       (if (or (char=? cbase cpath)
			       (and (char=? cpath #\/)
				    (char=? cbase #\.)))
			   (loop (-fx rpath 1) (-fx rbase 1))
			   #f)))))))
   
   (let* ((qtype (class-qualified-type-name-get module))
	  (base (let ((pre (prefix qtype)))
		   (cond
		      ((string=? pre "") ".")
		      ((string=? pre qtype) ".")
		      (else pre)))))
      (unless (or (string=? base ".") (compare-path? (jvm-filename base) dir))
	 (warning
	    (format "Incompatible package name and class path for module \"~a\"."
	       *module*)
	    (format " fname: ~a\n" (jvm-filename base))
	    (format " qtype: ~a\n" qtype)
	    (format " base : ~a\n" base)
	    (format " dir  : ~a\n" dir)))))

(define *jvm-dir-name* ".")

(define (jvmasdump classfile port)
   (pp classfile port :width 10240 :case-sensitivity 'lower))

(define (addsuffix name)
   (string-append name
      (case *pass*
	 ((jast) ".jast")
	 ((jvmas) ".jas")
	 (else ".class"))))

(define (jasname cf)
   (match-case cf
      (((class ?name) . ?-)
       (addsuffix (symbol->string name)))))

;*---------------------------------------------------------------------*/
;*    jvm-filename ...                                                 */
;*---------------------------------------------------------------------*/
(define (jvm-filename name)
   (if (string? *jvm-directory*)
       (if (string=? name ".")
	   *jvm-directory*
	   (make-file-name *jvm-directory* name))
       name))

;*---------------------------------------------------------------------*/
;*    jvm-dirname ...                                                  */
;*---------------------------------------------------------------------*/
(define (jvm-dirname file)
   (let* ((dfile (dirname file))
	  (dir (jvm-filename dfile)))
      (if (and (not (string=? dfile ""))
	       (not (directory? dfile))
	       (not (file-exists? dfile))
	       (or (not (string? *jvm-directory*))
		   (directory? *jvm-directory*)))
	  ;; we create the necessary directories to put the JVM class file
	  (make-directories dir))
      dir))

;*---------------------------------------------------------------------*/
;*    start-jvm-emission! ...                                          */
;*---------------------------------------------------------------------*/
(define (start-jvm-emission!)
   (cond
      ((string? *dest*)
       (let ((dname (dirname *dest*)))
	  (if (not (string=? dname ""))
	      (set! *jvm-dir-name* (jvm-dirname *dest*)))))
      ((eq? *pass* 'ld)
       (if (pair? *src-files*)
	   (set! *jvm-dir-name* (jvm-dirname (car *src-files*))))))
   (if (not (and (file-exists? *jvm-dir-name*) (directory? *jvm-dir-name*)))
       (error "start-jvm-emission!"
	      "Can't write dest file because directory doesn't exist"
	      *jvm-dir-name*)
       #t))

      
;*---------------------------------------------------------------------*/
;*    Link                                                             */
;*---------------------------------------------------------------------*/
(define-method (backend-link me::jvm result)
   ;; CARE move the code here...
   (jvm-ld "" #f))

;*---------------------------------------------------------------------*/
;*    backend-cnst-table-name ::jvm                                    */
;*---------------------------------------------------------------------*/
(define-method (backend-cnst-table-name me::jvm offset)
   "__cnst")

;*---------------------------------------------------------------------*/
;*    make-link-package ...                                            */
;*---------------------------------------------------------------------*/
(define (make-link-package)
   "JVMMAIN")

;*---------------------------------------------------------------------*/
;*    make-link-module ...                                             */
;*---------------------------------------------------------------------*/
(define (make-link-module)
   (string->symbol (make-link-package)))

;*---------------------------------------------------------------------*/
;*    make-tmp-file-name ...                                           */
;*---------------------------------------------------------------------*/
(define (make-tmp-file-name)
   (string-append (make-link-package) ".bgl"))
 
;*---------------------------------------------------------------------*/
;*    use-libraries ...                                                */
;*---------------------------------------------------------------------*/
(define (use-libraries libs)
   (case (module5-default-version)
      ((5) (if (pair? libs) (use-library! (car libs))))
      (else (for-each use-library! libs))))

;*---------------------------------------------------------------------*/
;*    eval-libraries ...                                               */
;*---------------------------------------------------------------------*/
(define (eval-libraries libs)
   (case (module5-default-version)
      ((5) (if (pair? libs) (add-eval-library! (car libs))))
      (else (for-each add-eval-library! libs))))

;*---------------------------------------------------------------------*/
;*    backend-link-objects ::jvm ...                                   */
;*---------------------------------------------------------------------*/
(define-method (backend-link-objects me::jvm sources)
   
   (define (do-link mainobj module)
      (read-jfile)
      (jvm-ld mainobj module))
   
   (define (link-sources sources)
      (if (null? sources)
	  (let ((first (prefix (car *o-files*))))
	     (warning "link" "No source file found" " -- " *o-files*)
	     ;; we load the library init files.
	     (load-library-init)
	     (do-link first #f))
	  ;; build the module clause
	  (let loop ((sources sources)
		     (cls '())
		     (main-module #f)
		     (main #f)
		     (src '("" . ""))
		     (libraries '()))
	     (if (null? sources)
		 (if main
		     ;; no need to generate a main, one alread exists
		     (let ((first (prefix (car *o-files*))))
			(when (string=? (suffix (car src)) "bgl")
			   (module5-default-version-set! 5))
			;; if libraries are used by some module we add them
			;; to the link
			(for-each (lambda (lib)
				     (match-case lib
					((library . ?libs)
					 (use-libraries libs))
					((eval . ?clauses)
					 (for-each (match-lambda
						      ((library . ?libs)
						       (use-libraries libs)
						       (eval-libraries libs)))
					    clauses))))
			   libraries)
			;; we load the library init files.
			(load-library-init)
			(set! *src-files* (list (car src)))
			(do-link (cdr src) main-module))
		     ;; generate a main an link
		     (let ((tmp (make-tmp-file-name)))
			(make-tmp-main tmp main (make-link-module) cls libraries)
			(set! *src-files* (list tmp))
			;; we have to remove extra mco files before compiler
			;; otherwise the compiler will warn about that files.
			(let liip ((ra  *rest-args*)
				   (res '()))
			   (cond
			      ((null? ra)
			       (set! *rest-args* (reverse! res)))
			      ((member (suffix (car ra)) *mco-suffix*)
			       (liip (cdr ra) res))
			      (else
			       (liip (cdr ra) (cons (car ra) res)))))
			(unwind-protect
			   (compiler)
			   ;; we load the library init files.
			   (load-library-init)
			   (let* ((pre (prefix tmp))
				  (class-file (string-append pre ".class")))
			      (when *rm-tmp-files*
				 (when (file-exists? tmp)
				    (delete-file tmp)))))
			0))
		 (let ((port (open-input-file (caar sources))))
		    (if (not (input-port? port))
			(error "" "Illegal file" (caar sources))
			(let ((exp (compiler-read port)))
			   (close-input-port port)
			   (match-case exp
			      ((module ?name . ?-)
			       (let ((libs (find-libraries (cddr exp)))
				     (nmain (find-main (cddr exp))))
				  (loop (cdr sources)
				     (cons (cons name (caar sources)) cls)
				     (if nmain name main-module)
				     (or nmain main)
				     (if nmain (car sources) src)
				     (append libs libraries))))
			      (else
			       (loop (cdr sources)
				  cls main-module main src libraries))))))))))
   
   (with-trace 'linker "backend-line-objects ::jvm"
      (trace-item "sources=" sources)
      (link-sources sources)))

;*---------------------------------------------------------------------*/
;*    backend-check-inlines ::jvm ...                                  */
;*---------------------------------------------------------------------*/
(define-method (backend-check-inlines me::jvm)
   (unless *lib-mode* (check-jvm-inlines)))

;*---------------------------------------------------------------------*/
;*    backend-subtype? ::jvm ...                                       */
;*---------------------------------------------------------------------*/
(define-method (backend-subtype? b::jvm t1 t2)
   (or (eq? t1 t2)
       (eq? (type-id t1) (type-id t2))
       (and (tclass? t1) (eq? (type-id t2) 'object))
       (eq? (type-name t2) 'java.lang.Object)
       (eq? (type-name t1) (type-name t2))
       (is-subtype? t1 t2)
       (sub-type? t1 t2) ))
