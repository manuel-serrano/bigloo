(module backend_dotnet
   (include "Engine/pass.sch")
   (import engine_param
	   engine_pass
	   tools_error
	   module_module
	   module_alibrary
	   type_type
	   ast_var
	   engine_link
	   engine_compiler
	   backend_backend
	   backend_bvm
	   backend_dotnet_class
	   backend_c_main	; BAD make-bigloo-main
	   read_jvm	; BAD module->qualified-type
	   cc_exec	; BAD exec
	   msil_compile
	   msil_ld
	   msil_inline
	   init_setrc
	   read_reader))

;*---------------------------------------------------------------------*/
;*    registered backend                                               */
;*---------------------------------------------------------------------*/
(register-backend! '.net build-dotnet-backend)

;*---------------------------------------------------------------------*/
;*    build-dotnet-backend ...                                         */
;*---------------------------------------------------------------------*/
(define (build-dotnet-backend)
   (instantiate::dotnet
      (language '.net)
      (heap-compatible 'jvm)
      (heap-suffix "jheap")
      (srfi0 'bigloo-.net)
      (foreign-clause-support '(java))
      (debug-support '())))

;*---------------------------------------------------------------------*/
;*    Compilation                                                      */
;*---------------------------------------------------------------------*/
(define-method (backend-compile me::dotnet)
   ;; the dotnet prelude (hello message and *DEST* update)
   (pass-prelude ".Net (saw)" start-dotnet-emission!)
   (verbose 2 "      [module: " *module* " qualified type name: "
	    (dotnet-qname me) "]"#\Newline)
   ;; CARE: BPS, fix the backend qualified name !!
   (dotnet-qname-set! me (module->qualified-type *module*))
   ;; if we are going to link and we have not found a main yet, we
   ;; have to produce a fake one
   (if (and (not *main*) (memq *pass* '(ld distrib)))
       (set! *main* (make-bigloo-main)))
   ;; the dotnet driver
   (define (emit dest)
      (let ((dir *dotnet-dir-name*))
	 (let ((port (if (not (string? dest))
			 (current-output-port)
			 (open-output-file (make-file-name dir dest)))))
	    (msil-compile me port)
	    (if (not (eq? port (current-output-port)))
		(close-output-port port)))))
   (let ((bname (cond
		   ((eq? *pass* 'ld)
		    (if (pair? *src-files*)
			(addsuffix (prefix (basename (car *src-files*))))
			"a.class"))
		   ((not (string? *dest*))
		    (if (pair? *src-files*)
			(addsuffix (prefix (basename (car *src-files*))))
			#f))
		   (else
		    (addsuffix (prefix (basename *dest*)))))))
      ;; assembly code emission
      (cond
	 ((eq? *pass* 'il)
	  (emit bname))
	 (*dotnet-use-external-asm*
	  (emit bname)
	  (dotnet-external-asm (make-file-name *dotnet-dir-name* bname)))
	 (else
	  #unspecified)))
   (stop-on-pass 'il (lambda () 'done))
   (stop-on-pass 'cc (lambda () 'done))
   'ok )

(define *dotnet-dir-name* ".")

(define (addsuffix name)
   (string-append name ".il") )

(define (ilname cf)
   (match-case cf
      (((class ?name) . ?-)
       (addsuffix (symbol->string name)))))

;*---------------------------------------------------------------------*/
;*    start-dotnet-emission! ...                                       */
;*---------------------------------------------------------------------*/
(define (start-dotnet-emission!)
   (cond
      ((string? *dest*)
       (let ((dname (dirname *dest*)))
	  (if (not (string=? dname ""))
	      (set! *dotnet-dir-name* (dirname *dest*)))))
      ((eq? *pass* 'ld)
       (if (pair? *src-files*)
	   (set! *dotnet-dir-name* (dirname (car *src-files*))))))
   (if (not (and (file-exists? *dotnet-dir-name*)
		 (directory? *dotnet-dir-name*) ))
       (error "start-dotnet-emission!"
	      "Can't write dest file because directory doesn't exist"
	      *dotnet-dir-name*)
       #t))

;*---------------------------------------------------------------------*/
;*    *dotnet-external-asms* ...                                       */
;*---------------------------------------------------------------------*/
(define *dotnet-external-asms* 
   (list (cons 'pnet dotnet-external-pnet-asm)))

;*---------------------------------------------------------------------*/
;*    dotnet-external-asm ...                                          */
;*---------------------------------------------------------------------*/
(define (dotnet-external-asm name)
   (let* ((id *dotnet-external-asm-style*)
	  (c (assq id *dotnet-external-asms*)))
      (if (and (pair? c)
	       (procedure? (cdr c))
	       (correct-arity? (cdr c) 1))
	  ((cdr c) name)
	  (error "asm (dotnet)"
		 (apply string-append
			"Unknown linker style, supported: "
			(map (lambda (x)
				(if (and (pair? x)
					 (symbol? (car x)))
				    (string-append (symbol->string (car x))
						   " ")
				    ""))
			     *dotnet-external-asms*))
		 *dotnet-external-asm-style*))))

;*---------------------------------------------------------------------*/
;*    dotnet-external-pnet-asm ...                                     */
;*---------------------------------------------------------------------*/
(define (dotnet-external-pnet-asm name)
   (let* ((il (string-append (prefix name) ".il"))
	  (cmd (string-append *dotnet-external-asm* " " il)))
      (verbose 1 "   . ilasm (" *dotnet-external-asm* ")" #\Newline)
      (verbose 2 "      ["  cmd #\] #\Newline)
      (let ((res (exec cmd #t "ilasm")))
	 (when *rm-tmp-files* (delete-file il))
	 res)))
      
;*---------------------------------------------------------------------*/
;*    Link                                                             */
;*---------------------------------------------------------------------*/
(define-method (backend-link me::dotnet result)
   ;; CARE move the code here...
   (dotnet-ld) )

;*---------------------------------------------------------------------*/
;*    backend-cnst-table-name ::dotnet                                 */
;*---------------------------------------------------------------------*/
(define-method (backend-cnst-table-name me::dotnet offset)
   "__cnst")

;*---------------------------------------------------------------------*/
;*    make-link-package ...                                            */
;*---------------------------------------------------------------------*/
(define (make-link-package)
   "dotnetmain")

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
;*    backend-link-objects ::dotnet ...                                */
;*---------------------------------------------------------------------*/
(define-method (backend-link-objects me::dotnet sources)
   (define (do-link first)
      (set! *src-files* '())
      (dotnet-ld))
   (if (null? sources)
       (let ((first (prefix (car *o-files*))))
	  (warning "link" "No source file found" " -- " *o-files*)
	  ;; we load the library init files.
	  (load-library-init)
	  (do-link first))
       ;; on construit la clause du module
       (let loop ((sources   sources)
		  (cls       '())
		  (main      #f)
		  (fmain     "")
		  (libraries '()))
	  (if (null? sources)
	      (if main
		  ;; ce n'est pas la peine de generer un main, il y en a
		  ;; deja un
		  (let ((first (prefix (car *o-files*))))
		     ;; if libraries are used by some module we add them
		     ;; to the link
		     (for-each (lambda (lib)
				  (match-case lib
				     ((library . ?libs)
				      (for-each (lambda (lib)
						   (use-library! lib 'now))
						libs))))
			       libraries)
		     ;; we load the library init files.
		     (load-library-init)
		     (set! *src-files* (list fmain))
		     (do-link first))
		  ;; on genere un main puis on link.
		  (let ((tmp (make-tmp-file-name)))
		     (make-tmp-main tmp main (make-link-module) cls libraries)
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
			(load-library-init)
			(let* ((pre (prefix tmp))
			       (obj-file (string-append pre ".obj")))
			   (when (file-exists? tmp)
			      (delete-file tmp))
			   (when (file-exists? obj-file)
			      (delete-file obj-file))))
		     0))
	      (let ((port (open-input-file (caar sources))))
		 (if (not (input-port? port))
		     (error "" "Illegal file" (caar sources))
		     (let ((exp (compiler-read port)))
			(close-input-port port)
			(match-case exp
			   ((module ?name ??- (main ?new-main) . ?-)
			    (add-qualified-type!
			     name
			     (string-replace (jvm-class-sans-directory
					      (prefix (cdar sources)))
					     (file-separator)
					     #\.))
			    (if main
				(error ""
				       (string-append
					"Redeclaration of the main (files "
					fmain
					" and "
					(caar sources) ")")
				       (cons main new-main)))
			    (loop (cdr sources)
				  (cons (list name
					      (string-append
					       "\"" (caar sources) "\""))
					cls)
				  new-main
				  (caar sources)
				  (append (find-libraries (cddr exp))
					  libraries)))
			   ((module ?name . ?-)
			    (add-qualified-type!
			     name
			     (string-replace (jvm-class-sans-directory
					      (prefix (cdar sources)))
					     (file-separator)
					     #\.))
			    (loop (cdr sources)
				  (cons (list name
					      (string-append
					       "\"" (caar sources) "\""))
					cls)
				  main
				  fmain
				  (append (find-libraries (cddr exp))
					  libraries)))
			   (else
			    ;; ah, ce n'etait pas un fichier bigloo,
			    ;; on saute (en meprisant :-)
			    (loop (cdr sources)
				  cls
				  main
				  fmain
				  libraries))))))))))   

;*---------------------------------------------------------------------*/
;*    backend-check-inlines ::dotnet ...                               */
;*---------------------------------------------------------------------*/
(define-method (backend-check-inlines me::dotnet)
   (unless *lib-mode* (check-msil-inlines)))
