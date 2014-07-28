;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cc/ld.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 17 09:37:55 1992                          */
;*    Last change :  Mon Jul 28 09:20:44 2014 (serrano)                */
;*    Copyright   :  1992-2014 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The (system) link.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module cc_ld
   (export  (ld name ::bool)
	    (library-suffixes::pair))
   (import  tools_speek
	    tools_error
	    tools_misc
	    backend_backend
	    cc_exec
	    engine_param
	    engine_configure
	    module_eval))

;*---------------------------------------------------------------------*/
;*    ld ...                                                           */
;*---------------------------------------------------------------------*/
(define (ld name need-to-return)
   (cond
      ((string=? (os-class) "unix")
       (unix-ld name need-to-return))
      ((string=? (os-class) "mingw")
       (unix-ld name need-to-return))
      ((string=? (os-class) "win32")
       (win32-ld name))
      (else
       (user-error "ld" "Unknown os" (os-class)))))

;*---------------------------------------------------------------------*/
;*    library-suffix ...                                               */
;*---------------------------------------------------------------------*/
(define (library-suffixes)
   (cond
      (*profile-library* '("_p" "_s"))
      (*unsafe-library* '("_u" "_s"))
      (else '("_s"))))

;*---------------------------------------------------------------------*/
;*    library-eval-suffix ...                                          */
;*---------------------------------------------------------------------*/
(define (library-eval-suffixes)
   (cond
      (*profile-library* '("_ep" "_es"))
      (*unsafe-library* '("_eu" "_es"))
      (else '("_es"))))

;*---------------------------------------------------------------------*/
;*    secondary-library-suffix ...                                     */
;*---------------------------------------------------------------------*/
(define (secondary-library-suffix)
   "_s")
   
;*---------------------------------------------------------------------*/
;*    profile-gc-debug-library-suffix ...                              */
;*---------------------------------------------------------------------*/
(define (profile-gc-debug-library-suffix)
   (cond
      (*profile-library* "_p")
      (else "")))
   
;*---------------------------------------------------------------------*/
;*    library->os-file ...                                             */
;*---------------------------------------------------------------------*/
(define (library->os-file library suffixes staticp forcep foreignp)
   (let ((backend (backend-srfi0 (the-backend))))
      (if (or foreignp
	      (and *ld-relative*
		   (not *profile-library*)
		   (not (or staticp *static-all-bigloo?* *static-bigloo?*))))
	  (string-append (cond
			    ((or (string=? (os-class) "unix")
				 (string=? (os-class) "mingw"))
			     "-l")
			    ((string=? (os-class) "win32")
			     "")
			    (else
			     (user-error "ld" "Unknown os" (os-class))))
			 (if foreignp
			     (symbol->string library)
			     (library-file-name library (car suffixes) backend)))
	  (let loop ((ss suffixes))
	     (if (null? ss)
		 (if staticp
		     (error "bigloo"
			    (string-append "Can't find any `"
					   (symbol->string library)
					   "' library")
			    *lib-dir*)
		     (library->os-file library suffixes #t forcep foreignp))
		 (let* ((fname ((if staticp
				    make-static-lib-name
				    make-shared-lib-name)
				(library-file-name library (car ss) backend)
				backend))
			(name (find-file/path fname *lib-dir*)))
		    (if (string? name)
			name
			(begin
			   (warning "bigloo"
				    (string-append "Can't find library \""
						   fname
						   "\"")
				    " in path \"" *lib-dir* "\"")
			   (loop (cdr ss))))))))))

;*---------------------------------------------------------------------*/
;*    unix-ld ...                                                      */
;*---------------------------------------------------------------------*/
(define (unix-ld name need-to-return)
   
   (define (rpath-options)
      (let ((rpathfmt (bigloo-config 'c-compiler-rpath)))
	 (if (string-null? rpathfmt)
	     ""
	     (format " ~( )"
		(map (lambda (path)
			(format rpathfmt path))
		   (delete-duplicates *cflags-rpath*))))))
   
   (verbose 1 "   . ld (" *cc* ")" #\Newline)
   ;; we add additional, machine specific, link options.
   (let ((staticp (or (not (bigloo-config 'have-shared-library))
		      (string-case *ld-options*
			 ((: (* all) "-static")
			  #t)
			 (else
			  #f)))))
      (if staticp
	  (set! *ld-options* (string-append (bigloo-config 'static-link-option)
				" " *ld-options*))
	  (set! *ld-options* (string-append (bigloo-config 'shared-link-option)
				" " *ld-options*)))
      (let* ((dest (if (string? *dest*)
		       *dest*
		       (default-executable-name)))
	     ;; the standard bigloo library
	     (bigloo-lib (library->os-file *bigloo-lib*
			    (library-suffixes)
			    (or *static-all-bigloo?*
				*static-bigloo?*
				staticp)
			    #f #f))
	     ;; the garbarge collector library
	     (gc-lib (library->os-file *gc-lib*
			(list (profile-gc-debug-library-suffix)
			   "")
			(or *profile-library*
			    *static-all-bigloo?*
			    *static-bigloo?*
			    staticp)
			#f
			(not *gc-custom?*)))
	     ;; the eval libraries
	     (eval-libs (let loop ((lib (get-eval-libraries))
				   (res ""))
			   (if (null? lib)
			       res
			       (loop (cdr lib)
				  (string-append
				     (library->os-file
					(car lib)
					(library-eval-suffixes)
					(or *static-all-bigloo?* staticp)
					#f #f)
				     " "
				     res)))))
	     ;; the extra bigloo libraries
	     (add-libs (let loop ((lib (delete-duplicates
					  *additional-bigloo-libraries*))
				  (res  ""))
			  (if (null? lib)
			      res
			      (loop (cdr lib)
				 (string-append
				    (library->os-file
				       (car lib)
				       (library-suffixes)
				       (or *static-all-bigloo?* staticp)
				       #f #f)
				    " "
				    res)))))
	     ;; the extra user C libraries
	     (other-libs (let loop ((lib (reverse *bigloo-user-lib*))
				    (res ""))
			    (if (null? lib)
				res
				(loop (cdr lib)
				   (string-append (car lib) " " res)))))
	     (ld-args (string-append
			 ;; object file name
			 (unix-filename name "." *c-object-file-extension*) " "
			 ;; to be linked with files
			 (string*->string (map unix-filename *with-files*))
			 ;; other object files
			 (string*->string (map unix-filename *o-files*))
			 ;; the executable name
			 " " *ld-o-option* (unix-filename dest)
			 ;; cc options
			 " "  (format "~( )" *cc-options*)
			 ;; rpath options
			 (rpath-options)
			 ;; optional debug option
			 (if (or *c-debug* (>fx *bdb-debug* 0))
			     (string-append " " *ld-debug-option*)
			     "")
			 ;; optional executable stripping
			 (if *strip*
			     (string-append " " (bigloo-config 'c-strip-flag))
			     "")
			 ;; user ld options
			 " " *ld-options*
			 ;; the library path
			 (let loop ((path *lib-dir*))
			    (if (null? path)
				""
				(string-append "-L"
				   (car path)
				   " "
				   (loop (cdr path)))))
			 ;; the gc library -path
			 (let ((s (bigloo-config 'non-custom-gc-directory)))
			    (if (>fx (string-length s) 0)
				(string-append "-L" s)
				""))
			 ;; ld optimization flags
			 (if (not *c-debug*)
			     (string-append " " *ld-optim-flags*)
			     "")
			 ;; additional eval Bigloo libraries
			 " " eval-libs
			 ;; additional Bigloo libraries
			 " " add-libs
			 ;; standard bigloo library
			 " " bigloo-lib
			 ;; standard GC library
			 " " gc-lib
			 ;; dloptn library
			 " " (bigloo-config 'dlopen-lib)
			 ;; user libraries
			 " " other-libs
			 ;; then we insert a second time the additional libs
			 " " (if *double-ld-libs?* add-libs "")
			 ;; post user ld options
			 " " (format "~( )" *ld-post-options*)))
	     (cmd         (string-append *cc* " " ld-args)))
	 (verbose 2 "      ["  cmd #\] #\Newline)
	 (exec cmd need-to-return "ld"))))

;*---------------------------------------------------------------------*/
;*    win32-ld ...                                                     */
;*---------------------------------------------------------------------*/
(define (win32-ld name)
   (verbose 1 "   . ld (" *cc* ")" #\Newline)
   ;; we add additional, machine specific, link options.
   (let ((staticp (or 
		   (not (bigloo-config 'have-shared-library))
		   (string-case *ld-options*
		      ((: (* all) "-static")
		       #t)
		      (else
		       #f)))))
      (if staticp
	  (set! *ld-options* (string-append (bigloo-config 'static-link-option)
				" " *ld-options*))
	  (set! *ld-options* (string-append (bigloo-config 'shared-link-option)
				" " *ld-options*)))
      ;; when compiling for multithreading, add a translation for the
      ;; Bigloo library
      (when *multi-threaded-gc?*
	 (library-translation-table-add! *bigloo-lib* "bigloo_mt"))
      (let* ((dest (if (string? *dest*)
                       *dest*
                       (default-executable-name)))
             ;; the standard bigloo library
             (bigloo-lib (library->os-file *bigloo-lib*
			    (library-suffixes)
			    #t #f #f))
	     ;; the extra bigloo libraries
	     (add-libs (let loop ((lib *additional-bigloo-libraries*)
				  (res  '()))
			  (if (null? lib)
			      res
			      (loop (cdr lib)
				 (cons
				    (library->os-file
				       (car lib)
				       (library-suffixes)
				       (or *static-all-bigloo?* staticp) #f #f)
				    res)))))
	     ;; the extra user C libraries
	     (other-libs  *bigloo-user-lib*)
	     (ld-args     (append
			     ;; object file name
			     (list (string-append name
				      "."
				      *c-object-file-extension*))
			     ;; to be linked with files
			     *with-files*
			     ;; other object files
			     *o-files*
			     ;; the executable name
			     (list (string-append *ld-o-option* dest))
			     ;; cc options
			     (append-map (lambda (o)
					    (string-split-char o #\space))
				*cc-options*)
			     ;; linker options
			     (list "/link")
			     ;; optional debug option
			     (if (or *c-debug* (>fx *bdb-debug* 0))
				 (string-split-char *ld-debug-option* #\space)
				 '())
			     ;; ld optimization flags
			     (if (not *c-debug*)
				 (string-split-char *ld-optim-flags* #\space)
				 '())
			     ;; optional executable stripping
			     (if (and *strip*
				      (not (string=? (bigloo-config 'c-strip-flag) "")))
				 (list (bigloo-config 'c-strip-flag))
				 '())
			     ;; user ld options
			     (string-split-char *ld-options* #\space)
			     ;; the library path
			     (let loop ((path *lib-dir*))
				(if (null? path)
				    '()
				    (cons (string-append "/LIBPATH:" (car path))
				       (loop (cdr path)))))
			     ;; additional Bigloo libaries
			     add-libs
			     ;; standard bigloo library
			     (list bigloo-lib)
			     ;; dloptn library
			     (if (string=? (bigloo-config 'dlopen-lib) "")
				 '()
				 (list (bigloo-config 'dlopen-lib)))
			     ;; user libraries
			     other-libs
			     ;; then we insert a second time the additional libs
			     (if *double-ld-libs?* add-libs '())
			     ;; post user ld options
			     (append-map (lambda (o)
					    (string-split-char o #\space))
				*ld-post-options*))))
	 (verbose 2 "      " (map (lambda (str)
				     (string-append "[" str "]"))
				(cons *cc* ld-args))
	    #\Newline)
	 (apply run-process *cc* (append ld-args '(wait: #t))))))
