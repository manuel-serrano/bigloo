;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMsil/dnld.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec  5 11:22:04 2002                          */
;*    Last change :  Wed Dec 16 05:39:25 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The DotNet link and shell script generation                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module msil_ld
   (import engine_param
	   engine_configure
	   engine_link
	   tools_speek
	   tools_error
	   tools_misc
	   read_reader
	   module_module
	   ast_var
	   type_type
	   cc_ld
	   cc_exec
	   backend_backend)
   (export (dotnet-ld))
   (eval   (export *dotnet-linkers*)
	   (export *dotnet-sh-clrs*)
	   (export *dotnet-msdos-clrs*)))

;*---------------------------------------------------------------------*/
;*    *dotnet-linkers* ...                                             */
;*---------------------------------------------------------------------*/
(define *dotnet-linkers* 
   (list (cons 'pnet make-pnet-ld-command)
	 (cons 'mono make-mono-ld-command)))

;*---------------------------------------------------------------------*/
;*    *dotnet-sh-clrs* ...                                             */
;*---------------------------------------------------------------------*/
(define *dotnet-sh-clrs* 
   (list (cons 'pnet generate-pnet-sh)
	 (cons 'mono generate-mono-sh)
	 (cons 'rotor generate-rotor-sh)))

;*---------------------------------------------------------------------*/
;*    *dotnet-msdos-clrs* ...                                          */
;*---------------------------------------------------------------------*/
(define *dotnet-msdos-clrs* 
   (list (cons 'pnet generate-pnet-msdos)
	 (cons 'mono generate-mono-msdos)
	 (cons 'rotor generate-rotor-msdos)))

;*---------------------------------------------------------------------*/
;*    dotnet-library-suffix ...                                        */
;*---------------------------------------------------------------------*/
(define (dotnet-library-suffix)
   (cond
      (*profile-library* "_p")
      ((and *unsafe-library* (not *purify*)) "_u")
      (else "_s")))

;*---------------------------------------------------------------------*/
;*    dotnet-ld ...                                                    */
;*---------------------------------------------------------------------*/
(define (dotnet-ld)
   (let* ((target (if (string? *dest*)
		      *dest*
		      (default-script-name)))
	  (backend (backend-srfi0 (the-backend)))
	  (exename (string-append (prefix target) ".exe"))
	  (bigloo-lib (library-file-name *bigloo-lib*
					 (dotnet-library-suffix)
					 (backend-srfi0 (the-backend))))
	  (add-libs (map (lambda (lib)
			    (make-shared-lib-name
			     (library-file-name lib
						(dotnet-library-suffix)
						backend)
			     backend))
			 *additional-bigloo-libraries*))
	  (sobjects (string*->string (map (lambda (s)
					     (string-append (prefix s) ".obj"))
					  *src-files*)))
	  (objects (string-append sobjects
				  (string*->string *with-files*)
				  (string*->string *o-files*)))
	  (cmd (make-ld-command exename
				objects
				*lib-dir*
				(cons bigloo-lib add-libs))))
      (verbose 1 "   . ld (" *dotnet-ld* ")" #\Newline)
      (verbose 2 "      ["  cmd #\] #\Newline)
      (exec cmd #t "ld")
      (generate-dotnet-script target (dotnet-libdir))))

;*---------------------------------------------------------------------*/
;*    dotnet-libdir ...                                                */
;*---------------------------------------------------------------------*/
(define (dotnet-libdir)
   (if (string? *dotnet-dll-path*)
       *dotnet-dll-path*
       (bigloo-config 'dll-directory)))

;*---------------------------------------------------------------------*/
;*    make-ld-command ...                                              */
;*---------------------------------------------------------------------*/
(define (make-ld-command exe objects path libs::pair)
   (let* ((id (string->symbol *dotnet-ld-style*))
	  (c (assq id *dotnet-linkers*)))
      (if (and (pair? c)
	       (procedure? (cdr c))
	       (correct-arity? (cdr c) 4))
	  ((cdr c) exe objects path libs)
	  (error "ld (dotnet)"
		 (apply string-append
			"Unknown linker style, supported: "
			(map (lambda (x)
				(if (and (pair? x)
					 (symbol? (car x)))
				    (string-append (symbol->string (car x))
						   " ")
				    ""))
			     *dotnet-linkers*))
		 *dotnet-ld-style*))))

;*---------------------------------------------------------------------*/
;*    generate-dotnet-script ...                                       */
;*---------------------------------------------------------------------*/
(define (generate-dotnet-script target path)
   (verbose 1 "   . " *dotnet-shell*)
   (verbose 2 " (" target ")")
   (verbose 1 #\Newline)
   (cond
      ((string=? *dotnet-shell* "sh")
       (generate-dotnet-shell-script target path *dotnet-sh-clrs*))
      ((string=? *dotnet-shell* "msdos")
       (generate-dotnet-shell-script target path *dotnet-msdos-clrs*))
      (else
       (warning "generate-dotnet-script"
		"Illegal shell `" *dotnet-shell* "' -- using `sh'")
       (generate-dotnet-shell-script target path *dotnet-sh-clrs*))))

;*---------------------------------------------------------------------*/
;*    generate-dotnet-shell-script ...                                 */
;*---------------------------------------------------------------------*/
(define (generate-dotnet-shell-script target path lst)
   (let* ((id (string->symbol *dotnet-clr-style*))
	  (c (assq id lst)))
      (if (and (pair? c)
	       (procedure? (cdr c))
	       (correct-arity? (cdr c) 2))
	  ((cdr c) target path)
	  (error "ld (dotnet)"
		 (apply string-append
			"Unknown clr style, supported: "
			(map (lambda (x)
				(if (and (pair? x)
					 (symbol? (car x)))
				    (string-append (symbol->string (car x))
						   " ")
				    ""))
			     lst))
		 *dotnet-clr-style*))))

;*---------------------------------------------------------------------*/
;*    make-pnet-ld-command ...                                         */
;*---------------------------------------------------------------------*/
(define (make-pnet-ld-command exe objects path libs::pair)
   (let ((lpath (let loop ((path path))
		   (if (null? path)
		       ""
		       (string-append "-L \""
				      (car path)
				      "\" "
				      (loop (cdr path)))))))
      (apply string-append
	     *dotnet-ld* " "
	     lpath
	     " -o " exe
	     " -- "
	     objects
	     (map (lambda (lib) (string-append " -l" lib)) libs))))

;*---------------------------------------------------------------------*/
;*    generate-pnet-sh ...                                             */
;*---------------------------------------------------------------------*/
(define (generate-pnet-sh target path)
   (with-output-to-file target
      (lambda ()
	 (print "#!/bin/sh")
	 (newline)
	 (print "DOTNETCLR=" *dotnet-clr*)
	 (print "$DOTNETCLR " *dotnet-clr-opt*
		" $BIGLOODOTNETOPT $BUGLOODOTNETOPT "
		" -L \"" path "\" "
		(prefix target) ".exe "
		"$*")))
   (chmod target 'read 'write 'execute))

;*---------------------------------------------------------------------*/
;*    generate-pnet-msdos ...                                          */
;*---------------------------------------------------------------------*/
(define (generate-pnet-msdos target path)
   (with-output-to-file target
      (lambda ()
	 (print "@" *dotnet-clr* " " *dotnet-clr-opt*
		 " %BIGLOODOTNETOPT% %BUGLOODOTNETOPT% "
		 " -L \"" (uncygdrive path) "\" "
		 (prefix target) ".exe"
		 " %*")))
   (chmod target 'read 'write 'execute))

;*---------------------------------------------------------------------*/
;*    make-mono-ld-command ...                                         */
;*---------------------------------------------------------------------*/
(define (make-mono-ld-command exe objects path libs::pair)
   (let ((lpath (let loop ((path path))
		   (if (null? path)
		       ""
		       (string-append "-L\""
				      (car path)
				      "\" "
				      (loop (cdr path)))))))
      (apply string-append
             *dotnet-ld* " "
             lpath
             " -o " exe
             " -- "
             objects
             (map (lambda (lib) (string-append " -l" lib)) libs))))

;*---------------------------------------------------------------------*/
;*    generate-mono-sh ...                                             */
;*---------------------------------------------------------------------*/
(define (generate-mono-sh target path)
   (with-output-to-file target
      (lambda ()
	 (print "#!/bin/sh")
	 (newline)
	 (print "DOTNETCLR=" *dotnet-clr*)
	 (print "MONO_PATH=$MONO_PATH:" (uncygdrive path) " $DOTNETCLR "
		*dotnet-clr-opt*
		" $BIGLOODOTNETOPT $BUGLOODOTNETOPT "
		(prefix target) ".exe "
		"$*")))
   (chmod target 'read 'write 'execute))

;*---------------------------------------------------------------------*/
;*    generate-mono-msdos ...                                          */
;*---------------------------------------------------------------------*/
(define (generate-mono-msdos target path)
   (with-output-to-file target
      (lambda ()
	 (print "@set MONO_PATH=\""
		(uncygdrive (string-replace path #\/ #\\))
		"\"\n"
		"@" *dotnet-clr* " " *dotnet-clr-opt*
		" %BIGLOODOTNETOPT% %BUGLOODOTNETOPT% "
		(prefix target) ".exe "
		"%*")))
   (chmod target 'read 'write 'execute))

;*---------------------------------------------------------------------*/
;*    generate-rotor-sh ...                                            */
;*---------------------------------------------------------------------*/
(define (generate-rotor-sh target path)
   (with-output-to-file target
      (lambda ()
	 (print "#!/bin/sh")
	 (newline)
	 (print "DOTNETCLR=" *dotnet-clr*)
	 (print "$DOTNETCLR* " *dotnet-clr-opt*
		" $BIGLOODOTNETOPT $BUGLOODOTNETOPT "
		(prefix target) ".exe "
		"$*")))
   (chmod target 'read 'write 'execute))

;*---------------------------------------------------------------------*/
;*    generate-rotor-msdos ...                                         */
;*---------------------------------------------------------------------*/
(define (generate-rotor-msdos target path)
   (with-output-to-file target
      (lambda ()
	 (print "@" *dotnet-clr* " " *dotnet-clr-opt*
		" %BIGLOODOTNETOPT% %BUGLOODOTNETOPT% "
		(prefix target) ".exe "
		"%*")))
   (chmod target 'read 'write 'execute))
