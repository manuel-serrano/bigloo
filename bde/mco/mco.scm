;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/mco/mco.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 31 07:37:29 1998                          */
;*    Last change :  Wed Jan 16 10:27:43 2008 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bigloo module checksum generator.                            */
;*    -------------------------------------------------------------    */
;*    We have implemented that application in order to get rid of      */
;*    Bigloo to produce module checksum. Bigloo has a startup time     */
;*    that is too long and that make it not suitable to compute        */
;*    efficiently modules checksum.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_param
   (import (module_checksum "../comptime/Module/checksum.scm"))
   (export *mco-include-path*)
   (main main))

;*---------------------------------------------------------------------*/
;*    Global parameters                                                */
;*---------------------------------------------------------------------*/
(define *mco-version* "0.1")
(define *mco-files* '())
(define *mco-verbose* #t)
(define *mco-output* #f)
(define *mco-include-path* '("."))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; we parse command line arguments
   (parse-args argv)
   ;; we now start prof.
   (mco))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args cmd-args)
   (define (usage args-parse-usage level)
      (print "usage: bglmco -o output files...")
      (newline)
      (args-parse-usage #f)
      (newline)
      (exit 0))
   (args-parse (cdr cmd-args)
      (("?")
       (usage args-parse-usage 1))
      ((("-help" "--help") (help "This help message"))
       (usage args-parse-usage 1))
      (("-v" (help "Verbose mode"))
       (set! *mco-verbose* #t))
      (("-s" (help "Silent mode"))
       (set! *mco-verbose* #f))
      (("-o" ?file (help "mco file name"))
       (set! *mco-output* file))
      (("-I" ?path (help "Add <name> to the include path"))
       (set! *mco-include-path* (cons path *mco-include-path*))
       (set! *load-path* (cons path *load-path*)))
      (else
       (set! *mco-files* (cons else *mco-files*))))
   ;; we clean up the configuration setting
   (if (not (pair? *mco-files*))
       (error "mco" "No source file specified" #f))
   (set! *mco-files* (reverse! *mco-files*))
   ;; if not output file is specified, we use the basename of the
   ;; first source file
   (if (not (string? *mco-output*))
       (set! *mco-output* (string-append (prefix (car *mco-files*)) ".mco"))))

;*---------------------------------------------------------------------*/
;*    mco ...                                                          */
;*    -------------------------------------------------------------    */
;*    We open all the input file in order to find the module clause    */
;*---------------------------------------------------------------------*/
(define (mco)
   (if *mco-verbose*
       (hello-world *mco-files*))
   (let ((mod (find-module-clause *mco-files*)))
      (if (not mod)
	  (error "mco" "Can't find module clause" *mco-files*)
	  (let ((checksum (module-checksum mod *mco-include-path*)))
	     (if (not (mco-checksum=? checksum *mco-output*))
		 (emit-checksum checksum *mco-output*))))))

;*---------------------------------------------------------------------*/
;*    hello-world ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function is called in verbose mode to display the files     */
;*    that are processed by this mco session.                          */
;*---------------------------------------------------------------------*/
(define (hello-world files)
   (if (pair? files)
       (let loop ((files files))
	  (cond
	     ((null? (cdr files))
	      (print (car files) ":"))
	     (else
	      (display (car files))
	      (display " ")
	      (loop (cdr files)))))))
   
;*---------------------------------------------------------------------*/
;*    mco-checksum=? ...                                               */
;*    -------------------------------------------------------------    */
;*    Compares if the computed checksum is the same that the one       */
;*    currently contained in the mco file.                             */
;*---------------------------------------------------------------------*/
(define (mco-checksum=? checksum file)
   (if (not (file-exists? file))
       #f
       (let* ((port (open-input-file file))
	      (num  (unwind-protect (read port)
				    (close-input-port port))))
	  (eq? num checksum))))

;*---------------------------------------------------------------------*/
;*    emit-checksum ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-checksum checksum file)
   (if (file-exists? file)
       (delete-file file))
   (let ((port (open-output-file file)))
      (unless (output-port? port)
	 (error 'bglmco "Cannot open file for output" file))
      (fprint port checksum)
      (close-output-port port)))

;*---------------------------------------------------------------------*/
;*    find-module-clause ...                                           */
;*    -------------------------------------------------------------    */
;*    Open all the SRC files in order to find the module clause.       */
;*---------------------------------------------------------------------*/
(define (find-module-clause src)
   (define module?
      (match-lambda
	 ((module (? symbol?) . ?-)
	  #t)
	 (else
	  #f)))
   (let loop ((src src))
      (if (null? src)
	  #f
	  (if (file-exists? (car src))
	      (let* ((port (open-input-file (car src)))
		     (mod  (unwind-protect (let ((exp (read port)))
					      (if (module? exp)
						  exp
						  #f))
					   (close-input-port port))))
		 (if mod
		     mod
		     (loop (cdr src))))))))
