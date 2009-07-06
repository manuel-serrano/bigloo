;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Init/parse-args.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 29 11:56:20 1995                          */
;*    Last change :  Fri Aug 11 15:00:04 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Command-line parsing                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module init_parse-args
   (export  (parse-args args::pair))
   (import  engine_param
	    tools_version))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args cmd-args)
   (bind-exit (exit)
      (args-parse (cdr cmd-args)
	 ;; help
	 (section "Help")
	 (("--help" (synopsis "-help|--help" "Print this message."))
	  (help args-parse-usage)
	  (exit #f))
	 (("-help")
	  (help args-parse-usage)
	  (exit #f))
	 (("--bare-help")
	  (bare-help args-parse-usage)
	  (exit #f))
	 (("?")
	  (help args-parse-usage)
	  (exit #f))
	 ;; gdb compatibility
	 (section "Gdb compatibility")
	 (("--quiet" (synopsis "Do not print version number on startup."))
	  (set! *verbose* -1))
	 ;; language
	 (section "Mode and Language")
	 (("-mode"
	   ?mode
	   (synopsis "-mode [mixte|scheme|c]"
		     "Set Bdb default mode (mixte default)"))
	  (cond
	     ((string=? mode "mixte")
	      (set! *bdb-mode* 'mixte)
	      (set! *heap-explorer?* #t))
	     ((string=? mode "scheme")
	      (set! *bdb-mode* 'scheme)
	      (set! *heap-explorer?* #t))
	     ((string=? mode "c")
	      (set! *bdb-mode* 'c)
	      (set! *heap-explorer?* #f))
	     (else
	      (help args-parse-usage)
	      (exit #f))))
	 (("-suffix" ?suffix (synopsis "Recognize suffix as Scheme source"))
	  (set! *src-suffix* (cons suffix *src-suffix*)))
	 ;; bdb debugging configuration
	 (section "Bdb debug configuration")
	 (("-v" (synopsis "-v[23]" "Set verbose mode on."))
	  (set! *verbose* 1))
	 (("--v" (synopsis "--verbose" "Print version information and exit"))
	  (short-version)
	  (exit #f))
	 (("--verbose")
	  (short-version)
	  (exit #f))
	 (("-v2")
	  (if (<fx *verbose* 2)
	      (set! *verbose* 2)))
	 (("-v3")
	  (if (<fx *verbose* 3)
	      (set! *verbose* 3)))
	 (("-v?rest")
	  (error "init-parse-args" "Illegal `-v' argument" rest))
	 (("-t" (synopsis "-t[234]" "Enables tracing mode"))
	  (set! *verbose* 4))
	 (("-t2")
	  (set! *verbose* 5))
	 (("-t3")
	  (set! *verbose* 6))
	 (("-t4")
	  (set! *verbose* 7))
	 (("-t?rest")
	  (error "init-parse-args" "Illegal `-t' argument" rest))
	 (("-active-prompt" (synopsis "Enables active prompt (default)"))
	  (set! *active-prompt* #t))
	 (("-no-active-prompt" (synopsis "Disables active prompt"))
	  (set! *active-prompt* #f))
	 ;; bdb environment configuration
	 (section "Bdb environment")
	 (("--bee" (synopsis "--bee" "Runs Bdb as a Bee client"))
	  (set! *bee-client?* #t))
	 (("--emacs" (synopsis "--emacs|--fullname" "Runs Bdb as an Emacs client"))
	  (set! *emacs-client?* #t))
	 ((("--fullname" "-fullname"))
	  (set! *emacs-client?* #t))
	 (("--root-dir" ?dir (synopsis "Sets the Bee root directory"))
	  (set! *root-directory* dir))
	 (("--afile" ?afile (synopsis "Sets the Bee bglafile file name"))
	  (set! *afile* afile))
	 (("--etags" ?etags (synopsis "Sets the Bee bgltags file name"))
	  (set! *etags* etags))
	 (("--no-demangling" (synopsis "Disable demangling (i.e., C mode)"))
	  (set! *bdb-mode* 'c))
	 (("--no-heap-explorer" (synopsis "Disable heap explorer (i.e., C mode)"))
	  (set! *heap-explorer?* #f))
	 ;; gdb setting
	 (section "Gdb settings")
	 (("--gdb" ?gdb (synopsis "The gdb binary file"))
	  (set! *gdb* gdb))
	 ;; executable
	 (else
	  (if (string? *exec*)
	      (set! *args* (cons else *args*))
	      (set! *exec* else))))))
	
;*---------------------------------------------------------------------*/
;*    query ...                                                        */
;*---------------------------------------------------------------------*/
(define (query)
   (version)
   (newline)
   (exit 0))

;*---------------------------------------------------------------------*/
;*    help ...                                                         */
;*---------------------------------------------------------------------*/
(define (help usage)
   (version)
   (print "usage: bdb [options] executable-file")
   (newline)
   (bare-help usage))

;*---------------------------------------------------------------------*/
;*    bare-help ...                                                    */
;*---------------------------------------------------------------------*/
(define (bare-help usage)
   (usage #f)
   (newline)
   (print "Runtime Command file:")
   (print "   - ~/.bdbrc"))


   
