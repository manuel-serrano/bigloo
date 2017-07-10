;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/main.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov  2 17:24:13 1992                          */
;*    Last change :  Sun Jul  9 09:35:03 2017 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The recette entry point                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module main
   
   (main   recette)
   
   (import vital
	   bps
	   hash
	   bool
	   list
	   vector
	   srfi4
	   struct
	   print
	   bchar
	   string
	   kwote
	   case
	   bind-exit
	   vararity
	   apply
	   globalisation
	   glo_cell
	   kapture
	   filtre
	   match
	   rgc-trap
	   rgc-jm
	   rgc-eval
	   rgc-insert
	   rgc
	   lalr
	   input-port
	   mmap
	   read
	   callcc
	   fringe
	   tail
	   external
	   sqic
	   reval
	   inline
	   letrec
	   macro
	   flonum
	   number
	   bignum
	   define
	   cse
	   error
	   include
	   0cfa
	   sua
	   alias
	   alias-aux
	   module
	   import1
	   import2
	   object
	   object-sans
	   object5
	   object5-sans
	   hygiene
	   wind
	   dsssl
	   peek
	   unicode
	   optim
	   pregexp
	   system
	   date
           process
           weakptr
	   crypto
	   crc)
   (export (do-test name thunk good?)
	   (test-module name file)
	   *recette-port*
	   *bigloo-path*
	   *silent*)
   
   (option (bigloo-debug-set! 0)))
   
;*---------------------------------------------------------------------*/
;*    Des variables statiques                                          */
;*---------------------------------------------------------------------*/
(define *test-number*   0)
(define *nb-test*       0)
(define *nb-err*        0)
(define *module-name*   "")
(define *verbose*       #f)
(define *silent*        #t)
(define *recette-port*  #f)
(define *tick-number*   -1)
(define *callcc?*       #t)
(define *dumping*       #f)
(define *modules*       '())
(define *bigloo-path*   (if (string=? (os-class) "unix")
                            "../bin/bigloo.sh"
                            "..\\bin\\bigloo.exe"))

;*---------------------------------------------------------------------*/
;*    tick ...                                                         */
;*---------------------------------------------------------------------*/
(define (tick)
   (set! *tick-number* (+fx 1 *tick-number*))
   (if (=fx *tick-number* 4)
       (set! *tick-number* 0))
   (write-char (integer->char 8))
   (case *tick-number*
      ((0) (write-char #\|))
      ((1) (write-char #\/))
      ((2) (write-char #\-))
      ((3) (write-char #\\))))

;*---------------------------------------------------------------------*/
;*    if-module ...                                                    */
;*---------------------------------------------------------------------*/
(define (if-module sym thunk)
   (if (or (null? *modules*) (memq sym *modules*))
       (thunk)))

;*---------------------------------------------------------------------*/
;*    do-test ...                                                      */
;*---------------------------------------------------------------------*/
(define (do-test name thunk wanted)
   (set! *test-number* (+ 1 *test-number*))
   (set! *nb-test* (+ 1 *nb-test*))
   (define (correct? result wanted)
      (or (equal? result wanted)
	  (and (flonum? result)
	       (flonum? wanted)
	       (<fl (absfl (-fl result wanted)) 0.00001))))
   (let ((err (cons 1 2)))
      (define (test-fail result)
	 (set! *nb-err* (+ 1 *nb-err*))
	 (display* *test-number* #\. *module-name* "(" name ") ")
	 (if (eq? result err)
	     (display "abort: ")
	     (begin
		(display* "fail: ")
		(display "provided [" )
		(write-circle result)
		(display "], ")))
	 (display "wanted [")
	 (write-circle wanted)
	 (print "]"))
      ;; reset error notifiers
      (cond-expand
	 (bigloo2.7
	  #f)
	 (else
	  '(set! *error-notifier* #f)))
      (bind-exit (esc)
	 (with-exception-handler
	    (lambda (e)
	       (newline (current-error-port))
	       (test-fail err)
	       (flush-output-port (current-output-port))
	       (flush-output-port (current-error-port))
	       (error-notify e)
	       (newline (current-error-port))
	       (esc #f))
	    (lambda ()
	       (let ((result (thunk)))
		  (if (correct? result wanted)
		      (begin
			 (if (not *silent*)
			     (begin
				(display* *test-number* #\. *module-name* " : "
					  name " --> ")
				(display "ok.")))
			 (if *verbose*
			     (begin
				(display " [")
				(write-circle result)
				(print "]"))
			     (if (not *silent*)
				 (newline))))
		      (test-fail result))))))))

;*---------------------------------------------------------------------*/
;*    test-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-module module-name file-name)
   (set! *module-name* module-name)
   (set! *test-number* 0)
   (if (not *silent*)
       (newline))
   (print file-name ":"))

;*---------------------------------------------------------------------*/
;*    recette-resume ...                                               */
;*---------------------------------------------------------------------*/
(define (recette-resume inc)
   (set! *nb-err* (+fx inc *nb-err*))
   (close-output-port *recette-port*)
   (print #\Newline "------------------------------")
   (if (> *nb-err* 0)
       (begin
	  (fprint (current-error-port) "recette failed: "
	     *nb-err* " error(s) occurred.")
	  -1)
       (begin
	  (print "recette succeeded: the " *nb-test*
	     " tests are clear.")
	  0)))

;*---------------------------------------------------------------------*/
;*    recette ...                                                      */
;*---------------------------------------------------------------------*/
(define (recette argv)
   (args-parse (cdr argv)
      (section "Misc")
      ((("-help" "--help") (help "This help message"))
       (args-parse-usage #f)
       (exit 0))
      (section "Verbosity")
      (("-v" (help "Be verbose"))
       (set! *silent* #f)
       (set! *verbose* #f))
      (("-V" (help "Be verbose and show values"))
       (set! *silent* #f)
       (set! *verbose* #t))
      (("--verbose" ?level (help "Verbosisty (1 = -v, 2 = -V, else = error)"))
       (cond
	  ((string=? level "1")
	   (set! *silent* #f)
	   (set! *verbose* #f))
	  ((string=? level "2")
	   (set! *silent* #f)
	   (set! *verbose* #t))
	  (else
	   (error 'recette "Illegal `--verbose' argument" level))))
      (("-args-parse" ?dum1 ?dum2 (help "Use this option to check ags-parse"))
       (print "dummy1=" dum1 " dummy2=" dum2))
      (("--args-parse" ?dum1 ?dum2 (help "--args-parse dummy1 dummy2"
					 "Use this option to check ags-parse"))
       (print "dummy1=" dum1 " dummy2=" dum2))
      (("-a?dummy" (help "Use this option to check ags-parse"))
       (print "dummy=" dummy))
      (("-A?dummy" (help "-a<dummy>" "Use this option to check ags-parse"))
       (print "dummy=" dummy))
      (("--no-call/cc" (help "Don't check for call/cc"))
       (set! *callcc?* #f))
      (("--dump" ?fname (help "Don't test but dump a binary structure"))
       (set! *dumping* fname))
      ((("-m" "--module") ?mod (help "Module to be tested"))
       (set! *modules* (cons (string->symbol mod) *modules*)))
      (("--bigloo" ?bigloo-path (help "Path to the Bigloo compiler"))
       (set! *bigloo-path* bigloo-path))
      (else
       (error 'recette "Illegal argument" else)))
   (if (string? *dumping*)
       (begin
	  (dump-obj *dumping*)
	  (print #\Newline "------------------------------")
	  (print (cond-expand
		    (bigloo-c "C dump done...")
		    (bigloo-jvm "JVM dump done...")
		    (bigloo-.net ".NET dump done..."))))
       (begin
	  (set! *recette-port* (open-output-file "recette.log"))
	  (if (not (output-port? *recette-port*))
	      (error "recette-port" "Can't open output-file" "recette.log"))
	  (if-module 'vital test-vital)
	  (if-module 'bps test-bps)
	  (if-module 'cell test-cell)
	  (if-module 'modulel test-modulel)
	  (if-module 'hash test-hash)
	  (if-module 'bool test-bool)
	  (if-module 'number test-number)
	  (if-module 'flonum test-flonum)
	  (if-module 'bignum test-bignum)
	  (if-module 'list test-list)
	  (if-module 'vector test-vector)
	  (if-module 'srfi4 test-srfi4)
	  (if-module 'struct test-struct)
	  (if-module 'print test-print)
	  (if-module 'char test-char)
	  (if-module 'string test-string)
	  (if-module 'kwote test-kwote)
	  (if-module 'case test-case)
	  (if-module 'bind-exit test-bind-exit)
	  (if-module 'vararity test-vararity)
	  (if-module 'apply test-apply)
	  (if-module 'globalisation test-globalisation)
	  (if-module 'kapture test-kapture)
	  (if-module 'filtre test-filtre)
	  (if-module 'match test-match)
	  (if-module 'rgc test-rgc)
	  (if-module 'rgc-trap test-rgc-trap)
	  (if-module 'rgc-jm test-rgc-jm)
	  (if-module 'rgc-eval test-rgc-eval)
	  (if-module 'rgc-insert test-rgc-insert)
	  (if-module 'lalr test-lalr)
	  (if-module 'input-port test-input-port)
	  (if-module 'mmap test-mmap)
	  (if-module 'read test-read)
	  (if *callcc?*
	      (begin
		 (if-module 'callcc test-callcc)
		 (if-module 'fringe test-fringe)
		 (if-module 'wind test-wind)))
	  (if-module 'dsssl test-dsssl)
	  (if-module 'tail test-tail)
	  (if-module 'external test-external)
	  (if-module 'sqic test-sqic)
	  (if-module 'eval test-eval)
	  (if-module 'inline test-inline)
	  (if-module 'letrec test-letrec)
	  (if-module 'macro test-macro)
	  (if-module 'define test-define)
	  (if-module 'cse test-cse)
	  (if-module 'error test-error)
	  (if-module 'include test-include)
	  (if-module '0cfa test-0cfa)
	  (if-module 'sua test-sua)
	  (if-module 'alias test-alias)
	  (if-module 'object test-object)
	  (if-module 'object-sans test-object-sans)
	  (if-module 'object5 test-object5)
	  (if-module 'object5-sans test-object5-sans)
	  (if-module 'hygiene test-hygiene)
	  (if-module 'peek test-peek)
	  (if-module 'unicode test-unicode)
	  (if-module 'optim test-optim)
	  (if-module 'pregexp test-pregexp)
	  (if-module 'system test-system)
	  (if-module 'date test-date)
	  (if-module 'weakptr test-weakptr)
	  (if-module 'crc test-crc)
	  (cond-expand
	     (bigloo-.net #t)
	     (else (if-module 'process test-process)))
	  (if-module 'wind (lambda () (test-wind-sans-handler recette-resume)))
	  (recette-resume 0))))

 
 
