;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/eval.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Oct 22 09:34:28 1994                          */
;*    Last change :  Mon Dec 20 17:08:19 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Bigloo evaluator                                                 */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/eval.texi@                                */
;*       @node Eval@                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __eval

   (extern  (export c-debug-repl "bgl_debug_repl")

	    (reset-console!::obj (::obj) "reset_console")
	    (macro sigsetmask::int (::int) "BGL_SIGSETMASK")
	    (macro sigint::int "SIGINT"))
   
   (java    (class foreign
	       (method static reset-console!::obj (::obj) "reset_console")
	       (method static sigsetmask::int (::int) "sigsetmask")
	       (field static sigint::int "SIGINT")))
 
   (import  __type
	    __object
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __param
	    __thread
	    
	    __pp_circle
	    
	    __reader
	    __intext
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __macro
	    __install_expanders
	    __progn
	    __expand
	    __evcompile
	    __evmeaning
	    __evaluate
	    __everror
	    __evprimop
	    __evenv
	    __evmodule

	    (extend.r.macro-env __match_normalize))
   
   (eval    (export *c-debug-repl-value*))

   (export  (eval ::obj #!optional (env (default-environment)))
	    (eval! ::obj #!optional (env (default-environment)))
	    (eval-evaluate-set! ::obj)
	    (byte-code-compile::bstring ::obj #!optional (env (default-environment)))
	    (byte-code-run::obj ::bstring)
	    (scheme-report-environment <version>)
	    (null-environment <version>)
	    (interaction-environment)
	    (default-environment)
	    *load-verbose*
	    (load <string> #!optional (env (default-environment)))
	    (loadq <string> #!optional (env (default-environment)))
	    (repl)
	    (c-debug-repl ::obj)
	    (quit)
	    (expand-define-macro <expression> <expander>)
	    (expand-define-hygiene-macro <expression> <expander>)
	    (expand-define-expander <expression> <expander>)
	    (expand-define-pattern <expression>)
	    (set-prompter! ::procedure)
	    (get-prompter::procedure)
	    *load-path*
	    *user-pass-name*
	    *user-pass*
	    (notify-assert-fail vars body loc)
	    *nil*
	    (transcript-on ::bstring)
	    (transcript-off)
	    (set-repl-printer! ::procedure)
	    (native-repl-printer::procedure))

   (option  (set! *unsafe-type* #f)))

;*---------------------------------------------------------------------*/
;*    byte-code-evaluate ...                                           */
;*---------------------------------------------------------------------*/
(define (byte-code-evaluate eexp env loc)
   (let ((cexp (evcompile eexp '() env '_ #f loc #t #t))
	 (denv::dynamic-env (current-dynamic-env)))
      (let ()
	 ;; it is needed to protect the stack trace frame of the caller
	 ($env-push-trace denv #unspecified #unspecified)
	 (let ((tmp (evmeaning cexp '() denv)))
	    ($env-pop-trace denv)
	    tmp))))

;*---------------------------------------------------------------------*/
;*    default-evaluate ...                                             */
;*    -------------------------------------------------------------    */
;*    DEFAULT-EVALUATE is very special. It must be explicitly typed    */
;*    as obj because since eval in called by evprimop before the eval  */
;*    module is initialized, the type of the variable must not         */
;*    allow the compiler to remove the test from EVAL!.                */
;*---------------------------------------------------------------------*/
(define default-evaluate::obj byte-code-evaluate)

;*---------------------------------------------------------------------*/
;*    eval-evaluate-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (eval-evaluate-set! comp)
   (case comp
      ((classic)
       (set! default-evaluate byte-code-evaluate))
      ((new)
       (set! default-evaluate evaluate2))
      (else
       (if (procedure? comp)
	   (set! default-evaluate comp)
	   (error "eval-evaluate-set!" "Illegal compiler" comp)))))

;*---------------------------------------------------------------------*/
;*    Expanders setup.                                                 */
;*    -------------------------------------------------------------    */
;*    The expanders are initialized by the initialization of the       */
;*    module EXPANDERS. However, in order to prevent Bigloo from       */
;*    cutting the import of that module, we force a call to one of     */
;*    its functions.                                                   */
;*---------------------------------------------------------------------*/
(install-all-expanders!)

;*---------------------------------------------------------------------*/
;*    eval ...                                                         */
;*---------------------------------------------------------------------*/
(define (eval exp #!optional (env (default-environment)))
   (eval/expander exp env expand default-evaluate))
 
;*---------------------------------------------------------------------*/
;*    eval! ...                                                        */
;*---------------------------------------------------------------------*/
(define (eval! exp #!optional (env (default-environment)))
   (let ((evaluate (if (procedure? default-evaluate)
		       default-evaluate
		       byte-code-evaluate)))
      (eval/expander exp env expand! evaluate)))

;*---------------------------------------------------------------------*/
;*    eval/expander ...                                                */
;*---------------------------------------------------------------------*/
(define (eval/expander exp::obj env expand::procedure evaluate::procedure)
   (let ((loc (get-source-location exp))
	 (sexp (if (procedure? *user-pass*) (*user-pass* exp) exp)))
      (if (and loc (> (bigloo-debug) 0))
	  (with-handler
	     (lambda (e)
		(eval-exception-handler e loc))
	     (evaluate (expand sexp) env loc))
	  (evaluate (expand sexp) env loc))))

;*---------------------------------------------------------------------*/
;*    eval-exception-handler ...                                       */
;*---------------------------------------------------------------------*/
(define (eval-exception-handler e loc)
   (when (and (&exception? e) (not (&exception-fname e)))
      (match-case loc
	 ((at ?fname ?loc)
	  (&exception-fname-set! e fname)
	  (&exception-location-set! e loc))))
   (raise e))

;*---------------------------------------------------------------------*/
;*    byte-code-compile ...                                            */
;*---------------------------------------------------------------------*/
(define (byte-code-compile exp #!optional (env (default-environment)))
   (let* ((loc (get-source-location exp))
	  (sexp  (if (procedure? *user-pass*) (*user-pass* exp) exp)))
      (obj->string
       (evcompile (expand sexp) '() env '_ #t loc #f #t))))

;*---------------------------------------------------------------------*/
;*    byte-code-run ...                                                */
;*---------------------------------------------------------------------*/
(define (byte-code-run byte-code::bstring)
   (with-handler
      evmeaning-exception-handler
      (evmeaning (string->obj byte-code) '() (current-dynamic-env))))

;*---------------------------------------------------------------------*/
;*    scheme-report-environment ...                                    */
;*---------------------------------------------------------------------*/
(define (scheme-report-environment version)
   (if (=fx version 5)
       'scheme-report-environment
       (error 'scheme-report-environment
	      "Version not supported"
	      version)))
   
;*---------------------------------------------------------------------*/
;*    null-environment ...                                             */
;*---------------------------------------------------------------------*/
(define (null-environment version)
   (if (=fx version 5)
       'null-environment
       (error 'scheme-report-environment
	      "Version not supported"
	      version)))
   
;*---------------------------------------------------------------------*/
;*    interaction-environment ...                                      */
;*---------------------------------------------------------------------*/
(define (interaction-environment)
   'interaction-environment)
   
;*---------------------------------------------------------------------*/
;*    default-environment ...                                          */
;*---------------------------------------------------------------------*/
(define (default-environment)
   (let ((m (eval-module)))
      (if (evmodule? m)
	  m
	  (interaction-environment))))
   
;*---------------------------------------------------------------------*/
;*    prompt ...                                                       */
;*---------------------------------------------------------------------*/
(define *prompt* (lambda (num)
		    (display num)
		    (display ":=> ")
		    (flush-output-port (current-output-port))))

;*---------------------------------------------------------------------*/
;*    set-prompter! ...                                                */
;*---------------------------------------------------------------------*/
(define (set-prompter! proc)
   (if (not (correct-arity? proc 1))
       (error 'set-prompter!
	      "argument has to be a procedure of 1 argument"
	      proc)
       (set! *prompt* proc)))

;*---------------------------------------------------------------------*/
;*    get-prompter ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-prompter::procedure)
   *prompt*)

;*---------------------------------------------------------------------*/
;*    Global repl parameters ...                                       */
;*---------------------------------------------------------------------*/
(define *repl-num* 0)
(define *repl-quit* (lambda (x) (%exit x)))

;*---------------------------------------------------------------------*/
;*    repl ...                                                         */
;*---------------------------------------------------------------------*/
(define (repl)
   (let ((repl-quit *repl-quit*)
	 (repl-num  *repl-num*))
      (bind-exit (quit)
	 (set! *repl-quit* quit)
	 (set! *repl-num* (+fx 1 *repl-num*))
	 (unwind-protect
	    (internal-repl)
	    (begin
	       (set! *repl-num* repl-num)
	       (set! *repl-quit* repl-quit))))
      (newline)
      (flush-output-port (current-output-port))))

;*---------------------------------------------------------------------*/
;*    get-eval-reader ...                                              */
;*---------------------------------------------------------------------*/
(define (get-eval-reader)
   (or (bigloo-load-reader)
       (lambda (p) (read p #t))))

;*---------------------------------------------------------------------*/
;*    internal-repl ...                                                */
;*---------------------------------------------------------------------*/
(define (internal-repl)
   (let ((old-intrhdl (get-signal-handler sigint))
	 (read (lambda ()
		  ((get-eval-reader) (current-input-port)))))
      (unwind-protect
	 (let loop ()
	    (bind-exit (re-enter-internal-repl)
	       ;; we setup ^C interupt
	       (letrec ((intrhdl (lambda (n)
				    (notify-interrupt n)
				    ;; we flush current input port
				    (reset-console! (current-input-port))
				    ;; we restore signal handling
				    (sigsetmask 0)
				    (re-enter-internal-repl #unspecified))))
		  (signal sigint intrhdl))
	       ;; and we loop until eof
	       (newline)
	       (let luup ()
		  (*prompt* *repl-num*)
		  (bind-exit (skip)
		     (let ((exp (with-exception-handler
				   (lambda (e)
				      (if (&error? e)
					  (begin
					     (evmeaning-reset-error!)
					     (error-notify e)
					     (when (eof-object? (&error-obj e))
						(reset-eof (current-input-port)))
					     (sigsetmask 0)
					     (skip #unspecified))
					  (raise e)))
				   read)))
			(if (eof-object? exp)
			    (quit)
			    (let ((v (with-exception-handler
					(lambda (e)
					   (if (&error? e)
					       (begin
						  (error-notify e)
						  (sigsetmask 0)
						  (skip #unspecified))
					       (raise e)))
					(lambda () (eval exp)))))
			       (if (not (eq? *transcript* (current-output-port)))
				   (fprint *transcript* ";; " exp))
			       (*repl-printer* v *transcript*)
			       (newline *transcript*)))))
		  (luup)))
	    (loop))
	 (if (procedure? old-intrhdl)
	     (signal sigint old-intrhdl)
	     (signal sigint (lambda (n) (exit 0)))))))

;*---------------------------------------------------------------------*/
;*    default-repl-printer ...                                         */
;*---------------------------------------------------------------------*/
(define (default-repl-printer exp . port)
   (apply display-circle exp port))

;*---------------------------------------------------------------------*/
;*    *repl-printer* ...                                               */
;*---------------------------------------------------------------------*/
(define *repl-printer* default-repl-printer)

;*---------------------------------------------------------------------*/
;*    set-repl-printer! ...                                            */
;*---------------------------------------------------------------------*/
(define (set-repl-printer! disp)
   (if (not (correct-arity? disp -2))
       (error 'set-repl-printer! "Illegal repl-printer (wrong arity)" disp)
       (let ((old *repl-printer*))
	  (set! *repl-printer* disp)
	  old)))

;*---------------------------------------------------------------------*/
;*    native-repl-printer ...                                          */
;*---------------------------------------------------------------------*/
(define (native-repl-printer)
   default-repl-printer)

;*---------------------------------------------------------------------*/
;*    *c-debug-repl-value* ...                                         */
;*---------------------------------------------------------------------*/
(define *c-debug-repl-value* #unspecified)

;*---------------------------------------------------------------------*/
;*    c-debug-repl ...                                                 */
;*---------------------------------------------------------------------*/
(define (c-debug-repl val)
   (set! *c-debug-repl-value* val)
   (let loop ()
      (display "?* ")
      (let ((exp ((get-eval-reader) (current-input-port))))
	 (unless (eof-object? exp)
	    (print (eval exp))
	    (loop)))))

;*---------------------------------------------------------------------*/
;*    quit ...                                                         */
;*---------------------------------------------------------------------*/
(define (quit)
   (*repl-quit* 0))

;*---------------------------------------------------------------------*/
;*    *load-path*                                                      */
;*---------------------------------------------------------------------*/
(define *load-path* '())

;*---------------------------------------------------------------------*/
;*    find-file ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-file name)
   (if (not (string? name))
       (error 'find-file "Illegal file name" name)
       (if (file-exists? name)
	   name
	   (let loop ((path *load-path*))
	      (if (null? path)
		  name
		  (let ((f (make-file-name (car path) name)))
		     (if (file-exists? f)
			 f
			 (loop (cdr path)))))))))

;*---------------------------------------------------------------------*/
;*    *load-verbose* ...                                               */
;*---------------------------------------------------------------------*/
(define *load-verbose* #t)

;*---------------------------------------------------------------------*/
;*    load ...                                                         */
;*---------------------------------------------------------------------*/
(define (load file-name #!optional (env (default-environment)))
   (loadv file-name *load-verbose* env))

(define (loadq file-name #!optional (env (default-environment)))
   (loadv file-name #f env))

(define (loadv file-name v? env)
   (let* ((path (find-file file-name))
	  (port (open-input-file path))
	  (read (get-eval-reader))
	  (mod ($eval-module)))
      (if (input-port? port)
	  (unwind-protect
	     (let* ((sexp (read port))
		    (mainsym (if (and (pair? sexp) (eq? (car sexp) 'module))
				 (let ((clause (assq 'main (cddr sexp))))
				    (if (pair? clause)
					(if (and (pair? (cdr clause))
						 (null? (cddr clause))
						 (symbol? (cadr clause)))
					    (cadr clause)
					    (error 'load
						   "Illegal main clause"
						   clause))))
				 #f)))
		(let loop ((sexp sexp))
		   (evmeaning-reset-error!)
		   (cond
		      ((eof-object? sexp)
		       (close-input-port port)
		       (when (symbol? mainsym)
			  (eval! `(,mainsym (command-line)) env))
		       path)
		      (else
		       (let ((v (eval! sexp env)))
			  (when v?
			     (display-circle v)
			     (newline))
			  (loop (read port)))))))
	     ($eval-module-set! mod))
	  (error 'load "Can't open file" file-name))))

;*---------------------------------------------------------------------*/
;*    evexpand-error ...                                               */
;*---------------------------------------------------------------------*/
(define (evexpand-error proc mes obj)
   (if (epair? obj)
       (match-case (cer obj)
	  ((at ?fname ?loc)
	   (error/location proc mes obj fname loc))
	  (else
	   (error proc mes obj)))
       (error proc mes obj)))

;*---------------------------------------------------------------------*/
;*    On met dans ce fichier les definitions de                        */
;*    `expand-define-expander' et `expand-define-macro' car elles      */
;*    contiennent des appels a `Eval'.                                 */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    expand-define-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (expand-define-expander x e)
   
   (define (install-define-expander name expd-lam expd-lam/loc expd-eval)
      (install-expander
       name
       (lambda (x e)
	  (if (not (procedure? expd-eval))
	      (evexpand-error name "illegal expander" x)
	      (if (not (correct-arity? expd-eval 2))
		  (evexpand-error name
				  "wrong number of argument for expand"
				  x)
		  (with-handler
		     (lambda (exc)
			(let ((nexc (if (&error? exc)
					(let ((obj (&error-obj exc)))
					   (if (epair? obj)
					       (match-case (cer obj)
						  ((at ?fname ?loc)
						   (duplicate::&error exc
						      (fname fname)
						      (location loc)))
						  (else
						   exc))
					       exc))
					exc)))
			   (raise nexc)))
		     (expd-eval x e))))))
      #unspecified)
   
   (match-case x
      ((?- (and (? symbol?) ?name) :eval! ?macro)
       (let* ((expd-lam macro)
	      (expd-lam/loc (evepairify expd-lam x))
	      (expd-eval (eval! expd-lam/loc)))
	  (install-define-expander name expd-lam expd-lam/loc expd-eval)))
      ((?- (and (? symbol?) ?name) . ?macro)
       (let* ((expd-lam (expand-progn macro))
	      (expd-lam/loc (evepairify expd-lam x))
	      (expd-eval (eval expd-lam/loc)))
	  (install-define-expander name expd-lam expd-lam/loc expd-eval)))
      (else
       (evexpand-error 'define-expander
		       "Illegal `define-expander' syntax"
		       x))))

;*---------------------------------------------------------------------*/
;*    expand-define-macro ...                                          */
;*---------------------------------------------------------------------*/
(define (expand-define-macro x e)
   (match-case x
      ((or (?- (?name . ?args) . ?body)
	   (?- ?name (lambda ?args . ?body)))
       (let ((fname (gensym))
	     (loc (gensym)))
	  (install-expander
	   name
	   (let* ((evexpd `(lambda (x1 e)
			      (let ((,fname #f) ,loc)
				 (when (epair? x1)
				    (match-case (cer x1)
				       ((at ?f ?l)
					(set! ,fname f)
					(set! ,loc l))))
				 (let* ((n (let* ,(destructure
						   name fname loc
						   args '(cdr x1) '())
					      ,(expand-progn body)))
					(ne (e n e)))
				    (evepairify* ne x1)))))
		  (evexpd/loc (evepairify evexpd x))
		  (expd-eval (eval! evexpd/loc)))
	      (lambda (x e)
		 (with-handler
		    (lambda (e)
		       (let ((ne (if (&error? e)
				     (let ((obj (&error-obj e)))
					(if (epair? obj)
					    (match-case (cer obj)
					       ((at ?fname ?loc)
						(duplicate::&error e
						   (fname fname)
						   (location loc)))
					       (else
						e))
					    e))
				     e)))
			  (raise e)))
		    (expd-eval x e))))))
       #unspecified)
      (else
       (evexpand-error 'define-macro "Illegal `define-macro' syntax" x))))

;*---------------------------------------------------------------------*/
;*    expand-define-hygiene-macro ...                                  */
;*---------------------------------------------------------------------*/
(define (expand-define-hygiene-macro x e)
   (match-case x
      ((?- (quote (?name . ?args)) . ?body)
       (let ((body (map cadr body))
	     (fname (gensym))
	     (loc (gensym)))
	  (install-expander
	   name
	   (let* ((expd-lam `(lambda (x e)
				(let ((,fname #f) ,loc)
				   (when (epair? x)
				      (match-case (cer x)
					 ((at ?f ?l)
					  (set! ,fname f)
					  (set! ,loc l))))
				   (e (let* ,(destructure
					      name fname loc
					      args '(cdr x) '())
					 ,(expand-progn body))
				      e))))
		  (expd-lam/loc (evepairify expd-lam x))
		  (expd-eval (eval! expd-lam/loc)))
	      (lambda (x e)
		 (with-handler
		    (lambda (e)
		       (let ((ne (if (&error? e)
				     (let ((obj (&error-obj e)))
					(if (epair? obj)
					    (match-case (cer obj)
					       ((at ?fname ?loc)
						(duplicate::&error e
						   (fname fname)
						   (location loc)))
					       (else
						e))
					    e))
				     e)))
			  (exception-notify ne)
			  (raise ne)))
		    (expd-eval x e)))))
	  #unspecified))
      (else
       (evexpand-error 'define-hygiene-macro
		       "Illegal `define-hygiene-macro' syntax"
		       x))))

;*---------------------------------------------------------------------*/
;*    destructure ...                                                  */
;*---------------------------------------------------------------------*/
(define (destructure id fname loc pat arg bindings)
   (define (err msg obj)
      `(if (string? ,fname)
	  (error/location ',id ,msg ',obj ,fname ,loc)
	  (error ',id ,msg ',obj)))
   (let loop ((pat pat)
	      (arg arg)
	      (bindings bindings))
      (cond
	 ((null? pat)
	  (cons `(,(gensym)
		  (if (not (null? ,arg))
		      ,(err "Too many arguments provided" arg)
		      '()))
		bindings))
	 ((symbol? pat)
	  (cons `(,pat ,arg) bindings))
	 ((pair? pat)
	  (loop (car pat)
		`(if (pair? ,arg)
		     (car ,arg)
		     ,(err "Missing value for argument" (car pat)))
		(loop (cdr pat) `(cdr ,arg) bindings)))
	 (else
	  (evexpand-error ',id "Illegal macro parameter" pat)))))

;*---------------------------------------------------------------------*/
;*    expand-define-pattern ...                                        */
;*---------------------------------------------------------------------*/
(define (expand-define-pattern x)
   (match-case x
      ((?- ?name ?var ?body)
       (extend.r.macro-env name (eval! `(lambda ,var ,body)))
       ''dummy)
      (else
       (evexpand-error 'expand-define-pattern "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    notify-assert-fail ...                                           */
;*---------------------------------------------------------------------*/
(define (notify-assert-fail vars fail-body loc)
   (let ((port (current-error-port)))
      (bind-exit (skip)
	 (with-exception-handler
	    (lambda (e)
	       (error-notify e)
	       (skip #unspecified))
	    (lambda ()
	       (if (pair? loc)
		   (error/location "assert"
				   "assertion failed"
				   fail-body
				   (car loc)
				   (cdr loc))
		   (error 'assert "assertion failed" fail-body)))))
      (fprint port "-----------------------")
      (fprint port "Variables' value are : ")
      (for-each (lambda (f)
		   (display "   " port)
		   (display f port)
		   (display " : " port)
		   (*repl-printer* (eval f) port)
		   (newline port))
		vars)
      (fprint port "-----------------------")
      (let ((old-prompter (get-prompter)))
	 (set-prompter! (lambda (num) (display "*:=> ")))
	 (repl)
	 (set-prompter! old-prompter))))

;*---------------------------------------------------------------------*/
;*    *nil* ...                                                        */
;*---------------------------------------------------------------------*/
(define *nil* #t)

;*---------------------------------------------------------------------*/
;*    *user-pass* ...                                                  */
;*---------------------------------------------------------------------*/
(define *user-pass*      (unspecified))  ;; l'eventuelle user pass 
(define *user-pass-name* "User")         ;; le nom de la user pass

;*---------------------------------------------------------------------*/
;*    *transcript* ...                                                 */
;*---------------------------------------------------------------------*/
(define *transcript* (current-output-port))

;*---------------------------------------------------------------------*/
;*    transcript-on ...                                                */
;*---------------------------------------------------------------------*/
(define (transcript-on file::bstring)
   (if (not (eq? *transcript* (current-output-port)))
       (error 'transcript-on "A transcript is already in use" *transcript*)
       (begin
	  (set! *transcript* (append-output-file file))
	  (fprint *transcript* ";; session started on " (date))
	  #unspecified)))

;*---------------------------------------------------------------------*/
;*    transcript-off ...                                               */
;*---------------------------------------------------------------------*/
(define (transcript-off)
   (if (eq? *transcript* (current-output-port))
       (error 'transcript-off
	      "No transcript is currently in use"
	      *transcript*)
       (begin
	  (close-output-port *transcript*)
	  (set! *transcript* (current-output-port))))
   #unspecified)
   
