;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Init/parse-args.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 29 11:56:20 1995                          */
;*    Last change :  Sun Dec 30 09:05:07 2007 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Command-line parsing                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module init_parse-args
   (export  (parse-args args::pair))
   (import  engine_param
	    parser_lexer
	    write_version))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args cmd-args)
   (args-parse (cdr cmd-args)
;*--- test preliminiaire ----------------------------------------------*/
	 (("-" ?name (help "A source file name."))
	  (set! *src* (cons name *src*)))
;*--- L'aide ----------------------------------------------------------*/
	 (("?") (help args-parse-usage))
	 (("-help") (help args-parse-usage))
;*--- la version ------------------------------------------------------*/
	 (("-version" (help "The current cigloo release."))
	  (print *cigloo-name*)
	  (exit 0))
	 (("-revision" (help "The current cigloo release (short format)."))
	  (print *cigloo-version* (if (char? *cigloo-level*)
				      *cigloo-level*
				      ""))
	  (exit 0))
;*--- query -----------------------------------------------------------*/
	 (("-query" (help "Dump the current configuration."))
	  (query))
;*--- -q --------------------------------------------------------------*/
	 (("-q" (help "Do not load rc file."))
	  'nothing)
;*--- Le nom du resultat ----------------------------------------------*/
	 (("-o" ?name (help "Name the output file <name>."))
	  (set! *dest* name))
;*--- --to-stdout -----------------------------------------------------*/
	 (("--to-stdout" (help "Write C code on current output channel."))
	  (set! *verbose* -1)
	  (set! *dest* '--to-stdout))
;*--- Les options de verbosite ----------------------------------------*/
	 (("-s" (help "Be silent."))
	  (set! *verbose* -1))
	 (("-v" (help "-v[23]" "Be verbose."))
	  (set! *verbose* 1))
	 (("-v2")
	  (set! *verbose* 2))
	 (("-v3")
	  (set! *verbose* 3))
	 (("-w" (help "Inhibit all warning messages."))
	  (bigloo-warning-set! 0))
	 (("-Wall" (help "Warn about all possible errors."))
	  (bigloo-warning-set! 2))
;*--- open includes ---------------------------------------------------*/
	 (("-open-include" ?name (help "Open include <name>."))
	  (if (or (pair? *open-include*) (null? *open-include*))
	      (set! *open-include* (cons name *open-include*))))
	 (("-open-includes" (help "Open all includes."))
	  (set! *open-include* 'all))
;*--- scan includes ---------------------------------------------------*/
	 (("-scan-include" ?name (help "Scan include <name>."))
	  (if (or (pair? *scan-include*) (null? *open-include*))
	      (set! *scan-include* (cons name *scan-include*))))
	 (("-scan-includes" (help "Scan all includes."))
	  (set! *scan-include* 'all))
;*--- -I option -------------------------------------------------------*/
	 (("-I" ?name (help "-I <name> | -I<name>"
				"Add <name> to the include directories list."))
	  (set! *include-path* (cons name *include-path*)))
	 (("-I?name")
	  (set! *include-path* (cons name *include-path*)))
;*--- define ----------------------------------------------------------*/
	 (("-define" (help "Produce clauses for #define directives that do not take arguments [default]."))
	  (set! *define* #t))
	 (("-no-define" (help "Don't produce clauses for #define directives that do not take arguments."))
	  (set! *define* #f))
;*--- define-fun ------------------------------------------------------*/
	 (("-define-fun" (help "Produce clauses for #define directives that take arguments [default]."))
	  (set! *define-fun* #t))
	 (("-no-define-fun" (help "Don't produce clauses for #define directives that take arguments"))
	  (set! *define-fun* #f))
;*--- type ------------------------------------------------------------*/
	 (("-type" ?name (help "Add the type <name> to cigloo."))
	  (define-type-id name))
;*--- type ------------------------------------------------------------*/
	 (("-opaque-type" ?name (help "Defines the opaque type <name>."))
	  (set! *opaque-type* (cons name *opaque-type*))
	  (define-type-id name))
;*--- no-type ---------------------------------------------------------*/
	 (("-no-type" ?name (help "Don't emit definition for type <name>."))
	  (set! *no-type* (cons name *no-type*)))
;*--- macro -----------------------------------------------------------*/
	 (("-macro" (help "-macro[-fun|-var]"
			      "Produces macro definitions for functions and variables."))
	  (set! *macro-function* #t)
	  (set! *macro-variable* #t))
	 (("-macro-fun")
	  (set! *macro-function* #t))
	 (("-macro-var")
	  (set! *macro-variable* #t))
;*--- directives ------------------------------------------------------*/
	 (("-no-directives" (help "Do not emit directives header."))
	  (set! *directives* #f))
;*--- include-directive -----------------------------------------------*/
	 (("-include-directive" (help "Produce bigloo include directive."))
	  (set! *include-directive* #t))
;*--- hook ------------------------------------------------------------*/
	 (("-hookfile" ?name (help "Load <name> to find user hooks."))
	  (set! *hookfile* name))
;*--- stub ------------------------------------------------------------*/
	 (("-fun-stub" (help "Produce Eval stubs for functions (only with include directives)"))
	  (set! *eval-stub?* #t))
;*--- gcc extensions --------------------------------------------------*/
	 (("-gcc" (help "Enable gcc extensions (e.g. inline, attributes)"))
	  (set! *gcc-extensions?* #t))
;*--- enum macros -----------------------------------------------------*/
         (("-enum-macros" (help "Produce a macro for each enum member"))
           (set! *enum-macros* #t))
;*--- enums -----------------------------------------------------------*/
	 (("-int-enum" (help "Treat enums as simple #define directives of type int."))
	  (set! *int-enum* #t))
;*--- use cpp ---------------------------------------------------------*/
         (("-use-cpp" (help "Preprocess C code with cpp"))
           (set! *use-cpp* #t))
;*--- omit underscore -------------------------------------------------*/
         (("-omit-underscore" (help "Omit macros begining with an underscore"))
           (set! *omit-underscore* #t))
;*--- idents ----------------------------------------------------------*/
	 (("-ident-style=?style" (help "Set ident style (scheme, plain)"))
	  (set! *ident-style* (string->symbol style)))
;*--- les sources -----------------------------------------------------*/
	 (else
	  (set! *src* (cons else *src*)))))
	
;*---------------------------------------------------------------------*/
;*    query ...                                                        */
;*---------------------------------------------------------------------*/
(define (query)
   (version)
   (newline)
   (print "setups:")
   (newline)
   (print "*include-path*         : " *include-path*)
   (print "*c-type-alist*         : " *c-type-alist*)
   (print "*c-unsigned-type-alist*: " *c-unsigned-type-alist*)
   (print "*c-signed-type-alist*  : " *c-signed-type-alist*)
   (print "*default-type*         : " *default-type*)
   (print "*hookfile*             : " *hookfile*)
   (exit 0))

;*---------------------------------------------------------------------*/
;*    help ...                                                         */
;*---------------------------------------------------------------------*/
(define (help usage)
   (version)
   (print "usage: cigloo [options] [src_name]*")
   (newline)
   (usage #f)
   (newline)
   (print "Shell Variables:")
   (print "   - TMPDIR             --  Tmp directory (default \"/tmp\").")
   (newline)
   (print "Runtime Command file:")
   (print "   - ~/.cigloorc")
   (exit 0))


   
