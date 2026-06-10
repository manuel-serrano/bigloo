;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/cigloo/Init/parse_args.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 29 11:56:20 1995                          */
;*    Last change :  Tue Jun  9 08:13:58 2026 (serrano)                */
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
	 (("-" ?name (help "A source file name."))
	  (set! *src* (cons name *src*)))

	 (("?") (help args-parse-usage))
	 (("-help") (help args-parse-usage))

	 (("-version" (help "The current cigloo release."))
	  (print *cigloo-name*)
	  (exit 0))
	 (("-revision" (help "The current cigloo release (short format)."))
	  (print *cigloo-version* (if (char? *cigloo-level*)
				      *cigloo-level*
				      ""))
	  (exit 0))

	 (("-query" (help "Dump the current configuration."))
	  (query))
	 (("-q" (help "Do not load rc file."))
	  'nothing)

	 (("-o" ?name (help "Name the output file <name>."))
	  (set! *dest* name))
	 (("--to-stdout" (help "Write C code on current output channel."))
	  (set! *verbose* -1)
	  (set! *dest* '--to-stdout))

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

	 (("-open-include" ?name (help "Open include <name>."))
	  (if (or (pair? *open-include*) (null? *open-include*))
	      (set! *open-include* (cons name *open-include*))))
	 (("-open-includes" (help "Open all includes."))
	  (set! *open-include* 'all))
	 (("-scan-include" ?name (help "Scan include <name>."))
	  (if (or (pair? *scan-include*) (null? *open-include*))
	      (set! *scan-include* (cons name *scan-include*))))
	 (("-scan-includes" (help "Scan all includes."))
	  (set! *scan-include* 'all))
	 (("-I" ?name (help "-I <name> | -I<name>"
				"Add <name> to the include directories list."))
	  (set! *include-path* (cons name *include-path*)))
	 (("-I?name")
	  (set! *include-path* (cons name *include-path*)))

	 (("-define" (help "Produce clauses for #define directives that do not take arguments [default]."))
	  (set! *define* #t))
	 (("-no-define" (help "Don't produce clauses for #define directives that do not take arguments."))
	  (set! *define* #f))
	 (("-define-fun" (help "Produce clauses for #define directives that take arguments [default]."))
	  (set! *define-fun* #t))
	 (("-no-define-fun" (help "Don't produce clauses for #define directives that take arguments"))
	  (set! *define-fun* #f))

	 (("-type" ?name (help "Add the type <name> to cigloo."))
	  (define-type-id name))
	 (("-opaque-type" ?name (help "Defines the opaque type <name>."))
	  (set! *opaque-type* (cons name *opaque-type*))
	  (define-type-id name))
	 (("-no-type" ?name (help "Don't emit definition for type <name>."))
	  (set! *no-type* (cons name *no-type*)))

	 (("-macro" (help "-macro[-fun|-var]"
			      "Produces macro definitions for functions and variables."))
	  (set! *macro-function* #t)
	  (set! *macro-variable* #t))
	 (("-macro-fun")
	  (set! *macro-function* #t))
	 (("-macro-var")
	  (set! *macro-variable* #t))

	 (("-no-directives" (help "Do not emit directives header."))
	  (set! *directives* #f))
	 (("-include-directive" (help "Produce bigloo include directive."))
	  (set! *include-directive* #t))

	 (("-hookfile" ?name (help "Load <name> to find user hooks."))
	  (set! *hookfile* name))
	 (("-fun-stub" (help "Produce Eval stubs for functions (only with include directives)"))
	  (set! *eval-stub?* #t))

	 (("-gcc" (help "Enable gcc extensions (e.g. inline, attributes)"))
	  (set! *gcc-extensions?* #t))

         (("-enum-macros" (help "Produce a macro for each enum member"))
           (set! *enum-macros* #t))
	 (("-int-enum" (help "Treat enums as simple #define directives of type int."))
	  (set! *int-enum* #t))

         (("-use-cpp" (help "Preprocess C code with cpp"))
           (set! *use-cpp* #t))
         (("-omit-underscore" (help "Omit macros begining with an underscore"))
           (set! *omit-underscore* #t))
	 (("-ident-style=?style" (help "Set ident style (scheme, plain)"))
	  (set! *ident-style* (string->symbol style)))
	 (("--module4" (help "Generate module 4 syntax"))
	  (set! *module* 4))
	 (("--module5" (help "Generate module 5 syntax"))
	  (set! *module* 5))
	 (("-cp" ?path (help "Ignored option for compatibility with other tools"))
	  #unspecified)
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


   
