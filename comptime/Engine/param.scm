;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/param.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  3 12:44:17 1995                          */
;*    Last change :  Thu Mar 26 07:22:36 2009 (serrano)                */
;*    Copyright   :  1995-2009 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Global control of the compiler                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_param
   (import  (tools_date "Tools/date.scm")
	    ;;; tools_date is a generated file and hence, it can't be
	    ;; set in the .afile file.
	    tools_misc
	    engine_configure)
   (export  *bigloo-version*
	    *bigloo-specific-version*
	    *bigloo-name*
	    *bigloo-cmd-name*
	    *bigloo-args*
	    *rest-args*
	    *bigloo-author*    
	    *bigloo-email*     
	    *bigloo-url*     
	    *bigloo-date*
	    *bigloo-tmp*
	    *bigloo-licensing?*
	    *lib-mode*
	    *init-mode*
	    *object-init-mode*
	    *dlopen-init*
	    *max-c-token-length*
	    *c-split-string*
	    *max-c-foreign-arity*
	    *verbose*
	    *hello*
	    *unsafe-type*
	    *unsafe-range*     
	    *unsafe-struct*    
	    *unsafe-arity*
	    *unsafe-version*
	    *unsafe-library*
	    *unsafe-eval*
	    *warning-overriden-slots*
	    *warning-overriden-variables*
	    *profile-library*
	    *trace-name*
	    *trace-write-length*
	    *additional-traces*
	    *inlining?*
	    *user-inlining?*
	    *inlining-kfactor*
	    *inlining-reduce-kfactor*
	    *optim*
	    *optim-unroll-loop?*
	    *optim-loop-inlining?*
	    *optim-O-macro?*
	    *optim-cfa-arithmetic?*
	    *optim-integrate?*
	    *optim-dataflow?*
	    *optim-dataflow-for-errors?*
;* 	    *optim-dataflow-early-aliasing?*                           */
	    *optim-reduce-beta?*
	    *optim-jvm-inlining*
	    *optim-jvm-constructor-inlining*
	    *optim-jvm-peephole*
	    *optim-jvm-branch*
	    *optim-jvm-fasteq*
	    *optim-symbol-case*
	    *purify*
	    *jvm-env*
	    *genericity*
	    *shared-cnst?*
	    ;; -------------------------------------------------------------
	    ;; warning, any change about this variable name must be reported
	    ;; in the no-trace-no-check macro of Llib/error.scm file
	    *compiler-debug*
	    *error-localization*
	    *compiler-sharing-debug?*
	    ;; -------------------------------------------------------------
	    *bmem-profiling*
	    *debug-module*
	    *c-debug*
	    *c-debug-lines-info*
	    *c-debug-option*
	    *c-user-header*
	    *c-user-foot*
	    *jvm-debug*
	    *bdb-debug*
	    *bdb-debug-no-line-directives?*
	    *profile-mode*
	    *prof-table-name*
	    *module-shape?*             
	    *key-shape?*
	    *type-shape?*
            *typename-shape?*
	    *access-shape?*
	    *location-shape?*
	    *user-shape?*
	    *name-shape?*
	    *tmp-dest*         
	    *dest*
	    *shell*
	    *cc-style*
	    *cc*
	    *cflags*
	    *cflags-optim*
	    *cflags-prof*
	    *stdc*               
	    *cc-options*       
	    *rm-tmp-files*       
	    *cc-o-option*
	    *c-object-file-extension*
	    *ld-style*
	    *ld-options*
	    *ld-o-option*
	    *ld-optim-flags*
	    *ld-debug-option*
	    *ld-post-options*
	    *cc-move*
	    *ld-relative*
	    *strip*            
	    *bigloo-lib*
	    *gc-lib*
	    *gc-custom?*
	    *multi-threaded-gc?*
	    *bigloo-abort?*
	    *static-bigloo?*
	    *static-all-bigloo?*
	    *double-ld-libs?*
	    *bigloo-user-lib*
	    *additional-bigloo-libraries*
	    *bigloo-libraries-c-setup*
	    *additional-bigloo-zips*
	    *default-lib-dir*
	    *ld-library-dir*
	    *lib-dir*
	    *lib-src-dir*
	    *include-multiple*
	    *include-foreign*
	    *additional-include-foreign*
	    *indent*
	    *access-files*
	    *access-file-default*
	    *qualified-type-file*
	    *qualified-type-file-default*
	    *src-files*
	    *o-files*          
	    *c-files*          
	    *with-files*
	    *early-with-modules*
	    *interpreter*      
	    *startup-file*     
	    *call/cc?*
	    *reflection?*
	    *class-nil?*
	    *garbage-collector*
	    *pass*
	    *jvm-jar?*
	    *jvm-shell*
	    *jvm-java*
	    *jvm-options*
	    *jvm-bigloo-classpath*
	    *jvm-classpath*
	    *jvm-mainclass*
	    *jvm-path-separator*
	    *jvm-jarpath*
	    *jvm-directory*
	    *jvm-catch*
	    *jvm-cinit-module*
	    *dotnet-shell*
	    *dotnet-ld*
	    *dotnet-ld-style*
	    *dotnet-clr*
	    *dotnet-clr-style*
	    *dotnet-clr-opt*
	    *dotnet-dll-path*
	    *dotnet-external-asm*
	    *dotnet-external-asm-style*
	    *dotnet-use-external-asm*
	    *dotnet-mono-workaround-switch*
	    *dotnet-pnet-workaround-switch*
	    *dotnet-tail*
	    *dotnet-tail-across-modules*
	    *dotnet-tail-funcall*
	    *module-checksum-object?*
	    *heap-base-name*
	    *heap-name*
	    *heap-library*
	    *heap-jvm-name*
	    *heap-dump-names*
	    *jvm-foreign-class-id*
	    *jvm-foreign-class-name*
	    *additional-heap-name*
	    *additional-heap-names*
	    *extend-entry*
	    *auto-mode*
	    *src-suffix*
	    *c-suffix*
	    *csharp-suffix*
	    *obj-suffix*
	    *mco-suffix*
	    *mco-include-path*
	    *ast-case-sensitive*
	    *user-heap-size*
	    *reader*
	    *target-language*
	    *saw*
	    *saw-register-reallocation?*
	    *saw-register-allocation?*
	    *saw-register-allocation-onexpression?*
	    *saw-register-allocation-max-size*
	    *saw-register-allocation-functions*
	    *saw-no-register-allocation-functions*
	    *global-tail-call?*
	    *globalize-integrate-28c*
	    *builtin-allocators*
	    *eval-options*
	    *allow-type-redefinition*
	    *pre-processor*
	    (bigloo-variables-usage ::bool))
   (eval    (export-all)))

;*---------------------------------------------------------------------*/
;*    *bigloo-variables* ...                                           */
;*    -------------------------------------------------------------    */
;*    This variable hold the list of all the Bigloo control            */
;*    variables with there description. This variable is used by       */
;*    the only function `bigloo-variables-usage'.                      */
;*---------------------------------------------------------------------*/
(define *bigloo-variables* '())

;*---------------------------------------------------------------------*/
;*    add-doc-variable! ...                                            */
;*---------------------------------------------------------------------*/
(define (add-doc-variable! id doc)
   (set! *bigloo-variables* (cons (cons id doc) *bigloo-variables*)))

;*---------------------------------------------------------------------*/
;*    doc-define ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (doc-define var doc val)
   `(begin
       (define ,var ,val)
       (add-doc-variable! ',var ,doc)))

;*---------------------------------------------------------------------*/
;*    bigloo-variables-usage ...                                       */
;*    -------------------------------------------------------------    */
;*    If MANUAL? is true the formatting is done according to manual    */
;*    width constraints.                                               */
;*---------------------------------------------------------------------*/
(define (bigloo-variables-usage manual?)
   (print "   All the Bigloo control variables can be changed from the")
   (print "   interpreter, by the means of the `-eval' option, or using")
   (print "   the module clause `option'. For instance the option")
   (print "   \"-eval '(set! *strip* #t)'\" will set the variable")
   (print "   `*strip*' to the value `#t'.")
   (print "   These variables are:")
   (newline)
   (let loop ((l (sort *bigloo-variables*
		       (lambda (x y)
			  (string<? (symbol->string (car x))
				    (symbol->string (car y)))))))
      (if (pair? l)
	  (let ((var (car l)))
	     (if manual?
		 (begin
		    (print "   - " (car var) " : ")
		    (print "     " (cdr var))
		    (display "     default: ")
		    (write (eval (car var)))
		    (newline))
		 (begin
		    (display* "   - " (car var) " : " (cdr var) " [")
		    (write (eval (car var)))
		    (print "]")))
	     (loop (cdr l))))))

;*---------------------------------------------------------------------*/
;*    Les auteurs et le nom du soft                                    */
;*---------------------------------------------------------------------*/
;; the bigloo version
(doc-define *bigloo-version*
	    "The Bigloo major release number"
	    (bigloo-config 'release-number))
;; the bigloo specific version
(doc-define *bigloo-specific-version*
	    "The Bigloo specific version"
	    (bigloo-config 'specific-version))
;; the bigloo name
(doc-define *bigloo-name*
	    "The Bigloo name"
	    (string-append "Bigloo "
                           *bigloo-specific-version*
                           "("
                           *bigloo-version*
                           ")"))
(define *bigloo-cmd-name*   'nothing-yet)
(define *bigloo-args*       'nothing-yet)
(define *rest-args*         '())
(define *bigloo-author*     "Inria -- Sophia Antipolis")
(define *bigloo-email*      "bigloo@sophia.inria.fr")
(define *bigloo-url*        "http://www.inria.fr/mimosa/fp/Bigloo")
(define *bigloo-date*       (bigloo-date))
;; the tmp directory
(doc-define *bigloo-tmp*
	    "The tmp directory name"
	    (let ((Venv (getenv "TMPDIR")))
	       (if (string? Venv)
		   Venv
		   (os-tmp))))
;; Shall we include the license in the C files ?
(doc-define *bigloo-licensing?*
	    "Add the Bigloo license ?"
	    #f)

;*---------------------------------------------------------------------*/
;*    Le bavardage ...                                                 */
;*---------------------------------------------------------------------*/
(doc-define *verbose*
	    "The verbosity level"
	    0)

(doc-define *hello*
	    "Say hello (when verbose)"
	    #f)

;*---------------------------------------------------------------------*/
;*    Les noms des differents fichiers                                 */
;*---------------------------------------------------------------------*/
;; the source files
(doc-define *src-files*
	    "The sources files"
	    '())
(define *tmp-dest*          #f)
;; the target name
(doc-define *dest*
	    "The target name"
	    #f)

;*---------------------------------------------------------------------*/
;*    Le compilateur C et ses options                                  */
;*---------------------------------------------------------------------*/
;; the shell
(doc-define *shell*
	    "The shell to exec C compilations"
	    (bigloo-config 'shell))
;; the c compiler style
(doc-define *cc-style*
	    "The C compiler style"
	    (bigloo-config 'c-compiler-style))
;; the c compiler
(doc-define *cc*
	    "The C compiler"
	    (bigloo-config 'c-compiler))
;; the c compiler  option
(doc-define *cflags*
	    "The C compiler option"
	    (bigloo-config 'c-flag))
;; the c compiler optimization option
(doc-define *cflags-optim*
	    "The C compiler optimization option"
	    (bigloo-config 'c-compiler-optim-flag))
;; the c compiler profile option
(doc-define *cflags-prof*
	    "The C compiler profiling option"
	    (bigloo-config 'c-prof-flag))
;; the c compiler -o option
(doc-define *cc-o-option*
	    "The C compiler -o option"
	    (bigloo-config 'c-compiler-o-option))
;; the c object file extension
(doc-define *c-object-file-extension*
	    "The C object file extension"
	    (bigloo-config 'c-object-file-extension))
;; The C production type
(doc-define *stdc*
	    "Shall we produced ISO C?"
	    #f)
;; the CC option
(doc-define *cc-options*
	    "cc options"
	    (bigloo-config 'c-flag))
;; shall we remove the .c and .il produced file?
(doc-define *rm-tmp-files*
	    "Shall the .c and .il produced files be removed?"
	    #t)
;; ld style
(doc-define *ld-style*
	    "ld style"
	    (bigloo-config 'c-linker-style))
;; ld options
(doc-define *ld-options*
	    "ld options"
	    "")
;; the linker -o option
(doc-define *ld-o-option*
	    "The C linker -o option"
	    (bigloo-config 'c-linker-o-option))
;; the linker debugging option
(doc-define *ld-debug-option*
	    "The C linker debugging option"
	    (bigloo-config 'c-linker-debug-option))
;; the linker optimization flags
(doc-define *ld-optim-flags*
	    "The C linker optimization flags"
	    (bigloo-config 'c-linker-optim-flags))
;; ld post options
(doc-define *ld-post-options*
	    "ld post options"
	    "")
;; cc-move
(doc-define *cc-move*
	    "Use mv instean of -o when C compiling"
	    #t)
;; library link mode
(doc-define *ld-relative*
	    "Relative or absolute path names for libraries"
	    #t)
;; strip ?
(doc-define *strip*
	    "Shall we strip the executable?"
	    #t)
;; the installation lib dir path
(doc-define *ld-library-dir*
	    "The ld lib dir path (without version)"
	    (bigloo-config 'ld-library-dir))
;; the default lib dir path
(doc-define *default-lib-dir*
	    "The default lib dir path (without version)"
	    (bigloo-config 'library-directory))
;; the lib dir path
(doc-define *lib-dir*
	    "The lib dir path"
	    (let ((lib-env (build-path-from-shell-variable "BIGLOOLIB")))
	       (if (not (pair? lib-env))
		   (list "." *default-lib-dir*)
		   (cons "." lib-env))))
;; the lib source dir path
(doc-define *lib-src-dir*
	    "The lib dir path"
	    (make-file-name (car *lib-dir*) "runtime"))
;; the bigloo library
(doc-define *bigloo-lib*
	    "The Bigloo library"
	    'bigloo)
;; the gc library
(doc-define *gc-lib*
	    "The Gc library"
	    (if (string? (bigloo-config 'gc-custom))
		(string->symbol (bigloo-config 'gc-custom))
		(string->symbol (bigloo-config 'gc-lib))))
;; are we using a custom GC library?
(doc-define *gc-custom?*
	    "Are we using a custom GC library?"
	    (bigloo-config 'gc-custom))
;; are we using a multi-threaded GC?
(doc-define *multi-threaded-gc?*
	    "Are we using a multi-threaded GC?"
	    #f)
;; do we have bigloo-abort?
(doc-define *bigloo-abort?*
	    "Do we have the bigloo-abort function in executables?"
	    (bigloo-config 'have-bigloo-abort))
;; does we use a static version of the bigloo library?
(doc-define *static-bigloo?*
	    "Do we use the static Bigloo library"
	    #f)
;; does we use a static version of all the bigloo libraries?
(doc-define *static-all-bigloo?*
	    "Do we use the static version of all Bigloo libraries"
	    #f)
;; does we include twice the additional user libraries?
(doc-define *double-ld-libs?*
	    "Do we include twice the additional user libraries"
	    #t)
;; the user C libraries
(doc-define *bigloo-user-lib*
	    "The user extra C libraries"
	    (string-split-char (bigloo-config 'user-libraries) #\space))
;; the user Bigloo libraries
(doc-define *additional-bigloo-libraries*
	    "The user extra Bigloo libraries"
	    '())
;; A list of C functions to be called when starting the application
(doc-define *bigloo-libraries-c-setup*
	    "A list of C functions to be called when starting the application"
	    '())
;; the user Bigloo zip files
(doc-define *additional-bigloo-zips*
	    "The user extra Bigloo Zip files"
	    '())
;; the load path
(set! *load-path* (append *load-path* *lib-dir*))
;; Include twice a Bigloo include file
(doc-define *include-multiple*
	    "Enable/disable multiple inclusion of same file"
	    #f)
;; the C include files
(doc-define *include-foreign*
	    "The C included files"
	    (list "bigloo.h"))
;; the additional C include files
(doc-define *additional-include-foreign*
	    "The additional C included files"
	    '())
;; the bigloo heap base name
(doc-define *heap-base-name*
	    "The Bigloo heap base name"
	    "bigloo")
;; the heap name
(doc-define *heap-name*
	    "The Bigloo heap file name"
	    (string-append *heap-base-name* ".heap"))
;; the library the heap belongs to
(doc-define *heap-library*
	    "The library the heap belongs to"
	    'bigloo)
;; the jvm heap name
(doc-define *heap-jvm-name*
	    "The Bigloo heap file name for the JVM backend"
	    (string-append *heap-base-name* ".jheap"))
;; the heap dumped names
(doc-define *heap-dump-names*
	    "The name of the heap to be dumped"
	    '())
;; the jvm foreign class id
(doc-define *jvm-foreign-class-id*
	    "The identifier of the Jlib foreign class"
	    'foreign)
;; the jvm foreign class name
(doc-define *jvm-foreign-class-name*
	    "The name of the Jlib foreign class"
	    "bigloo.foreign")
;; the additional heap name
(doc-define *additional-heap-name*
	    "A name of an additional heap file name to be build"
	    #f)
;; the additional heap names
(doc-define *additional-heap-names*
	    "A list of Bigloo additional heap file name"
	    '())
;; indent
(doc-define *indent*
	    "The name of the C beautifier"
	    (bigloo-config 'c-beautifier))
;; debugging level
(doc-define *compiler-debug*
	    "Debugging level"
	    0)
;; error-localization
(doc-define *error-localization*
	    "Localize error calls in the source code"
	    #f)
(doc-define *compiler-sharing-debug?*
	    "Compiler self sharing debug"
	    #f)
;; profiling with bmem
(doc-define *bmem-profiling*
	    "Instrument code for bmem profiling"
	    #f)
;; debugging level
(doc-define *debug-module*
	    "Module initilazation debugging"
	    0)
;; C debugging mode?
(doc-define *c-debug*
	    "C debugging mode?"
	    #f)
;; C debugging mode?
(doc-define *c-debug-lines-info*
	    "Emit # line directives"
	    #f)
;; C debugging option
(doc-define *c-debug-option*
	    "cc debugging option"
	    (bigloo-config 'c-compiler-debug-option))
;; C header
(doc-define *c-user-header*
	    "C header"
	    '())
;; C header
(doc-define *c-user-foot*
	    "C foot"
	    '())
;; jvm debuggin mode?
(doc-define *jvm-debug*
	    "JVM debugging mode?"
	    #f)
;; The bdb debugging option
(doc-define *bdb-debug*
	    "Bdb debugging mode"
	    0)
(define *bdb-debug-no-line-directives?* #f)
;; The Bigloo profiling option
(doc-define *profile-mode*
	    "Bigloo profile mode"
	    0)
;; The Bigloo profiling translation table name
(doc-define *prof-table-name*
	    "Bprof translation table file name"
	    "bmon.out")

;*---------------------------------------------------------------------*/
;*    Access and qualifed-type                                         */
;*---------------------------------------------------------------------*/
(doc-define *access-files*
	    "The access file names"
	    '())
(doc-define *access-file-default*
	    "The default access file name"
	    ".afile")

(doc-define *qualified-type-file*
	    "The qualifed-type association file name"
	    #f)
(doc-define *qualified-type-file-default*
	    "The qualifed-type association file name"
	    ".jfile")

;*---------------------------------------------------------------------*/
;*    Link files                                                       */
;*---------------------------------------------------------------------*/
(doc-define *o-files*
	    "The additional obect files"
	    '())
(doc-define *c-files*
	    "The C source files"
	    '())
(doc-define *with-files*
	    "The additional modules"
	    '())
(define *early-with-modules* '())

;*---------------------------------------------------------------------*/
;*    Des variables de controle sur `comment on doit compiler'         */
;*---------------------------------------------------------------------*/
(doc-define *interpreter*
	    "Shall we interprete the source file?"
	    #f)
(doc-define *startup-file*
	    "A startup file for the interpreter"
	    #f)
(doc-define *call/cc?*
	    "Shall we enable call/cc?"
	    #f)
(doc-define *reflection?*
	    "Shall we produce reflection code for classes"
	    #t)
(doc-define *class-nil?*
	    "Shall we produce class-nil function for classes"
	    #t)
(doc-define *pass*
	    "Stop after the pass"
	    'ld)
(doc-define *jvm-jar?*
	    "Enable/disable a JAR file production for the JVM back-end"
	    #f)
(doc-define *jvm-shell*
	    "Shell to be used when producing JVM run scripts"
	    (bigloo-config 'java-shell))
(doc-define *jvm-java*
	    "JVM to be used to run Java programs"
	    (bigloo-config 'java))
(doc-define *jvm-options*
	    "JVM options"
	    "")
(doc-define *jvm-bigloo-classpath*
	    "JVM Bigloo classpath"
	    #f)
(doc-define *jvm-classpath*
	    "JVM classpath"
	    ".")
(doc-define *jvm-mainclass*
	    "JVM main class"
	    #f)
(doc-define *jvm-path-separator*
	    "JVM classpath"
	    #f)
(doc-define *jvm-jarpath*
	    "JVM jarpath"
	    #f)
(doc-define *jvm-directory*
	    "JVM object directory"
	    #f)
(doc-define *jvm-catch*
	    "Catch internal errors"
	    #t)
(doc-define *jvm-cinit-module*
	    "Enable JVM class constructors to initiliaze bigloo modules"
	    #f)
(doc-define *dotnet-shell*
	    ".NET object file linker"
	    (bigloo-config 'dotnet-shell))
(doc-define *dotnet-ld-style*
	    ".NET object file linker style"
	    (bigloo-config 'dotnet-ld-style))
(doc-define *dotnet-ld*
	    ".NET object file linker"
	    (bigloo-config 'dotnet-ld))
(doc-define *dotnet-clr*
	    "CLR to be used to run .NET programs"
	    (bigloo-config 'dotnet-clr))
(doc-define *dotnet-clr-style*
	    "CLR style to be used to run .NET programs"
	    (bigloo-config 'dotnet-clr-style))
(doc-define *dotnet-clr-opt*
	    "CLR extra options to be used to run .NET programs"
 	    (bigloo-config 'dotnet-clr-opt))
(doc-define *dotnet-dll-path*
	    "Bigloo.dll path"
	    #f)
(doc-define *dotnet-external-asm*
	    "Force using and external assembler for .NET code"
	    (bigloo-config 'dotnet-asm))
(doc-define *dotnet-use-external-asm*
	    "Force using and external assembler for .NET code"
	    #t)
(doc-define *dotnet-external-asm-style*
	    "Force using and external assembler for .NET code"
	    'pnet)
(doc-define *dotnet-mono-workaround-switch*
	    "Workaround mono 0.23..0.30 bug"
	    #t)
(doc-define *dotnet-pnet-workaround-switch*
	    "Workaround pnet switch bug"
	    #t)
(doc-define *dotnet-tail*
	    "Enable/disable tail call generations"
	    #f)
(doc-define *dotnet-tail-across-modules*
	    "Enable/disable tail call generations across modules"
	    #f)
(doc-define *dotnet-tail-funcall*
	    "Enable/disable tail call generations for funcall"
	    #f)
(doc-define *module-checksum-object?*
	    "Produce a module checksum object (.mco)"
	    #f)
(doc-define *garbage-collector*
	    "The garbage collector"
	    'boehm)

;*---------------------------------------------------------------------*/
;*    Les modes de compilations                                        */
;*---------------------------------------------------------------------*/
(doc-define *unsafe-type*
	    "Runtime type safety"
	    #f)
(doc-define *unsafe-arity*
	    "Runtime type arity safety"
	    #f)
(doc-define *unsafe-range*
	    "Runtime range safety"
	    #f)
(doc-define *unsafe-struct*
	    "Runtime struct range safety"
	    #f)
(doc-define *unsafe-version*
            "Module version safety"
	    #f)
(doc-define *unsafe-library*
	    "Use the unsafe library version"
	    #f)
(doc-define *unsafe-eval*
	    "Disable type check for eval functions"
	    #f)
(doc-define *warning-overriden-slots*
	    "Set to #t to warn about virtual slot overriding"
	    #t)
(doc-define *warning-overriden-variables*
	    "Set to #t to warn about variable overriding"
	    #f)
(doc-define *profile-library*
	    "Use the profiled library version"
	    #f)
(define *module-shape?* #f)
(define *key-shape?* #f)
(define *type-shape?* #f)
(define *typename-shape?* #f)
(define *access-shape?* #f)
(define *location-shape?* #f)
(define *user-shape?* #f)
(define *name-shape?* #f)
(define *genericity* #t)
(doc-define *shared-cnst?*
	    "Shared constant compilation?"
	    #t)
(doc-define *lib-mode*
	    "Lib-mode compilation?"
	    #f)
(doc-define *init-mode*
	    "Module initialization mode"
	    'read)
(doc-define *object-init-mode*
	    "Object initialization mode"
	    'stagged)
(doc-define *dlopen-init*
	    "Emit a standard Bigloo dynamic loading init entry point"
	    #f)
(doc-define *max-c-token-length*
	    "Max C token length"
	    1024)
(doc-define *c-split-string*
	    "C split long strings"
	    (bigloo-config 'c-string-split))
(doc-define *max-c-foreign-arity*
	    "Max C function arity"
	    16)
(doc-define *trace-name*
	    "Trace file name"
	    "trace")
(doc-define *trace-write-length*
	    "Trace dumping max level"
	    80)
(define *additional-traces* '())

;*---------------------------------------------------------------------*/
;*    Optimizations                                                    */
;*---------------------------------------------------------------------*/
(doc-define *optim*
	    "Optimization level"
	    0)
(doc-define *optim-unroll-loop?*
	    "Loop unrolling optimization"
	    #unspecified)
(doc-define *optim-loop-inlining?*
	    "Loop inlining optimization"
	    #t)
(doc-define *optim-O-macro?*
	    "Enable optimization by macro-expansion"
	    #f)
(doc-define *optim-jvm-inlining*
	    "Enable JVM inlining"
	    0)
(doc-define *optim-jvm-constructor-inlining*
	    "Enable JVM inlining for constructors"
	    0)
(doc-define *optim-jvm-peephole*
	    "Enable JVM peephole optimization"
	    0)
(doc-define *optim-jvm-branch*
	    "Enable JVM branch tensioning"
	    0)
(doc-define *optim-jvm-fasteq*
	    "EQ? no longer works on integers (use =FX instead)"
	    #f)
(doc-define *optim-symbol-case*
	    "Optimize case forms descrimining on symbols only"
	    #f)
(doc-define *purify*
	    "Produce byte code verifier compliant JVM code"
	    #t)
(doc-define *jvm-env*
	    "List of environment variables to be available in the compiled code"
	    '())
(doc-define *optim-jvm*
	    "Enable optimization by inlining jvm code"
	    0)
(doc-define *optim-cfa-arithmetic?*
	    "Enable refined arithmetic specialization"
	    #f)
(doc-define *optim-integrate?*
	    "Enable function integration (closure analysis)"
	    #t)
(doc-define *optim-dataflow?*
	    "Enable simple dataflow optimization"
	    #f)
(doc-define *optim-dataflow-for-errors?*
	    "Enable simple dataflow optimization for eliminating bad error messages"
	    #t)
;* (doc-define *optim-dataflow-early-aliasing?*                        */
;* 	    "Enable dataflow optimization for resolving early aliases" */
;* 	    #f)                                                        */
(doc-define *optim-reduce-beta?*
	    "Enable simple beta reduction"
	    #f)
(doc-define *inlining?*
	    "Inlining optimization"
	    #t)
(doc-define *user-inlining?*
	    "User inlining optimization"
	    #t)
(doc-define *inlining-kfactor*
	    "Inlining growth factor"
	    (lambda (olevel) (*fx 2 olevel)))
(doc-define *inlining-reduce-kfactor*
	    "Inlinine growth factor reductor"
	    (lambda (kfactor) (/fx kfactor 2)))

;*---------------------------------------------------------------------*/
;*    *extend-entry* ...                                               */
;*---------------------------------------------------------------------*/
(doc-define *extend-entry*
	    "Extend entry"
	    #f)

;*---------------------------------------------------------------------*/
;*    *src-suffix* ...                                                 */
;*    -------------------------------------------------------------    */
;*    The list of suffix recognized by the compiler and the linker.    */
;*---------------------------------------------------------------------*/
(doc-define *src-suffix*
	    "Scheme legal suffixes"
	    '("scm" "bgl"))

;*---------------------------------------------------------------------*/
;*    *c-suffix* ...                                                   */
;*    -------------------------------------------------------------    */
;*    The list of C suffixes recognized by the compiler and the linker.*/
;*---------------------------------------------------------------------*/
(doc-define *c-suffix*
	    "C legal suffixes"
	    '("c"))

;*---------------------------------------------------------------------*/
;*    *csharp-suffix* ...                                              */
;*    -------------------------------------------------------------    */
;*    The list of C# suffixes recognized by the compiler and the       */
;*    linker.                                                          */
;*---------------------------------------------------------------------*/
(doc-define *csharp-suffix*
	    "C# legal suffixes"
	    '("cs"))

;*---------------------------------------------------------------------*/
;*    *obj-suffix* ...                                                 */
;*    -------------------------------------------------------------    */
;*    The suffix list of the object file                               */
;*---------------------------------------------------------------------*/
(doc-define *obj-suffix*
	    "Object legal suffixes"
	    (list *c-object-file-extension*
		  (static-library-suffix)
		  (shared-library-suffix)))

;*---------------------------------------------------------------------*/
;*    *mco-suffix*                                                     */
;*    -------------------------------------------------------------    */
;*    The suffix list of the module checksum object files.             */
;*---------------------------------------------------------------------*/
(doc-define *mco-suffix*
	    "Module checksum object legal suffixes"
	    '("mco"))

;*---------------------------------------------------------------------*/
;*    *mco-include-path* ...                                           */
;*---------------------------------------------------------------------*/
(doc-define *mco-include-path*
	    "Module checksum C include path"
	    '("."))

;*---------------------------------------------------------------------*/
;*    auto-modes (emacs like)                                          */
;*---------------------------------------------------------------------*/
(doc-define *auto-mode*
	    "auto-mode (extend mode) list"
	    '(("ml"  . "caml")
	      ("mli" . "caml")
	      ("oon" . "meroon")
	      ("snow" . "snow")
	      ("spi" . "pkgcomp")))

;*---------------------------------------------------------------------*/
;*    *ast-case-sensitive* ...                                         */
;*---------------------------------------------------------------------*/
(doc-define *ast-case-sensitive*
	    "Case sensitivity"
	    #t)

;*---------------------------------------------------------------------*/
;*    *user-heap-size*                                                 */
;*---------------------------------------------------------------------*/
(doc-define *user-heap-size*
	    "Heap size (in MegaByte) or #f for default value"
	    #f)

;*---------------------------------------------------------------------*/
;*    *reader* ...                                                     */
;*---------------------------------------------------------------------*/
(doc-define *reader*
	    "The way the reader reads input file ('plain or 'intern)"
	    'plain)

;*---------------------------------------------------------------------*/
;*    *target-language* ...                                            */
;*---------------------------------------------------------------------*/
(doc-define *target-language*
	    "The target language (either c, c-saw, jvm, or .net)"
	    (string->symbol (bigloo-config 'default-back-end)))

;*---------------------------------------------------------------------*/
;*    *saw* ...                                                        */
;*---------------------------------------------------------------------*/
(doc-define *saw*
	    "Do we go to the saw-mill?"
	    #f)

;*---------------------------------------------------------------------*/
;*    *saw-register-reallocation?* ...                                 */
;*---------------------------------------------------------------------*/
(doc-define *saw-register-reallocation?*
	    "Enable/disable saw register re-allocation"
	    #f)

;*---------------------------------------------------------------------*/
;*    *saw-register-allocation?* ...                                   */
;*---------------------------------------------------------------------*/
(doc-define *saw-register-allocation?*
	    "Enable/disable saw register allocation"
	    #f)

;*---------------------------------------------------------------------*/
;*    *saw-register-allocation-onexpression?* ...                      */
;*---------------------------------------------------------------------*/
(doc-define *saw-register-allocation-onexpression?*
	    "Enable/disable saw register allocation on expression"
	    #f)

;*---------------------------------------------------------------------*/
;*    *saw-register-allocation-max-size* ...                           */
;*---------------------------------------------------------------------*/
(doc-define *saw-register-allocation-max-size*
	    "Max function size for optimizing the register allocation"
	    4000)

;*---------------------------------------------------------------------*/
;*    *saw-register-allocation-functions* ...                          */
;*---------------------------------------------------------------------*/
(doc-define *saw-register-allocation-functions*
	    "The list of functions allowing register allocation"
	    '())

;*---------------------------------------------------------------------*/
;*    *saw-no-register-allocation-functions* ...                       */
;*---------------------------------------------------------------------*/
(doc-define *saw-no-register-allocation-functions*
	    "The list of functions disabling register allocation"
	    '())

;*---------------------------------------------------------------------*/
;*    *global-tail-call?* ...                                          */
;*---------------------------------------------------------------------*/
(doc-define *global-tail-call?*
	    "Do we apply the self-global-tail-call stage?"
	    #f)

;*---------------------------------------------------------------------*/
;*    *globalize-integrate-28c* ...                                    */
;*---------------------------------------------------------------------*/
(doc-define *globalize-integrate-28c*
	    "Enable the old closure integration technique (deprecated)"
	    #f)

;*---------------------------------------------------------------------*/
;*    *builtin-allocators* ...                                         */
;*    -------------------------------------------------------------    */
;*    The builtin allocators (used only for Kprof).                    */
;*---------------------------------------------------------------------*/
(define *builtin-allocators*
   '(("CONS" . "make_pair")
     ("%STRING->SYMBOL" . "make_symbol")
     ("%MAKE-STRING" . "string_to_bstring_len")
     ("%MAKE-OUTPUT-PORT" . "bgl_make_output_port")
     ("%MAKE-INPUT-PORT" . "bgl_make_input_port")
     ("%MAKE-ERROR-PORT" . "make_error_port")))

;*---------------------------------------------------------------------*/
;*    *eval-options* ...                                               */
;*---------------------------------------------------------------------*/
(doc-define *eval-options*
	    "A user variable to store dynamic command line options"
	    '())

;*---------------------------------------------------------------------*/
;*    *allow-type-redefinition* ...                                    */
;*---------------------------------------------------------------------*/
(doc-define *allow-type-redefinition*
	    "If true, allow type redefinitions"
            #f)

;*---------------------------------------------------------------------*/
;*    *pre-processor* ...                                              */
;*---------------------------------------------------------------------*/
(doc-define *pre-processor*
	    "An optional function that pre-processes the source file"
	    (lambda (x) x))
   
;*---------------------------------------------------------------------*/
;*    Other variables that are defined inside the interpreter...       */
;*---------------------------------------------------------------------*/
(add-doc-variable! '*load-path* "The load path")
(add-doc-variable! '*user-pass* "The user specific compilation pass")
