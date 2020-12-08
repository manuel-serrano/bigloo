;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Engine/param.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  3 12:44:17 1995                          */
;*    Last change :  Mon Dec 16 05:40:27 2019 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
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
	    *dlopen-init-gc*
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
	    *unsafe-heap*
	    *warning-overriden-slots*
	    *warning-overriden-variables*
	    *warning-types*
	    *warning-type-error*
	    *warning-default-slot-value*
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
	    *optim-atom-inlining?*
	    *optim-O-macro?*
	    *optim-isa?*
	    *optim-cfa?*
	    *optim-cfa-fixnum-arithmetic?*
	    *optim-cfa-flonum-arithmetic?*
	    *optim-cfa-free-var-tracking?*
	    *optim-cfa-apply-tracking?*
	    *optim-cfa-pair?*
	    *optim-cfa-pair-quote-max-length*
	    *optim-cfa-unbox-closure-args*
	    *optim-cfa-force-loose-local-function?*
	    *optim-integrate?*
	    *optim-dataflow?*
	    *optim-dataflow-for-errors?*
	    *optim-dataflow-types?*
	    *optim-initflow?*
	    *optim-sync-failsafe?*
	    *optim-reduce-beta?*
	    *optim-jvm-inlining*
	    *optim-jvm-constructor-inlining*
	    *optim-jvm-peephole*
	    *optim-jvm-branch*
	    *optim-jvm-fasteq*
	    *optim-symbol-case*
	    *optim-return?*
	    *optim-return-goto?*
	    *optim-tagged-fxop?*
	    *optim-stackable?*
	    *optim-uncell?*
	    *purify*
	    *jvm-env*
	    *arithmetic-genericity*
	    *arithmetic-overflow*
	    *shared-cnst?*
	    ;; -------------------------------------------------------------
	    ;; warning, any change about this variable name must be reported
	    ;; in the no-trace-no-check macro of Llib/error.scm file
	    *compiler-debug*
	    *compiler-debug-trace*
	    *error-localization*
	    *compiler-sharing-debug?*
	    *compiler-type-debug?*
	    *compiler-stack-debug?*
	    ;; -------------------------------------------------------------
	    *bmem-profiling*
	    *sync-profiling*
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
	    *typenode-shape?*
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
	    *cflags-rpath*
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
	    *gc-force-register-roots?*
	    *bigloo-abort?*
	    *static-bigloo?*
	    *static-all-bigloo?*
	    *double-ld-libs?*
	    *bigloo-user-lib*
	    *additional-bigloo-libraries*
	    *bigloo-libraries-c-setup*
	    *additional-bigloo-zips*
	    *lib-dir*
	    *default-lib-dir*
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
	    *garbage-collector*
	    *auto-link-main*
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
	    *saw-spill*
	    *saw-bbv?*
	    *global-tail-call?*
	    *builtin-allocators*
	    *eval-options*
	    *allow-type-redefinition*
	    *pre-processor*
	    (bigloo-variables-usage ::bool)
	    (reinitialize-bigloo-variables!))
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

;; holds a list of lambdas that can be executed to
;; reinitialise the control variables.
(define *param-updaters* '())

(define (add-updater! proc)
   (set! *param-updaters* (cons proc *param-updaters*)))

(define (reinitialize-bigloo-variables!)
   (for-each (lambda (proc) (proc)) (reverse *param-updaters*)))

(define-macro (param-define var doc val)
   `(begin
       (define ,var ,val)
       (add-doc-variable! ',var ,doc)
       ,(when (and (pair? val)
		   (not (eq? 'quote (car val)))
		   (not (eq? 'lambda (car val))))
	   `(add-updater! (lambda () (set! ,var ,val))))))

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
(param-define *bigloo-version*
   "The Bigloo major release number"
   (bigloo-config 'release-number))
;; the bigloo specific version
(param-define *bigloo-specific-version*
   "The Bigloo specific version"
   (bigloo-config 'specific-version))
;; the bigloo name
(param-define *bigloo-name*
   "The Bigloo name"
   (string-append "Bigloo "
      *bigloo-specific-version*
      "("
      *bigloo-version*
      ")"))
(define *bigloo-cmd-name* 'nothing-yet)
(define *bigloo-args* 'nothing-yet)
(define *rest-args* '())
(define *bigloo-author* "Inria -- Sophia Antipolis")
(define *bigloo-email* "bigloo@lists-sop.inria.fr")
(define *bigloo-url* "http://www-sop.inria.fr/indes/fp/Bigloo")
(define *bigloo-date* (bigloo-date))
;; the tmp directory
(param-define *bigloo-tmp*
   "The tmp directory name"
   (let ((tmp (getenv "TMPDIR")))
      (if (string? tmp)
	  tmp
	  (os-tmp))))
;; Shall we include the license in the C files ?
(param-define *bigloo-licensing?*
   "Add the Bigloo license ?"
   #f)

;*---------------------------------------------------------------------*/
;*    Le bavardage ...                                                 */
;*---------------------------------------------------------------------*/
(param-define *verbose*
   "The verbosity level"
   0)

(param-define *hello*
   "Say hello (when verbose)"
   #f)

;*---------------------------------------------------------------------*/
;*    Les noms des differents fichiers                                 */
;*---------------------------------------------------------------------*/
;; the source files
(param-define *src-files*
	      "The sources files"
	      '())
(define *tmp-dest* #f)
;; the target name
(param-define *dest*
   "The target name"
   #f)

;*---------------------------------------------------------------------*/
;*    Le compilateur C et ses options                                  */
;*---------------------------------------------------------------------*/
;; the shell
(param-define *shell*
   "The shell to exec C compilations"
   (bigloo-config 'shell))
;; the c compiler style
(param-define *cc-style*
   "The C compiler style"
   (bigloo-config 'c-compiler-style))
;; the c compiler
(param-define *cc*
   "The C compiler"
   (bigloo-config 'c-compiler))
;; the c compiler  option
(param-define *cflags*
   "The C compiler option"
   (bigloo-config 'c-flags))
;; the c compiler optimization option
(param-define *cflags-optim*
   "The C compiler optimization option"
   (bigloo-config 'c-compiler-optim-flag))
;; the c compiler profile option
(param-define *cflags-prof*
   "The C compiler profiling option"
   (bigloo-config 'c-prof-flag))
;; the c compiler rpath option
(param-define *cflags-rpath*
   "The C compiler rpath option"
   (list (bigloo-config 'library-directory)))
;; the c compiler -o option
(param-define *cc-o-option*
   "The C compiler -o option"
   (bigloo-config 'c-compiler-o-option))
;; the c object file extension
(param-define *c-object-file-extension*
   "The C object file extension"
   (bigloo-config 'c-object-file-extension))
;; The C production type
(param-define *stdc*
   "Shall we produce ISO C?"
   #f)
;; the CC option
(param-define *cc-options*
   "cc options"
   '(""))
;; shall we remove the .c and .il produced file?
(param-define *rm-tmp-files*
   "Shall the .c and .il produced files be removed?"
   #t)
;; ld style
(param-define *ld-style*
   "ld style"
   (bigloo-config 'c-linker-style))
;; ld options
(param-define *ld-options*
   "ld options"
   (bigloo-config 'c-linker-flags))
;; the linker -o option
(param-define *ld-o-option*
   "The C linker -o option"
   (bigloo-config 'c-linker-o-option))
;; the linker debugging option
(param-define *ld-debug-option*
   "The C linker debugging option"
   (bigloo-config 'c-linker-debug-option))
;; the linker optimization flags
(param-define *ld-optim-flags*
   "The C linker optimization flags"
   (bigloo-config 'c-linker-optim-flags))
;; ld post options
(param-define *ld-post-options*
   "ld post options"
   '(""))
;; cc-move
(param-define *cc-move*
   "Use mv instead of -o when C compiling"
   #t)
;; library link mode
(param-define *ld-relative*
   "Relative or absolute path names for libraries"
   #t)
;; strip ?
(param-define *strip*
   "Shall we strip the executable?"
   #t)
;; the lib dir path
(param-define *lib-dir*
   "The lib dir path"
   (let ((lib-env (build-path-from-shell-variable "BIGLOOLIB")))
      (if (not (pair? lib-env))
	  (list "." (bigloo-config 'library-directory))
	  (cons "." lib-env))))
(param-define *default-lib-dir*
   "Depreacted, don't use"
   (bigloo-config 'library-directory))
(param-define *ld-library-dir*
   "Depreacted, don't use"
   (bigloo-config 'library-directory))
;; the lib source dir path
(param-define *lib-src-dir*
   "The lib dir path"
   (make-file-name (car *lib-dir*) "runtime"))
;; the bigloo library
(param-define *bigloo-lib*
   "The Bigloo library"
   'bigloo)
;; the gc library
(param-define *gc-lib*
   "The Gc library"
   (if (string? (bigloo-config 'gc-custom))
       (string->symbol (bigloo-config 'gc-custom))
       (string->symbol (bigloo-config 'gc-lib))))
;; are we using a custom GC library?
(param-define *gc-custom?*
   "Are we using a custom GC library?"
   (bigloo-config 'gc-custom))
;; are we using a multi-threaded GC?
(param-define *multi-threaded-gc?*
   "Are we using a multi-threaded GC?"
   #f)
;; force root registration (when GC support not complete)
(param-define *gc-force-register-roots?*
   "Force GC roots registration for global variables (for experts only)"
   #t)
;; do we have bigloo-abort?
(param-define *bigloo-abort?*
   "Do we have the bigloo-abort function in executables?"
   (bigloo-config 'have-bigloo-abort))
;; do we use a static version of the bigloo library?
(param-define *static-bigloo?*
   "Do we use the static Bigloo library"
   #f)
;; do we use a static version of all the bigloo libraries?
(param-define *static-all-bigloo?*
   "Do we use the static version of all Bigloo libraries?"
   #f)
;; do we include the additional user libraries twice?
(param-define *double-ld-libs?*
   "Do we include the additional user libraries twice?"
   #t)
;; the user C libraries
(param-define *bigloo-user-lib*
   "The user extra C libraries"
   (string-split-char (bigloo-config 'user-libraries) #\space))
;; the user Bigloo libraries
(param-define *additional-bigloo-libraries*
   "The user extra Bigloo libraries"
   '())
;; A list of C functions to be called when starting the application
(param-define *bigloo-libraries-c-setup*
   "A list of C functions to be called when starting the application"
   '())
;; the user Bigloo zip files
(param-define *additional-bigloo-zips*
   "The user extra Bigloo Zip files"
   '())
;; the load path
(define *old-load-path* *load-path*)
(set! *load-path* (append *load-path* *lib-dir*))
(add-updater! (lambda ()
		 (set! *load-path* (append *old-load-path* *lib-dir*))))

;; Include a Bigloo include file twice
(param-define *include-multiple*
   "Enable/disable multiple inclusion of same file"
   #f)
;; the C include files
(param-define *include-foreign*
   "The C included files"
   (list "bigloo.h"))
;; the additional C include files
(param-define *additional-include-foreign*
   "The additional C included files"
   '())
;; the bigloo heap base name
(param-define *heap-base-name*
   "The Bigloo heap base name"
   "bigloo")
;; the heap name
(param-define *heap-name*
   "The Bigloo heap file name"
   (string-append *heap-base-name* ".heap"))
;; the library the heap belongs to
(param-define *heap-library*
   "The library the heap belongs to"
   'bigloo)
;; the jvm heap name
(param-define *heap-jvm-name*
   "The Bigloo heap file name for the JVM backend"
   (string-append *heap-base-name* ".jheap"))
;; the heap dumped names
(param-define *heap-dump-names*
   "The name of the heap to be dumped"
   '())
;; the jvm foreign class id
(param-define *jvm-foreign-class-id*
   "The identifier of the Jlib foreign class"
   'foreign)
;; the jvm foreign class name
(param-define *jvm-foreign-class-name*
   "The name of the Jlib foreign class"
   "bigloo.foreign")
;; the additional heap name
(param-define *additional-heap-name*
   "A name of an additional heap file name to be build"
   #f)
;; the additional heap names
(param-define *additional-heap-names*
   "A list of Bigloo additional heap file name"
   '())
;; indent
(param-define *indent*
   "The name of the C beautifier"
   (bigloo-config 'c-beautifier))
;; debugging level
(param-define *compiler-debug*
   "Debugging level"
   0)
;; trace level
(param-define *compiler-debug-trace*
   "Debugging trace level"
   0)
;; error-localization
(param-define *error-localization*
   "Localize error calls in the source code"
   #f)
(param-define *compiler-sharing-debug?*
	      "Compiler self sharing debug"
	      #f)
(param-define *compiler-type-debug?*
   "Compiler self type debug"
   #f)
(param-define *compiler-stack-debug?*
	      "Compiler self stack trace debug"
	      #f)
;; profiling with bmem
(param-define *bmem-profiling*
	      "Instrument code for bmem profiling"
	      #f)
;; synchronize profiling
(param-define *sync-profiling*
	      "Instrument code for synchronize profiling"
	      #f)
;; debugging level
(param-define *debug-module*
   "Module initilazation debugging"
   0)
;; C debugging mode?
(param-define *c-debug*
   "C debugging mode?"
   #f)
;; C debugging mode?
(param-define *c-debug-lines-info*
   "Emit # line directives"
   #f)
;; C debugging option
(param-define *c-debug-option*
   "cc debugging option"
   (bigloo-config 'c-compiler-debug-option))
;; C header
(param-define *c-user-header*
   "C header"
   '())
;; C header
(param-define *c-user-foot*
   "C foot"
   '())
;; jvm debuggin mode?
(param-define *jvm-debug*
   "JVM debugging mode?"
   #f)
;; The bdb debugging option
(param-define *bdb-debug*
   "Bdb debugging mode"
   0)
(define *bdb-debug-no-line-directives?* #f)
;; The Bigloo profiling option
(param-define *profile-mode*
   "Bigloo profile mode"
   0)
;; The Bigloo profiling translation table name
(param-define *prof-table-name*
   "Bprof translation table file name"
   "bmon.out")

;*---------------------------------------------------------------------*/
;*    Access and qualifed-type                                         */
;*---------------------------------------------------------------------*/
(param-define *access-files*
   "The access file names"
   '())
(param-define *access-file-default*
   "The default access file name"
   ".afile")

(param-define *qualified-type-file*
   "The qualifed-type association file name"
   #f)
(param-define *qualified-type-file-default*
   "The qualifed-type association file name"
   ".jfile")

;*---------------------------------------------------------------------*/
;*    Link files                                                       */
;*---------------------------------------------------------------------*/
(param-define *o-files*
   "The additional obect files"
   '())
(param-define *c-files*
   "The C source files"
   '())
(param-define *with-files*
   "The additional modules"
   '())
(define *early-with-modules* '())

;*---------------------------------------------------------------------*/
;*    Des variables de controle sur `comment on doit compiler'         */
;*---------------------------------------------------------------------*/
(param-define *interpreter*
   "Shall we interprete the source file?"
   #f)
(param-define *startup-file*
   "A startup file for the interpreter"
   #f)
(param-define *call/cc?*
   "Shall we enable call/cc?"
   #f)
(param-define *auto-link-main*
   "Enable automatically a main generation when linking"
   #t)

(param-define *pass*
   "Stop after the pass"
   'ld)
(param-define *jvm-jar?*
   "Enable/disable a JAR file production for the JVM back-end"
   #f)
(param-define *jvm-shell*
   "Shell to be used when producing JVM run scripts"
   (bigloo-config 'java-shell))
(param-define *jvm-java*
   "JVM to be used to run Java programs"
   (bigloo-config 'java))
(param-define *jvm-options*
   "JVM options"
   "")
(param-define *jvm-bigloo-classpath*
   "JVM Bigloo classpath"
   #f)
(param-define *jvm-classpath*
   "JVM classpath"
   ".")
(param-define *jvm-mainclass*
   "JVM main class"
   #f)
(param-define *jvm-path-separator*
   "JVM classpath"
   #f)
(param-define *jvm-jarpath*
   "JVM jarpath"
   #f)
(param-define *jvm-directory*
   "JVM object directory"
   #f)
(param-define *jvm-catch*
   "Catch internal errors"
   #t)
(param-define *jvm-cinit-module*
   "Enable JVM class constructors to initiliaze bigloo modules"
   #f)
(param-define *module-checksum-object?*
   "Produce a module checksum object (.mco)"
   #f)
(param-define *garbage-collector*
   "The garbage collector"
   'boehm)

;*---------------------------------------------------------------------*/
;*    Les modes de compilations                                        */
;*---------------------------------------------------------------------*/
(param-define *unsafe-type*
   "Runtime type safety"
   #f)
(param-define *unsafe-arity*
   "Runtime type arity safety"
   #f)
(param-define *unsafe-range*
   "Runtime range safety"
   #f)
(param-define *unsafe-struct*
   "Runtime struct range safety"
   #f)
(param-define *unsafe-version*
   "Module version safety"
   #f)
(param-define *unsafe-library*
   "Use the unsafe library version"
   #f)
(param-define *unsafe-eval*
   "Disable type checking for eval functions"
   #f)
(param-define *unsafe-heap*
   "Disable heap version checking"
   #f)
(param-define *warning-overriden-slots*
   "Set to #t to warn about virtual slot overriding"
   #t)
(param-define *warning-overriden-variables*
   "Set to #t to warn about variable overriding"
   #f)
(param-define *warning-types*
   "Set to #t to warn about type checks"
   #f)
(param-define *warning-type-error*
   "Set to #t to treat type warnigns as error"
   #f)
(param-define *warning-default-slot-value*
   "Set to #t to warn about non-inlinable default slot values"
   #f)
(param-define *profile-library*
   "Use the profiled library version"
   #f)
(define *module-shape?* #f)
(define *key-shape?* #f)
(define *type-shape?* #f)
(define *typenode-shape?* #f)
(define *typename-shape?* #f)
(define *access-shape?* #f)
(define *location-shape?* #f)
(define *user-shape?* #f)
(define *name-shape?* #f)
(define *arithmetic-genericity* #t)
(define *arithmetic-overflow* #t)
(param-define *shared-cnst?*
   "Shared constant compilation?"
   #t)
(param-define *lib-mode*
   "Lib-mode compilation?"
   #f)
(param-define *init-mode*
   "Module initialization mode"
   'read)
(param-define *object-init-mode*
   "Object initialization mode"
   'stagged)
(param-define *dlopen-init*
   "Emit a standard Bigloo dynamic loading init entry point"
   #f)
(param-define *dlopen-init-gc*
   "Emit a standard GC init call when initialization the module"
   #f)
(param-define *max-c-token-length*
   "Max C token length"
   1024)
(param-define *c-split-string*
   "C split long strings"
   (bigloo-config 'c-string-split))
(param-define *max-c-foreign-arity*
   "Max C function arity"
   16)
(param-define *trace-name*
   "Trace file name"
   "trace")
(param-define *trace-write-length*
   "Trace dumping max level"
   80)
(define *additional-traces* '())

;*---------------------------------------------------------------------*/
;*    Optimizations                                                    */
;*---------------------------------------------------------------------*/
(param-define *optim*
   "Optimization level"
   0)
(param-define *optim-unroll-loop?*
   "Loop unrolling optimization"
   #unspecified)
(param-define *optim-loop-inlining?*
   "Loop inlining optimization"
   #t)
(param-define *optim-atom-inlining?*
   "Skip atom in inlining parameter counting"
   #t)
(param-define *optim-O-macro?*
   "Enable optimization by macro-expansion"
   #f)
(param-define *optim-jvm-inlining*
   "Enable JVM inlining"
   0)
(param-define *optim-jvm-constructor-inlining*
   "Enable JVM inlining for constructors"
   0)
(param-define *optim-jvm-peephole*
   "Enable JVM peephole optimization"
   0)
(param-define *optim-jvm-branch*
   "Enable JVM branch tensioning"
   0)
(param-define *optim-jvm-fasteq*
   "EQ? no longer works on integers (use =FX instead)"
   #f)
(param-define *optim-symbol-case*
   "Optimize case forms descrimining on symbols only"
   #f)
(param-define *purify*
   "Produce byte code verifier compliant JVM code"
   #t)
(param-define *jvm-env*
   "List of environment variables to be available in the compiled code"
   '())
(param-define *optim-jvm*
   "Enable optimization by inlining jvm code"
   0)
(param-define *optim-isa?*
   "Enable isa type tests optimization (inlining)"
   #f)
(param-define *optim-cfa?*
   "Enable CFA"
   #t)
(param-define *optim-cfa-fixnum-arithmetic?*
   "Enable refined fixnum arithmetic specialization"
   #f)
(param-define *optim-cfa-flonum-arithmetic?*
   "Enable refined flonum arithmetic specialization"
   #f)
(param-define *optim-cfa-free-var-tracking?*
   "Enable closure free-variables specialization"
   #f)
(param-define *optim-cfa-apply-tracking?*
   "Track values across apply"
   #f)
(param-define *optim-cfa-pair?*
   "Track values across pairs"
   #f)
(param-define *optim-cfa-pair-quote-max-length*
   "Maximum length for pair literal tracking"
   4)
(param-define *optim-cfa-unbox-closure-args*
   "Unbox closure arguments"
   #f)
(param-define *optim-cfa-force-loose-local-function?*
   "Force loosing local function approximations (for debugging)"
   #f)
(param-define *optim-integrate?*
   "Enable function integration (closure analysis)"
   #t)
(param-define *optim-dataflow?*
   "Enable simple dataflow optimization"
   #f)
(param-define *optim-dataflow-for-errors?*
   "Enable simple dataflow optimization for eliminating bad error messages"
   #t)
(param-define *optim-dataflow-types?*
   "Enable dataflow optimization for types"
   #f)
(param-define *optim-initflow?*
   "Enable initflow optimization for global variables"
   #f)
(param-define *optim-sync-failsafe?*
   "Enable failsafe synchronize optimization"
   #f)
(param-define *optim-reduce-beta?*
   "Enable simple beta reduction"
   #f)
(param-define *inlining?*
   "Inlining optimization"
   #t)
(param-define *user-inlining?*
   "User inlining optimization"
   #t)
(param-define *inlining-kfactor*
   "Inlining growth factor"
   (lambda (olevel) (*fx 2 olevel)))
(param-define *inlining-reduce-kfactor*
   "Inlinine growth factor reductor"
   (lambda (kfactor) (/fx kfactor 2)))
(param-define *optim-return?*
   "Optimize set-exit used as return"
   #f)
(param-define *optim-return-goto?*
   "Optimize set-exit by enabling local return"
   #f)
(param-define *optim-tagged-fxop?*
   "Optimize tagged fixnum operations"
   #f)
(param-define *optim-stackable?*
   "Optimize stackable allocation"
   #f)
(param-define *optim-uncell?*
   "Remove useless cells"
   #f)

;*---------------------------------------------------------------------*/
;*    *extend-entry* ...                                               */
;*---------------------------------------------------------------------*/
(param-define *extend-entry*
   "Extend entry"
   #f)

;*---------------------------------------------------------------------*/
;*    *src-suffix* ...                                                 */
;*    -------------------------------------------------------------    */
;*    The list of suffix recognized by the compiler and the linker.    */
;*---------------------------------------------------------------------*/
(param-define *src-suffix*
   "Scheme legal suffixes"
   '("scm" "bgl"))

;*---------------------------------------------------------------------*/
;*    *c-suffix* ...                                                   */
;*    -------------------------------------------------------------    */
;*    The list of C suffixes recognized by the compiler and the linker.*/
;*---------------------------------------------------------------------*/
(param-define *c-suffix*
   "C legal suffixes"
   '("c"))

;*---------------------------------------------------------------------*/
;*    *csharp-suffix* ...                                              */
;*    -------------------------------------------------------------    */
;*    The list of C# suffixes recognized by the compiler and the       */
;*    linker.                                                          */
;*---------------------------------------------------------------------*/
(param-define *csharp-suffix*
   "C# legal suffixes"
   '("cs"))

;*---------------------------------------------------------------------*/
;*    *obj-suffix* ...                                                 */
;*    -------------------------------------------------------------    */
;*    The suffix list of the object file                               */
;*---------------------------------------------------------------------*/
(param-define *obj-suffix*
   "Object legal suffixes"
   (list *c-object-file-extension*
      (static-library-suffix)
      (shared-library-suffix)))

;*---------------------------------------------------------------------*/
;*    *mco-suffix*                                                     */
;*    -------------------------------------------------------------    */
;*    The suffix list of the module checksum object files.             */
;*---------------------------------------------------------------------*/
(param-define *mco-suffix*
   "Module checksum object legal suffixes"
   '("mco"))

;*---------------------------------------------------------------------*/
;*    *mco-include-path* ...                                           */
;*---------------------------------------------------------------------*/
(param-define *mco-include-path*
   "Module checksum C include path"
   '("."))

;*---------------------------------------------------------------------*/
;*    auto-modes (emacs like)                                          */
;*---------------------------------------------------------------------*/
(param-define *auto-mode*
   "auto-mode (extend mode) list"
   '(("ml"  . "caml")
     ("mli" . "caml")
     ("oon" . "meroon")
     ("snow" . "snow")
     ("spi" . "pkgcomp")))

;*---------------------------------------------------------------------*/
;*    *ast-case-sensitive* ...                                         */
;*---------------------------------------------------------------------*/
(param-define *ast-case-sensitive*
   "Case sensitivity"
   #t)

;*---------------------------------------------------------------------*/
;*    *user-heap-size*                                                 */
;*---------------------------------------------------------------------*/
(param-define *user-heap-size*
   "Heap size (in MegaByte) or #f for default value"
   0)

;*---------------------------------------------------------------------*/
;*    *reader* ...                                                     */
;*---------------------------------------------------------------------*/
(param-define *reader*
   "The way the reader reads input file ('plain or 'intern)"
   'plain)

;*---------------------------------------------------------------------*/
;*    *target-language* ...                                            */
;*---------------------------------------------------------------------*/
(param-define *target-language*
   "The target language (either c, c-saw, jvm, or .net)"
   (string->symbol (bigloo-config 'default-back-end)))

;*---------------------------------------------------------------------*/
;*    *saw* ...                                                        */
;*---------------------------------------------------------------------*/
(param-define *saw*
   "Do we go to the saw-mill?"
   #f)

;*---------------------------------------------------------------------*/
;*    *saw-register-reallocation?* ...                                 */
;*---------------------------------------------------------------------*/
(param-define *saw-register-reallocation?*
   "Enable/disable saw register re-allocation"
   #f)

;*---------------------------------------------------------------------*/
;*    *saw-register-allocation?* ...                                   */
;*---------------------------------------------------------------------*/
(param-define *saw-register-allocation?*
   "Enable/disable saw register allocation"
   #f)

;*---------------------------------------------------------------------*/
;*    *saw-register-allocation-onexpression?* ...                      */
;*---------------------------------------------------------------------*/
(param-define *saw-register-allocation-onexpression?*
   "Enable/disable saw register allocation on expression"
   #f)

;*---------------------------------------------------------------------*/
;*    *saw-register-allocation-max-size* ...                           */
;*---------------------------------------------------------------------*/
(param-define *saw-register-allocation-max-size*
   "Max function size for optimizing the register allocation"
   4000)

;*---------------------------------------------------------------------*/
;*    *saw-register-allocation-functions* ...                          */
;*---------------------------------------------------------------------*/
(param-define *saw-register-allocation-functions*
   "The list of functions allowing register allocation"
   '())

;*---------------------------------------------------------------------*/
;*    *saw-no-register-allocation-functions* ...                       */
;*---------------------------------------------------------------------*/
(param-define *saw-no-register-allocation-functions*
   "The list of functions disabling register allocation"
   '())

;*---------------------------------------------------------------------*/
;*    *saw-spill* ...                                                  */
;*---------------------------------------------------------------------*/
(param-define *saw-spill*
   "Enable saw spill optimization"
   #f)

;*---------------------------------------------------------------------*/
;*    *saw-bbv?* ...                                                   */
;*---------------------------------------------------------------------*/
(param-define *saw-bbv?*
   "Enable/disable saw basic-blocks versionning"
   #f)

;*---------------------------------------------------------------------*/
;*    *global-tail-call?* ...                                          */
;*---------------------------------------------------------------------*/
(param-define *global-tail-call?*
   "Do we apply the self-global-tail-call stage?"
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
(param-define *eval-options*
   "A user variable to store dynamic command line options"
   '())

;*---------------------------------------------------------------------*/
;*    *allow-type-redefinition* ...                                    */
;*---------------------------------------------------------------------*/
(param-define *allow-type-redefinition*
   "If true, allow type redefinitions"
   #f)

;*---------------------------------------------------------------------*/
;*    *pre-processor* ...                                              */
;*---------------------------------------------------------------------*/
(param-define *pre-processor*
   "An optional function that pre-processes the source file"
   (lambda (x) x))

;*---------------------------------------------------------------------*/
;*    Other variables that are defined inside the interpreter...       */
;*---------------------------------------------------------------------*/
(add-doc-variable! '*load-path* "The load path")
(add-doc-variable! '*user-pass* "The user specific compilation pass")
