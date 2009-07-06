;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/blib/env.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 11:04:39 1999                          */
;*    Last change :  Fri Nov 22 14:26:08 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The handling of the demangling environment inside the patient.   */
;*    This makes use of hash tables but to get simplicity it does not  */
;*    use objects, just structures.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bdb_env
   
   (extern (bdb-table->list::obj (::obj) "bdb_table_to_list")
	   (export bgl->c                "bgl2c")
	   (export bgl->c-funcall        "bgl2c_funcall")
	   (export bgl-bgl->c            "bglbgl2c")
	   (export c-bgl->c              "cbgl2c")
	   (export c->bgl                "c2bgl")
	   (export c-c->bgl              "cc2bgl"))
   
   (export (bdb-set-module-info! ::obj ::obj)
	   (bgl->c               ::bstring)
	   (bgl->c-funcall       ::bstring)
	   (bgl-bgl->c           ::bstring ::bstring)
	   (c-bgl->c             ::bstring ::bstring)
	   (c->bgl               ::bstring)
	   (c-c->bgl             ::bstring ::bstring)
	   (bgl-source           ::bstring)
	   (bgl-get-classes::pair-nil)))

;*---------------------------------------------------------------------*/
;*    file-info                                                        */
;*---------------------------------------------------------------------*/
(define-struct file-info
   ;; the name of the file
   name
   ;; the module the file belongs to
   module)

;*---------------------------------------------------------------------*/
;*    module-info                                                      */
;*---------------------------------------------------------------------*/
(define-struct module-info
   ;; the name of the module
   name
   ;; the list of the source file (strings)
   sources
   ;; the list of global that this module defines
   globals
   ;; the name of the C function that initialize this module
   init-c-name)

;*---------------------------------------------------------------------*/
;*    function-info                                                    */
;*---------------------------------------------------------------------*/
(define-struct function-info
   ;; the C name under which this function is know by gdb
   c-name
   ;; the Scheme name under which Bigloo knows this function
   scm-name
   ;; the source file containing that definition
   file-name
   ;; the line number in that source file
   line-num)

;*---------------------------------------------------------------------*/
;*    local-info                                                       */
;*---------------------------------------------------------------------*/
(define-struct local-info
   ;; the Bigloo identifier for the variable
   scm-name
   ;; the c identifier for the variable
   c-name
   ;; the name to the c variable associated to this
   ;; variable if the scheme variable holds a function
   value-c-name
   ;; is this variable a constant function?
   function?)

;*---------------------------------------------------------------------*/
;*    global-info                                                      */
;*---------------------------------------------------------------------*/
(define-struct global-info
   ;; the Bigloo identifier for the variable
   scm-name
   ;; the c identifier for the variable
   c-name
   ;; the name to the c variable associated to this
   ;; variable if the scheme variable holds a function
   value-c-name
   ;; is this variable a constant function?
   function?
   ;; the module this global belongs to
   module
   ;; the list of locals contained in the global
   locals)

;*---------------------------------------------------------------------*/
;*    bgl->c ...                                                       */
;*---------------------------------------------------------------------*/
(define (bgl->c bgl::bstring)
   (let ((global (find-scm-global bgl)))
	 (if (global-info? global)
	     (global-info-c-name global)
	     #f)))

;*---------------------------------------------------------------------*/
;*    bgl->c-funcall ...                                               */
;*---------------------------------------------------------------------*/
(define (bgl->c-funcall bgl::bstring)
   (let ((global (find-scm-global bgl)))
      (if (global-info? global)
	  (if (global-info-function? global)
	      (global-info-value-c-name global)
	      #f)
	  #f)))

;*---------------------------------------------------------------------*/
;*    bgl-bgl->c ...                                                   */
;*---------------------------------------------------------------------*/
(define (bgl-bgl->c bgl::bstring loc::bstring)
   (let ((global (find-scm-global bgl)))
      (if (global-info? global)
	  (let ((local (find-scm-local loc global)))
	     (if (local-info? local)
		 (local-info-c-name local)
		 #f))
	  #f)))

;*---------------------------------------------------------------------*/
;*    c-bgl->c ...                                                     */
;*---------------------------------------------------------------------*/
(define (c-bgl->c c::bstring loc::bstring)
   (let ((global (find-c-global c)))
      (if (global-info? global)
	  (let ((local (find-scm-local loc global)))
	     (if (local-info? local)
		 (local-info-c-name local)
		 #f))
	  #f)))

;*---------------------------------------------------------------------*/
;*    c->bgl ...                                                       */
;*---------------------------------------------------------------------*/
(define (c->bgl c::bstring)
   (let ((global (find-c-global c)))
      (if (global-info? global)
	  (global-info-scm-name global)
	  #f)))

;*---------------------------------------------------------------------*/
;*    c-c->bgl ...                                                     */
;*---------------------------------------------------------------------*/
(define (c-c->bgl c::bstring loc::bstring)
   (let ((global (find-c-global c)))
      (if (global-info? global)
	  (let ((local (find-c-local loc global)))
	     (if (local-info? local)
		 (local-info-scm-name local)
		 #f))
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    bdb-set-module-info! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function is called automatically on the startup of every    */
;*    debugged program by the program itself.                          */
;*    -------------------------------------------------------------    */
;*    This function is called as many times as the application counts  */
;*    of modules. Each module declares itself to this function.        */
;*---------------------------------------------------------------------*/
(define (bdb-set-module-info! module-name table)
   (initialize-env!)
   (define (match-global pattern mod-info)
      (match-case pattern
	 ;; the pattern that matches global functions
	 ((?fname (?scm-name ((?value-c-name . ?lnum) . ?bp-c-name)) . ?locals)
	  (let ((glo (bdb:bind-global! (string-upcase scm-name)
				       bp-c-name
				       value-c-name
				       #t
				       mod-info)))
	     ;; and we mark that this initialization function is a regular
	     ;; scheme function
	     (bdb:bind-c-function! bp-c-name lnum scm-name fname)
	     ;; now, in turn, the local variables
	     (global-info-locals-set! glo 
				      (map (lambda (local)
					      (local-info
					       ;; scm-name
					       (string-upcase (car local))
					       ;; c-name
					       (cdr local)
					       ;; value-c-name
					       #unspecified
					       ;; function?
					       #f))
					   locals))))
	 ;; the pattern that matches global variables
	 ((?- (?scm-name . ?c-name))
	  (bdb:bind-global! (string-upcase scm-name) c-name c-name #f mod-info))
	 (else
	  (error "bdb" "Illegal pattern format" pattern))))
   (match-case (bdb-table->list table)
      ((?module ?globals . ?classes)
       (set! *class-env* (append classes *class-env*))
       (match-case module
	  ((?name ?lnum ?init . ?src)
	   (let ((mod-info (bind-module! (string-upcase name)
					 init
					 src
					 lnum)))
	      ;; we now bind all the source files
	      (for-each (lambda (file)
			   (bdb:bind-file! file mod-info))
			src)
	      ;; we now proceed the global variables. we use
	      ;; two encoding frameworks for global variables.
	      ;; the first one denotes variables and the second
	      ;; denotes functions.
	      (for-each (lambda (pattern)
			   (match-global pattern mod-info))
			globals)))
	  (else
	   (error "bdb" "Illegal module table format" module))))
      (else
       (error "bdb" "Illegal module format" (bdb-table->list table)))))

;*---------------------------------------------------------------------*/
;*    *module-env* ...                                                 */
;*    -------------------------------------------------------------    */
;*    The module environment.                                          */
;*---------------------------------------------------------------------*/
(define *module-env*     #unspecified)
(define *global-scm-env* #unspecified)
(define *global-c-env*   #unspecified)
(define *file-env*       #unspecified)
(define *function-env*   #unspecified)
(define *class-env*      #unspecified)

;*---------------------------------------------------------------------*/
;*    initialize-env! ...                                              */
;*---------------------------------------------------------------------*/
(define (initialize-env!)
   (if (not (hashtable? *module-env*))
       (begin
 	  (set! *module-env* (make-hashtable))
	  (set! *global-scm-env* (make-hashtable))
	  (set! *global-c-env* (make-hashtable))
	  (set! *file-env* (make-hashtable))
	  (set! *function-env* (make-hashtable))
	  (set! *class-env* '()))))

;*---------------------------------------------------------------------*/
;*    bind-module! ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function binds a module with an empty list of global. The   */
;*    globals will be added later on.                                  */
;*---------------------------------------------------------------------*/
(define (bind-module!::struct name::bstring cname::bstring file-list lnum)
   (let ((new (module-info
	       ;; name
	       name
	       ;; sources
	       file-list
	       ;; globals
	       '()
	       ;; init-c-name
	       cname)))
      ;; we bind the module initialization function
      (bdb:bind-c-function! cname
			    lnum
			    (string-append "(@ " name ")")
			    (car file-list))
      ;; and we bind the module itself
      (hashtable-put! *module-env* name new)
      new))

;*---------------------------------------------------------------------*/
;*    find-scm-global ...                                              */
;*    -------------------------------------------------------------    */
;*    Find a global variable from its Scheme name.                     */
;*---------------------------------------------------------------------*/
(define (find-scm-global scm-name . module)
   (if (not (hashtable? *global-scm-env*))
       #f
       (let ((module (if (null? module)
			 '()
			 (car module))))
	  (find-global/env hashtable-get *global-scm-env* scm-name module))))

;*---------------------------------------------------------------------*/
;*    find-c-global ...                                                */
;*    -------------------------------------------------------------    */
;*    Find a global from variable from its C name.                     */
;*---------------------------------------------------------------------*/
(define (find-c-global c-name . module)
   (if (not (hashtable? *global-c-env*))
       #f
       (let ((module (if (null? module)
			 '()
			 (car module))))
	  (find-global/env hashtable-get *global-c-env* c-name module))))

;*---------------------------------------------------------------------*/
;*    find-global/env ...                                              */
;*---------------------------------------------------------------------*/
(define (find-global/env get-hash env name module)
   (let ((bucket (get-hash env name)))
      (cond
	 ((not (pair? bucket))
	  #f)
	 ((null? (cdr bucket))
	  #f)
	 ((null? module)
	  (cadr bucket))
	 ((not (module-info? module))
	  #f)
	 (else
	  (let loop ((globals (cdr bucket)))
	     (cond
		((null? globals)
		 #f)
		((eq? (global-info-module (car globals)) module)
		 (car globals))
		(else
		 (loop (cdr globals)))))))))

;*---------------------------------------------------------------------*/
;*    bdb:bind-global! ...                                             */
;*---------------------------------------------------------------------*/
(define (bdb:bind-global! scm-name c-name::bstring val-c-name fun? module)
   (let ((global (find-scm-global scm-name module)))
      (if (global-info? global)
	  (error "bdb" "Illegal global redefinition" scm-name)
	  (let ((new (global-info
		      ;; scm-name
		      scm-name
		      ;; c-name
		      c-name
		      ;; value-c-name
		      val-c-name
		      ;; function?
		      fun?
		      ;; module
		      module
		      ;; locals
		      '()))
		(scm-bucket (hashtable-get *global-scm-env* scm-name))
		(c-bucket   (hashtable-get *global-c-env* scm-name)))
	     (module-info-globals-set! module
				       (cons new (module-info-globals module)))
	     (if (not (pair? scm-bucket))
		 (hashtable-put! *global-scm-env*
				 scm-name 
				 (list scm-name new))
		 (set-cdr! (cdr scm-bucket) (cons new (cddr scm-bucket))))
	     (if (not (pair? c-bucket))
		 (hashtable-put! *global-c-env*
				 c-name
				 (list c-name new))
		 (set-cdr! (cdr c-bucket) (cons new (cddr c-bucket))))
	     new))))

;*---------------------------------------------------------------------*/
;*    bdb:bind-c-function! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function binds a module with an empty list of global. The   */
;*    globals will be added later on.                                  */
;*---------------------------------------------------------------------*/
(define (bdb:bind-c-function! cname::bstring line::long scm-name::bstring file)
   (let ((new (function-info
	       ;; c-name
	       cname
	       ;; scm-name
	       scm-name
	       ;; file-name
	       file
	       ;; line-num
	       line)))
      (hashtable-put! *function-env* cname new)
      new))

;*---------------------------------------------------------------------*/
;*    find-scm-local ...                                               */
;*---------------------------------------------------------------------*/
(define (find-scm-local scm-name::bstring global)
   (find-local/get-key local-info-scm-name scm-name global))

;*---------------------------------------------------------------------*/
;*    find-c-local ...                                                 */
;*---------------------------------------------------------------------*/
(define (find-c-local c-name::bstring global)
   (find-local/get-key local-info-c-name c-name global))

;*---------------------------------------------------------------------*/
;*    find-local/get-key ...                                           */
;*---------------------------------------------------------------------*/
(define (find-local/get-key get-key name::bstring global)
   (let loop ((locals (global-info-locals global)))
      (cond
	 ((null? locals)
	  #f)
	 ((string=? (get-key (car locals)) name)
	  (car locals))
	 (else
	  (loop (cdr locals))))))

;*---------------------------------------------------------------------*/
;*    find-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (find-module module::bstring)
   (if (not (hashtable? *module-env*))
       #f
       (hashtable-get module *module-env*)))

;*---------------------------------------------------------------------*/
;*    find-file ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-file file::bstring)
   (if (not (hashtable? *file-env*))
       #f
       (hashtable-get *file-env* file)))

;*---------------------------------------------------------------------*/
;*    bdb:bind-file! ...                                               */
;*    -------------------------------------------------------------    */
;*    This function binds a file with an empty list of global. The     */
;*    globals will be added later on.                                  */
;*---------------------------------------------------------------------*/
(define (bdb:bind-file! name::bstring module)
   (let ((new (file-info
	       ;; name
	       name
	       ;; module
	       module)))
      (hashtable-put! *file-env* name new)
      new))

;*---------------------------------------------------------------------*/
;*    bgl-source ...                                                   */
;*---------------------------------------------------------------------*/
(define (bgl-source fun)
   (let ((global  (find-scm-global fun))
	 (default '("no source file" . 0)))
      (if (global-info? global)
	  (let* ((c-name (global-info-c-name global))
		 (finfo (hashtable-get *function-env* c-name)))
	     (if (function-info? finfo)
		 (cons (function-info-file-name finfo)
		       (function-info-line-num finfo))
		 default))
	  default)))
	  
;*---------------------------------------------------------------------*/
;*    bgl-get-classes ...                                              */
;*---------------------------------------------------------------------*/
(define (bgl-get-classes)
   *class-env*)
	  
   
