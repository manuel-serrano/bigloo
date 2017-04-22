;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Bdb/bdb_emit.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  8 17:32:59 1998                          */
;*    Last change :  Fri Apr 21 18:44:14 2017 (serrano)                */
;*    Copyright   :  1992-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The emission of the Bdb identifier translation table.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bdb_emit
   (include "Ast/node.sch"
	    "Ast/unit.sch"
	    "Tools/location.sch")
   (import  tools_shape
	    tools_error
	    tools_misc
	    tools_location
	    type_env
	    type_cache
	    object_class
	    object_slots
	    ast_sexp
	    ast_env
	    ast_ident
	    ast_unit
	    module_module
	    module_include
	    engine_param
	    engine_configure
	    object_class
	    backend_c_prototype
	    backend_cplib)
   (export  (emit-bdb-info globals ::output-port)))

;*---------------------------------------------------------------------*/
;*    emit-bdb-info ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-bdb-info globals port)
   ;; we save the output port
   (set! *c-port* port)
   ;; the declaration of the association table
   (newline port)
   (newline port)
   (fprint port "/* bdb association table */")
   (let ((global-table-name "__bdb_info"))
      (fprint port
	      #"static struct bdb_fun_info {\n"
	      #"   char *sname, *cname;\n"
	      #"} " global-table-name "[] = { "))
   ;; first we emit the magic number to ensure version correctness
   (fprint port "   /* Magic number to ensure comp/dbg compatibility */")
   (fprint port
	   "   {(char *)" bgl-foreign-BDB_LIBRARY_MAGIC_NUMBER
	   ", (char *)" bgl-foreign-BDB_LIBRARY_MAGIC_NUMBER "},")
   ;; then we emit information about the module and the source files
   (fprint port "   /* Module and source file identification */")
   (fprint port
	   "   {\"" *module* "\", \""
	   (bigloo-module-mangle (symbol->string
				  (unit-initializer-id
				   (unit-id (get-toplevel-unit))))
				 (symbol->string *module*))
	   "\" },")
   (for-each (lambda (src)
		(fprint port "   {\"" src "\", 0 },"))
	     *src-files*)
   (fprint port
	   "   { 0, (char *)"
	   (let ((loc (find-location *module-clause*)))
	      (if (location? loc)
		  (location-lnum loc)
		  "0"))
	   " },")
   ;; we start scanning the global functions
   (fprint port "   /* Global functions */")
   (for-each (lambda (global)
		(enter-function (global-id global))
		(bdb-global-sfun! global)
		(leave-function))
	     globals)
   ;; then the non function global variables.
   (fprint port "   /* Global variables */")
   (for-each-global! (lambda (global)
			(if (and (or (and (eq? (global-module global) *module*)
					  (eq? (global-import global) 'export))
				     (>fx (global-occurrence global) 0))
				 (svar? (global-value global)))
			    (bdb-global-svar! global))))
   (fprint port "   {0, 0},")
   ;; and at last, the Bigloo classes
   (emit-bdb-class-types port)
   (fprint port #"   0};\n"))

;*---------------------------------------------------------------------*/
;*    emit-bdb-class-types ...                                         */
;*---------------------------------------------------------------------*/
(define (emit-bdb-class-types oport)
   (if (pair? (get-class-list))
       (fprint oport "   /* Bigloo classes */"))
   (for-each (lambda (class)
		(with-access::tclass class (holder)
		   (if (eq? (global-module holder) *module*)
		       (fprint oport
			       "   {\"" (type-size class)
			       " *\", 0 },"))))
	     (get-class-list)))

;*---------------------------------------------------------------------*/
;*    bdb-global-sfun! ...                                             */
;*---------------------------------------------------------------------*/
(define (bdb-global-sfun! global)
   (let* ((sfun     (global-value global))
	  (sfun-loc (sfun-loc sfun))
	  (clo      (sfun-the-closure sfun)))
      (if (and (location? sfun-loc)
	       (global-user? global))
	  (begin
	     (set-variable-name! global)
	     (let ((fname    (location-fname sfun-loc))
		   (lnum     (location-lnum sfun-loc))
		   (id       (let ((id (global-id global)))
				(if (eq? id 'TOPLEVEL-INIT)
				    (symbol-append '@ (global-module global))
				    id)))
		   (val-name (if (global? clo)
				 (global-name clo)
				 0))
		   (bp-name  (global-name global)))
		(fprint *c-port* "   {\"" fname "\", (char *)" lnum " },")
		(fprint *c-port* "   {\"" id "\", 0},")
		(if (not (number? val-name))
		    (fprint *c-port* "   {\"" val-name "\", \"" bp-name "\"},")
		    (fprint *c-port* "   {" val-name ", \"" bp-name "\"},"))
		(bdb-sfun! sfun)
		(fprint *c-port* "     {0, 0},"))))))

;*---------------------------------------------------------------------*/
;*    bdb-global-svar! ...                                             */
;*---------------------------------------------------------------------*/
(define (bdb-global-svar! global)
   (let* ((svar     (global-value global))
	  (svar-loc (svar-loc svar)))
      (if (location? svar-loc)
	  (begin
	     (set-variable-name! global)
	     (let ((fname (location-fname svar-loc))
		   (lnum  (location-lnum svar-loc))
		   (id    (global-id global))
		   (name  (global-name global)))
		(fprint *c-port* "   {\"" fname "\", (char *)" lnum " },")
		(fprint *c-port* "   {\"" id "\", \"" name "\"},"))))))
   
;*---------------------------------------------------------------------*/
;*    bdb-sfun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (bdb-sfun! sfun)
   (with-access::sfun sfun (args body)
      (for-each bdb-emit-local-info! args)
      (bdb! body)))

;*---------------------------------------------------------------------*/
;*    *c-port* ...                                                     */
;*---------------------------------------------------------------------*/
(define *c-port* #unspecified)

;*---------------------------------------------------------------------*/
;*    bdb-local-variable? ...                                          */
;*    -------------------------------------------------------------    */
;*    Does this local variable should be dumped to the BDB table?      */
;*---------------------------------------------------------------------*/
(define (bdb-local-variable? local)
   (define (bdb-supported-type? local)
      ;; the supported BDB types are the Bigloo type plus some
      ;; literal C types.
      (let ((t (local-type local)))
	 (or (eq? (type-class t) 'bigloo)
	     (eq? t *int*)
	     (eq? t *long*)
	     (eq? t *bool*)
	     (eq? t *real*)
	     (eq? t *char*)
	     (eq? t *string*))))
   ;; a local variable is supported by BDB if it belong to the user
   ;; and if it is of a supported type.
   (and (local-user? local) (bdb-supported-type? local)))

;*---------------------------------------------------------------------*/
;*    bdb-emit-local-info! ...                                         */
;*---------------------------------------------------------------------*/
(define (bdb-emit-local-info! local)
   (if (bdb-local-variable? local)
       (begin
	  (set-variable-name! local)
	  (let ((id   (local-id local))
		(name (local-name local)))
	     (fprint *c-port* "     {\"" id "\", \"" name "\"},")))))
   
;*---------------------------------------------------------------------*/
;*    bdb! ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (bdb! node::node)
   #unspecified)
   
;*---------------------------------------------------------------------*/
;*    bdb! ::sequence ...                                              */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::sequence)
   (with-access::sequence node (nodes)
      (bdb*! nodes)))

;*---------------------------------------------------------------------*/
;*    bdb! ::sync ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::sync)
   (with-access::sync node (mutex prelock body)
      (bdb! mutex)
      (bdb! prelock)
      (bdb! body)))

;*---------------------------------------------------------------------*/
;*    bdb! ::extern ...                                                */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::extern)
   (with-access::extern node (expr*)
      (bdb*! expr*)))

;*---------------------------------------------------------------------*/
;*    bdb! ::cast ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::cast)
   (with-access::cast node (arg)
      (bdb! arg)))

;*---------------------------------------------------------------------*/
;*    bdb! ::setq ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::setq)
   (with-access::setq node (value)
      (bdb! value)))

;*---------------------------------------------------------------------*/
;*    bdb! ::conditional ...                                           */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::conditional)
   (with-access::conditional node (test true false)
      (bdb! test)
      (bdb! true)
      (bdb! false)))

;*---------------------------------------------------------------------*/
;*    bdb! ::fail ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::fail)
   (with-access::fail node (proc msg obj)
      (bdb! proc)
      (bdb! msg)
      (bdb! obj)))

;*---------------------------------------------------------------------*/
;*    bdb! ::switch ...                                                */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::switch)
   (with-access::switch node (clauses test)
      (bdb! test)
      (for-each (lambda (clause) (bdb! (cdr clause))) clauses)))

;*---------------------------------------------------------------------*/
;*    bdb! ::let-fun ...                                               */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (bdb-sfun! (local-value local)))
		locals)
      (bdb! body)))

;*---------------------------------------------------------------------*/
;*    bdb! ::let-var ...                                               */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::let-var)
   (with-access::let-var node (body bindings)
       (for-each (lambda (binding)
		    (let ((var (car binding)))
		       (bdb! (cdr binding))
		       (bdb-emit-local-info! (car binding))))
		 bindings)
       (bdb! body)))
 
;*---------------------------------------------------------------------*/
;*    bdb! ::set-ex-it ...                                             */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::set-ex-it)
   (with-access::set-ex-it node (body)
      (bdb! body)))

;*---------------------------------------------------------------------*/
;*    bdb! ::jump-ex-it ...                                            */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (bdb! exit)
      (bdb! value)))

;*---------------------------------------------------------------------*/
;*    bdb! ::make-box ...                                              */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::make-box)
   (with-access::make-box node (value)
      (bdb! value)))

;*---------------------------------------------------------------------*/
;*    bdb! ::box-set! ...                                              */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::box-set!)
   (with-access::box-set! node (value)
      (bdb! value)))

;*---------------------------------------------------------------------*/
;*    bdb! ::app-ly ...                                                */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::app-ly)
   (with-access::app-ly node (fun arg)
      (bdb! fun)
      (bdb! arg)))

;*---------------------------------------------------------------------*/
;*    bdb! ::funcall ...                                               */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::funcall)
   (with-access::funcall node (fun args)
      (bdb! fun)
      (bdb*! args)))
		
;*---------------------------------------------------------------------*/
;*    bdb*! ...                                                        */
;*---------------------------------------------------------------------*/
(define (bdb*! nodes)
   (let loop ((hook nodes))
      (if (null? hook)
	  'done
	  (begin
	     (bdb! (car hook))
	     (loop (cdr hook))))))

;*---------------------------------------------------------------------*/
;*    bdb! ::app ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (bdb! node::app)
   (with-access::app node (args)
      (bdb*! args)))
	      
