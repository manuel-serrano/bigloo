;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Module/module5.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 17:14:08 2025                          */
;*    Last change :  Sun Sep 21 22:58:27 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Compilation of the a Module5 clause.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_module5
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   
   (import engine_param
	   tools_error
	   module_module
	   module_class
	   expand_eps
	   ast_node
	   ast_var
	   ast_env
	   ast_glo-decl
	   type_type
	   type_env)

   (export (module5-expand ::pair-nil)
	   (module5-import-def ::Module ::Decl)
	   (module5-ast! ::Module)
	   (module5-main ::Module)
	   (module5-imported-unit ::Module ::procedure)
	   (module5-extern-plugin-c ::Module ::pair))

   (export (class CDef::Def
	      (args read-only)
	      (name::bstring read-only)
	      (infix::bool read-only)
	      (macro::bool read-only))))

;*---------------------------------------------------------------------*/
;*    module5-expand ...                                               */
;*---------------------------------------------------------------------*/
(define (module5-expand x)
   (module5-expander x initial-expander))

;*---------------------------------------------------------------------*/
;*    module5-import-def ...                                           */
;*---------------------------------------------------------------------*/
(define (module5-import-def mod::Module decl::Decl)
   (with-access::Decl decl ((dmod mod) def id)
      (if (eq? mod dmod)
	  def
	  (module5-get-export-def dmod id))))

;*---------------------------------------------------------------------*/
;*    module5-ast! ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5-ast! mod::Module)
   
   (define (procedure-args src id mid)
      (match-case src
	 ((define (?- . ?args) . ?-) args)
	 ((define-inline (?- . ?args) . ?-) args)
	 ((define-method (?- . ?args) . ?-) args)
	 ((define-generic (?- . ?args) . ?-) args)
	 ((define (and (? symbol?)) (lambda ?args . ?-)) args)
	 (else (error mid (format "Illegal procedure source \"~a\"" id) src))))
   
   (define (import-kind src ronly)
      (if (not ronly)
	  'variable
	  (match-case src
	     ((define (?- . ?args) . ?-) 'procedure)
	     ((define-inline (?- . ?args) . ?-) 'inline)
	     ((define-generic (?- . ?args) . ?-) 'generic)
	     ((define (and (? symbol?)) (lambda ?args . ?-)) 'procedure)
	     ((define-macro (?- . ?args) . ?-) 'macro)
	     ((define-expander (?- . ?args) . ?-) 'expander)
	     ((define-class . ?-) 'class)
	     (else 'variable))))

   (define (declare-definition! kind id alias mid scope src def::Def)
      (case kind
	 ((variable)
	  (declare-global-svar! id alias
	     mid scope src src))
	 ((procedure)
	  (declare-global-sfun! id alias (procedure-args src id mid)
	     mid scope 'sfun src src))
	 ((inline)
	  (declare-global-sfun! id alias (procedure-args src id mid)
	     mid scope 'sifun src src))
	 ((generic)
	  (declare-global-sfun! id alias (procedure-args src id mid)
	     mid scope 'sgfun src src))
	 ((macro)
	  (with-access::Def def (src)
	     (add-macro-definition! src id)))
	 ((expander)
	  (with-access::Def def (src)
	     (add-macro-definition! src id)))
	 ((c-function)
	  (with-access::CDef def (name type infix args macro)
	     (declare-global-cfun! id alias 'foreign name type args
		#f macro src src)))
	 ((c-variable)
	  (with-access::CDef def (name type macro)
	     (declare-global-cvar! id alias name type macro src src)))
	 ((class)
	  (with-access::Def def (src id scope decl)
	     (with-access::Decl decl (scope)
		(tprint "ID=" id)
		(declare-global-svar! id id mid scope src src)
		(declare-class! (cdr src) mid scope #f #f src src))))
	 (else
	  (error "module5-ast"
	     (format "Unsupported definition kind \"~a\"" kind)
	     id))))
   
   (with-access::Module mod (defs decls exports (mid id))
      
      (open-string-hashtable-for-each defs
	 (lambda (k d)
	    (with-access::Def d (src kind id decl ronly)
	       (let ((alias (if (isa? decl Decl)
				(with-access::Decl decl (alias) alias)
				id))
		     (scope (if (isa? decl Decl)
				(with-access::Decl decl (scope) scope)
				'static)))
		  (declare-definition! kind id id mid scope src d)))))
      
      (open-string-hashtable-for-each decls
	 (lambda (k d)
	    (with-access::Decl d (def id alias ronly scope (imod mod))
	       (when (eq? scope 'import)
		  (let ((def (module5-get-export-def imod id)))
		     (with-access::Def def (kind id src)
			(with-access::Module imod ((mid id))
			   (declare-definition! kind id alias mid 'import src def))))))))))

;*---------------------------------------------------------------------*/
;*    module5-main ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5-main mod::Module)
   (with-access::Module mod (main id)
      (when main
	 (let ((v (find-global/module main id)))
	    (if v
		(with-access::global v (import)
		   (set! import 'export)
		   v)
		(error id "Cannot find main definition" main))))))

;*---------------------------------------------------------------------*/
;*    module5-imported-unit ...                                        */
;*---------------------------------------------------------------------*/
(define (module5-imported-unit mod::Module expand)
   (with-access::Module mod (inits path)
      (let ((body (map (lambda (imod)
			  (with-access::Module imod (id checksum)
			     (module5-expand-and-resolve! imod
				(lambda ()
				   (create-hashtable :weak 'open-string)))
			     (module5-checksum! imod)
			     (declare-global-sfun! 'module-initialization
				'module-initialization
				'(checksum::long path::string) id 'import 'sfun
				#f #f)
			     `((@ module-initialization ,id) ,checksum ,path)))
		     inits)))
	 
	 (unit 'imported-modules 12 body #f #f))))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc mod msg obj container)
   (let ((id (if (isa? mod Module)
		 (with-access::Module mod (id) id)
		 "module5")))
      (match-case (cond
		   ((epair? obj) (cer obj))
		   ((epair? container) (cer container))
		   (else #f))
	 ((at ?fname ?loc) (error/location id msg obj fname loc))
	 (else (error id msg obj)))))

;*---------------------------------------------------------------------*/
;*    parse-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-ident id src mod)
   (let* ((s (symbol->string id))
	  (l (-fx (string-length s) 2)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i l)
	     (values id #unspecified))
	    ((char=? (string-ref s i) #\:)
	     (if (char=? (string-ref s (+fx i 1)) #\:)
		 (if (=fx i (-fx l 3))
		     (error/loc mod "Illegal identifier" id src)
		     (values (string->symbol (substring s 0 i))
			(substring s (+fx i 2))))
		 (loop (+fx i 2))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-c mod::Module expr::pair)

   (define (parse-include string clause mod::Module)
      (unless (member string *include-foreign*)
	 (set! *include-foreign* (cons string *include-foreign*))))

   (define (illegal-args args src mod)
      (let loop ((args args))
	 (cond
	    ((null? args)
	     #f)
	    ((symbol? args)
	     (multiple-value-bind (id type)
		(parse-ident args src mod)
		(unless (string? type)
		   args)))
	    ((not (pair? args))
	     args)
	    ((not (symbol? (car args)))
	     args)
	    (else
	     (multiple-value-bind (id type)
		(parse-ident (car args) src mod)
		(if (string? type)
		    (loop (cdr args))
		    args))))))
      
   (define (parse-function macro infix ident args name clause mod::Module)
      (multiple-value-bind (id type)
	 (parse-ident ident clause mod)
	 (cond
	    ((not (string? type))
	     (error/loc mod "Missing C type" ident clause))
	    ((illegal-args args clause mod)
	     =>
	     (lambda (args) (error/loc mod "Illegal C args" args clause)))
	    (else
	     (co-instantiate
		   ((def (instantiate::CDef
			    (id id)
			    (type (string->symbol type))
			    (kind 'c-function)
			    (src clause)
			    (ronly #t)
			    (decl decl)
			    (args args)
			    (name name)
			    (macro macro)
			    (infix infix)))
		    (decl (instantiate::Decl
			     (id id)
			     (alias id)
			     (mod mod)
			     (src clause)
			     (ronly #t)
			     (scope 'extern)
			     (def def))))
		(with-access::Module mod (decls defs exports)
		   (hashtable-put! exports (symbol->string! id) decl)
		   (hashtable-put! decls (symbol->string! id) decl)
		   (hashtable-put! defs (symbol->string! id) def)))))))

   (define (parse-variable macro ident name clause mod::Module)
      (multiple-value-bind (id type)
	 (parse-ident ident clause mod)
	 (cond
	    ((not (string? type))
	     (error/loc mod "Missing C type" ident clause))
	    (else
	     (co-instantiate
		   ((def (instantiate::CDef
			    (id id)
			    (type (string->symbol type))
			    (kind 'c-variable)
			    (src clause)
			    (ronly #t)
			    (decl decl)
			    (args '())
			    (name name)
			    (macro macro)
			    (infix #f)))
		    (decl (instantiate::Decl
			     (id id)
			     (alias id)
			     (mod mod)
			     (src clause)
			     (ronly #t)
			     (scope 'extern)
			     (def def))))
		(with-access::Module mod (decls defs exports)
		   (hashtable-put! exports (symbol->string! id) decl)
		   (hashtable-put! decls (symbol->string! id) decl)
		   (hashtable-put! defs (symbol->string! id) def)))))))
   
   (define (parse-clause clause mod::Module)
      (with-access::Module mod (decls)
      (match-case clause
	 ((include (and (? string?) ?string))
	  (parse-include string clause mod))
	 ((type (and (? symbol?) ?id) (and (? string?) ?name))
	  (declare-type! id name 'C))
	 ((macro (and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #t #f ident args name clause mod))
	 ((infix macro (and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #t #t ident args name clause mod))
	 (((and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #f #f ident args name clause mod))
	 ((macro (and (? symbol?) ?ident) (and (? string?) ?name))
	  (parse-variable #t ident name clause mod))
	 (((and (? symbol?) ?ident) (and (? string?) ?name))
	  (parse-variable #f ident name clause mod))
	 (else
	  (error/loc mod "Illegal extern \"C\" module clause" clause expr)))))
   
   (for-each (lambda (c) (parse-clause c mod)) (cddr expr)))

