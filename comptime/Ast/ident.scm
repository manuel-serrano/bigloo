;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/ident.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 09:33:09 1996                          */
;*    Last change :  Mon Jan  1 10:34:03 2001 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The identifier managment                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_ident
   (import tools_error
	   tools_dsssl
	   type_type
	   type_env
	   type_cache)
   (export (type-ident?::bool ::symbol)
	   (type-of-id::type ::obj loc)
	   (type-of-id/import-location::type ::obj loc loci)
	   (id-of-id::symbol ::obj loc)
	   (fast-id-of-id::symbol ::obj loc)
	   (parse-id::pair ::obj loc)
	   (parse-id/import-location::pair ::obj loc loci)
	   (parse-dsssl::pair ::obj)
	   (check-id::pair ::pair ::obj)
	   (id->name::bstring ::obj)
	   (local-id->name::bstring ::symbol)
	   (mark-symbol-non-user!::symbol  ::symbol)
	   (user-symbol?::bool ::symbol)
	   (make-typed-ident::symbol ::symbol ::symbol)
	   (make-typed-formal::symbol ::symbol)))

;*---------------------------------------------------------------------*/
;*    4dots ...                                                        */
;*---------------------------------------------------------------------*/
(define 4dots (string->symbol "::"))

;*---------------------------------------------------------------------*/
;*    type-ident? ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-ident? sym::symbol)
   (let ((str (symbol->string sym)))
      (and (>fx (string-length str) 2)
	   (char=? (string-ref str 0) #\:)
	   (char=? (string-ref str 1) #\:))))

;*---------------------------------------------------------------------*/
;*    make-typed-ident ...                                             */
;*---------------------------------------------------------------------*/
(define (make-typed-ident sym1 sym2)
   (symbol-append sym1 4dots sym2))

;*---------------------------------------------------------------------*/
;*    make-typed-formal ...                                            */
;*---------------------------------------------------------------------*/
(define (make-typed-formal sym2)
   (symbol-append 4dots sym2))

;*---------------------------------------------------------------------*/
;*    type-of-id ...                                                   */
;*---------------------------------------------------------------------*/
(define (type-of-id::type id loc)
   (if (not (symbol? id))
       (user-error "Illegal identifier" "`'" id)
       (cdr (parse-id id loc))))

;*---------------------------------------------------------------------*/
;*    type-of-id/import-location ...                                   */
;*---------------------------------------------------------------------*/
(define (type-of-id/import-location::type id loc loci)
   (if (not (symbol? id))
       (user-error "Illegal identifier" "`'" id)
       (cdr (parse-id/import-location id loc loci))))

;*---------------------------------------------------------------------*/
;*    id-of-id ...                                                     */
;*---------------------------------------------------------------------*/
(define (id-of-id::symbol id loc)
   (if (not (symbol? id))
       (user-error "parse" "Illegal identifier" id)
       (car (parse-id id loc))))

;*---------------------------------------------------------------------*/
;*    fast-id-of-id ...                                                */
;*---------------------------------------------------------------------*/
(define (fast-id-of-id::symbol id loc)
   (define (untype-ident id)
      (let* ((string (symbol->string id))
	     (len    (string-length string)))
	 (let loop ((walker  0))
	    (cond
	       ((=fx walker len)
		id)
	       ((and (char=? (string-ref string walker) #\:)
		     (<fx walker (-fx len 1))
		     (char=? (string-ref string (+fx walker 1)) #\:))
		(string->symbol (substring string 0 walker)))
	       (else
		(loop (+fx walker 1)))))))
   (cond
      ((dsssl-named-constant? id)
       (gensym 'dsssl))
      ((and (pair? id) (symbol? (car id)))
       (untype-ident (car id)))
      ((not (symbol? id))
       (user-error "parse" "Illegal identifier" id))
      (else
       (untype-ident id))))

;*---------------------------------------------------------------------*/
;*    parse-id/use ...                                                 */
;*---------------------------------------------------------------------*/
(define (parse-id/use::pair id loc use-type::procedure)
   (if (not (symbol? id))
       (user-error "parse" "Illegal identifier" id)
       (let* ((string (symbol->string id))
	      (len    (string-length string)))
	  (let loop ((walker     0)
		     (id-stop    0)
		     (type-start 0))
	     (cond
		((=fx walker len)
		 (cond
		    ((and (=fx id-stop 0) (>fx type-start 0))
		     ;; this empty name variable can be useful to declare
		     ;; prototype so it is legal.
		     (let ((id  (string->symbol ""))
			   (tid (string->symbol (substring string
							   type-start
							   len))))
			(cons id (use-type tid loc))))
		    ((=fx id-stop 0)
		     (cons id (get-default-type)))
		    ((=fx type-start len)
		     ;; empty type are erroneous
		     (user-error "type-of-id"
				 "Illegal formal identifier"
				 id
				 (cons 'error-ident (get-default-type))))
		    (else
		     (let ((id  (string->symbol (substring string 0 id-stop)))
			   (tid (string->symbol (substring string
							   type-start
							   len))))
			(cons id (use-type tid loc))))))
		((and (char=? (string-ref string walker) #\:)
		      (<fx walker (-fx len 1))
		      (char=? (string-ref string (+fx walker 1)) #\:))
		 (if (>fx type-start 0)
		     (user-error "type-of-id"
				 "Illegal formal identifier"
				 id
				 (cons 'error-ident (get-default-type)))
		     (loop (+fx walker 2) walker (+fx walker 2))))
		(else
		 (loop (+fx walker 1) id-stop type-start)))))))

;*---------------------------------------------------------------------*/
;*    parse-id/import-location ...                                     */
;*---------------------------------------------------------------------*/
(define (parse-id/import-location::pair id loc loci)
   (parse-id/use id
		 loc
		 (lambda (tid loc) (use-type/import-loc! tid loc loci))))

;*---------------------------------------------------------------------*/
;*    parse-id ...                                                     */
;*---------------------------------------------------------------------*/
(define (parse-id::pair id loc)
   (parse-id/use id loc use-type!))

;*---------------------------------------------------------------------*/
;*    id->name ...                                                     */
;*---------------------------------------------------------------------*/
(define (id->name::bstring id)
   (let ((name (string-downcase (symbol->string id))))
      (bigloo-mangle name)))

;*---------------------------------------------------------------------*/
;*    local-id->name ...                                               */
;*---------------------------------------------------------------------*/
(define (local-id->name::bstring id::symbol)
   (id->name id))

;*---------------------------------------------------------------------*/
;*    check-id ...                                                     */
;*---------------------------------------------------------------------*/
(define (check-id id src)
   (if (empty-string? (symbol->string (car id)))
       (user-error "Illegal identifier" "`'" src)
       id))

;*---------------------------------------------------------------------*/
;*    parse-dsssl ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-dsssl obj)
   (cond
    ((dsssl-named-constant? obj)
     (cons obj *obj*))
    ((dsssl-defaulted-formal? obj)
     (cons obj *obj*))
    (else
     (user-error "Illegal formal parameter" "" obj))))

;*---------------------------------------------------------------------*/
;*    @label mark-symbol-non-user!@ ...                                */
;*    -------------------------------------------------------------    */
;*    Mark a symbol as a compiler identifier.                          */
;*    -------------------------------------------------------------    */
;*    if this property has to be changed (if non-user has to be        */
;*    changed the modification has to be set inside the                */
;*    @path ../../runtime/Eval/expd-bool.scm@ file                     */
;*    (the __expander_bool library module).                            */
;*---------------------------------------------------------------------*/
(define (mark-symbol-non-user! sym)
   (putprop! sym 'non-user #t)
   sym)

;*---------------------------------------------------------------------*/
;*    user-symbol? ...                                                 */
;*    -------------------------------------------------------------    */
;*    Is a symbol a user symbol?                                       */
;*---------------------------------------------------------------------*/
(define (user-symbol? symbol)
   (not (getprop symbol 'non-user)))
