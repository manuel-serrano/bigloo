;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Parser/parser.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 24 11:17:34 1995                          */
;*    Last change :  Mon Jul 31 10:11:01 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The C syntax                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module parser_parser
   (include "Translate/ast.sch")
   (import  translate_declaration
	    engine_param
	    parser_lexer
	    translate_decl)
   (export  parser
	    *anonymous-struct-alist*))

;*---------------------------------------------------------------------*/
;*    *anonymous-struct-alist* ...                                     */
;*---------------------------------------------------------------------*/
(define *anonymous-struct-alist* '())

;*---------------------------------------------------------------------*/
;*    parser ...                                                       */
;*---------------------------------------------------------------------*/
(define parser
   (lalr-grammar

      ;; tokens
      (CONSTANT PAR-OPEN PAR-CLO BRA-OPEN BRA-CLO ANGLE-OPEN ANGLE-CLO
       SEMI-COMMA COMMA DOT LDOTS -> ! % ^ & * = / + ~ - >> << BOR != ==
       <<= >>= += -= *= /= %= ^= &= OR= ++ -- ID && OR ? : TYPE-ID <= >=
       < > __asm__ asm auto break case char const __const continue default do double
       else enum extern float for fortran goto if int long register FILE
       return short signed sizeof static _Static_assert struct switch typedef union unsigned
       void volatile while __attribute__ __attribute inline __inline__ __inline
       __extension__ obj_t
       restrict __restrict__ __restrict
       __gnuc_va_list __builtin_va_list _Bool _Float128 )
 
      ;; we build the overall list in a inversed way in order to
      ;; remove a shift/reduce in the grammar.
      (start
       (()
	'())
       ((start external-definition)
        ;; since we return an empty-list for top-level C11 _Static_assert statements
        ;; we check here for that case and just return the the rest of the definitions
        (if (not (eq? external-definition '()))
            `(,external-definition ,@start)
            start))
       ((start c++-definition)
	`(,@c++-definition ,@start)))

      (external-definition
       ((function-definition)
	function-definition)
       ((declaration)
	declaration)
       ((static-assert-statement)
        '()))

      (c++-definition
       ((extern CONSTANT BRA-OPEN c++-declarations BRA-CLO)
	(if (string=? (cadr CONSTANT) "\"C\"")
	    c++-declarations
	    (error "cigloo"
		   "illegal syntax"
		   (list 'extern (car extern) "Illegal construction"))))
       ((extern CONSTANT BRA-OPEN c++-declarations BRA-CLO SEMI-COMMA)
	(if (string=? (cadr CONSTANT) "\"C\"")
	    c++-declarations
	    (error "cigloo"
		   "illegal syntax"
		   (list 'extern (car extern) "Illegal construction")))))

      (c++-declarations
       ((declaration)
	(list declaration))
       
       ((c++-declarations declaration)
	`(,declaration ,@c++-declarations)))
      
      (declaration
       ((declaration-specifiers SEMI-COMMA)
	(ast-declare #f
		     declaration-specifiers
		     '()))
       ((declaration-specifiers init-declarator-list SEMI-COMMA)
	;; since the C grammar does not seem to be Lalr we are
	;; obliged to do an awful hack in order to recognize, before
	;; reading next expression, typedef form in order to change
	;; the lexer behavior.
	(if (typedef-sspec? (storage-class-spec-of-decl-spec
			     declaration-specifiers))
	    (for-each (lambda (decl)
			 ;; there is no need for a match here as in the
			 ;; translate-declaration function because
			 ;; we know that decl is not in a list
			 (let* ((t-ident (get-decl-ident decl))
				(t-id    (ident-id t-ident)))
			    (define-type-id t-id)))
		      init-declarator-list))
	
	;; Record the alias of an anonymous struct.
	(let ((tspecs (type-spec-of-decl-spec declaration-specifiers)))
	   (for-each 
	    (lambda (decl)
	       (let* ((decl    (cond
				  ((decl? decl)
				   decl)
				  ((and (pair? decl)
					(decl? (car decl))
					(or (null? (cdr decl))
					    (and (pair? (cdr decl))
						 (eq? (cadr decl) #unspecified)
						 (null? (cddr decl)))))
				   (car decl))
				  (else
				   (error "cigloo"
					  "internal error"
					  "parsing"))))
		      (t-ident (get-decl-ident decl))
		      (t-id    (ident-id t-ident)))
		  ;; Look at the type being aliased.  If it is an 
		  ;; anonymous struct set its c-name to be this alias.
		  (for-each
		   (lambda (tspec)
		      (let* ((value (type-spec-value tspec))
			     (entry (and (struct-spec? value)
					 (assq value *anonymous-struct-alist*))))
			 (when entry
			    (set-cdr! entry t-ident))))
		   tspecs)))
	    init-declarator-list))
	(ast-declare #f
		     declaration-specifiers
		     init-declarator-list)))
      
      (declaration-specifiers
       ((__extension__ declaration-specifiers)
         declaration-specifiers)
       ((inline-specifier declaration-specifiers)
        declaration-specifiers)
       ((gcc-attributes declaration-specifiers)
        declaration-specifiers)
       ((gcc-attributes)
        '())
       ((storage-class-specifier)
	`(,storage-class-specifier))
       ((storage-class-specifier declaration-specifiers)
	`(,storage-class-specifier ,@declaration-specifiers))
       ((type-specifier)
	`(,type-specifier))
       ((type-specifier declaration-specifiers)
	`(,type-specifier ,@declaration-specifiers))
       ((type-qualifier-specifier)
	`(,type-qualifier-specifier))
       ((type-qualifier-specifier declaration-specifiers)
	`(,type-qualifier-specifier ,@declaration-specifiers)))
      
      (init-declarator-list
       ((init-declarator)
	`(,init-declarator))
       ((init-declarator COMMA init-declarator-list)
	`(,init-declarator ,@init-declarator-list)))
      
      (init-declarator
       ((declarator optional-gcc-attributes)
	declarator)
       ((declarator optional-gcc-attributes = initializer)
	`(,declarator ,initializer)))
      
      (storage-class-specifier
       ((typedef)
	(ast-storage-class-spec (car typedef) 'typedef))
       ((extern)
	(ast-storage-class-spec (car extern) 'extern))
       ((static)
	(ast-storage-class-spec (car static) 'static))
       ((auto)
	(ast-storage-class-spec (car auto) 'auto))
       ((register)
	(ast-storage-class-spec (car register) 'register)))

      
      (type-qualifier-specifier
       ((const)
	(ast-type-qualifier-spec (car const) 'const))
       ((__const)
	(ast-type-qualifier-spec (car __const) 'const))
       ((volatile)
	(ast-type-qualifier-spec (car volatile) 'volatile))
       ((restrict)
	(ast-type-qualifier-spec (car restrict) 'restrict))
       ((__restrict)
	(ast-type-qualifier-spec (car __restrict) 'restrict))
       ((__restrict__)
	(ast-type-qualifier-spec (car __restrict__) 'restrict)))
      
      (type-specifier
       ((FILE)
	(ast-type-spec (car FILE) 'FILE "FILE" #unspecified))
       ((char)
	(ast-type-spec (car char) 'char "char" #unspecified))
       ((short)
	(ast-type-spec (car short) 'short "short" #unspecified))
       ((int)
	(ast-type-spec (car int) 'int "int" #unspecified))
       ((long)
	(ast-type-spec (car long) 'long "long" #unspecified))
       ((signed)
	(ast-type-spec (car signed) 'signed "signed" #unspecified))
       ((unsigned)
	(ast-type-spec (car unsigned) 'unsigned "unsigned" #unspecified))
       ((float)
	(ast-type-spec (car float) 'float "float" #unspecified))
       ((double)
	(ast-type-spec (car double) 'double "double" #unspecified))
       ((_Float128)
        (ast-type-spec (car _Float128) '_Float128 "_Float128" #unspecified))
       ((_Bool)
        (ast-type-spec (car _Bool) '_Bool "_Bool" #unspecified))
       ((void)
        (ast-type-spec (car void) 'void "void" #unspecified))
       ((obj_t)
	(ast-type-spec (car obj_t) 'obj_t "obj" #unspecified))
       ((struct-or-union-specifier)
	(ast-type-spec (ast-coord struct-or-union-specifier)
		       'struct
		       #unspecified
		       struct-or-union-specifier))
       ((enum-specifier)
	(ast-type-spec (ast-coord enum-specifier)
		       'enum
		       #unspecified
		       enum-specifier))
       ((__gnuc_va_list)
        (ast-type-spec (car __gnuc_va_list) '__gnuc_va_list "__gnuc_va_list" #unspecified))
       ((__builtin_va_list)
        (ast-type-spec (car __builtin_va_list) '__builtin_va_list "__builtin_va_list" #unspecified))
       ((TYPE-ID)
	(ast-type-spec (car TYPE-ID) 'TYPE-ID (cadr TYPE-ID) #unspecified)))


      (optional-gcc-attributes
         (()
          #unspecified)
         ((gcc-attributes)
          #unspecified))
        
      (struct-or-union-specifier
       ((struct-or-union identifier BRA-OPEN struct-declaration-list BRA-CLO optional-gcc-attributes)
	(ast-struct-spec (car struct-or-union)
			 (cdr struct-or-union)
			 identifier
			 struct-declaration-list))
       ((struct-or-union TYPE-ID BRA-OPEN struct-declaration-list BRA-CLO optional-gcc-attributes)
	(ast-struct-spec (car struct-or-union)
			 (cdr struct-or-union)
			 (ast-ident (car TYPE-ID) (cadr TYPE-ID))
			 struct-declaration-list))
       ((struct-or-union BRA-OPEN struct-declaration-list BRA-CLO optional-gcc-attributes)
	(let* ((id (ast-ident #f (symbol->string
				  (gensym (string-append *iname* "__s")))))
	       (result (ast-struct-spec (car struct-or-union)
					(cdr struct-or-union)
					id
					struct-declaration-list)))
	  ;; Create an entry in the alist for this struct.
	  (set! *anonymous-struct-alist*
		(cons (list result) *anonymous-struct-alist*))
	  result))
       ((struct-or-union identifier optional-gcc-attributes)
	(ast-struct-spec (car struct-or-union)
			 (cdr struct-or-union)
			 identifier
			 '()))
       ((struct-or-union TYPE-ID optional-gcc-attributes)
	(ast-struct-spec (car struct-or-union)
			 (cdr struct-or-union)
			 (ast-ident (car TYPE-ID) (cadr TYPE-ID))
			 '())))
      
      (struct-or-union
       ((struct optional-gcc-attributes)
	(cons (car struct) 'struct))
       ((union optional-gcc-attributes)
	(cons (car union) 'union)))
      
      (struct-declaration-list
       ((struct-declaration)
	`(,struct-declaration))
       ((struct-declaration struct-declaration-list)
	`(,struct-declaration ,@struct-declaration-list)))
      
      (struct-declaration
       ((type-specifier-list struct-declarator-list optional-gcc-attributes SEMI-COMMA)
	`(,type-specifier-list ,struct-declarator-list)))
      
      (struct-declarator-list
       ((struct-declarator)
	`(,struct-declarator))
       ((struct-declarator COMMA struct-declarator-list)
	`(,struct-declarator ,@struct-declarator-list)))
      
      (struct-declarator
         (()
          (ast-decl (gensym "anonymous_member") #f #f))
         ((declarator)
          declarator)
         ((: constant-expr)
          (ast-decl : #f #f))
         ((declarator : constant-expr)
          declarator))
      
      (enum-specifier
       ((enum BRA-OPEN enumerator-list BRA-CLO)
	(ast-enum-spec (car enum) #f enumerator-list))
       ((enum identifier BRA-OPEN enumerator-list BRA-CLO)
	(ast-enum-spec (car enum) identifier enumerator-list))
       ((enum identifier)
	(ast-enum-spec (car enum) identifier #f)))
      
      (enumerator-list
         (()
          '())
       ((enumerator)
	`(,enumerator))
       ((enumerator COMMA enumerator-list)
	`(,enumerator ,@enumerator-list)))
      
      (enumerator
       ((identifier optional-gcc-attributes)
	identifier)
       ((identifier optional-gcc-attributes = constant-expr)
	identifier))
      
      (declarator
       ((declarator2)
	(ast-decl #f #f declarator2))
       ((pointer declarator2)
	(ast-decl #f pointer declarator2)))
       
      (declarator2
       ((identifier) 
	(ast-decl2 #f identifier #f #f #f #f #f))
       ((PAR-OPEN declarator PAR-CLO)
        (ast-decl2 #f #f declarator #f #f #f #f))
       ((declarator2 ANGLE-OPEN ANGLE-CLO)
        (ast-decl2 #f #f #f declarator2 '() #f #f))
       ((declarator2 ANGLE-OPEN constant-expr ANGLE-CLO)
        (ast-decl2 #f #f #f declarator2 constant-expr #f #f))
       ((declarator2 PAR-OPEN PAR-CLO)
        (ast-decl2 #f #f #f declarator2 #f '() #f))
       ; ((declarator2 PAR-OPEN void PAR-CLO)
       ;  (ast-decl2 #f #f #f declarator2 #f '() #f))
       ((declarator2 PAR-OPEN parameter-type-list PAR-CLO gcc-asm)
        (ast-decl2 #f #f #f declarator2 #f parameter-type-list #f))
       ((declarator2 PAR-OPEN parameter-type-list PAR-CLO)
        (ast-decl2 #f #f #f declarator2 #f parameter-type-list #f))
       ((declarator2 PAR-OPEN parameter-identifier-list PAR-CLO)
        (ast-decl2 #f #f #f declarator2 #f #f parameter-identifier-list)))

      
      (pointer
       ((*)
	(ast-ptr (car *) #f #f))
       ((* type-qualifier-specifier)
	(ast-ptr (car *) #f #f))
       ((* type-specifier-list)
	(ast-ptr (car *) type-specifier-list #f))
       ((* type-qualifier-specifier pointer)
	(ast-ptr (car *) #f pointer))
       ((* pointer)
	(ast-ptr (car *) #f pointer))
       ((* type-specifier-list pointer)
	(ast-ptr (car *) type-specifier-list pointer)))
      
      (type-specifier-list
       ((type-specifier)
	`(,type-specifier))
       ((type-qualifier-specifier type-specifier-list)
	`(,type-qualifier-specifier ,@type-specifier-list))
       ((type-specifier type-qualifier-specifier)
	`(,type-specifier ,type-qualifier-specifier))
       ((type-specifier type-specifier-list)
	`(,type-specifier ,@type-specifier-list)))
      
      (parameter-identifier-list
       ((identifier-list)
	(reverse! identifier-list))
       ((identifier-list COMMA LDOTS)
	(reverse! (cons '... identifier-list))))

      ;; we build the list in the reverse order.
      (identifier-list
       ((identifier)
	`(,identifier))
       ((identifier-list COMMA identifier)
	`(,identifier ,@identifier-list)))
      
      (parameter-type-list
       ((parameter-list)
	(reverse! parameter-list))
       ((parameter-list COMMA LDOTS)
	(reverse! (cons '... parameter-list))))

      ;; this list is built in the reverse order
      (parameter-list
       ((parameter-declaration)
	`(,parameter-declaration))
       ((parameter-list COMMA parameter-declaration)
	`(,parameter-declaration ,@parameter-list)))
      
      (parameter-declaration
       ((type-specifier-list declarator optional-gcc-attributes)
	(ast-para-decl #f type-specifier-list declarator #f))
       ((type-name optional-gcc-attributes)
	(ast-para-decl #f #f #f type-name)))
      
      (type-name
       ((type-specifier-list)
	(ast-t-name #f type-specifier-list #f))
       ((type-specifier-list abstract-declarator)
	(ast-t-name #f type-specifier-list abstract-declarator)))
      
      (abstract-declarator
       ((pointer)
	(ast-adecl #f pointer #f))
       ((abstract-declarator2)
	(ast-adecl #f #f abstract-declarator2))
       ((pointer abstract-declarator2)
	(ast-adecl #f pointer abstract-declarator2)))
      
      (abstract-declarator2
       ((PAR-OPEN abstract-declarator PAR-CLO)
	(ast-adecl2 #f abstract-declarator #f #f #f))
       ((ANGLE-OPEN ANGLE-CLO)
	(ast-adecl2 #f #f #f '() #f))
       ((ANGLE-OPEN constant-expr ANGLE-CLO)
	(ast-adecl2 #f #f #f constant-expr #f))
       ((abstract-declarator2 ANGLE-OPEN ANGLE-CLO)
	(ast-adecl2 #f #f abstract-declarator2 '() #f))
       ((abstract-declarator2 ANGLE-OPEN constant-expr ANGLE-CLO)
	(ast-adecl2 #f #f abstract-declarator2 constant-expr #f))
       ((PAR-OPEN PAR-CLO)
	(ast-adecl2 #f #f #f #f '()))
       ((PAR-OPEN parameter-type-list PAR-CLO)
	(ast-adecl2 #f #f #f #f parameter-type-list))
       ((abstract-declarator2 PAR-OPEN PAR-CLO)
	(ast-adecl2 #f #f abstract-declarator2 #f '()))
       ((abstract-declarator2 PAR-OPEN parameter-type-list PAR-CLO)
	(ast-adecl2 #f #f abstract-declarator2 #f parameter-type-list)))
      
      (initializer
       ((assignment-expr)
	#unspecified)
       ((BRA-OPEN initializer-list BRA-CLO)
	#unspecified)
       ((BRA-OPEN initializer-list COMMA BRA-CLO)
	#unspecified))
       
      (initializer-list
       ((initializer)
	#unspecified)
       ((initializer-list COMMA initializer)
	#unspecified))
      
      (primary-expr
       ((identifier)
	(ident-id identifier))
       ((CONSTANT)
	(cadr CONSTANT))
       ((PAR-OPEN expr PAR-CLO)
	`("(" ,expr ")")))
      
      (postfix-expr
       ((primary-expr)
	primary-expr)
       ((postfix-expr ANGLE-OPEN expr ANGLE-CLO)
	`(,postfix-expr "[" ,expr "]"))
       ((postfix-expr PAR-OPEN PAR-CLO)
	`(,postfix-expr "[]"))
       ((postfix-expr PAR-OPEN argument-expr-list PAR-CLO)
	`(,postfix-expr "(" ,argument-expr-list ")"))
       ((postfix-expr DOT member-name)
	`(,postfix-expr "." ,member-name))
       ((postfix-expr -> member-name)
	`(,postfix-expr "->" ,member-name))
       ((postfix-expr ++)
	`(,postfix-expr "++"))
       ((postfix-expr --)
	`(,postfix-expr "--")))

      (argument-expr-list
       ((assignment-expr)
	#unspecified)
       ((argument-expr-list COMMA assignment-expr)
	#unspecified))
      
      (unary-expr
       ((postfix-expr)
	postfix-expr)
       ((++ unary-expr)
	`("++" ,unary-expr))
       ((-- unary-expr)
	`("--" ,unary-expr))
       ((unary-operator cast-expr)
	`(,unary-operator ,cast-expr))
       ((sizeof unary-expr)
	`("sizeof(" ,unary-expr ")"))
       ((sizeof PAR-OPEN type-name PAR-CLO)
	`("sizeof(" ,type-name ")"))
       ;; expressions of the form a = {5, 6, 9}
       ((BRA-OPEN expr BRA-CLO)
        #unspecified)
       ;;; gcc statement expressions
       ((PAR-OPEN compound-statement PAR-CLO)
        #unspecified)
       ;; this is necessary to correctly parse multiple string
       ;; literals which are automatically concatenated in C
       ;; (e.g., "dog" "cat" -> "dogcat")
       ((constants)
        #unspecified))

      (unary-operator
       ((&)
	&)
       ((*)
	*)
       ((+)
	+)
       ((-)
	-)
       ((~)
	~)
       ((!)
	!))
      
      (cast-expr
         ((unary-expr)
          unary-expr)
         ((PAR-OPEN optional-gcc-attributes type-name PAR-CLO cast-expr)
          `("(" ,type-name ,")" ,cast-expr)))
      
      (multiplicative-expr
       ((cast-expr)
	cast-expr)
       ((multiplicative-expr * cast-expr)
	`(,multiplicative-expr "*" ,cast-expr))
       ((multiplicative-expr / cast-expr)
	`(,multiplicative-expr "/" ,cast-expr))
       ((multiplicative-expr % cast-expr)
	`(,multiplicative-expr "%" ,cast-expr)))
      
      (additive-expr
       ((multiplicative-expr)
	multiplicative-expr)
       ((additive-expr + multiplicative-expr)
	`(,additive-expr "+" ,multiplicative-expr))
       ((additive-expr - multiplicative-expr)
	`(,additive-expr "-" ,multiplicative-expr)))
      
      (shift-expr
       ((additive-expr)
	additive-expr)
       ((shift-expr << additive-expr)
	`(,shift-expr "<<" ,additive-expr))
       ((shift-expr >> additive-expr)
	`(,shift-expr ">>" ,additive-expr)))
      
      (relational-expr
       ((shift-expr)
	shift-expr)
       ((relational-expr < shift-expr)
	`(,relational-expr "<" ,shift-expr))
       ((relational-expr > shift-expr)
	`(,relational-expr ">" ,shift-expr))
       ((relational-expr <= shift-expr)
	`(,relational-expr "<=" ,shift-expr))
       ((relational-expr >= shift-expr)
	`(,relational-expr ">=" ,shift-expr)))
      
      (equality-expr
       ((relational-expr)
	relational-expr)
       ((equality-expr == relational-expr)
	`(,equality-expr "==" ,relational-expr))
       ((equality-expr != relational-expr)
	`(,equality-expr "!=" ,relational-expr)))
      
      (and-expr
       ((equality-expr)
	equality-expr)
       ((and-expr & equality-expr)
	`(,and-expr "&" ,equality-expr)))
      
      (exclusive-or-expr
       ((and-expr)
	and-expr)
       ((exclusive-or-expr ^ and-expr)
	`(,exclusive-or-expr "^" ,and-expr)))
      
      (inclusive-or-expr
       ((exclusive-or-expr)
	exclusive-or-expr)
       ((inclusive-or-expr BOR exclusive-or-expr)
	`(,inclusive-or-expr "|" ,exclusive-or-expr)))
      
      (logical-and-expr
       ((inclusive-or-expr)
	inclusive-or-expr)
       ((logical-and-expr && inclusive-or-expr)
	`(,logical-and-expr "&&" ,inclusive-or-expr)))
      
      (logical-or-expr
       ((logical-and-expr)
	logical-and-expr)
       ((logical-or-expr OR logical-and-expr)
	`(,logical-or-expr "||" ,logical-and-expr)))
      
      (conditional-expr
       ((logical-or-expr)
	logical-or-expr)
       ((logical-or-expr@lexp1 ? expr : conditional-expr)
	`(,lexp1 "?" ,expr ":" ,conditional-expr)))
   
      (assignment-expr
       ((conditional-expr)
	conditional-expr)
       ((DOT identifier assignment-operator assignment-expr)
        `(, identifier ,assignment-operator ,assignment-expr))
       ((unary-expr assignment-operator assignment-expr)
	`(,unary-expr ,assignment-operator ,assignment-expr)))

      (member-name
         ((identifier)
          identifier)
         ((TYPE-ID)
          TYPE-ID))
      
      (assignment-operator
       ((=)
	=)
       ((*=)
	*=)
       ((/=)
	/=)
       ((%=)
	%=)
       ((+=)
	+=)
       ((-=)
	-=)
       ((<<=)
	<<=)
       ((>>=)
	>>=)
       ((&=)
	&=)
       ((^=)
	^=)
       ((OR=)
	OR=))
      
      (expr  
       ((assignment-expr)
	assignment-expr)
       ((expr COMMA assignment-expr)
	`(,expr ,assignment-expr)))

      
      (constant-expr
       ((conditional-expr)
	conditional-expr))
      
      (statement
       ((labeled-statement)
	#unspecified)
       ((compound-statement) 
	#unspecified)
       ((expression-statement) 
	#unspecified)
       ((selection-statement) 
	#unspecified)
       ((iteration-statement) 
	#unspecified)
       ((jump-statement) 
	#unspecified)
       ((static-assert-statement)
        #unspecified)
       ((gcc-attribute-statement)
        #unspecified))
      
      
      (labeled-statement
       ((identifier : statement)
	#unspecified)
       ((case constant-expr : statement)
	#unspecified)
       ((default : statement)
	#unspecified))
      
      (compound-statement
       ((BRA-OPEN BRA-CLO)
	'())
       ((BRA-OPEN declaration-statement-list BRA-CLO)
	declaration-statement-list))

      (declaration-statement-list
         ((declaration-list)
          declaration-list)
         ((statement-list)
          '())
         ((declaration-list declaration-statement-list)
          (append declaration-list declaration-statement-list))
         ((statement-list declaration-statement-list)
          declaration-statement-list))
       
      (declaration-list
       ((declaration)
	`(,declaration))
       ((declaration declaration-list)
	`(,declaration ,@declaration-list)))
      
      (statement-list
       ((statement)
	#unspecified)
       ((statement statement-list)
	#unspecified))
      
      (expression-statement
       ((SEMI-COMMA)
	SEMI-COMMA)
       ((expr SEMI-COMMA)
	`(,expr ,SEMI-COMMA)))
      
      (selection-statement
       ((if PAR-OPEN expr PAR-CLO statement)
	#unspecified)
       ((if PAR-OPEN expr PAR-CLO statement else statement)
	#unspecified)
       ((switch PAR-OPEN expr PAR-CLO statement)
	#unspecified))
      
      (iteration-statement
       ((while PAR-OPEN expr PAR-CLO statement)
	#unspecified)
       ((do statement while PAR-OPEN expr PAR-CLO SEMI-COMMA)
	#unspecified)
       ((for PAR-OPEN SEMI-COMMA SEMI-COMMA PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN SEMI-COMMA SEMI-COMMA expr PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN SEMI-COMMA expr SEMI-COMMA PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN SEMI-COMMA expr SEMI-COMMA expr PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN expr SEMI-COMMA SEMI-COMMA PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN expr SEMI-COMMA SEMI-COMMA expr PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN expr SEMI-COMMA expr SEMI-COMMA PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN expr SEMI-COMMA expr SEMI-COMMA expr PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN declaration SEMI-COMMA PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN declaration SEMI-COMMA expr PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN declaration  expr SEMI-COMMA PAR-CLO statement)
	#unspecified)
       ((for PAR-OPEN declaration  expr SEMI-COMMA expr PAR-CLO statement)
	#unspecified))
      
      (jump-statement
       ((goto identifier SEMI-COMMA)
	#unspecified)
       ((continue SEMI-COMMA)
	#unspecified)
       ((break SEMI-COMMA)
	#unspecified)
       ((return SEMI-COMMA)
	#unspecified)
       ((return expr SEMI-COMMA)
	#unspecified)
       )

      ;; handle C11 _Static_assert keyword
      (static-assert-statement
         ((_Static_assert PAR-OPEN expr PAR-CLO SEMI-COMMA)
          #unspecified))

      ;; handle attributes modifying expressions and statements 
      (gcc-attribute-statement
         ((gcc-attribute SEMI-COMMA)
          #unspecified))
      
      (function-definition
       ((declarator function-body)
	(ast-fun-def #f #f '() declarator function-body))
       ((declaration-specifiers declarator function-body)
	(ast-fun-def #f #f declaration-specifiers declarator function-body)))

      (inline-specifier
       ((inline)
	#unspecified)
       ((__inline__)
	#unspecified)
       ((__inline)
        #unspecified))
	
      (function-body
       ((compound-statement)
	compound-statement))
      
      (identifier
       ((ID)
	(ast-ident (car ID) (cadr ID))))

      ;; This is not an ANSI syntax. This is a Gcc specific syntax in order
      ;; to be able to parse the __attribute__ expression. 
      (gcc-attributes 
        ((gcc-attribute)
          #unspecified)
        ((gcc-attribute gcc-attributes)
          #unspecified))

      (attribute-spec
         ((__attribute__)
          #unspecified)
         ((__attribute)
          #unspecified))
      
      (gcc-attribute
         ((attribute-spec PAR-OPEN PAR-OPEN gcc-attribute-list PAR-CLO PAR-CLO)
          #unspecified))

      (gcc-attribute-list
         (()
          #unspecified)
         ((gcc-attrib)
          #unspecified)
         ((gcc-attrib COMMA gcc-attribute-list)))

       (gcc-attrib
          ((ID)
           #unspecified)
          ((ID PAR-OPEN gcc-attrib-params PAR-CLO)
           #unspecified))

       (gcc-attrib-params
          (()
           #unspecified)
          ((ID)
           #unspecified)
          ((ID PAR-OPEN type-name PAR-CLO)
           #unspecified)
          ((ID COMMA expr)
           #unspecified)
          ((expr)
           #unspecified))
       
      ;; __asm__ is Gcc specific syntax
      ;; this currently only handles __asm__ statements of the form   __asm__ ("" "__isoc99_fscanf")
      ;; used in directing standard functions to specific implementations.
      (gcc-asm
         ((asm-keyword asm-qualifiers PAR-OPEN asm-instructions PAR-CLO)
          #unspecified))

      (asm-keyword
         ((asm)
          #unspecified)
         ((__asm__)
          #unspecified))
      
      (asm-qualifier
         ((volatile)
          #unspecified)
         ((inline)
          #unspecified))
      
      (asm-qualifiers
         (()
          #unspecified)
         ((asm-qualifier asm-qualifiers)
          #unspecified))

      (asm-instruction
         ((constants)
          #unspecified))

      ;; this is really only useful for handling the fact that C will
      ;; concatenate adjacent string constants (e.g., "start" "end" -> "startend")
      ;; This is used in a number of cases, most notably in __asm__ expressions of the
      ;; form  __asm__ ("" "__isoc99_fscanf")
      (constants
         ((CONSTANT)
          #unspecified)
         ((CONSTANT constants)
          #unspecified))
      
      (asm-instructions
         (()
          #unspecified)
         ((asm-instruction asm-instructions)
          #unspecified))

      ))
