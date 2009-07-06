;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/css-parser.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 20 07:52:58 2005                          */
;*    Last change :  Tue Jun 30 08:41:31 2009 (serrano)                */
;*    Copyright   :  2005-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    CSS parsing                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_css-parser
   
   (option (set! *unsafe-type* #t)
	   (set! *unsafe-arity* #t))

   (import __web_css-ast)
   
   (export (css-grammar)
	   (css-parser o::css-stylesheet
		       make-klass::procedure
		       make-element-name::procedure
		       make-declaration::procedure)))

;*---------------------------------------------------------------------*/
;*    css-grammar ...                                                  */
;*---------------------------------------------------------------------*/
(define (css-grammar)

   (lalr-grammar
      
      (CDO CDC INCLUDES DASHMATCH STRING IDENT HASH
	   IMPORT_SYM PAGE_SYM MEDIA_SYM FONT_FACE_SYM CHARSET_SYM
	   ATKEYWORD IMPORTANT_SYM EMS EXS LENGTH ANGLE TIME FREQ DIMEN
	   PERCENTAGE NUMBREC URI FUNCTION UNICODERANGE RGB NUMBER
	   COLON SEMI-COLON COMMA
	   BRA-OPEN BRA-CLO ANGLE-OPEN ANGLE-CLO PAR-CLO
	   SLASH * + > - DOT = EXTENSION)
      
      (stylesheet
       ((charset? comment* import*)
	(instantiate::css-stylesheet
	   (charset charset?)
	   (comment* comment*)
	   (import* import*)))
       ((charset? comment* import* rule+)
	(instantiate::css-stylesheet
	   (charset charset?)
	   (comment* comment*)
	   (import* import*)
	   (rule* rule+))))
      
      (charset?
       (() #f)
       ((charset) charset))
      
      (charset
       ((CHARSET_SYM STRING SEMI-COLON)
	(instantiate::css-charset
	   (charset (car CHARSET_SYM))
	   (spec (car STRING)))))
      
      (comment*
       (() '())
       ((comment* comment) `(,@comment* ,comment)))
      
      (comment
       ((CDO STRING CDC)
	(instantiate::css-comment
	   (cdo (car CDO))
	   (cdc (car CDC))
	   (content (car STRING)))))
      
      (import*
       (() '())
       ((import* one-import) `(,@import* ,one-import)))
      
      (one-import
       ((import comment*) `(,import ,@comment*)))
      
      (import
       ((IMPORT_SYM STRING medium* SEMI-COLON)
	(instantiate::css-import
	   (value (car STRING))
	   (medium* medium*)))
       ((IMPORT_SYM URI medium* SEMI-COLON)
	(instantiate::css-import
	   (value (instantiate::css-uri (value (car URI))))
	   (medium* medium*))))
      
      (rule+
       ((one-rule) (list one-rule))
       ((rule+ one-rule) `(,@rule+ ,one-rule)))
      
      (one-rule
       ((rule comment*) (list rule comment*)))
      
      (rule
       ((ruleset) ruleset)
       ((media) media)
       ((page) page)
       ((font_face) font_face))
      
      (media
       ((MEDIA_SYM medium+ BRA-OPEN ruleset* BRA-CLO)
	(instantiate::css-media
	   (medium+ medium+)
	   (ruleset* ruleset*))))
      
      (page
       ((PAGE_SYM ident? pseudo_page? BRA-OPEN declaration* BRA-CLO)
	(instantiate::css-page
	   (ident ident?)
	   (pseudopage pseudo_page?)
	   (declaration* declaration*))))
      
      (font_face
       ((FONT_FACE_SYM BRA-OPEN declaration* BRA-CLO)
	(instantiate::css-fontface
	   (declaration* declaration*))))
      
      (medium*
       (() '())
       ((medium* COMMA medium) `(,@medium* ,medium)))
      
      (medium+
       ((medium) (list medium))
       ((medium+ COMMA medium) `(,@medium+ ,medium)))
      
      (medium
       ((IDENT) (car IDENT))
       ((EXTENSION) (instantiate::css-ext (value (car EXTENSION)))))
      
      (ident?
       (() #f)
       ((IDENT) IDENT)
       ((EXTENSION) (instantiate::css-ext (value (car EXTENSION)))))
      
      (pseudo_page?
       (() #f)
       ((pseudo_page) pseudo_page))
      
      (pseudo_page
       ((COLON IDENT)
	(instantiate::css-pseudopage
	   (ident (car IDENT))))
       ((COLON EXTENSION)
	(instantiate::css-pseudopage
	   (ident (instantiate::css-ext
		     (value (car EXTENSION)))))))
      
      (property
       ((IDENT) (car IDENT))
       ((EXTENSION) (instantiate::css-ext (value (car EXTENSION)))))
      
      (ruleset*
       (() '())
       ((ruleset* ruleset) `(,@ruleset* ,ruleset)))
      
      (ruleset
       ((selector+ BRA-OPEN declaration* BRA-CLO)
	(instantiate::css-ruleset
	   (selector+ selector+)
	   (declaration* declaration*))))
      
      (selector+
       ((selector) (list selector))
       ((selector+ COMMA selector) `(,@selector+ ,selector)))
      
      (selector
       ((simple_selector compound_selector*)
	`(,simple_selector ,@compound_selector*))
       ((simple_selector_attr+ compound_selector*)
	(cons (instantiate::css-selector
		 (attr* simple_selector_attr+))
	      compound_selector*)))
      
      (compound_selector*
       (()
	'())
       ((compound_selector* combinator simple_selector)
	`(,@compound_selector* ,combinator ,simple_selector)))
      
      (combinator
       ((+) '+)
       ((>) '>)
       (() '| |))
      
      (simple_selector
       ((element_name)
	(instantiate::css-selector
	   (element element_name)))
       ((element_name simple_selector_attr+)
	(instantiate::css-selector
	   (element element_name)
	   (attr* simple_selector_attr+))))
      
      (simple_selector_attr+
       ((simple_selector_attr)
	(list simple_selector_attr))
       ((simple_selector_attr+ simple_selector_attr)
	`(,@simple_selector_attr+ ,simple_selector_attr)))
      
      (simple_selector_attr
       ((HASH) (instantiate::css-selector-hash (name (car HASH))))
       ((klass) klass)
       ((attrib) attrib)
       ((pseudo) pseudo))
      
      (klass
       ((DOT IDENT)
	(instantiate::css-selector-class
	   (name (car IDENT))))
       ((DOT EXTENSION)
	(instantiate::css-selector-class
	   (name (instantiate::css-ext (value (car EXTENSION)))))))
      
      (element_name
       ((IDENT)
	(instantiate::css-selector-name
	   (name (car IDENT))))
       ((EXTENSION)
	(instantiate::css-selector-name
	   (name (instantiate::css-ext (value (car EXTENSION))))))
       ((*)
	(instantiate::css-selector-name
	   (name '*))))
      
      (attrib
       ((ANGLE-OPEN IDENT ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car IDENT))))
       ((ANGLE-OPEN EXTENSION ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car EXTENSION))))
       ((ANGLE-OPEN IDENT attrib-left attrib-right ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car IDENT))
	   (op attrib-left)
	   (arg attrib-right)))
       ((ANGLE-OPEN EXTENSION attrib-left attrib-right ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car EXTENSION))
	   (op attrib-left)
	   (arg attrib-right))))
      
      (attrib-left
       ((=) "=")
       ((INCLUDES) (car INCLUDES))
       ((DASHMATCH) (car DASHMATCH)))
      
      (attrib-right
       ((IDENT) (car IDENT))
       ((STRING) (car STRING))
       ((EXTENSION) (instantiate::css-ext (value (car EXTENSION)))))
      
      (pseudo
       ((COLON IDENT)
	(instantiate::css-selector-pseudo
	   (expr (car IDENT))))
       ((COLON EXTENSION)
	(instantiate::css-selector-pseudo
	   (expr (car EXTENSION))))
       ((COLON FUNCTION IDENT PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (car IDENT))
	   (fun (car FUNCTION))))
       ((COLON FUNCTION EXTENSION PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (car EXTENSION))
	   (fun (car FUNCTION))))
       ((COLON FUNCTION STRING PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (car STRING))
	   (fun (car FUNCTION))))
       ((COLON FUNCTION URI PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (instantiate::css-uri (value (car URI))))
	   (fun (car FUNCTION))))
       ((COLON FUNCTION unary_operator NUMBER PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (make-unary unary_operator (car NUMBER)))
	   (fun (car FUNCTION)))))
      
      (declaration*
       (()
	'())
       ((declaration* declaration)
	`(,@declaration* ,declaration)))
      
      (declaration
       ((property COLON expr SEMI-COLON)
	(instantiate::css-declaration
	   (property property)
	   (expr expr)
	   (prio "")))
       ((property COLON expr prio SEMI-COLON)
	(instantiate::css-declaration
	   (property property)
	   (expr expr)
	   (prio prio))))
      
      (prio
       ((IMPORTANT_SYM) (car IMPORTANT_SYM)))
      
      (expr
       ((term) (list term))
       ((expr term) `(,@expr ,term))
       ((expr operator term) `(,@expr ,operator ,term)))

      (operator
       ((SLASH) "/")
       ((COMMA) ","))
      
      (unary_operator
       (() #f)
       ((-) "-")
       ((+) "+"))
      
      (term
       ((unary_operator NUMBER) (make-unary unary_operator (car NUMBER)))
       ((unary_operator PERCENTAGE) (make-unary unary_operator (car PERCENTAGE)))
       ((unary_operator LENGTH) (make-unary unary_operator (car LENGTH)))
       ((unary_operator EMS) (make-unary unary_operator (car EMS)))
       ((unary_operator EXS) (make-unary unary_operator (car EXS)))
       ((unary_operator ANGLE) (make-unary unary_operator (car ANGLE)))
       ((unary_operator TIME) (make-unary unary_operator (car TIME)))
       ((unary_operator FREQ) (make-unary unary_operator (car FREQ)))
       ((unary_operator function) (make-unary unary_operator function))
       ((unary_operator EXTENSION) (make-unary unary_operator (car EXTENSION)))
       ((STRING) (car STRING))
       ((IDENT) (car IDENT))
       ((URI) (instantiate::css-uri (value (car URI))))
       ((RGB) (car RGB))
       ((UNICODERANGE) (car UNICODERANGE))
       ((hexcolor) hexcolor))
      
      (function
       ((FUNCTION expr PAR-CLO)
	(instantiate::css-function
	   (fun (car FUNCTION))
	   (expr expr))))
      
      (hexcolor
       ((HASH) (instantiate::css-hash-color (value (car HASH)))))))

;*---------------------------------------------------------------------*/
;*    make-unary ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-unary operator val)
   (if operator
       (list operator val)
       val))

;*---------------------------------------------------------------------*/
;*    css-parser ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is only given for backward compatiblity.           */
;*---------------------------------------------------------------------*/
(define (css-parser o::css-stylesheet make-klass make-elname make-decl)
   (with-access::css-stylesheet o (charset comment* import* rule*)
      (list (if charset
		(css->list charset make-klass make-elname make-decl)
		'())
	    (css->list* comment* make-klass make-elname make-decl)
	    (css->list* import* make-klass make-elname make-decl)
	    (css->list* rule* make-klass make-elname make-decl))))

;*---------------------------------------------------------------------*/
;*    css->list* ...                                                   */
;*---------------------------------------------------------------------*/
(define (css->list* o make-klass make-elname make-decl)
   (map (lambda (o) (css->list o make-klass make-elname make-decl)) o))

;*---------------------------------------------------------------------*/
;*    css->sep-list* ...                                               */
;*---------------------------------------------------------------------*/
(define (css->sep-list* o make-klass make-elname make-decl sep)
   (if (null? o)
       '()
       (let ((tail (append-map (lambda (o)
				  (let ((l (css->list o
						      make-klass
						      make-elname
						      make-decl)))
				     (list sep l)))
			       (cdr o))))
	  (cons (css->list (car o) make-klass make-elname make-decl)
		tail))))

;*---------------------------------------------------------------------*/
;*    css-expr->list ...                                               */
;*---------------------------------------------------------------------*/
(define (css-expr->list o make-klass make-elname make-decl)
   (css->sep-list* o make-klass make-elname make-decl " "))

;*---------------------------------------------------------------------*/
;*    css->list ::obj ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (css->list o::obj make-klass make-elname make-decl)
   (cond
      ((pair? o)
       (css->list* o make-klass make-elname make-decl))
      ((symbol? o)
       (symbol->string o))
      (else
       o)))

;*---------------------------------------------------------------------*/
;*    css->list ::css-ext ...                                          */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-ext make-klass make-elname make-decl)
   (with-access::css-ext o (value)
      value))

;*---------------------------------------------------------------------*/
;*    css->list ::css-uri ...                                          */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-uri make-klass make-elname make-decl)
   (with-access::css-uri o (value)
      (format "url(~a)" value)))

;*---------------------------------------------------------------------*/
;*    css->list ::css-charset ...                                      */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-charset make-klass make-elname make-decl)
   (with-access::css-charset o (charset spec)
      (list charset spec ";\n")))

;*---------------------------------------------------------------------*/
;*    css->list ::css-comment ...                                      */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-comment make-klass make-elname make-decl)
   (with-access::css-comment o (cdo cdc content)
      (list cdo content cdc)))

;*---------------------------------------------------------------------*/
;*    css->list ::css-import ...                                       */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-import make-klass make-elname make-decl)
   (with-access::css-import o (value medium*)
      (list "@import" value
	    (css->list* medium* make-klass make-elname make-decl)
	    ";\n")))

;*---------------------------------------------------------------------*/
;*    css->list ::css-media ...                                        */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-media make-klass make-elname make-decl)
   (with-access::css-media o (medium+ ruleset*)
      (list "@media "
	    (css->list* medium+ make-klass make-elname make-decl)
	    " { "
	    (css->list* ruleset* make-klass make-elname make-decl)
	    " }\n")))
      
;*---------------------------------------------------------------------*/
;*    css->list ::css-page ...                                         */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-page make-klass make-elname make-decl)
   (with-access::css-page o (ident pseudopage declaration*)
      (list "@page "
	    (if ident
		(css->list ident make-klass make-elname make-decl)
		"")
	    (if pseudopage
		(css->list pseudopage make-klass make-elname make-decl)
		"")
	    " { "
	    (css->list* declaration* make-klass make-elname make-decl)
	    " }\n")))
      
;*---------------------------------------------------------------------*/
;*    css->list ::css-fontface ...                                     */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-fontface make-klass make-elname make-decl)
   (with-access::css-fontface o (declaration*)
      (list "@font-face "
	    " { "
	    (css->list* declaration* make-klass make-elname make-decl)
	    " }\n")))

;*---------------------------------------------------------------------*/
;*    css->list ::css-pseudopage ...                                   */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-pseudopage make-klass make-elname make-decl)
   (with-access::css-pseudopage o (ident)
      (list ":" (css->list ident make-klass make-elname make-decl))))

;*---------------------------------------------------------------------*/
;*    css->list ::css-ruleset ...                                      */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-ruleset make-klass make-elname make-decl)
   (with-access::css-ruleset o (selector+ declaration*)
      (list (css->sep-list* selector+ make-klass make-elname make-decl ", ")
	    " {\n"
	    (css->list* declaration* make-klass make-elname make-decl)
	    "}\n")))

;*---------------------------------------------------------------------*/
;*    css->list ::css-selector ...                                     */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-selector make-klass make-elname make-decl)
   (with-access::css-selector o (element attr*)
      (list (css->list element make-klass make-elname make-decl)
	    (css->list* attr* make-klass make-elname make-decl))))

;*---------------------------------------------------------------------*/
;*    css->list ::css-selector-class ...                               */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-selector-class make-klass make-elname make-decl)
   (with-access::css-selector-class o (name)
      (if (string? name)
	  (make-klass name)
	  (css->list name make-klass make-elname make-decl))))

;*---------------------------------------------------------------------*/
;*    css->list ::css-selector-hash ...                                */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-selector-hash make-klass make-elname make-decl)
   (with-access::css-selector-hash o (name)
      (list "#" name)))

;*---------------------------------------------------------------------*/
;*    css->list ::css-selector-name ...                                */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-selector-name make-klass make-elname make-decl)
   (with-access::css-selector-name o (name)
      (if (string? name)
	  (make-elname name)
	  (css->list name make-klass make-elname make-decl))))

;*---------------------------------------------------------------------*/
;*    css->list ::css-selector-attr ...                                */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-selector-attr make-klass make-elname make-decl)
   (with-access::css-selector-attr o (ident op arg)
      (list "[" ident
	    (if op (css->list op  make-klass make-elname make-decl) "")
	    (if arg (css->list arg  make-klass make-elname make-decl) "")
	    "]")))

;*---------------------------------------------------------------------*/
;*    css->list ::css-selector-pseudo ...                              */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-selector-pseudo make-klass make-elname make-decl)
   (with-access::css-selector-pseudo o (expr fun)
      (if fun
	  (list ":"
		(css->list fun make-klass make-elname make-decl)
		"("
		(css->list expr make-klass make-elname make-decl)
		")")
	  (list ":" (css->list expr make-klass make-elname make-decl)))))

;*---------------------------------------------------------------------*/
;*    css->list ::css-declaration ...                                  */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-declaration make-klass make-elname make-decl)
   (with-access::css-declaration o (property expr prio)
      (list (make-decl (css->list property make-klass make-elname make-decl)
		       (css-expr->list expr make-klass make-elname make-decl)
		       (css->list prio make-klass make-elname make-decl))
	    ";\n")))

;*---------------------------------------------------------------------*/
;*    css->list ::css-function ...                                     */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-function make-klass make-elname make-decl)
   (with-access::css-function o (fun expr)
      (list (css->list fun  make-klass make-elname make-decl)
	    "("
	    (css-expr->list expr make-klass make-elname make-decl)
	    ")")))

;*---------------------------------------------------------------------*/
;*    css->list ::css-hash-color ...                                   */
;*---------------------------------------------------------------------*/
(define-method (css->list o::css-hash-color make-klass make-elname make-decl)
   (with-access::css-hash-color o (value)
      (string-append "#" value)))
