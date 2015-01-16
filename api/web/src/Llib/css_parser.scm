;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/css_parser.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 20 07:52:58 2005                          */
;*    Last change :  Wed Jan 14 11:12:19 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
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
	   KEYFRAMES_SYM ATKEYWORD IMPORTANT_SYM
	   EMS EXS CHS REMS LENGTH ANGLE TIME FREQ DIMEN
	   PERCENTAGE NUMBREC URI NOT_PSEUDO FUNCTION UNICODERANGE RGB NUMBER
	   COLON SEMI-COLON COMMA
	   BRA-OPEN BRA-CLO ANGLE-OPEN ANGLE-CLO PAR-OPEN PAR-CLO
	   SLASH * + > ~ - DOT = EXTENSION NOT_SYM ONLY_SYM AND_SYM VALUE S)
      
      (stylesheet
       ((S* charset? comment* import*)
	(instantiate::css-stylesheet
	   (charset charset?)
	   (comment* comment*)
	   (import* import*)))
       ((S* charset? comment* import* rule+)
	(instantiate::css-stylesheet
	   (charset charset?)
	   (comment* comment*)
	   (import* import*)
	   (rule* (css-stamp-rules! rule+)))))

      (S*
       (() #f)
       ((S S*) S))
      
      (S+
       ((S S*) S))
      
      (charset?
       (() #f)
       ((charset) charset))
      
      (charset
       ((CHARSET_SYM STRING S* SEMI-COLON)
	(instantiate::css-charset
	   (charset (car CHARSET_SYM))
	   (spec (car STRING)))))
      
      (comment*
       (() '())
       ((comment* comment) `(,@comment* ,comment)))
      
      (comment
       ((CDO S* STRING S* CDC S*)
	(instantiate::css-comment
	   (cdo (car CDO))
	   (cdc (car CDC))
	   (content (car STRING)))))
      
      (import*
       (()
	'())
       ((import* one-import)
	(if (pair? one-import)
	    `(,@import* ,one-import)
	    import*)))
      
      (one-import
       ((import comment*)
	(when (isa? import css-import)
	   `(,import ,@comment*))))
      
      (import
       ((VALUE S*) #f)
       ((IMPORT_SYM S* STRING S* medium* SEMI-COLON S*)
	(instantiate::css-import
	   (value (car STRING))
	   (medium* medium*)))
       ((IMPORT_SYM S* URI S* medium* SEMI-COLON S*)
	(instantiate::css-import
	   (value (instantiate::css-uri (value (car URI))))
	   (medium* medium*))))
      
      (rule+
       ((one-rule rule*) (cons one-rule rule*)))
      
      (rule*
       (() '())
       ((rule* one-rule-value) `(,@rule* ,one-rule-value)))
      
      (one-rule
       ((rule comment*) (list rule comment*)))
      
      (rule
       ((ruleset) ruleset)
       ((media) media)
       ((page) page)
       ((font_face) font_face)
       ((keyframes) keyframes))

      (one-rule-value
       ((rule-value comment*) (list rule-value comment*)))
      
      (rule-value
       ((rule) rule)
       ((VALUE S*) '()))
      
      (media
       ((MEDIA_SYM S* medium+ BRA-OPEN S* ruleset* BRA-CLO S*)
	(instantiate::css-media
	   (medium+ medium+)
	   (ruleset* ruleset*))))

      (page
       ((PAGE_SYM S* ident? pseudo_page? BRA-OPEN declaration* BRA-CLO S*)
	(instantiate::css-page
	   (ident ident?)
	   (pseudopage pseudo_page?)
	   (declaration* declaration*))))
      
      (font_face
       ((FONT_FACE_SYM S* BRA-OPEN declaration* BRA-CLO S*)
	(instantiate::css-fontface
	   (declaration* declaration*))))
      
      (keyframes
       ((KEYFRAMES_SYM S* IDENT S* BRA-OPEN S* keyframe* BRA-CLO S*)
	(instantiate::css-keyframes
	   (operator (car KEYFRAMES_SYM))
	   (ident (car IDENT))
	   (keyframe* keyframe*))))
      
      (medium*
       (() '())
       ((medium+) medium+))
      
      (medium+
       ((medium) (list medium))
       ((medium+ COMMA S* medium) `(,@medium+ ,medium)))

      (medium
       ((ONLY_SYM S* media_type media_expression*)
	(instantiate::css-media-query
	   (operator 'only)
	   (type media_type)
	   (expr* media_expression*)))
       ((NOT_SYM S* media_type media_expression*)
	(instantiate::css-media-query
	   (operator 'not)
	   (type media_type)
	   (expr* media_expression*)))
       ((media_type media_expression*)
	(instantiate::css-media-query
	   (operator #f)
	   (type media_type)
	   (expr* media_expression*))))

      (media_type
       ((IDENT S*) (car IDENT))
       ((EXTENSION S*) (instantiate::css-ext (value (car EXTENSION)))))

      (media_expression*
       (()
	'())
       ((AND_SYM S* media_expression media_expression*)
	(cons media_expression media_expression*)))

      (media_expression
       ((PAR-OPEN S* IDENT S* PAR-CLO S*)
	(cons (car IDENT) #f))
       ((PAR-OPEN S* IDENT S* COLON S* expr PAR-CLO S*)
	(cons (car IDENT) expr)))
	
      (ident?
       (() #f)
       ((IDENT) IDENT)
       ((EXTENSION) (instantiate::css-ext (value (car EXTENSION)))))
      
      (pseudo_page?
       (() #f)
       ((pseudo_page) pseudo_page))
      
      (pseudo_page
       ((COLON IDENT S*)
	(instantiate::css-pseudopage
	   (ident (car IDENT))))
       ((COLON EXTENSION S*)
	(instantiate::css-pseudopage
	   (ident (instantiate::css-ext
		     (value (car EXTENSION)))))))
      
      (property
       ((IDENT S*) (car IDENT))
       ((EXTENSION S*) (instantiate::css-ext (value (car EXTENSION)))))
      
      (ruleset*
       (() '())
       ((ruleset* ruleset) `(,@ruleset* ,ruleset)))
      
      (ruleset
       ((selector+ BRA-OPEN declaration* BRA-CLO S*)
	(instantiate::css-ruleset
	   (selector+ selector+)
	   (declaration* declaration*))))
      
      (keyframe*
       (() '())
       ((keyframe* keyframe) `(,@keyframe* ,keyframe)))
      
      (keyframe
       ((PERCENTAGE S* BRA-OPEN declaration* BRA-CLO S*)
	(instantiate::css-keyframe
	   (selector (car PERCENTAGE))
	   (declaration* declaration*)))
       ((IDENT S* BRA-OPEN declaration* BRA-CLO S*)
	(instantiate::css-keyframe
	   (selector (car IDENT))
	   (declaration* declaration*))))
      
      (selector+
       ((selector)
	(list selector))
       ((selector+ COMMA S* selector)
	`(,@selector+ ,selector)))
      
      (selector
       ((simple_selector S*)
	(list simple_selector))
       ((simple_selector combinator selector)
	(cons* simple_selector combinator selector))
       ((simple_selector S+ combinator selector)
	(cons* simple_selector combinator selector))
       ((simple_selector S+ selector)
	(cons* simple_selector '| | selector)))
      
      (combinator
       ((+ S*) '+)
       ((> S*) '>)
       ((~ S*) '~))
      
      (simple_selector
       ((element_name)
	(instantiate::css-selector
	   (element element_name)))
       ((element_name simple_selector_attr+)
	(instantiate::css-selector
	   (element element_name)
	   (attr* simple_selector_attr+)))
       ((simple_selector_attr+)
	(instantiate::css-selector
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
       ((ANGLE-OPEN IDENT S* ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car IDENT))))
       ((ANGLE-OPEN S IDENT S* ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car IDENT))))
       ((ANGLE-OPEN EXTENSION S* ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car EXTENSION))))
       ((ANGLE-OPEN S EXTENSION S* ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car EXTENSION))))
       ((ANGLE-OPEN IDENT S* attrib-left S* attrib-right S* ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car IDENT))
	   (op attrib-left)
	   (arg attrib-right)))
       ((ANGLE-OPEN S IDENT S* attrib-left S* attrib-right S* ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car IDENT))
	   (op attrib-left)
	   (arg attrib-right)))
       ((ANGLE-OPEN EXTENSION S* attrib-left S* attrib-right S* ANGLE-CLO)
	(instantiate::css-selector-attr
	   (ident (car EXTENSION))
	   (op attrib-left)
	   (arg attrib-right)))
       ((ANGLE-OPEN S EXTENSION S* attrib-left S* attrib-right S* ANGLE-CLO)
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
       ((COLON COLON IDENT)
	(instantiate::css-selector-pseudo
	   (expr (list ":" (car IDENT)))))
       ((COLON EXTENSION)
	(instantiate::css-selector-pseudo
	   (expr (list ":" (car EXTENSION)))))
       ((COLON COLON EXTENSION)
	(instantiate::css-selector-pseudo
	   (expr (car EXTENSION))))
       ((COLON FUNCTION S* IDENT S* PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (car IDENT))
	   (fun (car FUNCTION))))
       ((COLON FUNCTION S* EXTENSION S* PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (car EXTENSION))
	   (fun (car FUNCTION))))
       ((COLON FUNCTION S* STRING S* PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (car STRING))
	   (fun (car FUNCTION))))
       ((COLON FUNCTION S* URI S* PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (instantiate::css-uri (value (car URI))))
	   (fun (car FUNCTION))))
       ((COLON FUNCTION S* NUMBER S* PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr (car NUMBER))
	   (fun (car FUNCTION))))
       ((COLON NOT_PSEUDO simple_selector S* PAR-CLO)
	(instantiate::css-selector-pseudo
	   (expr simple_selector)
	   (fun "not"))))

      (declaration*
       ((S*)
	'())
       ((S* declaration)
	(list declaration))
       ((S* declaration SEMI-COLON declaration*)
	(cons declaration declaration*)))
      
      (declaration
       ((property COLON S* expr)
	(instantiate::css-declaration
	   (property property)
	   (expr expr)
	   (prio "")))
       ((property COLON S* expr prio)
	(instantiate::css-declaration
	   (property property)
	   (expr expr)
	   (prio prio))))
      
      (prio
       ((IMPORTANT_SYM S*) (car IMPORTANT_SYM)))
      
      (expr
       ((term) (list term))
       ((expr term) `(,@expr ,term))
       ((expr operator term) `(,@expr ,operator ,term)))

      (operator
       ((SLASH S*) "/")
       ((COMMA S*) ",")
       ((* S*) "*")
       ((- S*) "-")
       ((= S*) "="))
      
      (term
       ((NUMBER S*) (car NUMBER))
       ((PERCENTAGE S*) (car PERCENTAGE))
       ((LENGTH S*) (car LENGTH))
       ((EMS S*) (car EMS))
       ((EXS S*) (car EXS))
       ((CHS S*) (car CHS))
       ((REMS S*) (car REMS))
       ((ANGLE S*) (car ANGLE))
       ((TIME S*) (car TIME))
       ((FREQ S*) (car FREQ))
       ((STRING S*) (car STRING))
       ((VALUE S*) (car VALUE))
       ((IDENT S*) (car IDENT))
       ((URI S*) (instantiate::css-uri (value (car URI))))
       ((RGB S*) (car RGB))
       ((UNICODERANGE S*) (car UNICODERANGE))
       ((hexcolor) hexcolor)
       ((function) function))

      (qualified
       ((IDENT COLON qualified-ident)
	(string-append (car IDENT) ":" qualified-ident)))
      
      (qualified-ident
       ((IDENT DOT)
	(string-append (car IDENT) "."))
       ((IDENT DOT qualified-ident)
	(string-append (car IDENT) "." qualified-ident)))
       
      (function
       ((FUNCTION S* expr PAR-CLO S*)
	(instantiate::css-function
	   (fun (car FUNCTION))
	   (expr expr)))
       ((FUNCTION S* PAR-CLO S*)
	(instantiate::css-function
	   (fun (car FUNCTION))
	   (expr '())))
       ((qualified FUNCTION S* expr PAR-CLO S*)
	(instantiate::css-function
	   (fun (string-append qualified (car FUNCTION)))
	   (expr expr)))
       ((qualified FUNCTION S* PAR-CLO S*)
	(instantiate::css-function
	   (fun (string-append qualified (car FUNCTION)))
	   (expr '()))))
      
      (hexcolor
       ((HASH S*) (instantiate::css-hash-color (value (car HASH)))))))

;*---------------------------------------------------------------------*/
;*    css-mutex ...                                                    */
;*---------------------------------------------------------------------*/
(define css-mutex (make-mutex "css"))

;*---------------------------------------------------------------------*/
;*    css-stamp ...                                                    */
;*---------------------------------------------------------------------*/
(define css-stamp 0)

;*---------------------------------------------------------------------*/
;*    css-stamp-rules! ...                                             */
;*    -------------------------------------------------------------    */
;*    Stamps are used by the CSS priority algorithm.                   */
;*---------------------------------------------------------------------*/
(define (css-stamp-rules! os)
   
   (define (loop l)
      (if (null? l)
	  os
	  (let ((o (car l)))
	     (cond
		((pair? o)
		 (loop o))
		((isa? o css-ruleset)
		 (with-access::css-ruleset o (stamp)
		    (set! stamp css-stamp))
		 (set! css-stamp (+fx 1 css-stamp)))
		((isa? o css-media)
		 (with-access::css-media o (ruleset*)
		    (loop ruleset*))))
	     (loop (cdr l)))))

   (synchronize css-mutex
      (loop os))

   os)

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
