;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Ieee/symbol.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul  4 15:05:26 1992                          */
;*    Last change :  Tue Feb  4 08:05:09 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.4. Symbols (page 18, r4)                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __r4_symbols_6_4

   (cond-expand
      ((and (not bigloo-c) (not bigloo-jvm))
       (include "Ieee/symbol-generic.sch")))
   
   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __object
	    __thread
	    __bit
	    
	    __r4_control_features_6_9
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_characters_6_6
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    
	    __r4_output_6_10_3
	    __r4_ports_6_10_1

	    __evenv
	    __bignum)

   (extern  (macro c-symbol?::bool (::obj) "SYMBOLP")
	    (macro $symbol?::bool (::obj) "SYMBOLP")
	    (macro $make-symbol::symbol (::bstring) "")
	    (c-string->symbol::symbol (::string) "string_to_symbol")
	    ($string->symbol::symbol (::string) "string_to_symbol")
	    ($bstring->symbol::symbol (::bstring) "bstring_to_symbol")
	    ($gensym::symbol (::obj) "bgl_gensym")
	    (macro $symbol->string::bstring (::obj) "SYMBOL_TO_STRING")
	    (macro c-symbol->string::bstring (::obj) "SYMBOL_TO_STRING")
	    (macro $symbol-plist::obj (::obj) "GET_SYMBOL_PLIST")
	    (macro c-symbol-plist::obj (::obj) "GET_SYMBOL_PLIST")
	    (macro $set-symbol-plist::obj (::obj ::obj) "SET_SYMBOL_PLIST")
	    ($symbol-exists?::bool (::string) "symbol_exists_p")
	    
	    (macro c-keyword?::bool (::obj) "KEYWORDP")
	    (macro $keyword?::bool (::obj) "KEYWORDP")
	    (macro $make-keyword::keyword (::bstring) "")
	    (c-string->keyword::keyword (::string) "string_to_keyword")
	    ($string->keyword::keyword (::string) "string_to_keyword")
	    ($bstring->keyword::keyword (::bstring) "bstring_to_keyword")
	    (macro c-keyword->string::bstring (::keyword) "KEYWORD_TO_STRING")
	    (macro $keyword->string::bstring (::keyword) "KEYWORD_TO_STRING")
	    (macro c-keyword-plist::obj (::obj) "GET_KEYWORD_PLIST")
	    (macro $keyword-plist::obj (::obj) "GET_KEYWORD_PLIST")
	    (macro $set-keyword-plist::obj (::obj ::obj) "SET_KEYWORD_PLIST")
	    (macro cnst->integer::long (::obj) "CCNST"))

   (wasm    (c-symbol? "(ref.test (ref $symbol) ~0)")
            ($symbol? "(ref.test (ref $symbol) ~0)")
	    ($make-symbol "(call $make-symbol ~0 (global.get $BNIL))")
	    ($symbol-plist "(struct.get $symbol $cval (ref.cast (ref $symbol) ~0))")
	    (c-symbol-plist "(struct.get $symbol $cval (ref.cast (ref $symbol) ~0))")
	    ($symbol-plist "(struct.get $symbol $cval (ref.cast (ref $symbol) ~0))")
	    ($set-symbol-plist "(call $set-symbol-plist ~0 ~1)")
		
	    ($symbol->string "(struct.get $symbol $str (ref.cast (ref $symbol) ~0))")
	    (c-symbol->string "(struct.get $symbol $str (ref.cast (ref $symbol) ~0))")

	    (c-keyword? "(ref.test (ref $keyword) ~0)")
	    ($keyword? "(ref.test (ref $keyword) ~0)")
	    ($make-keyword "(call $make-keyword ~0 (global.get $BNIL))")
	    ($keyword-plist "(struct.get $keyword $cval (ref.cast (ref $keyword) ~0))")
	    (c-keyword-plist "(struct.get $keyword $cval (ref.cast (ref $keyword) ~0))")
	    ($keyword-plist "(struct.get $keyword $cval (ref.cast (ref $keyword) ~0))")
	    ($set-keyword-plist "(call $set-keyword-plist ~0 ~1)")

	    ($keyword->string "(struct.get $keyword $str (ref.cast (ref $keyword) ~0))")
	    (c-keyword->string "(struct.get $keyword $str (ref.cast (ref $keyword) ~0))"))
   
   (java    (class foreign
	       (method static c-symbol?::bool (::obj)
		       "SYMBOLP")
	       (method static $symbol?::bool (::obj)
		       "SYMBOLP")
	       (method static c-string->symbol::symbol (::string)
		       "string_to_symbol")
	       (method static $string->symbol::symbol (::string)
		       "string_to_symbol")
	       (method static $symbol->string::bstring (::symbol)
		       "SYMBOL_TO_STRING")
	       (method static $bstring->symbol::symbol (::bstring)
		       "string_to_symbol")
	       (method static c-symbol->string::bstring (::symbol)
		       "SYMBOL_TO_STRING")
	       (method static $symbol->string::bstring (::symbol)
		       "SYMBOL_TO_STRING")
	       (method static c-symbol-plist::obj (::symbol)
		       "GET_SYMBOL_PLIST")
	       (method static $symbol-plist::obj (::symbol)
		       "GET_SYMBOL_PLIST")
	       (method static $set-symbol-plist::obj (::symbol ::obj)
		       "SET_SYMBOL_PLIST")
	       (method static $symbol-exists?::bool (::string)
		       "symbol_exists_p")
	       
	       (method static c-keyword?::bool (::obj)
		       "KEYWORDP")
	       (method static $keyword?::bool (::obj)
		       "KEYWORDP")
	       (method static c-string->keyword::keyword (::string)
		       "string_to_keyword")
	       (method static $string->keyword::keyword (::string)
		       "string_to_keyword")
	       (method static $bstring->keyword::keyword (::bstring)
		       "string_to_keyword")
	       (method static c-keyword->string::bstring (::keyword)
		       "KEYWORD_TO_STRING")
	       (method static $keyword->string::bstring (::keyword)
		       "KEYWORD_TO_STRING")
	       (method static c-keyword-plist::obj (::keyword)
		       "GET_KEYWORD_PLIST")
	       (method static $keyword-plist::obj (::keyword)
		       "GET_KEYWORD_PLIST")
	       (method static $set-keyword-plist::obj (::keyword ::obj)
		       "SET_KEYWORD_PLIST")
	       (method static cnst->integer::long (::obj)
		       "CCNST")))
   
   (export  (inline symbol?::bool ::obj)
	    (inline symbol-exists?::bool ::bstring)
	    (inline symbol->string::bstring ::symbol)
	    (inline symbol->string!::bstring ::symbol)
	    (inline string->symbol::symbol ::bstring)
	    (string->symbol-ci::symbol ::bstring)
	    (symbol-append::symbol . symbols)
	    (inline symbol-plist::obj ::obj)
	    (getprop ::obj ::obj)
	    (putprop! ::obj ::obj ::obj)
	    (remprop! ::obj ::obj)
	    (gensym::symbol  #!optional arg)
	    (inline keyword?::bool ::obj)
	    (inline keyword->string::bstring ::keyword)
	    (inline keyword->string!::bstring ::keyword)
	    (inline string->keyword::keyword ::bstring)
	    (symbol->keyword::keyword ::symbol)
	    (keyword->symbol::symbol ::keyword))
   
   (pragma  (c-symbol? (predicate-of symbol) no-cfa-top nesting fail-safe)
	    ($symbol? (predicate-of symbol) no-cfa-top nesting fail-safe)
	    (symbol? side-effect-free nesting fail-safe)
	    (c-symbol->string args-safe fail-safe)
	    ($symbol->string args-safe fail-safe)
	    (c-symbol-plist args-safe)
	    ($symbol-plist args-safe)
	    ($set-symbol-plist args-safe)
	    (c-keyword->string args-safe fail-safe)
	    ($keyword->string args-safe fail-safe)
	    (c-keyword-plist args-safe)
	    ($keyword-plist args-safe)
	    ($set-keyword-plist args-safe)
	    (cnst->integer args-safe)
	    ($bstring->symbol no-cfa-top nesting fail-safe)
	    (string->symbol no-cfa-top nesting fail-safe)
	    (string->symbol-ci no-cfa-top nesting fail-safe)
	    (getprop side-effect-free nesting fail-safe)
	    (c-keyword? (predicate-of keyword) no-cfa-top nesting fail-safe)
	    ($keyword? (predicate-of keyword) no-cfa-top nesting fail-safe)
	    (keyword? side-effect-free nesting fail-safe)
	    ($bstring->keyword no-cfa-top nesting fail-safe)
	    (string->keyword no-cfa-top nesting fail-safe)
	    (gensym fail-safe)))

;*---------------------------------------------------------------------*/
;*    symbol? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (symbol? obj)
   (c-symbol? obj))

;*---------------------------------------------------------------------*/
;*    symbol->string ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (symbol->string symbol)
   (string-copy (c-symbol->string symbol)))

;*---------------------------------------------------------------------*/
;*    symbol->string! ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (symbol->string! symbol)
   (c-symbol->string symbol))

;*---------------------------------------------------------------------*/
;*    string->symbol ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (string->symbol string)
   (cond-expand
      ((or bigloo-c bigloo-jvm)
       ($bstring->symbol string))
      (else
       ($$bstring->symbol string))))

;*---------------------------------------------------------------------*/
;*    string->symbol-ci ...                                            */
;*---------------------------------------------------------------------*/
(define (string->symbol-ci string)
   (string->symbol (string-upcase string)))

;*---------------------------------------------------------------------*/
;*    symbol-append ...                                                */
;*---------------------------------------------------------------------*/
(define (symbol-append . list)
   (string->symbol
    (if (null? list)
	""
	(let symbol-append ((list list))
	   (if (null? (cdr list))
	       (symbol->string (car list))
	       ($string-append (symbol->string (car list))
			       (symbol-append (cdr list))))))))

;*---------------------------------------------------------------------*/
;*    symbol-exists? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (symbol-exists? sym)
   (cond-expand
      ((and (not bigloo-c) (not bigloo-jvm))
       ($$symbol-exists? sym))
      (else
       ($symbol-exists? sym))))

;*---------------------------------------------------------------------*/
;*    *gensym-counter* ...                                             */
;*---------------------------------------------------------------------*/
(define *gensym-counter* 999)

;*---------------------------------------------------------------------*/
;*    gensym ...                                                       */
;*---------------------------------------------------------------------*/
(define (gensym #!optional arg)
   (cond-expand
      (bigloo-c
       (let ((arg (cond
		     ((not arg) arg)
		     ((symbol? arg) (symbol->string arg))
		     ((string? arg) arg)
		     (else (error "gensym" "Illegal argument" arg)))))
	  ($gensym arg)))
      (else
       (let ((string (cond
			((not arg) "g")
			((symbol? arg) (symbol->string arg))
			((string? arg) arg)
			(else (error "gensym" "Illegal argument" arg)))))
		
	  (when (>fx  *gensym-counter* 10)
		#f)
	  (let loop ()
	     (set! *gensym-counter* (+fx *gensym-counter* 1))
	     (let* ((n (integer->string *gensym-counter*))
		    (name (string-append string n)))
		(if (not (symbol-exists? name))
		    (string->symbol name)
		    (loop))))))))

;*---------------------------------------------------------------------*/
;*    symbol-plist ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (symbol-plist symbol)
   (cond
      ((symbol? symbol) 
       (c-symbol-plist symbol))
      ((keyword? symbol)
       (c-keyword-plist symbol))
      (else
       (error "symbol-plist"
	      "argument is neither a symbol nor a keyword"
	      symbol))))

;*---------------------------------------------------------------------*/
;*    getprop ...                                                      */
;*---------------------------------------------------------------------*/
(define (getprop symbol key)
   (if (or (symbol? symbol) (keyword? symbol))
       (let loop ((pl (symbol-plist symbol)))
	  (cond
	     ((null? pl)
	      #f)
	     ((eq? (car pl) key)
	      (cadr pl))
	     (else
	      (loop (cddr pl)))))
       (error "getprop" "argument is neither a symbol nor a keyword" symbol)))

;*---------------------------------------------------------------------*/
;*    putprop! ...                                                     */
;*---------------------------------------------------------------------*/
(define (putprop! symbol key val)
   (if (or (symbol? symbol) (keyword? symbol))
       (let loop ((pl (symbol-plist symbol)))
	  (cond
	     ((null? pl)
	      (let ((new (cons* key val (symbol-plist symbol))))
		 (if (symbol? symbol)
		     ($set-symbol-plist symbol new)
		     ($set-keyword-plist symbol new))
		 new))
	     ((eq? (car pl) key)
	      (set-car! (cdr pl) val))
	     (else
	      (loop (cddr pl)))))
       (error "getprop" "argument is neither a symbol nor a keyword" symbol)))

;*---------------------------------------------------------------------*/
;*    remprop! ...                                                     */
;*---------------------------------------------------------------------*/
(define (remprop! symbol key)
   (if (or (symbol? symbol) (keyword? symbol))
       (let loop ((old '())
		  (l   (symbol-plist symbol)))
	  (cond
	     ((null? l)
	      #f)
	     ((eq? (car l) key)
	      (cond
		 ((pair? old)
		  (set-cdr! (cdr old) (cddr l)))
		 (else
		  (if (symbol? symbol)
		      ($set-symbol-plist symbol (cddr l))
		      ($set-keyword-plist symbol (cddr l))))))
	     (else
	      (loop l (cddr l)))))
       (error "getprop" "argument is neither a symbol nor a keyword" symbol)))

;*---------------------------------------------------------------------*/
;*    keyword? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (keyword? obj)
   (c-keyword? obj))

;*---------------------------------------------------------------------*/
;*    keyword->string ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (keyword->string keyword)
   (string-copy ($keyword->string keyword)))

;*---------------------------------------------------------------------*/
;*    keyword->string! ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (keyword->string! keyword)
   ($keyword->string keyword))

;*---------------------------------------------------------------------*/
;*    string->keyword ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (string->keyword string)
   (cond-expand
      ((or bigloo-c bigloo-jvm)
       ($bstring->keyword string))
      (else
       ($$bstring->keyword string))))

;*---------------------------------------------------------------------*/
;*    symbol->keyword ...                                              */
;*---------------------------------------------------------------------*/
(define (symbol->keyword symbol)
   (string->keyword (symbol->string symbol)))

;*---------------------------------------------------------------------*/
;*    keyword->symbol ...                                              */
;*---------------------------------------------------------------------*/
(define (keyword->symbol keyword)
   (string->symbol (keyword->string keyword)))
