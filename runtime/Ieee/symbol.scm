;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/symbol.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul  4 15:05:26 1992                          */
;*    Last change :  Sun Aug 25 09:19:14 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.4. Symbols (page 18, r4)                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __r4_symbols_6_4

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
	    (c-string->symbol::symbol (::string) "string_to_symbol")
	    (c-bstring->symbol::symbol (::bstring) "bstring_to_symbol")
	    ($gensym::symbol (::obj) "bgl_gensym")
	    (macro c-symbol->string::bstring (::obj) "SYMBOL_TO_STRING")
	    (macro c-symbol-plist::obj (::obj) "GET_SYMBOL_PLIST")
	    (macro set-symbol-plist::obj (::obj ::obj) "SET_SYMBOL_PLIST")
	    (symbol-exists?::bool (::string) "symbol_exists_p")
	    
            (macro c-keyword?::bool (::obj) "KEYWORDP")
	    (c-string->keyword::keyword (::string) "string_to_keyword")
	    (c-bstring->keyword::keyword (::bstring) "bstring_to_keyword")
	    (macro c-keyword->string::bstring (::keyword) "KEYWORD_TO_STRING")
	    (macro c-keyword-plist::obj (::obj) "GET_KEYWORD_PLIST")
	    (macro set-keyword-plist::obj (::obj ::obj) "SET_KEYWORD_PLIST")
	    (macro cnst->integer::long (::obj) "CCNST"))
   
   (java    (class foreign
	       (method static c-symbol?::bool (::obj)
		       "SYMBOLP")
	       (method static c-string->symbol::symbol (::string)
		       "string_to_symbol")
	       (method static c-bstring->symbol::symbol (::bstring)
		       "string_to_symbol")
	       (method static c-symbol->string::bstring (::symbol)
		       "SYMBOL_TO_STRING")
	       (method static c-symbol-plist::obj (::symbol)
		       "GET_SYMBOL_PLIST")
	       (method static set-symbol-plist::obj (::symbol ::obj)
		       "SET_SYMBOL_PLIST")
	       (method static symbol-exists?::bool (::string)
		       "symbol_exists_p")
	       
	       (method static c-keyword?::bool (::obj)
		       "KEYWORDP")
	       (method static c-string->keyword::keyword (::string)
		       "string_to_keyword")
	       (method static c-bstring->keyword::keyword (::bstring)
		       "string_to_keyword")
	       (method static c-keyword->string::bstring (::keyword)
		       "KEYWORD_TO_STRING")
	       (method static c-keyword-plist::obj (::keyword)
		       "GET_KEYWORD_PLIST")
	       (method static set-keyword-plist::obj (::keyword ::obj)
		       "SET_KEYWORD_PLIST")
	       (method static cnst->integer::long (::obj)
		       "CCNST")))
   
   (export  (inline symbol?::bool ::obj)
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
	    (symbol? side-effect-free nesting fail-safe)
	    (c-symbol->string args-safe fail-safe)
	    (c-symbol-plist args-safe)
	    (set-symbol-plist args-safe)
	    (c-keyword->string args-safe fail-safe)
	    (c-keyword-plist args-safe)
	    (set-keyword-plist args-safe)
	    (cnst->integer args-safe)
	    (c-string->symbol no-cfa-top nesting fail-safe)
	    (c-bstring->symbol no-cfa-top nesting fail-safe)
	    (string->symbol no-cfa-top nesting fail-safe)
	    (string->symbol-ci no-cfa-top nesting fail-safe)
	    (getprop side-effect-free nesting fail-safe)
	    (c-keyword? (predicate-of keyword) no-cfa-top nesting fail-safe)
	    (keyword? side-effect-free nesting fail-safe)
	    (c-string->keyword no-cfa-top nesting fail-safe)
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
   (c-bstring->symbol string))

;*---------------------------------------------------------------------*/
;*    string->symbol-ci ...                                            */
;*---------------------------------------------------------------------*/
(define (string->symbol-ci string)
   (c-bstring->symbol (string-upcase string)))

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
	  (let loop ()
	     (set! *gensym-counter* (+fx *gensym-counter* 1))
	     (let ((name (string-append string
					(integer->string *gensym-counter*))))
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
		     (set-symbol-plist symbol new)
		     (set-keyword-plist symbol new))
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
		      (set-symbol-plist symbol (cddr l))
		      (set-keyword-plist symbol (cddr l))))))
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
   (string-copy (c-keyword->string keyword)))

;*---------------------------------------------------------------------*/
;*    keyword->string! ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (keyword->string! keyword)
   (c-keyword->string keyword))

;*---------------------------------------------------------------------*/
;*    string->keyword ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (string->keyword string)
   (c-string->keyword string))

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
