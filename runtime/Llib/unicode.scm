;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/unicode.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 20 19:17:18 1995                          */
;*    Last change :  Tue Sep  7 21:14:36 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Unicode (UCS-2) strings handling.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __unicode
   
   (import  __error
	    __param
	    __bexit
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __ucs2
	    __bignum
	    __bit
	    
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    
	    __evenv)
   
   (extern (macro c-ucs2-string?::bool (::obj)
		  "UCS2_STRINGP")
	   (c-make-ucs2-string::ucs2string (::int ::ucs2)
					   "make_ucs2_string")
	   (macro c-ucs2-string-length::int (::ucs2string)
		  "UCS2_STRING_LENGTH")
	   (macro c-ucs2-string-ref::ucs2 (::ucs2string ::int)
		  "UCS2_STRING_REF")
	   (macro c-ucs2-string-set!::obj (::ucs2string ::int ::ucs2)
		  "UCS2_STRING_SET")
	   
	   (c-ucs2-string=?::bool (::ucs2string ::ucs2string)
				  "ucs2_strcmp")
	   (ucs2-strcicmp::bool (::ucs2string ::ucs2string)
				"ucs2_strcicmp")
	   (ucs2-string_lt::bool (::ucs2string ::ucs2string)
				 "ucs2_string_lt")
	   (ucs2-string_le::bool (::ucs2string ::ucs2string)
				 "ucs2_string_le")
	   (ucs2-string_gt::bool (::ucs2string ::ucs2string)
				 "ucs2_string_gt")
	   (ucs2-string_ge::bool (::ucs2string ::ucs2string)
				 "ucs2_string_ge")
	   (ucs2-string_cilt::bool (::ucs2string ::ucs2string)
				   "ucs2_string_cilt")
	   (ucs2-string_cile::bool (::ucs2string ::ucs2string)
				   "ucs2_string_cile")
	   (ucs2-string_cigt::bool (::ucs2string ::ucs2string)
				   "ucs2_string_cigt")
	   (ucs2-string_cige::bool (::ucs2string ::ucs2string)
				   "ucs2_string_cige")
	   
	   (c-ucs2-string-copy::ucs2string (::ucs2string)
					   "c_ucs2_string_copy")
	   (c-subucs2-string::ucs2string (::ucs2string ::int ::int)
					 "c_subucs2_string")
	   (c-ucs2-string-append::ucs2string (::ucs2string ::ucs2string)
					     "ucs2_string_append")
	   
	   (c-ucs2-string->utf8-string::bstring (::ucs2string)
						"ucs2_string_to_utf8_string")
	   (c-utf8-string->ucs2-string::ucs2string (::bstring)
						   "utf8_string_to_ucs2_string"))
   
   (java   (class foreign
	      (method static c-ucs2-string?::bool (::obj)
		      "UCS2_STRINGP")
	      (method static c-make-ucs2-string::ucs2string (::int ::ucs2)
		      "make_ucs2_string")
	      (method static c-ucs2-string-length::int (::ucs2string)
		      "UCS2_STRING_LENGTH")
	      (method static c-ucs2-string-ref::ucs2 (::ucs2string ::int)
		      "UCS2_STRING_REF")
	      (method static c-ucs2-string-set!::obj (::ucs2string ::int ::ucs2)
		      "UCS2_STRING_SET")
	      
	      (method static c-ucs2-string=?::bool (::ucs2string ::ucs2string)
		      "ucs2_strcmp")
	      (method static ucs2-strcicmp::bool (::ucs2string ::ucs2string)
		      "ucs2_strcicmp")
	      (method static ucs2-string_lt::bool (::ucs2string ::ucs2string)
		      "ucs2_string_lt")
	      (method static ucs2-string_le::bool (::ucs2string ::ucs2string)
		      "ucs2_string_le")
	      (method static ucs2-string_gt::bool (::ucs2string ::ucs2string)
		      "ucs2_string_gt")
	      (method static ucs2-string_ge::bool (::ucs2string ::ucs2string)
		      "ucs2_string_ge")
	      (method static ucs2-string_cilt::bool (::ucs2string ::ucs2string)
		      "ucs2_string_cilt")
	      (method static ucs2-string_cile::bool (::ucs2string ::ucs2string)
		      "ucs2_string_cile")
	      (method static ucs2-string_cigt::bool (::ucs2string ::ucs2string)
		      "ucs2_string_cigt")
	      (method static ucs2-string_cige::bool (::ucs2string ::ucs2string)
		      "ucs2_string_cige")
	      
	      (method static c-ucs2-string-copy::ucs2string (::ucs2string)
		      "c_ucs2_string_copy")
	      (method static c-subucs2-string::ucs2string (::ucs2string ::int ::int)
		      "c_subucs2_string")
	      (method static c-ucs2-string-append::ucs2string (::ucs2string ::ucs2string)
		      "c_ucs2_string_append")
	      
	      (method static c-ucs2-string->utf8-string::bstring (::ucs2string)
		      "ucs2_string_to_utf8_string")
	      (method static c-utf8-string->ucs2-string::ucs2string (::bstring)
		      "utf8_string_to_ucs2_string")))
   
   (export  (inline ucs2-string?::bool ::obj)
	    (inline make-ucs2-string::ucs2string ::int
		    #!optional (filler::ucs2 (char->ucs2 #\space)))
	    (inline ucs2-string::ucs2string . ucs2s)
	    (inline ucs2-string-length::int ::ucs2string)
	    (inline ucs2-string-ref::ucs2 ::ucs2string ::int)
	    (inline ucs2-string-set!::obj ::ucs2string ::int ::ucs2)
	    (inline ucs2-string-ref-ur::ucs2 ::ucs2string ::int)
	    (inline ucs2-string-set-ur!::obj ::ucs2string ::int ::ucs2)
	    (inline ucs2-string=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string<?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string>?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string<=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string>=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci<?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci>?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci<=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci>=?::bool ::ucs2string ::ucs2string)
	    (inline subucs2-string::ucs2string ::ucs2string ::int ::int)
	    (inline subucs2-string-ur::ucs2string ::ucs2string ::int ::int)
	    (ucs2-string-append::ucs2string . ucs2-strings)
	    (ucs2-string->list ::ucs2string)
	    (list->ucs2-string::ucs2string ::obj)
	    (inline ucs2-string-copy::ucs2string ::ucs2string)
	    (ucs2-string-fill!::ucs2string ::ucs2string ::ucs2)
	    (ucs2-string-upcase::ucs2string ::ucs2string)
	    (ucs2-string-downcase::ucs2string ::ucs2string)
	    (ucs2-string-upcase!::ucs2string ::ucs2string)
	    (ucs2-string-downcase!::ucs2string ::ucs2string)
	    (inline ucs2-string->utf8-string::bstring ::ucs2string)
	    (inline utf8-string->ucs2-string::ucs2string ::bstring)
	    (inverse-utf8-table ::vector)
	    (utf8-string?::bool ::bstring)
	    (utf8->8bits::bstring ::bstring ::obj)
	    (utf8->8bits!::bstring ::bstring ::obj)
	    (utf8->iso-latin::bstring ::bstring)
	    (utf8->iso-latin!::bstring ::bstring)
	    (utf8->cp1252::bstring ::bstring)
	    (utf8->cp1252!::bstring ::bstring)
	    (8bits->utf8::bstring ::bstring ::obj)
	    (8bits->utf8!::bstring ::bstring ::obj)
	    (iso-latin->utf8::bstring ::bstring)
	    (iso-latin->utf8!::bstring ::bstring)
	    (cp1252->utf8::bstring ::bstring)
	    (cp1252->utf8!::bstring ::bstring))
   
   (pragma  (c-ucs2-string? (predicate-of ucs2string) no-cfa-top nesting)
	    (ucs2-string? side-effect-free no-cfa-top nesting)
	    (c-ucs2-string-ref side-effect-free no-cfa-top nesting)
	    (ucs2-string-ref-ur side-effect-free no-cfa-top nesting)
	    (ucs2-string-ref side-effect-free no-cfa-top nesting)
	    (c-ucs2-string-length side-effect-free no-cfa-top nesting)
	    (ucs2-string-length side-effect-free no-cfa-top nesting)
	    (ucs2-string=? side-effect-free nesting)
	    (ucs2-string-ci=? side-effect-free nesting)
	    (ucs2-string<? side-effect-free nesting)
	    (ucs2-string>? side-effect-free nesting)
	    (ucs2-string<=? side-effect-free nesting)
	    (ucs2-string>=? side-effect-free nesting)
	    (ucs2-string-ci<? side-effect-free nesting)
	    (ucs2-string-ci>? side-effect-free nesting)
	    (ucs2-string-ci<=? side-effect-free nesting)
	    (ucs2-string-ci>=? side-effect-free nesting)))
  
;*---------------------------------------------------------------------*/
;*    ucs2-string? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string? obj)
   (c-ucs2-string? obj))

;*---------------------------------------------------------------------*/
;*    make-ucs2-string ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (make-ucs2-string k #!optional
				 (filler::ucs2 (char->ucs2 #\space)))
   (c-make-ucs2-string k filler))
 
;*---------------------------------------------------------------------*/
;*    ucs2-string ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string . ucs2s)
   (list->ucs2-string ucs2s))

;*---------------------------------------------------------------------*/
;*    ucs2-string-length ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-length ucs2-string)
   (c-ucs2-string-length ucs2-string))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ref ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ref ucs2-string k)
   (if ($string-bound-check? k (ucs2-string-length ucs2-string))
       (c-ucs2-string-ref ucs2-string k)
       (error 'ucs2-string-ref
	      (string-append "index out of range [0.."
			     (integer->string
			      (-fx (ucs2-string-length ucs2-string) 1))
			     "]")
	      k)))
 
;*---------------------------------------------------------------------*/
;*    ucs2-string-set! ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-set! ucs2-string k ucs2)
   (if ($string-bound-check? k (ucs2-string-length ucs2-string))
       (c-ucs2-string-set! ucs2-string k ucs2)
       (error 'ucs2-string-set!
	      (string-append "index out of range [0.."
			     (integer->string
			      (-fx (ucs2-string-length ucs2-string) 1))
			     "]")
	      k)))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ref-ur ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ref-ur ucs2-string k)
   (c-ucs2-string-ref ucs2-string k))
 
;*---------------------------------------------------------------------*/
;*    ucs2-string-set-ur! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-set-ur! ucs2-string k ucs2)
   (c-ucs2-string-set! ucs2-string k ucs2))

;*---------------------------------------------------------------------*/
;*    ucs2-string=? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string=? ucs2-string1 ucs2-string2)
   (c-ucs2-string=? ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci=? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci=? ucs2-string1 ucs2-string2)
   (ucs2-strcicmp ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string<? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string<? ucs2-string1 ucs2-string2)
   (ucs2-string_lt ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string>? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string>? ucs2-string1 ucs2-string2)
   (ucs2-string_gt ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string<=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string<=? ucs2-string1 ucs2-string2)
   (ucs2-string_le ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string>=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string>=? ucs2-string1 ucs2-string2)
   (ucs2-string_ge ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci<? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci<? ucs2-string1 ucs2-string2)
   (ucs2-string_cilt ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci>? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci>? ucs2-string1 ucs2-string2)
   (ucs2-string_cigt ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci<=? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci<=? ucs2-string1 ucs2-string2)
   (ucs2-string_cile ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci>=? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci>=? ucs2-string1 ucs2-string2)
   (ucs2-string_cige ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    suucs2-string ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (subucs2-string ucs2-string start end)
   ;; no macro on inline so we don't use `and'
   (if (if (>=fx end start)
	   (if ($string-bound-check? start
				     (+fx (ucs2-string-length ucs2-string) 1))
	       ($string-bound-check? end
				     (+fx (ucs2-string-length ucs2-string) 1))
	       #f)
	   #f)
       (c-subucs2-string ucs2-string start end)
       (error "subucs2-string" "Illegal index" (cons start end))))

;*---------------------------------------------------------------------*/
;*    suucs2-string-ur ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (subucs2-string-ur ucs2-string start end)
   (c-subucs2-string ucs2-string start end))

;*---------------------------------------------------------------------*/
;*    ucs2-string-append ...                                           */
;*---------------------------------------------------------------------*/
(define (ucs2-string-append . list)
   (if (null? list)
       (make-ucs2-string 0)
       (let loop ((list list))
	  (if (null? (cdr list))
	      (car list)
	      (c-ucs2-string-append (car list) (loop (cdr list)))))))

;*---------------------------------------------------------------------*/
;*    list->ucs2-string ...                                            */
;*---------------------------------------------------------------------*/
(define (list->ucs2-string list)
   (let* ((len    (length list))
	  (ucs2-string (make-ucs2-string len)))
      (let loop ((i 0)
		 (l list))
	 (if (=fx i len)
	     ucs2-string
	     (begin
		(ucs2-string-set! ucs2-string i (car l))
		(loop (+fx i 1) (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string->list ...                                            */
;*---------------------------------------------------------------------*/
(define (ucs2-string->list ucs2-string)
   (let ((len (ucs2-string-length ucs2-string)))
      (let loop ((i   0)
		 (acc '()))
	 (if (=fx i len)
	     (reverse! acc)
	     (loop (+fx i 1)
		   (cons (ucs2-string-ref ucs2-string i)
			 acc))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-copy ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-copy ucs2-string)
   (c-ucs2-string-copy ucs2-string))

;*---------------------------------------------------------------------*/
;*    ucs2-string-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define (ucs2-string-fill! ucs2-string ucs2)
   (let ((len (ucs2-string-length ucs2-string)))
      (let loop ((i 0))
	 (if (=fx i len)
	     ucs2-string
	     (begin
		(ucs2-string-set! ucs2-string i ucs2)
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-upcase ...                                           */
;*---------------------------------------------------------------------*/
(define (ucs2-string-upcase ucs2-string)
   (let* ((len (ucs2-string-length ucs2-string))
	  (res (make-ucs2-string len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(ucs2-string-set! res
				  i
				  (ucs2-upcase (ucs2-string-ref ucs2-string
								i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-downcase ...                                         */
;*---------------------------------------------------------------------*/
(define (ucs2-string-downcase ucs2-string)
   (let* ((len (ucs2-string-length ucs2-string))
	  (res (make-ucs2-string len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(ucs2-string-set! res
				  i
				  (ucs2-downcase (ucs2-string-ref ucs2-string
								  i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-upcase! ...                                          */
;*---------------------------------------------------------------------*/
(define (ucs2-string-upcase! ucs2-string)
   (let* ((len (ucs2-string-length ucs2-string))
	  (res ucs2-string))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(ucs2-string-set! res
				  i
				  (ucs2-upcase (ucs2-string-ref ucs2-string
								i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-downcase! ...                                        */
;*---------------------------------------------------------------------*/
(define (ucs2-string-downcase! ucs2-string)
   (let* ((len (ucs2-string-length ucs2-string))
	  (res ucs2-string))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(ucs2-string-set! res
				  i
				  (ucs2-downcase (ucs2-string-ref ucs2-string
								  i)))
		(loop (+fx i 1)))))))
		 		 
;*---------------------------------------------------------------------*/
;*    ucs2-string->utf8-string ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string->utf8-string ucs2)
   (c-ucs2-string->utf8-string ucs2))

;*---------------------------------------------------------------------*/
;*    8bits ...                                                        */
;*---------------------------------------------------------------------*/
(define 8bits-inv
   '((#xe2
      (#x80 (#x90 . #\-)
	    (#x91 . #\-)
	    (#x92 . #\-)
	    (#x93 . #\-)
	    (#x94 . #\-)
	    (#x95 . #\-)
	    (#x98 . #\`)
	    (#x99 . #\')
	    (#x9a . #\,)
	    (#x9b . #\`)
	    (#xa4 . #\.)
	    (#xa7 . #\.)
	    (#xb2 . #\')
	    (#xb3 . #\")
	    (#xb5 . #\`)
	    (#xbb . #\")
	    (#xb8 . #\^)
	    (#xb9 . #\<)
	    (#xba . #\>))
      (#x81 (#x83 . #\*))
      (#x82 (#x8d .#\()
	    (#x8e .#\))))))
   
;*---------------------------------------------------------------------*/
;*    cp1252 ...                                                       */
;*---------------------------------------------------------------------*/
(define cp1252
   '#("\xe2\x82\xac" ;; 0x80
      ""             ;; 0x81
      "\xe2\x80\x9a" ;; 0x82
      "\xc6\x92"     ;; 0x83
      "\xe2\x80\x9e" ;; 0x84
      "\xe2\x80\xa6" ;; 0x85
      "\xe2\x80\xa0" ;; 0x86
      "\xe2\x80\xa1" ;; 0x87
      "\xcb\x86"     ;; 0x88
      "\xe2\x80\xb0" ;; 0x89
      "\xc5\xa0"     ;; 0x8a
      "\xe2\x80\xb9" ;; 0x8b
      "\xc5\x92"     ;; 0x8c
      ""             ;; 0x8d
      "\xc5\xbd"     ;; 0x8e
      ""             ;; 0x8f
      ""             ;; 0x90
      "\xe2\x80\x98" ;; 0x91
      "\xe2\x80\x99" ;; 0x92
      "\xe2\x80\x9c" ;; 0x93
      "\xe2\x80\x9d" ;; 0x94
      "\xe2\x80\xa2" ;; 0x95
      "\xe2\x80\x93" ;; 0x96
      "\xe2\x80\x94" ;; 0x97
      "\xcb\x9c"     ;; 0x98
      "\xe2\x84\xa2" ;; 0x99
      "\xc5\xa1"     ;; 0x9a
      "\xe2\x80\xba" ;; 0x9b
      "\xc5\x93"     ;; 0x9c
      ""             ;; 0x9d
      "\xc5\xbe"     ;; 0x9e
      "\xc5\xb8"))   ;; 0x9f

(define cp1252-inv #f)

;*---------------------------------------------------------------------*/
;*    inverse-utf8-table ...                                           */
;*---------------------------------------------------------------------*/
(define (inverse-utf8-table table)
   
   (define (make-table-entry s char)
      (let ((len (string-length s)))
	 (let loop ((i 0))
	    (if (=fx len i)
		(integer->char char)
		(list (cons (char->integer (string-ref s i)) (loop (+fx i 1))))))))
   
   (define (add-table-entry table s char)
      (let loop ((e (car (make-table-entry s char)))
		 (table table))
	 (if (null? e)
	     table
	     (let* ((n (car e))
		    (o (assq n table)))
		(if o
		    (let ((st (cdr o)))
		       (set-cdr! o (loop (cadr e) st))
		       table)
		    (cons e table))))))
   
   (let ((len (vector-length table)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i len)
	     res
	     (let ((s (vector-ref table i)))
		(if (>fx (string-length s) 0)
		    (loop (+fx i 1) (add-table-entry res s (+fx #x80 i)))
		    (loop (+fx i 1) res)))))))

;*---------------------------------------------------------------------*/
;*    utf8-string->ucs2-string ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (utf8-string->ucs2-string utf8)
   (c-utf8-string->ucs2-string utf8))

;*---------------------------------------------------------------------*/
;*    utf8->8bits-length ...                                           */
;*---------------------------------------------------------------------*/
(define (utf8->8bits-length str len)
   (let loop ((size 0)
	      (i 0))
      (if (>=fx i len)
	  size
	  (let ((c (char->integer (string-ref str i))))
	     (cond
		((<fx c #xc2)
		 (loop (+fx size 1) (+fx i 1)))
		((<=fx c #xdf)
		 (loop (+fx size 1) (+fx i 2)))
		((<=fx c #xef)
		 (loop (+fx size 1) (+fx i 3)))
		((<=fx c #xf7)
		 (loop (+fx size 1) (+fx i 4)))
		((<=fx c #xfb)
		 (loop (+fx size 1) (+fx i 5)))
		(else
		 (loop (+fx size 1) (+fx i 6))))))))

;*---------------------------------------------------------------------*/
;*    utf8-string? ...                                                 */
;*---------------------------------------------------------------------*/
(define (utf8-string? str)
   
   (define (in-range? c minc maxc)
      (let ((n (char->integer c)))
	 (and (>=fx n minc) (<=fx n maxc))))
   
   (let ((len (string-length str)))
      (let loop ((r 0))
	 (if (=fx r len)
	     #t
	     (let* ((c (string-ref str r))
		    (n (char->integer c)))
		(cond
		   ((<=fx n #x7f)
		    ;; 1 byte
		    (loop (+fx r 1)))
		   ((<fx n #xc2)
		    ;; error, reserved
		    #f)
		   ((<=fx n #xdf)
		    ;; 2 bytes sequence
		    (when (and (<fx r (-fx len 1))
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf))
		       (loop (+fx r 2))))
		   ((<=fx n #xef)
		    ;; 3 bytes sequence
		    (when (and (<fx r (-fx len 2))
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf))
		       (loop (+fx r 3))))
		   ((=fx n #xf0)
		    ;; 4 bytes sequence special1
		    (when (and (<fx r (-fx len 3))
			       (in-range? (string-ref str (+fx r 1)) #x90 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
		       (loop (+fx r 4))))
		   ((=fx n #xf4)
		    ;; 4 bytes sequence special2
		    (when (and (<fx r (-fx len 3))
			       (in-range? (string-ref str (+fx r 1)) #x80 #x8f)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
		       (loop (+fx r 4))))
		   ((<=fx n #xf7)
		    ;; 4 bytes sequence
		    (when (and (<fx r (-fx len 3))
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
		       (loop (+fx r 4))))
		   ((<=fx n #xfb)
		    (loop (+fx r 5)))
		   ((<=fx n #xfd)
		    (loop (+fx r 6)))
		   (else
		    #f)))))))

;*---------------------------------------------------------------------*/
;*    utf8->8bits-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define (utf8->8bits-fill! nstr str len table)
   
   (define (error-too-short r)
      (error 'utf8->8bits
	     "String too short"
	     (string-for-read (substring str (maxfx 0 (-fx r 10)) len))))
   
   (define (error-ill r)
      (error 'utf8->8bits
	     "Illegal string"
	     (string-for-read (substring str r (minfx len (+fx r 10))))))
   
   (let loop ((r 0)
	      (w 0))
      (if (=fx r len)
	  nstr
	  (let* ((c (string-ref str r))
		 (n (char->integer c)))
	     (cond
		((<=fx n #x7f)
		 (string-set! nstr w c)
		 (loop (+fx r 1) (+fx w 1)))
		((<fx n #xc2)
		 (error-ill r))
		((<=fx n #xdf)
		 (if (=fx r (-fx len 1))
		     (error-too-short r)
		     (let* ((nc (string-ref str (+fx r 1)))
			    (nn (char->integer nc)))
			(if (and (>=fx nn #x80) (<fx nn #xc0))
			    (let ((m (bit-or (bit-lsh (-fx n #xc2) 6) nn)))
			       (string-set! nstr w (integer->char m))
			       (loop (+fx r 2) (+fx w 1)))
			    (error-ill r)))))
		(table
		 (let liip ((subtable (assq n table))
			    (nr (+fx r 1)))
		    (cond
		       ((not subtable)
			(error-ill r))
		       ((char? (cdr subtable))
			(string-set! nstr w (cdr subtable))
			(loop nr (+fx w 1)))
		       ((=fx nr len)
			(error-too-short r))
		       (else
			(let ((nc (char->integer (string-ref str nr))))
			   (liip (assq nc (cdr subtable)) (+fx nr 1)))))))
		(else
		 (error-ill r)))))))

;*---------------------------------------------------------------------*/
;*    utf8->8bits ...                                                  */
;*---------------------------------------------------------------------*/
(define (utf8->8bits str table)
   (let* ((len (string-length str))
	  (nlen (utf8->8bits-length str len)))
      (if (=fx len nlen)
	  (string-copy str)
	  (utf8->8bits-fill! (make-string nlen) str len table))))

;*---------------------------------------------------------------------*/
;*    utf8->8bits! ...                                                 */
;*---------------------------------------------------------------------*/
(define (utf8->8bits! str table)
   (let* ((len (string-length str))
	  (nlen (utf8->8bits-length str len)))
      (if (=fx len nlen)
	  str
	  (utf8->8bits-fill! (make-string nlen) str len #f))))

;*---------------------------------------------------------------------*/
;*    utf8->iso-latin ...                                              */
;*---------------------------------------------------------------------*/
(define (utf8->iso-latin str)
   (utf8->8bits str 8bits-inv))

;*---------------------------------------------------------------------*/
;*    utf8->iso-latin! ...                                             */
;*---------------------------------------------------------------------*/
(define (utf8->iso-latin! str)
   (utf8->8bits! str 8bits-inv))

;*---------------------------------------------------------------------*/
;*    utf8->cp1252 ...                                                 */
;*---------------------------------------------------------------------*/
(define (utf8->cp1252 str)
   (unless cp1252-inv (set! cp1252-inv (inverse-utf8-table cp1252)))
   (utf8->8bits str cp1252-inv))

;*---------------------------------------------------------------------*/
;*    utf8->cp1252! ...                                                */
;*---------------------------------------------------------------------*/
(define (utf8->cp1252! str)
   (unless cp1252-inv (set! cp1252-inv (inverse-utf8-table cp1252)))
   (utf8->8bits! str cp1252-inv))

;*---------------------------------------------------------------------*/
;*    8bits->utf8-length ...                                           */
;*---------------------------------------------------------------------*/
(define (8bits->utf8-length str len table)
   
   (define (table-ref-len c)
      (let ((v (-fx c #x80)))
	 (if (<fx v (vector-length table))
	     (string-length (vector-ref table v))
	     2)))
   
   (let loop ((size 0)
	      (i 0))
      (if (=fx i len)
	  size
	  (let ((c (char->integer (string-ref-ur str i))))
	     (if (>=fx c #x80)
		 (if table
		     (loop (+fx size (table-ref-len c)) (+fx i 1))
		     (loop (+fx size 2) (+fx i 1)))
		 (loop (+fx size 1) (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    8bits->utf8-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define (8bits->utf8-fill! nstr str len table)
   (let loop ((r 0)
	      (w 0))
      (if (=fx r len)
	  nstr
	  (let ((c (char->integer (string-ref-ur str r))))
	     (cond
		((>=fx c #xc0)
		 (string-set-ur! nstr w (integer->char #xc3))
		 (string-set-ur! nstr (+fx w 1) (integer->char (-fx c #x40)))
		 (loop (+fx r 1) (+fx w 2)))
		((>=fx c #x80)
		 (if table
		     (let ((v (-fx c #x80)))
			(if (<fx v (vector-length table))
			    (let* ((enc (vector-ref table v))
				   (len (string-length enc)))
			       (blit-string! enc 0 nstr w len)
			       (loop (+fx r 1) (+fx w len)))
			    (begin
			       (string-set-ur! nstr w (integer->char #xc2))
			       (string-set-ur! nstr (+fx w 1) (integer->char c))
			       (loop (+fx r 1) (+fx w 2)))))
		     (begin
			(string-set-ur! nstr w (integer->char #xc2))
			(string-set-ur! nstr (+fx w 1) (integer->char c))
			(loop (+fx r 1) (+fx w 2)))))
		(else
		 (string-set-ur! nstr w (integer->char c))
		 (loop (+fx r 1) (+fx w 1))))))))

;*---------------------------------------------------------------------*/
;*    8bits->utf8 ...                                                  */
;*---------------------------------------------------------------------*/
(define (8bits->utf8 str table)
   (let* ((len (string-length str))
	  (nlen (8bits->utf8-length str len table)))
      (if (=fx len nlen)
	  (string-copy str)
	  (8bits->utf8-fill! ($make-string/wo-fill nlen) str len table))))

;*---------------------------------------------------------------------*/
;*    8bits->utf8! ...                                                 */
;*---------------------------------------------------------------------*/
(define (8bits->utf8! str table)
   (let* ((len (string-length str))
	  (nlen (8bits->utf8-length str len table)))
      (if (=fx len nlen)
	  str
	  (8bits->utf8-fill! ($make-string/wo-fill nlen) str len table))))

;*---------------------------------------------------------------------*/
;*    iso-latin->utf8 ...                                              */
;*---------------------------------------------------------------------*/
(define (iso-latin->utf8 str)
   (8bits->utf8 str #f))
   
;*---------------------------------------------------------------------*/
;*    iso-latin->utf8! ...                                             */
;*---------------------------------------------------------------------*/
(define (iso-latin->utf8! str)
   (8bits->utf8! str #f))

;*---------------------------------------------------------------------*/
;*    cp1252->utf8 ...                                                 */
;*---------------------------------------------------------------------*/
(define (cp1252->utf8 str)
   (8bits->utf8 str cp1252))
   
;*---------------------------------------------------------------------*/
;*    cp1252->utf8! ...                                                */
;*---------------------------------------------------------------------*/
(define (cp1252->utf8! str)
   (8bits->utf8! str cp1252))
