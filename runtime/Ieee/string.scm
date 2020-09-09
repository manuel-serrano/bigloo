;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/string.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 20 19:17:18 1995                          */
;*    Last change :  Fri Jun 14 14:30:35 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.7. Strings (page 25, r4)                                       */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Strings@                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r4_strings_6_7
   
   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    
	    __evenv)
   
   (extern  (macro $string?::bool (::obj) "STRINGP")
	    ($make-string::bstring (::long ::uchar) "make_string")
	    ($make-string/wo-fill::bstring (::long) "make_string_sans_fill")
	    ($string->bstring-len::bstring (str::string len::int) "string_to_bstring_len")
	    (macro $string-length::long (::bstring) "STRING_LENGTH")
	    (macro $string-ref::uchar (::bstring ::long) "STRING_REF")
	    (macro $string-set!::obj (::bstring ::long ::uchar) "STRING_SET")

	    (macro $memchr::string (::string ::int ::long ::long) "BGL_MEMCHR")
	    (macro $memchr-zero?::bool (::string) "BGL_MEMCHR_ZERO")
	    (macro $memchr-diff::long (::string ::string) "BGL_MEMCHR_DIFF")
	    
	    ($string=?::bool (::bstring ::bstring) "bigloo_strcmp")
	    (macro $memcmp::int (::string ::string ::long) "memcmp")
	    ($substring=?::bool (::bstring ::bstring ::long) "bigloo_strncmp")
	    ($substring-ci=?::bool (::bstring ::bstring ::long) "bigloo_strncmp_ci")
	    ($prefix-at?::bool (::bstring ::bstring ::long) "bigloo_strcmp_at")
	    ($prefix-ci-at?::bool (::bstring ::bstring ::long) "bigloo_strcmp_ci_at")
	    ($substring-at?::bool (::bstring ::bstring ::long ::long) "bigloo_strncmp_at")
	    ($substring-ci-at?::bool (::bstring ::bstring ::long ::long) "bigloo_strncmp_ci_at")
	    ($strcicmp::bool (::bstring ::bstring) "bigloo_strcicmp")
	    ($string_lt::bool (::bstring ::bstring) "bigloo_string_lt")
	    ($string_le::bool (::bstring ::bstring) "bigloo_string_le")
	    ($string_gt::bool (::bstring ::bstring) "bigloo_string_gt")
	    ($string_ge::bool (::bstring ::bstring) "bigloo_string_ge")
	    ($string_cilt::bool (::bstring ::bstring) "bigloo_string_cilt")
	    ($string_cile::bool (::bstring ::bstring) "bigloo_string_cile")
	    ($string_cigt::bool (::bstring ::bstring) "bigloo_string_cigt")
	    ($string_cige::bool (::bstring ::bstring) "bigloo_string_cige")
	    
	    ($substring::bstring (::bstring ::long ::long) "c_substring")
	    ($string-append::bstring (::bstring ::bstring) "string_append")
	    ($string-append-3::bstring (::bstring ::bstring ::bstring)
					"string_append_3")
	    
	    ($escape-C-string::bstring (::string ::long ::long)
				       "bgl_escape_C_string")
	    ($escape-scheme-string::bstring (::string ::long ::long)
					    "bgl_escape_scheme_string")
	    ($constant-string-to-string::bstring (::string)
						  "c_constant_string_to_string")
	    
	    (macro $string-bound-check?::bool (::long ::long) "BOUND_CHECK")
	    ($string-for-read::bstring (::bstring) "string_for_read")
	    ($symbol-for-read::bstring (::bstring) "symbol_for_read")
	    ($blit-string::obj (::bstring ::long ::bstring ::long ::long)
				"blit_string")
	    (macro $string-shrink!::bstring (::bstring ::long) "bgl_string_shrink"))

   (java    (class foreign
	       (method static $string?::bool (::obj)
		       "STRINGP")
	       (method static $make-string::bstring (::long ::uchar)
                  "make_string")

               (method static $string->bstring-len::bstring (::string ::long)
                  "string_to_bstring_len")
	   
	       (method static $string-length::long (::bstring)
		       "STRING_LENGTH")
	       
	       (method static $string-ref::uchar (::bstring ::long)
		       "STRING_REF")
	       (method static $string-set!::obj (::bstring ::long ::uchar)
		       "STRING_SET")
	       
	       (method static $string=?::bool (::bstring ::bstring)
		       "bigloo_strcmp")
	       (method static $substring=?::bool (::bstring ::bstring ::long)
		       "bigloo_strncmp")
	       (method static $substring-ci=?::bool (::bstring ::bstring ::long)
		       "bigloo_strncmp_ci")
	       (method static $prefix-at?::bool (::bstring ::bstring ::long)
		       "bigloo_strcmp_at")
	       (method static $prefix-ci-at?::bool (::bstring ::bstring ::long)
		       "bigloo_strcmp_ci_at")
	       (method static $substring-at?::bool (::bstring ::bstring ::long ::long)
		       "bigloo_strncmp_at")
	       (method static $substring-ci-at?::bool (::bstring ::bstring ::long ::long)
		       "bigloo_strncmp_ci_at")
	       
	       (method static $strcicmp::bool (::bstring ::bstring)
		       "strcicmp")
	       (method static $string_lt::bool (::bstring ::bstring)
		       "string_lt")
	       (method static $string_le::bool (::bstring ::bstring)
		       "string_le")
	       (method static $string_gt::bool (::bstring ::bstring)
		       "string_gt")
	       (method static $string_ge::bool (::bstring ::bstring)
		       "string_ge")
	       (method static $string_cilt::bool (::bstring ::bstring)
		       "string_cilt")
	       (method static $string_cile::bool (::bstring ::bstring)
		       "string_cile")
	       (method static $string_cigt::bool (::bstring ::bstring)
		       "string_cigt")
	       (method static $string_cige::bool (::bstring ::bstring)
		       "string_cige")
	       
	       (method static $substring::bstring (::bstring ::long ::long)
		       "c_substring")
	       (method static $string-append::bstring (::bstring ::bstring)
		       "string_append")
	       (method static $string-append-3::bstring (::bstring ::bstring ::bstring)
		       "string_append_3")
	       
	       (method static $escape-C-string::bstring (::string ::long ::long)
		       "bgl_escape_C_string")
	       (method static $escape-scheme-string::bstring (::string ::long ::long)
		       "bgl_escape_scheme_string")
	       (method static $constant-string-to-string::bstring (::string)
		       "c_constant_string_to_string")
	       
	       (method static $string-bound-check?::bool (::long ::long)
		       "BOUND_CHECK")
	       (method static $string-for-read::bstring (::bstring)
		       "string_for_read")
	       (method static $symbol-for-read::bstring (::bstring)
		       "symbol_for_read")
	       (method static $blit-string::obj (::bstring ::long ::bstring ::long ::long)
		       "blit_string")
	       (method static $make-string/wo-fill::bstring (::long)
		       "make_string_sans_fill")
	       (method static $string-shrink!::bstring (::bstring ::long)
		       "bgl_string_shrink")))
   
   (export  (inline string?::bool ::obj)
	    (inline string-null?::bool ::bstring)
	    (inline make-string::bstring ::long #!optional (char #\space))
	    (inline string::bstring . chars)
	    (inline string-length::long ::bstring)
	    (inline string-ref::uchar ::bstring ::long)
	    (inline string-set!::obj ::bstring ::long ::uchar)
	    (inline string-ref-ur::uchar ::bstring ::long)
	    (inline string-set-ur!::obj ::bstring ::long ::uchar)
	    (inline string=?::bool ::bstring ::bstring)
	    (inline string-ci=?::bool ::bstring ::bstring)
	    (inline substring=?::bool ::bstring ::bstring ::long)
	    (inline substring-ci=?::bool ::bstring ::bstring ::long)
	    (inline substring-at?::bool ::bstring ::bstring ::long #!optional (len -1))
	    (inline substring-ci-at?::bool ::bstring ::bstring ::long #!optional (len -1))
	    (inline empty-string?::bool ::bstring)
	    (inline string<?::bool ::bstring ::bstring)
	    (inline string>?::bool ::bstring ::bstring)
	    (inline string<=?::bool ::bstring ::bstring)
	    (inline string>=?::bool ::bstring ::bstring)
	    (inline string-ci<?::bool ::bstring ::bstring)
	    (inline string-ci>?::bool ::bstring ::bstring)
	    (inline string-ci<=?::bool ::bstring ::bstring)
	    (inline string-ci>=?::bool ::bstring ::bstring)
	    (substring::bstring string::bstring ::long #!optional (end::long (string-length string)))
	    (inline substring-ur::bstring ::bstring ::long ::long)
	    (string-contains ::bstring ::bstring #!optional (start::int 0))
	    (string-contains-ci ::bstring ::bstring #!optional (start::int 0))
	    (string-compare3::long ::bstring ::bstring)
	    (string-compare3-ci::long ::bstring ::bstring)
	    (string-append::bstring . strings)
	    (string->list::pair-nil ::bstring)
	    (list->string::bstring ::pair-nil)
	    (string-copy::bstring ::bstring)
	    (string-fill!::obj ::bstring ::uchar)
	    (string-upcase::bstring ::bstring)
	    (string-downcase::bstring ::bstring)
	    (string-upcase!::bstring ::bstring)
	    (string-downcase!::bstring ::bstring)
	    (string-capitalize!::bstring ::bstring)
	    (string-capitalize::bstring ::bstring)
	    (inline string-for-read::bstring ::bstring)
	    (inline string-as-read::bstring ::bstring)
	    (inline escape-scheme-string::bstring ::bstring)
	    (inline escape-C-string::bstring ::bstring)
	    (inline blit-string-ur! ::bstring ::long ::bstring ::long ::long)
	    (blit-string! ::bstring ::long ::bstring ::long ::long)
	    (inline string-shrink! ::bstring ::long)
	    (string-replace ::bstring ::char ::char)
	    (string-replace! ::bstring ::char ::char)
	    (string-delete::bstring string::bstring ::obj #!optional (start::int 0) (end::long (string-length string)))
	    (string-split::pair-nil ::bstring . opt)
	    (string-cut::pair-nil ::bstring . opt)
	    (string-index::obj ::bstring ::obj #!optional (start 0))
	    (string-char-index::obj ::bstring ::char #!optional (start 0) (count -1))
	    (string-char-index-ur::obj ::bstring ::char ::long ::long)
	    (string-index-right::obj s::bstring ::obj
	       #!optional (start (string-length s)))
	    (string-skip::obj ::bstring ::obj #!optional (start 0))
	    (string-skip-right::obj s::bstring ::obj
	       #!optional (start (string-length s)))
	    (string-prefix-length::int s1::bstring s2::bstring
	       #!optional start1 end1 start2 end2)
	    (string-suffix-length::int s1::bstring s2::bstring
	       #!optional start1 end1 start2 end2)
	    (string-prefix-length-ci::int s1::bstring s2::bstring
	       #!optional start1 end1 start2 end2)
	    (string-suffix-length-ci::int s1::bstring s2::bstring
	       #!optional start1 end1 start2 end2)
	    (string-prefix?::bool s1::bstring s2::bstring
	       #!optional start1 end1 start2 end2)
	    (string-suffix?::bool s1::bstring s2::bstring
	       #!optional start1 end1 start2 end2)
	    (string-prefix-ci?::bool s1::bstring s2::bstring
	       #!optional start1 end1 start2 end2)
	    (string-suffix-ci?::bool s1::bstring s2::bstring
	       #!optional start1 end1 start2 end2)
	    (string-natural-compare3::int ::bstring ::bstring
	       #!optional (start1 0) (start2 0))
	    (string-natural-compare3-ci::int ::bstring ::bstring
	       #!optional (start1 0) (start2 0))
	    (string-hex-intern::bstring ::bstring)
	    (string-hex-intern!::bstring ::bstring)
	    (string-hex-extern::bstring str::bstring #!optional (start::int 0) (end::long (string-length str))))
   
   (pragma  ($string? fail-safe (predicate-of bstring) no-cfa-top nesting)
	    (string? fail-safe side-effect-free no-cfa-top nesting)
	    (string-null? fail-safe side-effect-free no-cfa-top nesting)
	    ($string-ref fail-safe side-effect-free no-cfa-top nesting args-safe)
	    (string-ref-ur fail-safe side-effect-free no-cfa-top nesting)
	    (string-ref side-effect-free no-cfa-top nesting)
	    ($string-length fail-safe side-effect-free no-cfa-top nesting args-safe)
	    (string-length fail-safe side-effect-free no-cfa-top nesting)
	    ($string-bound-check? fail-safe side-effect-free no-cfa-top nesting)
	    ($string=? fail-safe side-effect-free nesting)
	    ($substring=? fail-safe side-effect-free nesting)
	    ($substring-ci=? fail-safe side-effect-free nesting)
	    ($substring-at? fail-safe side-effect-free nesting)
	    ($substring-ci-at? fail-safe side-effect-free nesting)
	    ($strcicmp fail-safe side-effect-free nesting)
	    ($string_lt fail-safe side-effect-free nesting)
	    ($string_gt fail-safe side-effect-free nesting)
	    ($string_le fail-safe side-effect-free nesting)
	    ($string_ge fail-safe side-effect-free nesting)
	    ($string_cilt fail-safe side-effect-free nesting)
	    ($string_cile fail-safe side-effect-free nesting)
	    ($string_cigt fail-safe side-effect-free nesting)
	    ($string_cige fail-safe side-effect-free nesting)
	    ($substring fail-safe nesting)
	    (empty-string? fail-safe side-effect-free nesting)
	    (string-split fail-safe side-effect-free nesting no-cfa-top)
	    (string-cut fail-safe side-effect-free nesting no-cfa-top)
	    (string-prefix-length fail-safe side-effect-free nesting)
	    (string-suffix-length fail-safe side-effect-free nesting)
	    (string-prefix-length-ci fail-safe side-effect-free nesting)
	    (string-suffix-length-ci fail-safe side-effect-free nesting)
	    (string-prefix? fail-safe side-effect-free nesting)
	    (string-suffix? fail-safe side-effect-free nesting)
	    (string-prefix-ci? fail-safe side-effect-free nesting)
	    (string-suffix-ci? fail-safe side-effect-free nesting))
   (cond-expand
      (bigloo-c
       (pragma
	  ($memchr no-alloc fail-safe side-effect-free)
	  ($memchr-zero? no-alloc fail-safe side-effect-free)
	  ($memchr-diff no-alloc fail-safe side-effect-free)))))
 
;*---------------------------------------------------------------------*/
;*    @deffn string?@ ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (string? obj)
   ($string? obj))

;*---------------------------------------------------------------------*/
;*    string-null? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (string-null? str)
   (=fx (string-length str) 0))

;*---------------------------------------------------------------------*/
;*    @deffn make-string@ ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (make-string k #!optional (char #\space))
   ($make-string k char))
 
;*---------------------------------------------------------------------*/
;*    @deffn string@ ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (string . chars)
   (list->string chars))

;*---------------------------------------------------------------------*/
;*    @deffn string-length@ ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (string-length string)
   ($string-length string))

;*---------------------------------------------------------------------*/
;*    @deffn string-ref@ ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (string-ref string k)
   ($string-ref string k))
 
;*---------------------------------------------------------------------*/
;*    @deffn string-set!@ ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (string-set! string k char)
   ($string-set! string k char))

;*---------------------------------------------------------------------*/
;*    @deffn string-ref-ur@ ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (string-ref-ur string k)
   ($string-ref string k))
 
;*---------------------------------------------------------------------*/
;*    @deffn string-set-ur!@ ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (string-set-ur! string k char)
   ($string-set! string k char))

;*---------------------------------------------------------------------*/
;*    @deffn string=?@ ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (string=? string1 string2)
   (cond-expand
      (bigloo-c
       (let ((l1 (string-length string1)))
	  (when (=fx l1 (string-length string2))
	     (=fx ($memcmp string1 string2 l1) 0))))
      (else
       ($string=? string1 string2))))

;*---------------------------------------------------------------------*/
;*    @deffn substring=?@ ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (substring=? string1 string2 len)
   ($substring=? string1 string2 len))

;*---------------------------------------------------------------------*/
;*    @deffn substring-at?@ ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (substring-at? string1 string2 off #!optional (len -1))
   (if (=fx len -1)
       ($prefix-at? string1 string2 off)
       ($substring-at? string1 string2 off len)))

;*---------------------------------------------------------------------*/
;*    @deffn substring-ci=?@ ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (substring-ci=? string1 string2 len)
   ($substring-ci=? string1 string2 len))

;*---------------------------------------------------------------------*/
;*    @deffn substring-ci-at?@ ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (substring-ci-at? string1 string2 off #!optional (len -1))
   (if (=fx len -1)
       ($prefix-ci-at? string1 string2 off)
       ($substring-ci-at? string1 string2 off len)))

;*---------------------------------------------------------------------*/
;*    @deffn empty-string?@ ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (empty-string? string)
   (=fx (string-length string) 0))

;*---------------------------------------------------------------------*/
;*    @deffn string-ci=?@ ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (string-ci=? string1 string2)
   ($strcicmp string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn string<?@ ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (string<? string1 string2)
   ($string_lt string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn string>?@ ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (string>? string1 string2)
   ($string_gt string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn string<=?@ ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (string<=? string1 string2)
   ($string_le string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn string>=?@ ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (string>=? string1 string2)
   ($string_ge string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn string-ci<?@ ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (string-ci<? string1 string2)
   ($string_cilt string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn string-ci>?@ ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (string-ci>? string1 string2)
   ($string_cigt string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn string-ci<=?@ ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (string-ci<=? string1 string2)
   ($string_cile string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn string-ci>=?@ ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (string-ci>=? string1 string2)
   ($string_cige string1 string2))

;*---------------------------------------------------------------------*/
;*    @deffn substring@ ...                                            */
;*---------------------------------------------------------------------*/
(define (substring string start #!optional (end::long (string-length string)))
   (let ((len (string-length string)))
      (cond
	 ((or (<fx start 0) (>fx start len))
	  (error "substring"
		 (string-append "Illegal start index " (fixnum->string start))
		 (list (string-length string) string)))
	 ((or (<fx end start) (>fx end len))
	  (error "substring"
		 (string-append "Illegal end index " (fixnum->string end))
		 (list (string-length string) string)))
	 (else
	  ($substring string start end)))))

;*---------------------------------------------------------------------*/
;*    @deffn substring-ur@ ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (substring-ur string start end)
   ($substring string start end))

;*---------------------------------------------------------------------*/
;*    string-contains ...                                              */
;*---------------------------------------------------------------------*/
(define (string-contains s1 s2 #!optional (start::int 0))
   (let ((l2 (string-length s2)))
      (if (=fx l2 1)
	  (string-index s1 (string-ref s2 0) start)
	  (let ((l1 (string-length s1))
		(i0 (if (<fx start 0) 0 start)))
	     (if (<fx l1 (+fx i0 l2))
		 #f
		 (let ((stop (-fx l1 l2)))
		    (let loop ((i i0))
		       (cond
			  ((substring-at? s1 s2 i)
			   i)
			  ((=fx i stop)
			   #f)
			  (else
			   (loop (+fx i 1)))))))))))

;*---------------------------------------------------------------------*/
;*    string-contains-ci ...                                           */
;*---------------------------------------------------------------------*/
(define (string-contains-ci s1 s2 #!optional (start::int 0))
   (let ((l1 (string-length s1))
	 (l2 (string-length s2))
	 (i0 (if (<fx start 0) 0 start)))
      (if (<fx l1 (+fx i0 l2))
	  #f
	  (let ((stop (-fx l1 l2)))
	     (let loop ((i i0))
		(cond
		   ((substring-ci-at? s1 s2 i)
		    i)
		   ((=fx i stop)
		    #f)
		   (else
		    (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-compare3@ ...                                      */
;*    -------------------------------------------------------------    */
;*    if a < b returns a negative number                               */
;*    if a = b returns 0                                               */
;*    if a > b return a positive number                                */
;*---------------------------------------------------------------------*/
(define (string-compare3::long a::bstring b::bstring)
  (let* ((al (string-length a))
         (bl (string-length b))
         (l (minfx al bl)))
    (let loop ((r 0))
       (if (=fx r l)
           (-fx al bl)
           (let ((c (-fx (char->integer (string-ref-ur a r))
                         (char->integer (string-ref-ur b r)))))
              (if (=fx c 0)
                  (loop (+fx r 1))
                  c))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-compare3-ci@ ...                                   */
;*---------------------------------------------------------------------*/
(define (string-compare3-ci::long a::bstring b::bstring)
  (let* ((al (string-length a))
         (bl (string-length b))
         (l (minfx al bl)))
    (let loop ((r 0))
       (if (=fx r l)
           (-fx al bl)
           (let ((c (-fx (char->integer (char-downcase (string-ref-ur a r)))
                         (char->integer (char-downcase (string-ref-ur b r))))))
              (if (=fx c 0)
                  (loop (+fx r 1))
                  c))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-append@ ...                                        */
;*    -------------------------------------------------------------    */
;*    To avoid allocating N strings where N is the size of the list    */
;*    of string, we precompute the global string size and we fill      */
;*    it.                                                              */
;*---------------------------------------------------------------------*/
(define (string-append . list)
   (if (null? list)
       ""
       (let* ((len (let string-append ((list list)
				       (res  0))
		      (if (null? list)
			  res
			  (string-append (cdr list)
			     (+fx res
				(string-length (car list)))))))
	      (res ($make-string/wo-fill len)))
	  (let string-append ((list list)
			      (w 0))
	     (if (null? list)
		 res
		 (let* ((s (car list))
			(l (string-length s)))
		    (blit-string-ur! s 0 res w l)
		    (string-append (cdr list) (+fx w l))))))))
 
;*---------------------------------------------------------------------*/
;*    @deffn list->string@ ...                                         */
;*---------------------------------------------------------------------*/
(define (list->string list)
   (let* ((len    (length list))
	  (string ($make-string/wo-fill len)))
      (let loop ((i 0)
		 (l list))
	 (if (=fx i len)
	     string
	     (begin
		(string-set-ur! string i (car l))
		(loop (+fx i 1) (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    @deffn string->list@ ...                                         */
;*---------------------------------------------------------------------*/
(define (string->list string)
   (let ((len (string-length string)))
      (let loop ((i   (-fx len 1))
		 (res '()))
	 (if (=fx i -1)
	     res
	     (loop (-fx i 1)
		   (cons (string-ref-ur string i) res))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-copy@ ...                                          */
;*---------------------------------------------------------------------*/
(define (string-copy string)
   (let* ((len (string-length string))
	  (new ($make-string/wo-fill len)))
      (let loop ((i (-fx len 1)))
	 (if (=fx i -1)
	     new
	     (begin
		(string-set-ur! new i (string-ref-ur string i))
		(loop (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-fill!@ ...                                         */
;*---------------------------------------------------------------------*/
(define (string-fill! string char)
   (let ((len (string-length string)))
      (let loop ((i 0))
	 (if (=fx i len)
	     #unspecified
	     (begin
		(string-set-ur! string i char)
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-upcase@ ...                                        */
;*---------------------------------------------------------------------*/
(define (string-upcase string)
   (let* ((len (string-length string))
	  (res ($make-string/wo-fill len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(string-set-ur! res i (char-upcase (string-ref-ur string i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-downcase@ ...                                      */
;*---------------------------------------------------------------------*/
(define (string-downcase string)
   (let* ((len (string-length string))
	  (res ($make-string/wo-fill len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(string-set-ur! res i (char-downcase (string-ref-ur string i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-upcase!@ ...                                       */
;*---------------------------------------------------------------------*/
(define (string-upcase! string)
   (let ((len (string-length string)))
      (let loop ((i 0))
	 (if (=fx i len)
	     string
	     (begin
		(string-set-ur! string i (char-upcase (string-ref-ur string i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-downcase!@ ...                                     */
;*---------------------------------------------------------------------*/
(define (string-downcase! string)
   (let ((len (string-length string)))
      (let loop ((i 0))
	 (if (=fx i len)
	     string
	     (begin
		(string-set-ur! string i (char-downcase (string-ref-ur string i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-capitalize!@ ...                                   */
;*    -------------------------------------------------------------    */
;*    "hELLO" -> "Hello"                                               */
;*    "*hello" -> "*Hello"                                             */
;*    "hello you" -> "Hello You"                                       */
;*---------------------------------------------------------------------*/
(define (string-capitalize! string)
   (let ((non-first-alpha #f)		 
	 (string-len (string-length string)))	 
      (do ((i 0 (+fx i 1)))			 
	    ((=fx i string-len) string)
	 (let ((c (string-ref-ur string i)))
	    (if (or (char-alphabetic? c) (>fx (char->integer c) 127))
		(if non-first-alpha
		    (string-set-ur! string i (char-downcase c))
		    (begin
		       (set! non-first-alpha #t)
		       (string-set-ur! string i (char-upcase c))))
		(set! non-first-alpha #f))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-capitalize@ ...                                    */
;*---------------------------------------------------------------------*/
(define (string-capitalize string)
   (string-capitalize! (string-copy string)))

;*---------------------------------------------------------------------*/
;*    @deffn string-for-read@ ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (string-for-read string)
   ($string-for-read string))

;*---------------------------------------------------------------------*/
;*    @deffn escape-C-string@ ...                                      */
;*    -------------------------------------------------------------    */
;*    Backward compatibility                                           */
;*---------------------------------------------------------------------*/
(define-inline (escape-C-string str)
   (string-as-read (substring str 1)))

;*---------------------------------------------------------------------*/
;*    @deffn escape-scheme-string@ ...                                 */
;*    -------------------------------------------------------------    */
;*    Backward compatibility                                           */
;*---------------------------------------------------------------------*/
(define-inline (escape-scheme-string str)
   ($escape-scheme-string str 0 (string-length str)))

;*---------------------------------------------------------------------*/
;*    @deffn string-as-read@ ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (string-as-read str)
   ($escape-C-string str 0 (string-length str)))

;*---------------------------------------------------------------------*/
;*    @deffn blit-string-ur!@ ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (blit-string-ur! s1 o1 s2 o2 l)
   ($blit-string s1 o1 s2 o2 l))

;*---------------------------------------------------------------------*/
;*    @deffn blit-string!@ ...                                         */
;*---------------------------------------------------------------------*/
(define (blit-string! s1 o1 s2 o2 l)
   (if (and ($string-bound-check? (+fx l o1) (+fx (string-length s1) 1))
	    ($string-bound-check? (+fx l o2) (+fx (string-length s2) 1)))
       ($blit-string s1 o1 s2 o2 l)
       (error "blit-string!:Index and length out of range"
	      (string-append "[src:" s1 "] [dest:" s2 "]")
	      (list (string-length s1) o1 (string-length s2) o2 l))))

;*---------------------------------------------------------------------*/
;*    @deffn string-shrink!@ ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (string-shrink! s l)
   ($string-shrink! s l))

;*---------------------------------------------------------------------*/
;*    string-replace ...                                               */
;*---------------------------------------------------------------------*/
(define (string-replace str c1 c2)
   (let* ((len (string-length str))
	  (new (make-string len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     new
	     (let ((c (string-ref-ur str i)))
		(if (char=? c c1)
		    (string-set-ur! new i c2)
		    (string-set-ur! new i c))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-replace! ...                                              */
;*---------------------------------------------------------------------*/
(define (string-replace! str c1 c2)
   (let ((len (string-length str)))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     str)
	    ((char=? (string-ref-ur str i) c1)
	     (string-set-ur! str i c2)
	     (loop (+fx i 1)))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-delete ...                                                */
;*---------------------------------------------------------------------*/
(define (string-delete string obj #!optional (start::int 0) (end::long (string-length string)))
   
   (define (string-delete-char new c start end)
      (let loop ((i start)
		 (j 0))
	 (if (=fx i end)
	     (string-shrink! new j)
	     (let ((cc (string-ref string i)))
		(if (char=? cc c)
		    (loop (+fx i 1) j)
		    (begin
		       (string-set! new j cc)
		       (loop (+fx i 1) (+fx j 1))))))))
   
   (define (string-delete-string new l start end)
      (let loop ((i start)
		 (j 0))
	 (if (=fx i end)
	     (string-shrink! new j)
	     (let ((cc (string-ref string i)))
		(if (memv cc l)
		    (loop (+fx i 1) j)
		    (begin
		       (string-set! new j cc)
		       (loop (+fx i 1) (+fx j 1))))))))
   
   (define (string-delete-predicate new pred start end)
      (let loop ((i start)
		 (j 0))
	 (if (=fx i end)
	     (string-shrink! new j)
	     (let ((cc (string-ref string i)))
		(if (pred cc)
		    (loop (+fx i 1) j)
		    (begin
		       (string-set! new j cc)
		       (loop (+fx i 1) (+fx j 1))))))))
   
   (cond
      ((<fx start 0)
       (error "string-delete" "start index out of range" start))
      ((>fx end (string-length string))
       (error "string-delete" "end index out of range" end))
      ((<fx end start)
       (error "string-delete" "Illegal indices" (cons start end)))
      (else
       (let* ((len (-fx end start))
	      (new (string-copy string)))
	  (cond
	     ((char? obj)
	      (string-delete-char new obj start end))
	     ((string? obj)
	      (string-delete-string new (string->list obj) start end))
	     ((procedure? obj)
	      (string-delete-predicate new obj start end))
	     (else
	      (error "string-delete" "Illegal char/charset/predicate" obj)))))))

;*---------------------------------------------------------------------*/
;*    delim? ...                                                       */
;*---------------------------------------------------------------------*/
(define (delim? delims char)
   (let ((len (string-length delims)))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     #f)
	    ((char=? char (string-ref-ur delims i))
	     #t)
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    skip-separator ...                                               */
;*---------------------------------------------------------------------*/
(define (skip-separator delims string len i)
   (cond
      ((=fx i len)
       len)
      ((delim? delims (string-ref-ur string i))
       (skip-separator delims string len (+fx i 1)))
      (else
       i)))

;*---------------------------------------------------------------------*/
;*    skip-non-separator ...                                           */
;*---------------------------------------------------------------------*/
(define (skip-non-separator delims string len i)
   (cond
      ((=fx i len)
       len)
      ((delim? delims (string-ref-ur string i))
       i)
      (else
       (skip-non-separator delims string len (+fx i 1)))))

;*---------------------------------------------------------------------*/
;*    @deffn string-split@ ...                                         */
;*---------------------------------------------------------------------*/
(define (string-split string . delimiters)
   (let* ((d (if (pair? delimiters)
		 (car delimiters)
		 " \t\n"))
	  (len (string-length string))
	  (i (skip-separator d string len 0)))
      (let loop ((i i)
		 (res '()))
	 (if (=fx i len)
	     (reverse! res)
	     (let* ((e (skip-non-separator d string len (+fx i 1)))
		    (nres (cons (substring string i e) res)))
		(if (=fx e len)
		    (reverse! nres)
		    (loop (skip-separator d string len (+fx e 1))
			  nres)))))))

;*---------------------------------------------------------------------*/
;*    @deffn string-cut@ ...                                           */
;*---------------------------------------------------------------------*/
(define (string-cut string . delimiters)
   (let* ((d (if (pair? delimiters)
		 (car delimiters)
		 " \t\n"))
	  (len (string-length string)))
      (let loop ((i 0)
		 (res '()))
	 (if (>=fx i len)
	     (reverse! (cons "" res))
	     (let* ((e (skip-non-separator d string len i))
		    (s (substring string i e))
		    (nr (cons s res)))
		(if (=fx e len)
		    (reverse! nr)
		    (loop (+fx e 1) nr)))))))

;*---------------------------------------------------------------------*/
;*    string-char-index-ur ...                                         */
;*---------------------------------------------------------------------*/
(define (string-char-index-ur string char start::long n::long)
   (cond-expand
      (bigloo-c
       (let* ((s0::string string)
	      (s1::string ($memchr s0 (char->integer char) n start)))
	  (unless ($memchr-zero? s1)
	     ($memchr-diff s1 s0))))
      (else
       (let ((len (+fx start n)))
	  (let loop ((i start))
	     (cond
		((>=fx i len)
		 #f)
		((char=? (string-ref-ur string i) char)
		 i)
		(else
		 (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    string-char-index ...                                            */
;*---------------------------------------------------------------------*/
(define (string-char-index string char #!optional (start 0) (count -1))
   (let* ((len (string-length string))
	  (n (if (and (integer? count)
		      (>=fx count 0) (<=fx count (-fx len start)))
		 count
		 (-fx len start))))
      (when (>fx len start)
	 (string-char-index-ur string char start n))))

;*---------------------------------------------------------------------*/
;*    string-index ...                                                 */
;*---------------------------------------------------------------------*/
(define (string-index string rs #!optional (start 0))
   (cond
      ((char? rs)
       (string-char-index string rs start))
      ((not (string? rs))
       (error "string-index" "Illegal regset" rs))
      ((=fx (string-length rs) 1)
       (string-char-index string (string-ref rs 0) start))
      ((<=fx (string-length rs) 10)
       (let ((len (string-length string))
	     (lenj (string-length rs)))
	  (let loop ((i start))
	     (if (>=fx i len)
		 #f
		 (let ((c (string-ref string i)))
		    (let liip ((j 0))
		       (if (=fx j lenj)
			   (loop (+fx i 1))
			   (if (char=? c (string-ref-ur rs j))
			       i
			       (liip (+fx j 1))))))))))
      (else
       (let ((t (make-string 256 #\n))
	     (len (string-length string)))
	  (let loop ((i (-fx (string-length rs) 1)))
	     (if (=fx i -1)
		 (let liip ((i start))
		    (cond
		       ((>=fx i len)
			#f)
		       ((char=? (string-ref
				   t (char->integer (string-ref-ur string i)))
			   #\y)
			i)
		       (else
			(liip (+fx i 1)))))
		 (begin
		    (string-set-ur! t (char->integer (string-ref-ur rs i)) #\y)
		    (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    string-index-right ...                                           */
;*---------------------------------------------------------------------*/
(define (string-index-right s rs #!optional (start (string-length s)))
   (define (string-char-index s c)
      (let loop ((i (-fx start 1)))
	 (cond
	    ((<fx i 0)
	     #f)
	    ((char=? (string-ref-ur s i) c)
	     i)
	    (else
	     (loop (-fx i 1))))))
   (cond
      ((>fx start (string-length s))
       (error "string-index-right" "index out of bound" start))
      ((char? rs)
       (string-char-index s rs))
      ((not (string? rs))
       (error "string-index-right" "Illegal regset" rs))
      ((=fx (string-length rs) 1)
       (string-char-index s (string-ref rs 0)))
      ((<=fx (string-length rs) 10)
       (let ((len (string-length s))
	     (lenj (string-length rs)))
	  (let loop ((i (-fx start 1)))
	     (if (<fx i 0)
		 #f
		 (let ((c (string-ref s i)))
		    (let liip ((j 0))
		       (if (=fx j lenj)
			   (loop (-fx i 1))
			   (if (char=? c (string-ref-ur rs j))
			       i
			       (liip (+fx j 1))))))))))
      (else
       (let ((t (make-string 256 #\n))
	     (len (string-length s)))
	  (let loop ((i (-fx (string-length rs) 1)))
	     (if (=fx i -1)
		 (let liip ((i (-fx start 1)))
		    (cond
		       ((<fx i 0)
			#f)
		       ((char=? (string-ref
				 t (char->integer (string-ref-ur s i)))
				#\y)
			i)
		       (else
			(liip (-fx i 1)))))
		 (begin
		    (string-set-ur! t (char->integer (string-ref-ur rs i)) #\y)
		    (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    string-skip ...                                                  */
;*---------------------------------------------------------------------*/
(define (string-skip string rs #!optional (start 0))
   
   (define (string-char-skip s c)
      (let ((len (string-length s)))
	 (let loop ((i start))
	    (cond
	       ((>=fx i len)
		#f)
	       ((char=? (string-ref-ur s i) c)
		(loop (+fx i 1)))
	       (else
		i)))))
   (define (string-pred-skip s pred)
      (let ((len (string-length s)))
	 (let loop ((i start))
	    (cond
	       ((>=fx i len)
		#f)
	       ((pred (string-ref-ur s i))
		(loop (+fx i 1)))
	       (else
		i)))))
   
   (cond
      ((char? rs)
       (string-char-skip string rs))
      ((procedure? rs)
       (string-pred-skip string rs))
      ((not (string? rs))
       (error "string-skip" "Illegal regset" rs))
      ((=fx (string-length rs) 1)
       (string-char-skip string (string-ref rs 0)))
      ((<=fx (string-length rs) 10)
       (let ((len (string-length string))
	     (lenj (string-length rs)))
	  (let loop ((i start))
	     (if (>=fx i len)
		 #f
		 (let ((c (string-ref string i)))
		    (let liip ((j 0))
		       (if (=fx j lenj)
			   i
			   (if (char=? c (string-ref-ur rs j))
			       (loop (+fx i 1))
			       (liip (+fx j 1))))))))))
      (else
       (let ((t (make-string 256 #\n))
	     (len (string-length string)))
	  (let loop ((i (-fx (string-length rs) 1)))
	     (if (=fx i -1)
		 (let liip ((i start))
		    (cond
		       ((>=fx i len)
			#f)
		       ((char=? (string-ref
				 t (char->integer (string-ref-ur string i)))
				#\y)
			(liip (+fx i 1)))
		       (else
			i)))
		 (begin
		    (string-set-ur! t (char->integer (string-ref-ur rs i)) #\y)
		    (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    string-skip-right ...                                            */
;*---------------------------------------------------------------------*/
(define (string-skip-right s rs #!optional (start (string-length s)))
   
   (define (string-char-skip s c)
      (let loop ((i (-fx start 1)))
	 (cond
	    ((<fx i 0)
	     #f)
	    ((char=? (string-ref-ur s i) c)
	     (loop (-fx i 1)))
	    (else
	     i))))
   
   (define (string-pred-skip s pred)
      (let loop ((i (-fx start 1)))
	 (cond
	    ((<fx i 0)
	     #f)
	    ((pred (string-ref-ur s i))
	     (loop (-fx i 1)))
	    (else
	     i))))
   
   (cond
      ((>fx start (string-length s))
       (error "string-index" "index out of bound" start))
      ((char? rs)
       (string-char-skip s rs))
      ((procedure? rs)
       (string-pred-skip s rs))
      ((not (string? rs))
       (error "string-index-right" "Illegal regset" rs))
      ((=fx (string-length rs) 1)
       (string-char-skip s (string-ref rs 0)))
      ((<=fx (string-length rs) 10)
       (let ((len (string-length s))
	     (lenj (string-length rs)))
	  (let loop ((i (-fx start 1)))
	     (if (<fx i 0)
		 #f
		 (let ((c (string-ref s i)))
		    (let liip ((j 0))
		       (if (=fx j lenj)
			   i
			   (if (char=? c (string-ref-ur rs j))
			       (loop (-fx i 1))
			       (liip (+fx j 1))))))))))
      (else
       (let ((t (make-string 256 #\n))
	     (len (string-length s)))
	  (let loop ((i (-fx (string-length rs) 1)))
	     (if (=fx i -1)
		 (let liip ((i (-fx start 1)))
		    (cond
		       ((<fx i 0)
			#f)
		       ((char=? (string-ref
				 t (char->integer (string-ref-ur s i)))
				#\y)
			(liip (-fx i 1)))
		       (else
			i)))
		 (begin
		    (string-set-ur! t (char->integer (string-ref-ur rs i)) #\y)
		    (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    user-start-index ...                                             */
;*---------------------------------------------------------------------*/
(define (user-start-index proc id i max default)
   (cond
      ((not i)
       default)
      ((<fx i 0)
       (error proc (string-append "Index negative start index `" id "'") i))
      ((>=fx i max)
       (error proc (string-append "Too large start index `" id "'") i))
      (else
       i)))

;*---------------------------------------------------------------------*/
;*    user-end-index ...                                               */
;*---------------------------------------------------------------------*/
(define (user-end-index proc id i max default)
   (cond
      ((not i)
       default)
      ((<=fx i 0)
       (error proc (string-append "Index negative end index `" id "'") i))
      ((>fx i max)
       (error proc (string-append "Too large end index `" id "'") i))
      (else
       i)))

;*---------------------------------------------------------------------*/
;*    string-prefix-length ...                                         */
;*---------------------------------------------------------------------*/
(define (string-prefix-length::int s1::bstring s2::bstring
				   #!optional start1 end1 start2 end2)
   (let* ((l1 (string-length s1))
	  (l2 (string-length s2))
	  (e1 (user-end-index 'string-prefix-length "end1" end1 l1 l1))
	  (e2 (user-end-index 'string-prefix-length "end2" end2 l2 l2))
	  (b1 (user-start-index 'string-prefix-length "start1" start1 l1 0))
	  (b2 (user-start-index 'string-prefix-length "start2" start2 l2 0)))
      (let loop ((i1 b1)
		 (i2 b2))
	 (cond
	    ((or (=fx i1 e1) (=fx i2 e2))
	     (-fx i1 b1))
	    ((char=? (string-ref s1 i1) (string-ref s2 i2))
	     (loop (+fx i1 1) (+fx i2 1)))
	    (else
	     (-fx i1 b1))))))
	 
;*---------------------------------------------------------------------*/
;*    string-prefix-length-ci ...                                      */
;*---------------------------------------------------------------------*/
(define (string-prefix-length-ci::int s1::bstring s2::bstring
				      #!optional start1 end1 start2 end2)
   (let* ((l1 (string-length s1))
	  (l2 (string-length s2))
	  (e1 (user-end-index 'string-prefix-length-ci "end1" end1 l1 l1))
	  (e2 (user-end-index 'string-prefix-length-ci "end2" end2 l2 l2))
	  (b1 (user-start-index 'string-prefix-length-ci "start1" start1 l1 0))
	  (b2 (user-start-index 'string-prefix-length-ci "start2" start2 l2 0)))
      (let loop ((i1 b1)
		 (i2 b2))
	 (cond
	    ((or (=fx i1 e1) (=fx i2 e2))
	     (-fx i1 b1))
	    ((char-ci=? (string-ref s1 i1) (string-ref s2 i2))
	     (loop (+fx i1 1) (+fx i2 1)))
	    (else
	     (-fx i1 b1))))))
	 
;*---------------------------------------------------------------------*/
;*    string-suffix-length ...                                         */
;*---------------------------------------------------------------------*/
(define (string-suffix-length::int s1::bstring s2::bstring
				   #!optional start1 end1 start2 end2)
   (let* ((l1 (string-length s1))
	  (l2 (string-length s2))
	  (b1 (user-end-index 'string-suffix-length "end1" end1 l1 l1))
	  (b2 (user-end-index 'string-suffix-length "end2" end2 l2 l2))
	  (e1 (user-start-index 'string-suffix-length "start1" start1 l1 0))
	  (e2 (user-start-index 'string-suffix-length "start2" start2 l2 0)))
      (let loop ((i1 (-fx b1 1))
		 (i2 (-fx b2 1)))
	 (cond
	    ((or (<fx i1 e1) (<fx i2 e2))
	     (-fx b1 (+fx i1 1)))
	    ((char=? (string-ref s1 i1) (string-ref s2 i2))
	     (loop (-fx i1 1) (-fx i2 1)))
	    (else
	     (-fx b1 (+fx i1 1)))))))

;*---------------------------------------------------------------------*/
;*    string-suffix-length-ci ...                                      */
;*---------------------------------------------------------------------*/
(define (string-suffix-length-ci::int s1::bstring s2::bstring
				      #!optional start1 end1 start2 end2)
   (let* ((l1 (string-length s1))
	  (l2 (string-length s2))
	  (b1 (user-end-index 'string-suffix-length-ci "end1" end1 l1 l1))
	  (b2 (user-end-index 'string-suffix-length-ci "end2" end2 l2 l2))
	  (e1 (user-start-index 'string-suffix-length-ci "start1" start1 l1 0))
	  (e2 (user-start-index 'string-suffix-length-ci "start2" start2 l2 0)))
      (let loop ((i1 (-fx b1 1))
		 (i2 (-fx b2 1)))
	 (cond
	    ((or (<fx i1 e1) (<fx i2 e2))
	     (-fx b1 (+fx i1 1)))
	    ((char-ci=? (string-ref s1 i1) (string-ref s2 i2))
	     (loop (-fx i1 1) (-fx i2 1)))
	    (else
	     (-fx b1 (+fx i1 1)))))))

;*---------------------------------------------------------------------*/
;*    string-prefix? ...                                               */
;*---------------------------------------------------------------------*/
(define (string-prefix?::bool s1::bstring s2::bstring
			      #!optional start1 end1 start2 end2)
   (let* ((l1 (string-length s1))
	  (l2 (string-length s2))
	  (e1 (user-end-index 'string-prefix? "end1" end1 l1 l1))
	  (e2 (user-end-index 'string-prefix? "end2" end2 l2 l2))
	  (b1 (user-start-index 'string-prefix? "start1" start1 l1 0))
	  (b2 (user-start-index 'string-prefix? "start2" start2 l2 0)))
      (let loop ((i1 b1)
		 (i2 b2))
	 (cond
	    ((=fx i1 e1)
	     #t)
	    ((=fx i2 e2)
	     #f)
	    ((char=? (string-ref s1 i1) (string-ref s2 i2))
	     (loop (+fx i1 1) (+fx i2 1)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    string-prefix-ci? ...                                            */
;*---------------------------------------------------------------------*/
(define (string-prefix-ci?::bool s1::bstring s2::bstring
				 #!optional start1 end1 start2 end2)
   (let* ((l1 (string-length s1))
	  (l2 (string-length s2))
	  (e1 (user-end-index 'string-prefix-ci? "end1" end1 l1 l1))
	  (e2 (user-end-index 'string-prefix-ci? "end2" end2 l2 l2))
	  (b1 (user-start-index 'string-prefix-ci? "start1" start1 l1 0))
	  (b2 (user-start-index 'string-prefix-ci? "start2" start2 l2 0)))
      (let loop ((i1 b1)
		 (i2 b2))
	 (cond
	    ((=fx i1 e1)
	     #t)
	    ((=fx i2 e2)
	     #f)
	    ((char-ci=? (string-ref s1 i1) (string-ref s2 i2))
	     (loop (+fx i1 1) (+fx i2 1)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    string-suffix? ...                                               */
;*---------------------------------------------------------------------*/
(define (string-suffix?::bool s1::bstring s2::bstring
			      #!optional start1 end1 start2 end2)
   (let* ((l1 (string-length s1))
	  (l2 (string-length s2))
	  (b1 (user-end-index 'string-suffix? "end1" end1 l1 l1))
	  (b2 (user-end-index 'string-suffix? "end2" end2 l2 l2))
	  (e1 (user-start-index 'string-suffix? "start1" start1 l1 0))
	  (e2 (user-start-index 'string-suffix? "start2" start2 l2 0)))
      (let loop ((i1 (-fx b1 1))
		 (i2 (-fx b2 1)))
	 (cond
	    ((<fx i1 e1)
	     #t)
	    ((<fx i2 e2)
	     #f)
	    ((char=? (string-ref s1 i1) (string-ref s2 i2))
	     (loop (-fx i1 1) (-fx i2 1)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    string-suffix-ci? ...                                            */
;*---------------------------------------------------------------------*/
(define (string-suffix-ci?::bool s1::bstring s2::bstring
				 #!optional start1 end1 start2 end2)
   (let* ((l1 (string-length s1))
	  (l2 (string-length s2))
	  (b1 (user-end-index 'string-prefix-length "end1" end1 l1 l1))
	  (b2 (user-end-index 'string-prefix-length "end2" end2 l2 l2))
	  (e1 (user-start-index 'string-prefix-length "start1" start1 l1 0))
	  (e2 (user-start-index 'string-prefix-length "start2" start2 l2 0)))
      (let loop ((i1 (-fx b1 1))
		 (i2 (-fx b2 1)))
	 (cond
	    ((<fx i1 e1)
	     #t)
	    ((<fx i2 e2)
	     #f)
	    ((char-ci=? (string-ref s1 i1) (string-ref s2 i2))
	     (loop (-fx i1 1) (-fx i2 1)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    string-natural-compare3 ...                                      */
;*---------------------------------------------------------------------*/
(define (string-natural-compare3 a b #!optional (start1 0) (start2 0))
   (strnatcmp a b #f start1 start2))

;*---------------------------------------------------------------------*/
;*    string-natural-compare3-ci ...                                   */
;*---------------------------------------------------------------------*/
(define (string-natural-compare3-ci a b #!optional (start1 0) (start2 0))
   (strnatcmp a b #t start1 start2))

;*---------------------------------------------------------------------*/
;*    strnatcmp ...                                                    */
;*---------------------------------------------------------------------*/
(define (strnatcmp a b foldcase start1 start2)
   (let loop ((ia start1)
	      (ib start2))
      (let ((ca (char-at a ia))
	    (cb (char-at b ib)))
	 (let while ()
	    (when (char-whitespace? ca)
	       (set! ia (+fx ia 1))
	       (set! ca (char-at a ia))))
	 (let while ()
	    (when (char-whitespace? cb)
	       (set! ib (+fx ib 1))
	       (set! cb (char-at b ib))))
	 (cond
	    ((and (char-numeric? ca) (char-numeric? cb))
	     (if (and (char=? ca #\0) (char=? cb #\0))
		 (loop (+fx ia 1) (+fx ib 1))
		 (let ((result (if (or (char=? ca #\0) (char=? cb #\0))
				   (compare-left a ia b ib)
				   (compare-right a ia b ib))))
		    (cond
		       ((fixnum? result)
			(loop (+fx ia result) (+fx ib result)))
		       (result
			+1)
		       (else
			-1)))))
	    ((and (char=? ca #a000) (char=? cb #a000))
	     0)
	    ((and foldcase
		  (begin (set! ca (char-upcase ca))
			 (set! cb (char-upcase cb))
			 #f))
	     'an-awful-hack)
	    ((char<? ca cb)
	     -1)
	    ((char>? ca cb)
	     +1)
	    (else
	     (loop (+fx ia 1) (+fx ib 1)))))))

;*---------------------------------------------------------------------*/
;*    compare-right ...                                                */
;*---------------------------------------------------------------------*/
(define (compare-right a ia b ib)
   (let loop ((bias #unspecified)
	      (i 0))
      (let ((ca (char-at a (+fx i ia)))
	    (cb (char-at b (+fx i ib))))
	 (cond
	    ((and (not (char-numeric? ca)) (not (char-numeric? cb)))
	     (if (eq? bias #unspecified) i bias))
	    ((not (char-numeric? ca))
	     #f)
	    ((not (char-numeric? cb))
	     #t)
	    ((char<? ca cb)
	     (loop (if (eq? bias #unspecified) #f bias) (+fx i 1)))
	    ((char>? ca cb)
	     (loop (if (eq? bias #unspecified) #t bias) (+fx i 1)))
	    ((and (char=? ca #a000) (char=? cb #a000))
	     (if (eq? bias #unspecified) i bias))
	    (else
	     (loop bias (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    compare-left ...                                                 */
;*---------------------------------------------------------------------*/
(define (compare-left a ia b ib)
   (let loop ((i 0))
      (let ((ca (char-at a (+fx ia i)))
	    (cb (char-at b (+fx ib i))))
	 (cond
	    ((and (not (char-numeric? ca)) (not (char-numeric? cb)))
	     i)
	    ((not (char-numeric? ca))
	     #f)
	    ((not (char-numeric? cb))
	     #t)
	    ((char<? ca cb)
	     #f)
	    ((char>? ca cb)
	     #t)
	    (else
	     (loop (+fx i 1)))))))
	     
;*---------------------------------------------------------------------*/
;*    char-at ...                                                      */
;*---------------------------------------------------------------------*/
(define (char-at s i)
   (if (>=fx i (string-length s))
       #a000
       (string-ref-ur s i)))

;*---------------------------------------------------------------------*/
;*    hex-string-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (hex-string-ref str i)
   (let ((n (string-ref-ur str i)))
      (cond
	 ((and (char>=? n #\0) (char<=? n #\9))
	  (-fx (char->integer n) (char->integer #\0)))
	 ((and (char>=? n #\a) (char<=? n #\f))
	  (+fx 10 (-fx (char->integer n) (char->integer #\a))))
	 ((and (char>=? n #\A) (char<=? n #\F))
	  (+fx 10 (-fx (char->integer n) (char->integer #\A))))
	 (else
	  (error "hex-string->string"
		 "Illegal string (illegal character)"
		 str)))))

;*---------------------------------------------------------------------*/
;*    string-hex-intern ...                                            */
;*---------------------------------------------------------------------*/
(define (string-hex-intern str)
   (let ((len (string-length str)))
      (if (oddfx? len)
	  (error "string-hex-intern" "Illegal string (length is odd)" str)
	  (let ((res (make-string (/fx len 2))))
	     (let loop ((i 0)
			(j 0))
		(if (=fx i len)
		    res
		    (let* ((c1 (hex-string-ref str i))
			   (c2 (hex-string-ref str (+fx i 1)))
			   (c (+fx (*fx c1 16) c2)))
		       (string-set! res j (integer->char c))
		       (loop (+fx i 2) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    string-hex-intern! ...                                           */
;*---------------------------------------------------------------------*/
(define (string-hex-intern! str)
   (let ((len (string-length str)))
      (if (oddfx? len)
	  (error "string-hex-intern!" "Illegal string (length is odd)" str)
	  (let loop ((i 0)
		     (j 0))
	     (if (=fx i len)
		 (string-shrink! str (/fx len 2))
		 (let* ((c1 (hex-string-ref str i))
			(c2 (hex-string-ref str (+fx i 1)))
			(c (+fx (*fx c1 16) c2)))
		    (string-set! str j (integer->char c))
		    (loop (+fx i 2) (+fx j 1))))))))

;*---------------------------------------------------------------------*/
;*    string-hex-extern ...                                            */
;*---------------------------------------------------------------------*/
(define (string-hex-extern::bstring str::bstring #!optional
	   (start::int 0) (end::long (string-length str)))
   
   (define (integer->hex n) (string-ref "0123456789abcdef" n))
   
   (let ((len (string-length str)))
      (cond
	 ((or (<fx start 0) (>fx start len))
	  (error "string-hex-extern"
	     (string-append "Illegal start index " (fixnum->string start))
	     (list (string-length str) str)))
	 ((or (<fx end start) (>fx end len))
	  (error "string-hex-extern"
	     (string-append "Illegal end index " (fixnum->string end))
	     (list (string-length str) str)))
	 (else
	  (let ((res (make-string (*fx 2 (-fx end start)))))
	     (let loop ((i start)
			(j 0))
		(if (=fx i end)
		    res
		    (let* ((n (char->integer (string-ref str i)))
			   (d0 (remainderfx n 16))
			   (d1 (/fx n 16)))
		       (string-set! res j (integer->hex d1))
		       (string-set! res (+fx j 1) (integer->hex d0))
		       (loop (+fx i 1) (+fx j 2))))))))))
