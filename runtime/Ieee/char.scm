;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Ieee/char.scm                */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 21 10:09:50 1992                          */
;*    Last change :  Fri Mar 11 17:38:53 2016 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.6 Characters (page 24, r4)                                     */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __r4_characters_6_6
   
   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    __r4_equivalence_6_2
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_vectors_6_8
	    __r4_strings_6_7
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    
	    __evenv)
   
   (extern  (macro c-char?::bool (::obj) "CHARP")
	    (macro c-char-upcase::uchar (::uchar) "toupper")
	    (macro c-char-downcase::uchar (::uchar) "tolower")
	    (infix macro c-char=?::bool (::uchar ::uchar) "==")
	    (infix macro c-char<?::bool (::uchar ::uchar) "<")
	    (infix macro c-char>?::bool (::uchar ::uchar) ">")
	    (infix macro c-char<=?::bool (::uchar ::uchar) "<=")
	    (infix macro c-char>=?::bool (::uchar ::uchar) ">=")
	    (macro c-char-alphabetic?::bool (::uchar) "isalpha")
	    (macro c-char-lower-case?::bool (::uchar) "islower")
	    (macro c-char-numeric?::bool (::uchar) "isdigit")
	    (macro c-char-upper-case?::bool (::uchar) "isupper")
	    (macro c-char-whitespace?::bool (::uchar) "isspace")
	    (macro c-char->integer::long (::uchar) "")
	    (macro c-integer->char::uchar (::long) "")
	    (infix macro c-char-or::uchar (::uchar ::uchar) "|")
	    (infix macro c-char-and::uchar (::uchar ::uchar) "&")
	    (macro c-char-not::uchar (::uchar) "~"))
   
   (java    (class foreign
	       (method static c-char?::bool (::obj) "CHARP")
	       (method static c-char-upcase::uchar (::uchar) "toupper")
	       (method static c-char-downcase::uchar (::uchar) "tolower")
	       (method static c-char=?::bool (::uchar ::uchar) "CHAR_EQ")
	       (method static c-char<?::bool (::uchar ::uchar) "CHAR_LT")
	       (method static c-char>?::bool (::uchar ::uchar) "CHAR_GT")
	       (method static c-char<=?::bool (::uchar ::uchar) "CHAR_LE")
	       (method static c-char>=?::bool (::uchar ::uchar) "CHAR_GE")
	       (method static c-char->integer::long  (::uchar) "CHAR_TO_INT")
	       (method static c-integer->char::uchar (::long) "INT_TO_CHAR")
	       (method static c-char-or::uchar (::uchar ::uchar) "CHAR_OR")
	       (method static c-char-and::uchar (::uchar ::uchar) "CHAR_AND")
	       (method static c-char-not::uchar (::uchar) "CHAR_NOT")))
   
   (export  (inline char?::bool             ::obj)
	    (inline char=?::bool            ::uchar ::uchar)
	    (inline char<?::bool            ::uchar ::uchar)
	    (inline char>?::bool            ::uchar ::uchar)
	    (inline char<=?::bool           ::uchar ::uchar)
	    (inline char>=?::bool           ::uchar ::uchar) 
	    (inline char-ci=?::bool         ::uchar ::uchar)
	    (inline char-ci<?::bool         ::uchar ::uchar)
	    (inline char-ci>?::bool         ::uchar ::uchar)
	    (inline char-ci<=?::bool        ::uchar ::uchar)
	    (inline char-ci>=?::bool        ::uchar ::uchar)
	    (inline char-alphabetic?::bool  ::uchar)
	    (inline char-numeric?::bool     ::uchar)
	    (inline char-whitespace?::bool  ::uchar)
	    (inline char-upper-case?::bool  ::uchar)
	    (inline char-lower-case?::bool  ::uchar)
	    (inline char->integer::long     ::uchar)
	    (integer->char::uchar           ::long)
	    (inline integer->char-ur::uchar ::long)
	    (inline char-upcase::uchar      ::uchar)
	    (inline char-downcase::uchar    ::uchar)
	    (inline char-or::uchar          ::uchar ::uchar)
	    (inline char-and::uchar         ::uchar ::uchar)
	    (inline char-not::uchar         ::uchar))
   
   (pragma  (c-char? no-alloc (predicate-of bchar) no-cfa-top nesting)
	    (char? no-alloc side-effect-free no-cfa-top nesting)
	    (c-char-upcase side-effect-free nesting args-safe)
	    (c-char-downcase side-effect-free nesting args-safe)
	    (c-char=? side-effect-free nesting args-safe)
	    (c-char<? side-effect-free nesting args-safe)
	    (c-char>? side-effect-free nesting args-safe)
	    (c-char<=? side-effect-free nesting args-safe)
	    (c-char>=? side-effect-free nesting args-safe)
	    (c-char->integer side-effect-free nesting args-safe)
	    (c-integer->char side-effect-free nesting args-safe)
	    (c-char-or side-effect-free nesting args-safe)
	    (c-char-and side-effect-free nesting args-safe)
	    (c-char-not side-effect-free nesting args-safe)
	    (char=? side-effect-free nesting)
	    (char<? side-effect-free nesting)
	    (char>? side-effect-free nesting)
	    (char<=? side-effect-free nesting)
	    (char>=? side-effect-free nesting)
	    (char-ci=? side-effect-free nesting)
	    (char-ci<? side-effect-free nesting)
	    (char-ci>? side-effect-free nesting)
	    (char-ci<=? side-effect-free nesting)
	    (char-ci>=? side-effect-free nesting)
	    (char-alphabetic? side-effect-free nesting)
	    (char-numeric? side-effect-free nesting)
	    (char-whitespace? side-effect-free nesting)
	    (char-upper-case? side-effect-free nesting)
	    (char-lower-case? side-effect-free nesting)
	    (char->integer side-effect-free nesting)
	    (integer->char side-effect-free nesting)
	    (integer->char-ur side-effect-free nesting)
	    (char-upcase side-effect-free nesting)
	    (char-downcase side-effect-free nesting)))
 
;*---------------------------------------------------------------------*/
;*    char? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (char? obj)
   (c-char? obj))

;*---------------------------------------------------------------------*/
;*    char=? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (char=? char1 char2)
   (c-char=? char1 char2))

;*---------------------------------------------------------------------*/
;*    char<? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (char<? char1 char2)
   (c-char<? char1 char2))

;*---------------------------------------------------------------------*/
;*    char>? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline  (char>? char1 char2)
   (c-char>? char1 char2))

;*---------------------------------------------------------------------*/
;*    char<=? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (char<=? char1 char2)
   (c-char<=? char1 char2))

;*---------------------------------------------------------------------*/
;*    char>=? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (char>=? char1 char2)
   (c-char>=? char1 char2))

;*---------------------------------------------------------------------*/
;*    char-ci=? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (char-ci=? char1 char2)
   (char=? (char-upcase char1) (char-upcase char2)))

;*---------------------------------------------------------------------*/
;*    char-ci<? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (char-ci<? char1 char2)
   (c-char<? (char-upcase char1) (char-upcase char2))) 

;*---------------------------------------------------------------------*/
;*    char-ci>? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline  (char-ci>? char1 char2)
   (c-char>? (char-upcase char1) (char-upcase char2)))

;*---------------------------------------------------------------------*/
;*    char-ci<=? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (char-ci<=? char1 char2)
   (c-char<=? (char-upcase char1) (char-upcase char2)))

;*---------------------------------------------------------------------*/
;*    char-ci>=? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (char-ci>=? char1 char2)
   (c-char>=? (char-upcase char1) (char-upcase char2)))

;*---------------------------------------------------------------------*/
;*    char-alphabetic? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (char-alphabetic? char)
   (cond-expand
      (bigloo-c (c-char-alphabetic? char))
      (else (or (and (char>=? char #\A)
		     (char<=? char #\Z))
		(and (char>=? char #\a)
		     (char<=? char #\z))))))

;*---------------------------------------------------------------------*/
;*    char-numeric? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (char-numeric? char)
   (cond-expand
      (bigloo-c (c-char-numeric? char))
      (else (if (char>=? char #\0)
		(char<=? char #\9)
		#f))))

;*---------------------------------------------------------------------*/
;*    char-withespace? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (char-whitespace? char)
   (cond-expand
      (bigloo-c (c-char-whitespace? char))
      (else (or (char=? char #\space)
		(char=? char #\tab)
		(char=? char #\return)
		(char=? char #\Newline)))))

;*---------------------------------------------------------------------*/
;*    char-upper-case? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (char-upper-case? char)
   (cond-expand
      (bigloo-c (c-char-upper-case? char))
      (else (if (char>=? char #\A)
		(char<=? char #\Z)
		#f))))

;*---------------------------------------------------------------------*/
;*    char-lower-case? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (char-lower-case? char)
   (cond-expand
      (bigloo-c (c-char-lower-case? char))
      (else (if (char>=? char #\a)
		(char<=? char #\z)
		#f))))

;*---------------------------------------------------------------------*/
;*    char->integer ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (char->integer char)
   (c-char->integer char))

;*---------------------------------------------------------------------*/
;*    integer->char ...                                                */
;*---------------------------------------------------------------------*/
(define (integer->char int)
   (if (and (>=fx int 0) (<=fx int 255))
       (c-integer->char int)
       (error "integer->char" "integer out of range" int)))

;*---------------------------------------------------------------------*/
;*    integer->char-ur ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (integer->char-ur int)
   (c-integer->char int))

;*---------------------------------------------------------------------*/
;*    char-upcase ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline  (char-upcase char)
   (c-char-upcase char))

;*---------------------------------------------------------------------*/
;*    char-downcase ...                                                */
;*---------------------------------------------------------------------*/
(define-inline  (char-downcase char)
   (c-char-downcase char))
		     
;*---------------------------------------------------------------------*/
;*    char-or ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (char-or char1 char2)
   (c-char-or char1 char2))

;*---------------------------------------------------------------------*/
;*    char-and ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (char-and char1 char2)
   (c-char-and char1 char2))

;*---------------------------------------------------------------------*/
;*    char-not ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (char-not char1)
   (c-char-not char1))
