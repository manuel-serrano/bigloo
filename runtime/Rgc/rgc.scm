;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Rgc/rgc.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 13 10:56:28 1998                          */
;*    Last change :  Sun Aug 25 09:12:22 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The runtime module of the Bigloo regular expression system.      */
;*    -------------------------------------------------------------    */
;*    For this module we have to use Scheme wrapper otherwise regular  */
;*    grammar may not be used within EVAL.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc
   
   (import  __error)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __object
	    __param
	    __thread
	    __bexit
	    __bignum
	    __bit
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r5_control_features_6_4
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r4_vectors_6_8)

   (extern  (macro $rgc-buffer-get-char::int (::input-port ::long)
		   "RGC_BUFFER_GET_CHAR")
	    (macro $rgc-buffer-length::long (::input-port)
		   "RGC_BUFFER_MATCH_LENGTH")
	    (macro $rgc-set-filepos!::long (::input-port)
		   "RGC_SET_FILEPOS")
	    (macro $rgc-start-match!::long (::input-port)
		   "RGC_START_MATCH")
	    (macro $rgc-stop-match!::long (::input-port ::long)
		   "RGC_STOP_MATCH")
 	    (macro $rgc-buffer-position::long (::input-port ::long)
		   "RGC_BUFFER_POSITION")
	    (macro $rgc-buffer-forward::long (::input-port)
		   "RGC_BUFFER_FORWARD")
	    (macro $rgc-buffer-bufpos::long (::input-port)
		   "RGC_BUFFER_BUFPOS")
	    (macro $rgc-buffer-character::char (::input-port)
		   "RGC_BUFFER_CHARACTER")
	    (macro $rgc-buffer-byte::int (::input-port)
		   "RGC_BUFFER_BYTE")
	    (macro $rgc-buffer-byte-ref::int (::input-port ::int)
		   "RGC_BUFFER_BYTE_REF")
	    ($rgc-buffer-unget-char::int (::input-port ::int)
	       "rgc_buffer_unget_char")
	    ($rgc-buffer-insert-substring::bool (::input-port ::bstring ::long ::long)
	       "rgc_buffer_insert_substring")
	    ($rgc-buffer-insert-char::bool (::input-port ::int)
	       "rgc_buffer_insert_char")
	    ($rgc-buffer-substring::bstring (::input-port ::long ::long)
	       "rgc_buffer_substring")
	    ($rgc-buffer-escape-substring::bstring (::input-port ::long ::long ::bool)
	       "rgc_buffer_escape_substring")
	    ($rgc-buffer-fixnum::long (::input-port)
	       "rgc_buffer_fixnum")
	    ($rgc-buffer-integer::obj (::input-port)
	       "rgc_buffer_integer")
	    ($rgc-buffer-flonum::double (::input-port)
	       "rgc_buffer_flonum")
	    ($rgc-buffer-symbol::symbol (::input-port)
	       "rgc_buffer_symbol")
	    ($rgc-buffer-subsymbol::symbol (::input-port ::long ::long)
	       "rgc_buffer_subsymbol")
	    ($rgc-buffer-downcase-subsymbol::symbol (::input-port ::long ::long)
	       "rgc_buffer_downcase_subsymbol")
 	    ($rgc-buffer-upcase-subsymbol::symbol (::input-port ::long ::long)
	       "rgc_buffer_upcase_subsymbol")
	    ($rgc-buffer-downcase-keyword::keyword (::input-port)
	       "rgc_buffer_downcase_keyword")
	    ($rgc-buffer-upcase-keyword::keyword (::input-port)
	       "rgc_buffer_upcase_keyword")
	    ($rgc-buffer-keyword::keyword (::input-port)
	       "rgc_buffer_keyword")
	    ($rgc-fill-buffer::bool (::input-port)
	       "rgc_fill_buffer")
	    ($rgc-buffer-bol?::bool (::input-port)
	       "rgc_buffer_bol_p")
	    ($rgc-buffer-eol?::bool (::input-port ::long ::long)
	       "rgc_buffer_eol_p")
	    ($rgc-buffer-bof?::bool (::input-port)
	       "rgc_buffer_bof_p")
	    ($rgc-buffer-eof?::bool (::input-port)
	       "rgc_buffer_eof_p")
	    ($rgc-buffer-eof2?::bool (::input-port ::long ::long)
	       "rgc_buffer_eof2_p")
	    ($rgc-blit-string!::long (::input-port ::string ::long ::long)
	       "bgl_rgc_blit_string"))

   (java    (class foreign
	       (method static $rgc-buffer-get-char::int (::input-port ::long)
		  "RGC_BUFFER_GET_CHAR")
	       (method static $rgc-buffer-length::long (::input-port)
		  "RGC_BUFFER_MATCH_LENGTH")
	       (method static $rgc-set-filepos!::long (::input-port)
		  "RGC_SET_FILEPOS")
	       (method static $rgc-start-match!::long (::input-port)
		  "RGC_START_MATCH")
	       (method static $rgc-stop-match!::long (::input-port ::long)
		  "RGC_STOP_MATCH")
	       (method static $rgc-buffer-position::long (::input-port ::long)
		  "RGC_BUFFER_POSITION")
	       (method static $rgc-buffer-forward::long (::input-port)
		  "RGC_BUFFER_FORWARD")
	       (method static $rgc-buffer-bufpos::long (::input-port)
		  "RGC_BUFFER_BUFPOS")
	       (method static $rgc-buffer-unget-char::int (::input-port ::int)
		  "rgc_buffer_unget_char")
	       (method static $rgc-buffer-insert-substring::bool (::input-port ::bstring ::long ::long)
		  "rgc_buffer_insert_substring")
	       (method static $rgc-buffer-insert-char::bool (::input-port ::int)
		  "rgc_buffer_insert_char")
	       (method static $rgc-buffer-character::char (::input-port)
		  "RGC_BUFFER_CHARACTER")
	       (method static $rgc-buffer-byte::int (::input-port)
		  "RGC_BUFFER_BYTE")
	       (method static $rgc-buffer-byte-ref::int (::input-port ::int)
		  "RGC_BUFFER_BYTE_REF")
	       (method static $rgc-buffer-substring::bstring (::input-port ::long ::long)
		  "rgc_buffer_substring")
	       (method static $rgc-buffer-escape-substring::bstring (::input-port ::long ::long ::bool)
		  "rgc_buffer_escape_substring")
	       (method static $rgc-buffer-fixnum::long (::input-port)
		  "rgc_buffer_fixnum")
	       (method static $rgc-buffer-integer::obj (::input-port)
		  "rgc_buffer_integer")
	       (method static $rgc-buffer-flonum::double (::input-port)
		  "rgc_buffer_flonum")
	       (method static $rgc-buffer-symbol::symbol (::input-port)
		  "rgc_buffer_symbol")
	       (method static $rgc-buffer-subsymbol::symbol (::input-port ::long ::long)
		  "rgc_buffer_subsymbol")
	       (method static $rgc-buffer-upcase-subsymbol::symbol (::input-port ::long ::long)
		  "rgc_buffer_upcase_subsymbol")
	       (method static $rgc-buffer-keyword::keyword (::input-port)
		  "rgc_buffer_keyword")
	       (method static $rgc-buffer-downcase-subsymbol::symbol (::input-port ::long ::long)
		  "rgc_buffer_downcase_subsymbol")
	       (method static $rgc-buffer-upcase-keyword::keyword (::input-port)
		  "rgc_buffer_upcase_keyword")
	       (method static $rgc-buffer-downcase-keyword::keyword (::input-port)
		  "rgc_buffer_downcase_keyword")
	       (method static $rgc-fill-buffer::bool (::input-port)
		  "rgc_fill_buffer")
	       (method static $rgc-buffer-bol?::bool (::input-port)
		  "rgc_buffer_bol_p")
	       (method static $rgc-buffer-eol?::bool (::input-port ::int ::int)
		  "rgc_buffer_eol_p")
	       (method static $rgc-buffer-bof?::bool (::input-port)
		  "rgc_buffer_bof_p")
	       (method static $rgc-buffer-eof?::bool (::input-port)
		  "rgc_buffer_eof_p")
	       (method static $rgc-buffer-eof2?::bool (::input-port ::int ::int)
		  "rgc_buffer_eof2_p")
	       (method static $rgc-blit-string!::long (::input-port ::string ::long ::long)
		  "bgl_rgc_blit_string")))
 
   (export  *unsafe-rgc*
	    (inline rgc-buffer-get-char::int ::input-port ::long)
	    (inline rgc-buffer-insert-substring!::bool ::input-port str::bstring from::long to::long)
	    (inline rgc-buffer-insert-char!::bool ::input-port ::long)
	    (inline rgc-buffer-unget-char::int ::input-port ::int)
	    (inline rgc-buffer-character::char ::input-port)
	    (inline rgc-buffer-byte::int ::input-port)
	    (inline rgc-buffer-byte-ref::int ::input-port ::int)
	    (inline rgc-buffer-substring::bstring ::input-port ::long ::long)
	    (inline rgc-buffer-escape-substring::bstring ::input-port ::long ::long ::bool)
	    (inline rgc-buffer-length::long ::input-port)
	    (inline rgc-buffer-fixnum::long ::input-port)
	    (inline rgc-buffer-integer::obj ::input-port)
	    (inline rgc-buffer-flonum::double ::input-port)
	    (inline rgc-buffer-symbol::symbol ::input-port)
	    (inline rgc-buffer-subsymbol::symbol ::input-port ::long ::long)
	    (inline rgc-buffer-downcase-symbol::symbol ::input-port)
	    (inline rgc-buffer-downcase-subsymbol::symbol ::input-port ::long ::long)
	    (inline rgc-buffer-upcase-symbol::symbol ::input-port)
	    (inline rgc-buffer-upcase-subsymbol::symbol ::input-port ::long ::long)
	    (inline rgc-buffer-keyword::keyword ::input-port)
	    (inline rgc-buffer-downcase-keyword::keyword ::input-port)
	    (inline rgc-buffer-upcase-keyword::keyword ::input-port)
	    (inline rgc-buffer-position::long ::input-port ::long)
	    (inline rgc-buffer-bufpos::long ::input-port)
	    (inline rgc-buffer-forward::long ::input-port)
	    (inline rgc-set-filepos! ::input-port)
	    (inline rgc-start-match!::long ::input-port)
	    (inline rgc-stop-match!::long ::input-port ::long)
	    (inline rgc-fill-buffer::bool ::input-port)
	    (inline rgc-buffer-bol?::bool ::input-port)
	    (inline rgc-buffer-eol?::bool ::input-port ::long ::long)
	    (inline rgc-buffer-bof?::bool ::input-port)
	    (inline rgc-buffer-eof?::bool ::input-port)
	    (inline rgc-buffer-eof2?::bool ::input-port ::long ::long)
	    (rgc-the-submatch ::obj ::long ::long ::long)))
 
;*---------------------------------------------------------------------*/
;*    *unsafe-rgc* ...                                                 */
;*    -------------------------------------------------------------    */
;*    @label unsafe-rgc@                                               */
;*    -------------------------------------------------------------    */
;*    If that variable is #t then regular parser do not check that     */
;*    there argument is an opened input port. If the variable is false */
;*    then the check is emitted. That variable is used at two          */
;*    different locations.                                             */
;*      - in the library:                                              */
;*          @ref rgcexpand.scm:unsafe-rgc@                             */
;*      - in the compiler:                                             */
;*          @ref ../../comptime/Init/parse-args.scm:unsafe-rgc@        */
;*---------------------------------------------------------------------*/
(define *unsafe-rgc* #f)

;*---------------------------------------------------------------------*/
;*    rgc-buffer-get-char ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-get-char input-port index)
   ($rgc-buffer-get-char input-port index))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-unget-char ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-unget-char input-port char)
   ($rgc-buffer-unget-char input-port char))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-insert-substring! ...                                 */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-insert-substring! input-port str from to)
   ($rgc-buffer-insert-substring input-port str from to))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-insert-char ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-insert-char! input-port char)
   ($rgc-buffer-insert-char input-port char))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-character ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-character input-port)
   ($rgc-buffer-character input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-byte ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-byte input-port)
   ($rgc-buffer-byte input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-byte-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-byte-ref input-port offset)
   ($rgc-buffer-byte-ref input-port offset))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-substring ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-substring input-port start stop)
   ($rgc-buffer-substring input-port start stop))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-escape-substring ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-escape-substring input-port start stop strict)
   ($rgc-buffer-escape-substring input-port start stop strict))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-length ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-length::long input-port::input-port)
   ($rgc-buffer-length input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-fixnum ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-fixnum::long input-port::input-port)
   ($rgc-buffer-fixnum input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-integer ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-integer::obj input-port::input-port)
   ($rgc-buffer-integer input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-flonum ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-flonum::double input-port::input-port)
   ($rgc-buffer-flonum input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-symbol ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-symbol::symbol input-port::input-port)
   ($rgc-buffer-symbol input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-subsymbol ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-subsymbol input-port start stop)
   ($rgc-buffer-subsymbol input-port start stop))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-downcase-symbol ...                                   */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-downcase-symbol::symbol input-port::input-port)
   ($rgc-buffer-downcase-subsymbol input-port 0 (rgc-buffer-length input-port)))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-downcase-subsymbol ...                                */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-downcase-subsymbol input-port::input-port start stop)
   ($rgc-buffer-downcase-subsymbol input-port start stop))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-upcase-symbol ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-upcase-symbol::symbol input-port::input-port)
   ($rgc-buffer-upcase-subsymbol input-port 0 (rgc-buffer-length input-port)))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-upcase-subsymbol ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-upcase-subsymbol input-port::input-port start stop)
   ($rgc-buffer-upcase-subsymbol input-port start stop))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-keyword ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-keyword::keyword input-port::input-port)
   ($rgc-buffer-keyword input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-downcase-keyword ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-downcase-keyword::keyword input-port::input-port)
   ($rgc-buffer-downcase-keyword input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-upcase-keyword ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-upcase-keyword::keyword input-port::input-port)
   ($rgc-buffer-upcase-keyword input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-position ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-position::long input-port::input-port forward)
   ($rgc-buffer-position input-port forward))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-forward ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-forward::long input-port::input-port)
   ($rgc-buffer-forward input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-bufpos ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-bufpos::long input-port::input-port)
   ($rgc-buffer-bufpos input-port))

;*---------------------------------------------------------------------*/
;*    rgc-set-filepos! ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (rgc-set-filepos! input-port::input-port)
   ($rgc-set-filepos! input-port))

;*---------------------------------------------------------------------*/
;*    rgc-start-match! ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (rgc-start-match! input-port)
   ($rgc-start-match! input-port))

;*---------------------------------------------------------------------*/
;*    rgc-stop-match! ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (rgc-stop-match! input-port forward)
   ($rgc-stop-match! input-port forward))

;*---------------------------------------------------------------------*/
;*    rgc-fill-buffer ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (rgc-fill-buffer::bool input-port::input-port)
   ($rgc-fill-buffer input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-bol? ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-bol?::bool input-port::input-port)
   ($rgc-buffer-bol? input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-eol? ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-eol?::bool input-port::input-port forward bufpos)
   ($rgc-buffer-eol? input-port forward bufpos))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-bof? ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-bof?::bool input-port::input-port)
   ($rgc-buffer-bof? input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-eof? ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-eof?::bool input-port::input-port)
   ($rgc-buffer-eof? input-port))

;*---------------------------------------------------------------------*/
;*    rgc-buffer-eof2? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (rgc-buffer-eof2?::bool input-port::input-port forward bufpos)
   ($rgc-buffer-eof2? input-port forward bufpos))

;*---------------------------------------------------------------------*/
;*    rgc-the-submatch ...                                             */
;*    -------------------------------------------------------------    */
;*    The start position of the submatch has been registered after     */
;*    the character has been removed from the port buffer. This means  */
;*    that we have do decrement the stored start position by 1.        */
;*---------------------------------------------------------------------*/
(define (rgc-the-submatch rgc-submatches pos match submatch)
;*    (print "rgc-the-submatch: " rgc-submatches)                      */
;*    (print "             pos: " pos)                                 */
;*    (print "           match: " match)                               */
;*    (print "        submatch: " submatch)                            */
   (let loop ((submatches rgc-submatches)
	      (start      -1)
	      (stop       -1))
      ;; we search for the first submatch stop
      (if (null? submatches)
	  (values start stop)
	  (let* ((mv   (car submatches))
		 (ru   (vector-ref mv 0))
		 (sm   (vector-ref mv 1))
		 (sp   (vector-ref mv 2))
		 (what (vector-ref mv 3)))
	     (cond
		((and (=fx ru match) (=fx sm submatch) (<=fx sp pos))
		 (case what
		    ((stop)
		     (if (<fx stop 0)
			 (loop (cdr submatches) start sp)
			 (loop (cdr submatches) start stop)))
		    ((start)
		     (values (-fx sp 1) stop))
		    ((start*)
		     (loop (cdr submatches) (-fx sp 1) stop))))
		((eq? what stop)
		 (values start stop))
		(else
		 (loop (cdr submatches) start stop)))))))
			       
   
