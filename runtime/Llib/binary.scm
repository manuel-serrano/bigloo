;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/binary.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  7 10:38:25 1994                          */
;*    Last change :  Sun Aug 25 09:08:39 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Les entrees/sorties compactees des objets Scheme (eventuellement */
;*    circulaires).                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __binary

   (import  __error
	    __intext
	    __param
	    __bexit
	    __object
	    __thread)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bignum
	    __rgc
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
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __evenv)

   (extern  (macro c-binary-port?::bool (::obj) "BINARY_PORTP")
	    
	    ($open-output-binary-file::obj (::bstring)
					    "open_output_binary_file")
	    ($append-output-binary-file::obj (::bstring)
					      "append_output_binary_file")
	    ($open-input-binary-file::obj (::bstring)
					   "open_input_binary_file")
	    ($close-binary-port::obj (::binary-port)
				      "close_binary_port")
	    ($flush-binary-port::obj (::binary-port)
				      "bgl_flush_binary_port")
	    ($input-obj::obj (::binary-port)
			      "input_obj")
	    ($output-obj::obj (::binary-port ::obj)
			       "output_obj")
	    (macro $output-char::obj (::binary-port ::char)
		   "BGL_OUTPUT_CHAR")
	    (macro $output-byte::obj (::binary-port ::byte)
		   "BGL_OUTPUT_CHAR")
	    (macro $input-char::int (::binary-port)
		   "BGL_INPUT_CHAR")
	    ($output-string::int (::binary-port ::bstring)
				  "bgl_output_string")
	    ($input-string::bstring (::binary-port ::int)
				     "bgl_input_string")
	    ($input-fill-string::int (::binary-port ::bstring)
				      "bgl_input_fill_string")
	    (macro $int-eof?::bool (::int)
		   "BGL_INT_EOFP"))

   (java    (class foreign
	       (method static c-binary-port?::bool (::obj)
		       "BINARY_PORTP")
	       (method static $open-output-binary-file::obj (::bstring)
		       "open_output_binary_file")
	       (method static $append-output-binary-file::obj (::bstring)
		       "append_output_binary_file")
	       (method static $open-input-binary-file::obj (::bstring)
		       "open_input_binary_file")
	       (method static $close-binary-port::obj (::binary-port)
		       "close_binary_port")
	       (method static $flush-binary-port::obj (::binary-port)
		       "bgl_flush_binary_port")
	       (method static $input-obj::obj (::binary-port)
		       "input_obj")
	       (method static $output-obj::obj (::binary-port ::obj)
		       "output_obj")
	       (method static $output-char::obj (::binary-port ::char)
		       "BGL_OUTPUT_CHAR")
	       (method static $output-byte::obj (::binary-port ::byte)
		       "BGL_OUTPUT_CHAR")
	       (method static $input-char::int (::binary-port)
		       "BGL_INPUT_CHAR")
	       (method static $output-string::int (::binary-port ::bstring)
		       "bgl_output_string")
	       (method static $input-string::bstring (::binary-port ::int)
		       "bgl_input_string")
	       (method static $input-fill-string::int (::binary-port ::bstring)
		       "bgl_input_fill_string")
	       (method static $int-eof?::bool (::int)
		       "BGL_INT_EOFP")))
   
   (export  (inline binary-port?::bool ::obj)
	    (open-output-binary-file::obj ::bstring)
	    (append-output-binary-file::obj ::bstring)
	    (open-input-binary-file::obj ::bstring)
	    (inline close-binary-port ::binary-port)
	    (inline flush-binary-port ::binary-port)
	    (inline output-obj ::binary-port ::obj)
	    (inline input-obj ::binary-port)
	    (inline output-char ::binary-port ::char)
	    (inline output-byte ::binary-port ::byte)
	    (inline input-char::obj ::binary-port)
	    (inline output-string ::binary-port ::bstring)
	    (inline input-string::bstring ::binary-port ::int)
	    (inline input-fill-string!::int ::binary-port ::bstring))
   
   (pragma  (c-binary-port? (predicate-of binary-port) nesting)
	    ($output-char nesting args-safe)
	    ($output-byte nesting args-safe)
	    ($input-char nesting args-safe)
	    ($int-eof? nesting args-safe)
	    (binary-port? side-effect-free nesting)))

;*---------------------------------------------------------------------*/
;*    binary-port? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (binary-port? obj)
   (c-binary-port? obj))

;*---------------------------------------------------------------------*/
;*    open-output-binary-file ...                                      */
;*---------------------------------------------------------------------*/
(define (open-output-binary-file str)
   ($open-output-binary-file str))

;*---------------------------------------------------------------------*/
;*    append-output-binary-file ...                                    */
;*---------------------------------------------------------------------*/
(define (append-output-binary-file str)
   ($append-output-binary-file str))

;*---------------------------------------------------------------------*/
;*    open-input-binary-file ...                                       */
;*---------------------------------------------------------------------*/
(define (open-input-binary-file str)
   ($open-input-binary-file str))

;*---------------------------------------------------------------------*/
;*    close-binary-port ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (close-binary-port port)
   ($close-binary-port port))

;*---------------------------------------------------------------------*/
;*    flush-binary-port ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (flush-binary-port port)
   ($flush-binary-port port))

;*---------------------------------------------------------------------*/
;*    input-obj ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (input-obj port)
   ($input-obj port))

;*---------------------------------------------------------------------*/
;*    output-obj ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (output-obj port obj)
   ($output-obj port obj))
	    
;*---------------------------------------------------------------------*/
;*    output-char ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (output-char port char)
   ($output-char port char))

;*---------------------------------------------------------------------*/
;*    output-byte ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (output-byte port char)
   ($output-byte port char))

;*---------------------------------------------------------------------*/
;*    input-char ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (input-char port)
   (let ((char ($input-char port)))
      (if ($int-eof? char)
	  beof
	  (integer->char char))))

;*---------------------------------------------------------------------*/
;*    output-string ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (output-string port string)
   ($output-string port string))

;*---------------------------------------------------------------------*/
;*    input-string ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (input-string port len)
   ($input-string port len))

;*---------------------------------------------------------------------*/
;*    input-fill-string! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (input-fill-string! port str)
   ($input-fill-string port str))
