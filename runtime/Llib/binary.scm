;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/binary.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  7 10:38:25 1994                          */
;*    Last change :  Tue Mar 11 15:49:18 2008 (serrano)                */
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
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
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
	    
	    (c-open-output-binary-file::obj (::bstring)
					    "open_output_binary_file")
	    (c-append-output-binary-file::obj (::bstring)
					      "append_output_binary_file")
	    (c-open-input-binary-file::obj (::bstring)
					   "open_input_binary_file")
	    (c-close-binary-port::obj (::binary-port)
				      "close_binary_port")
	    (c-flush-binary-port::obj (::binary-port)
				      "bgl_flush_binary_port")
	    (c-input-obj::obj (::binary-port)
			      "input_obj")
	    (c-output-obj::obj (::binary-port ::obj)
			       "output_obj")
	    (macro c-output-char::obj (::binary-port ::char)
		   "BGL_OUTPUT_CHAR")
	    (macro c-input-char::int (::binary-port)
		   "BGL_INPUT_CHAR")
	    (c-output-string::int (::binary-port ::bstring)
				  "bgl_output_string")
	    (c-input-string::bstring (::binary-port ::int)
				     "bgl_input_string")
	    (c-input-fill-string::int (::binary-port ::bstring)
				      "bgl_input_fill_string")
	    (macro c-int-eof?::bool (::int)
		   "BGL_INT_EOFP"))

   (java    (class foreign
	       (method static c-binary-port?::bool (::obj)
		       "BINARY_PORTP")
	       (method static c-open-output-binary-file::obj (::bstring)
		       "open_output_binary_file")
	       (method static c-append-output-binary-file::obj (::bstring)
		       "append_output_binary_file")
	       (method static c-open-input-binary-file::obj (::bstring)
		       "open_input_binary_file")
	       (method static c-close-binary-port::obj (::binary-port)
		       "close_binary_port")
	       (method static c-flush-binary-port::obj (::binary-port)
		       "bgl_flush_binary_port")
	       (method static c-input-obj::obj (::binary-port)
		       "input_obj")
	       (method static c-output-obj::obj (::binary-port ::obj)
		       "output_obj")
	       (method static c-output-char::obj (::binary-port ::char)
		       "BGL_OUTPUT_CHAR")
	       (method static c-input-char::int (::binary-port)
		       "BGL_INPUT_CHAR")
	       (method static c-output-string::int (::binary-port ::bstring)
		       "bgl_output_string")
	       (method static c-input-string::bstring (::binary-port ::int)
		       "bgl_input_string")
	       (method static c-input-fill-string::int (::binary-port ::bstring)
		       "bgl_input_fill_string")
	       (method static c-int-eof?::bool (::int)
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
	    (inline input-char::obj ::binary-port)
	    (inline output-string ::binary-port ::bstring)
	    (inline input-string::bstring ::binary-port ::int)
	    (inline input-fill-string!::int ::binary-port ::bstring))
   
   (pragma  (c-binary-port? (predicate-of binary-port) nesting)
	    (c-output-char nesting args-safe)
	    (c-input-char nesting args-safe)
	    (c-int-eof? nesting args-safe)
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
   (c-open-output-binary-file str))

;*---------------------------------------------------------------------*/
;*    append-output-binary-file ...                                    */
;*---------------------------------------------------------------------*/
(define (append-output-binary-file str)
   (c-append-output-binary-file str))

;*---------------------------------------------------------------------*/
;*    open-input-binary-file ...                                       */
;*---------------------------------------------------------------------*/
(define (open-input-binary-file str)
   (c-open-input-binary-file str))

;*---------------------------------------------------------------------*/
;*    close-binary-port ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (close-binary-port port)
   (c-close-binary-port port))

;*---------------------------------------------------------------------*/
;*    flush-binary-port ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (flush-binary-port port)
   (c-flush-binary-port port))

;*---------------------------------------------------------------------*/
;*    input-obj ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (input-obj port)
   (c-input-obj port))

;*---------------------------------------------------------------------*/
;*    output-obj ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (output-obj port obj)
   (c-output-obj port obj))
	    
;*---------------------------------------------------------------------*/
;*    output-char ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (output-char port char)
   (c-output-char port char))

;*---------------------------------------------------------------------*/
;*    input-char ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (input-char port)
   (let ((char (c-input-char port)))
      (if (c-int-eof? char)
	  beof
	  (integer->char char))))

;*---------------------------------------------------------------------*/
;*    output-string ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (output-string port string)
   (c-output-string port string))

;*---------------------------------------------------------------------*/
;*    input-string ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (input-string port len)
   (c-input-string port len))

;*---------------------------------------------------------------------*/
;*    input-fill-string! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (input-fill-string! port str)
   (c-input-fill-string port str))
