;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  8 10:11:38 1995                          */
;*    Last change :  Thu Mar 19 12:43:39 2020 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The construction of the library                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __make_lib
   
   (import  __configure
	    __param
	    __type
	    __bigloo
	    __module
	    __error
	    __os
	    __bexit
	    __structure
	    __hash
	    __tvector
            __weakptr
	    __bit
	    __binary
	    __intext
	    __foreign
	    __object
	    __dsssl
	    __ucs2
	    __unicode
	    __process
	    __socket
	    __regexp
	    __date
	    __thread
	    __mmap
	    __semaphore
	    __trace
	    __base64
	    __md5
	    __crc
	    __crc16
	    __sha1
	    __sha2
	    __gunzip
	    __tar
	    __kmp
	    __bm
	    __srfi4
	    __bignum
	    __aes
	    __url
	    __http
	    __ftp
	    __uuid
	    __custom
	    __i18n
	    
	    __reader

	    __pp
	    __pp_circle

	    __rgc
	    __rgc_expand

	    __macro
	    __progn
            __eval
            __evenv
	    __everror
            __expand
	    __expander_quote
	    (eval-begin-expander __expander_define)
	    (args-parse-usage __expander_args)
	    __library
	    __evobject
 	    (eval-find-module __evmodule)
 	    (eval-module __evmodule)
 	    (eval-module-set! __evmodule)
	    (evmodule? __evmodule)
 	    (evmodule-name __evmodule)
 	    (evmodule-path __evmodule)
 	    (evmodule-extension __evmodule)
 	    (evmodule-extension-set! __evmodule)
 	    (evmodule-check-unbound __evmodule)
	    (evmodule-comp! __evmodule)
	    (call-with-eval-module __evmodule)
	    
	    (register-eval-srfi! __expander_srfi0)
	    (unregister-eval-srfi! __expander_srfi0)
	    (register-compile-srfi! __expander_srfi0)
	    (unregister-compile-srfi! __expander_srfi0)
	    (register-srfi! __expander_srfi0)
	    (unregister-srfi! __expander_srfi0)
	    (eval-srfi? __expander_srfi0)
	    (compile-srfi? __expander_srfi0)
	    (expand-compile-cond-expand __expander_srfi0)
	    (expand-cond-expand __expander_srfi0)

	    __match_expand
	    (match-define-structure! __match_normalize)

	    __r4_booleans_6_1
	    __r4_equivalence_6_2
	    __r4_pairs_and_lists_6_3
	    __r4_symbols_6_4
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_strings_6_7
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_ports_6_10_1
	    __r4_input_6_10_2
	    __r4_output_6_10_3
	    
	    __r5_control_features_6_4
	    __r5_macro_4_3_syntax
	    
	    __lalr_driver
	    __lalr_expand))


