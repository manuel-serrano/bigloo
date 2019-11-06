;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/evprimop.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 15:32:04 1995                          */
;*    Last change :  Tue Apr  9 12:34:41 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module is only used to define all the interpeter primops.   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evprimop
   
   (option  (set! *unsafe-type* #t)
	    (set! *unsafe-arity* #t)
	    (set! *unsafe-range*  #t)
	    (set! *unsafe-struct* #t)
	    (set! *cflags-optim* ""))
   
   (import  __type
	    __object
	    __bigloo
	    __module
	    __param
	    __error
	    __configure
	    __os
	    __bexit
	    __bignum
	    __structure
	    __hash
	    __tvector
            __weakptr
	    __bit
	    __binary
	    __foreign
	    __intext 
	    __dsssl
	    __ucs2
	    __unicode
	    __process
	    __socket
 	    __regexp
	    __date
	    __custom
	    __trace
	    __base64
	    __md5
	    __crc
	    __crc16
	    __sha1
	    __sha2
	    __mmap
	    __semaphore
	    __thread
	    __gunzip
	    __tar
	    __kmp
	    __bm
	    __srfi4
	    __aes
	    __url
	    __http
	    __ftp
	    __uuid
	    __i18n
	    
	    __reader
  	    
	    __pp
	    __pp_circle
	    
	    __rgc
	    __lalr_driver
	    
            __eval
	    __macro
	    __everror
	    __expand
	    __progn
	    (eval-begin-expander __expander_define)
	    __expander_quote
	    
	    __library
	    
	    (define-primop! __evenv)
	    (define-primop-ref! __evenv)
	    (register-eval-srfi! __expander_srfi0)
	    (unregister-eval-srfi! __expander_srfi0)
	    (register-compile-srfi! __expander_srfi0)
	    (unregister-compile-srfi! __expander_srfi0)
	    (register-srfi! __expander_srfi0)
	    (unregister-srfi! __expander_srfi0)
	    (eval-srfi? __expander_srfi0)
	    (compile-srfi? __expander_srfi0)
	    (expand-cond-expand __expander_srfi0)
	    (args-parse-usage __expander_args)
	    __evobject
 	    (eval-find-module __evmodule)
 	    (eval-module __evmodule)
 	    (eval-module-set! __evmodule)
 	    (evmodule? __evmodule)
 	    (evmodule-name __evmodule)
 	    (evmodule-check-unbound __evmodule)
	    
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
	    __r5_macro_4_3_syntax)
   
   (eval    (export-all)

	    (class object)
	    (class &exception)
	    
	    (class &error)
	    (class &type-error)
	    (class &index-out-of-bounds-error)
	    (class &io-error)
	    (class &io-port-error)
	    (class &io-read-error)
	    (class &io-write-error)
	    (class &io-closed-error)
	    (class &io-file-not-found-error)
	    (class &io-parse-error)
	    (class &io-unknown-host-error)
	    (class &io-malformed-url-error)
	    (class &io-sigpipe-error)
	    (class &io-timeout-error)
	    (class &io-connection-error)
	    
	    (class &process-exception)

	    (class &security-exception)
	    (class &access-control-exception)
	    
	    (class &warning)
	    (class &eval-warning)

	    (class &http-error)
	    (class &http-redirection-error)
	    (class &http-status-error)
	    (class &http-redirection)

	    (class ftp)
	    (class &ftp-error)
	    (class &ftp-parse-error)

	    (class tar-header)))


 
