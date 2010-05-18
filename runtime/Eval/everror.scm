;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/everror.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 14 13:46:57 2004                          */
;*    Last change :  Thu Apr 29 18:01:49 2010 (serrano)                */
;*    Copyright   :  2004-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The error of evmeaning                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __everror
   
   (include "Eval/byte-code.sch")
   
   (import  __type
 	    __object
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __bit
	    __param
	    __bexit
	    __thread
	    
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
	    __r4_output_6_10_3)
   
   (extern (macro $evmeaning-byte-code::obj (::dynamic-env)
		  "BGL_ENV_BYTECODE")
	   (macro $evmeaning-byte-code-set!::obj (::dynamic-env ::obj)
		  "BGL_ENV_BYTECODE_SET"))
   
   (java   (class foreign
	      (method static $evmeaning-byte-code::obj (::dynamic-env)
		      "BGL_ENV_BYTECODE")
	      (method static $evmeaning-byte-code-set!::obj (::dynamic-env ::obj)
		      "BGL_ENV_BYTECODE_SET")))
   
   (export (evmeaning-error ::obj ::obj ::obj ::obj)
	   (evmeaning-type-error ::obj ::obj ::obj ::obj)
	   (evmeaning-reset-error!)
	   (evmeaning-set-error-location! ::obj)
	   (evmeaning-warning ::obj . ::obj)
	   (evmeaning-exception-handler ::obj)
	   (evmeaning-arity-error ::obj ::obj ::int ::int)))

;*---------------------------------------------------------------------*/
;*    evmeaning-reset-error! ...                                       */
;*---------------------------------------------------------------------*/
(define (evmeaning-reset-error!)
   ($evmeaning-byte-code-set! (current-dynamic-env) #f))

;*---------------------------------------------------------------------*/
;*    evmeaning-set-error-location! ...                                */
;*---------------------------------------------------------------------*/
(define (evmeaning-set-error-location! loc)
   ($evmeaning-byte-code-set! (current-dynamic-env)
			      (evcode -1 loc #unspecified)))
   
;*---------------------------------------------------------------------*/
;*    evmeaning-error ...                                              */
;*---------------------------------------------------------------------*/
(define (evmeaning-error bcode proc mes obj)
   (if (evcode? bcode)
       (match-case (evcode-loc bcode)
	  ((at ?fname ?loc)
	   (error/location proc mes obj fname loc))
	  (else
	   (error proc mes obj)))
       (error proc mes obj)))

;*---------------------------------------------------------------------*/
;*    evmeaning-type-error ...                                         */
;*---------------------------------------------------------------------*/
(define (evmeaning-type-error bcode proc mes obj)
   (if (evcode? bcode)
       (match-case (evcode-loc bcode)
	  ((at ?fname ?loc)
	   (bigloo-type-error/location proc mes obj fname loc))
	  (else
	   (bigloo-type-error proc mes obj)))
       (bigloo-type-error proc mes obj)))
   
;*---------------------------------------------------------------------*/
;*    evmeaning-warning ...                                            */
;*---------------------------------------------------------------------*/
(define (evmeaning-warning bcode . args)
   (if (evcode? bcode)
       (match-case (evcode-loc bcode)
	  ((at ?fname ?loc)
	   (warning-notify (make-&eval-warning fname loc #f args)))
	  (else
	   (warning-notify (make-&eval-warning #f #f #f args))))
       (warning-notify (make-&eval-warning #f #f #f args))))

;*---------------------------------------------------------------------*/
;*    evmeaning-annotate-exception! ...                                */
;*---------------------------------------------------------------------*/
(define (evmeaning-annotate-exception! e)
   (if (and (&exception? e)
	    (not (&exception-fname e))
	    (evcode? ($evmeaning-byte-code (current-dynamic-env))))
       (match-case (evcode-loc ($evmeaning-byte-code (current-dynamic-env)))
	  ((at ?fname ?loc)
	   (&exception-fname-set! e fname)
	   (&exception-location-set! e loc)
	   e)
	  (else
	   e))
       e))
   
;*---------------------------------------------------------------------*/
;*    evmeaning-exception-handler ...                                  */
;*    -------------------------------------------------------------    */
;*    This handler is just in charge of adding a location to           */
;*    the error/warning.                                               */
;*---------------------------------------------------------------------*/
(define (evmeaning-exception-handler e)
   (raise (evmeaning-annotate-exception! e)))

;*---------------------------------------------------------------------*/
;*    evmeaning-arity-error ...                                        */
;*---------------------------------------------------------------------*/
(define (evmeaning-arity-error code name provide expect)
   (let ((msg (format "Wrong number of arguments: ~a expected, ~a provided "
		      expect provide)))
      (evmeaning-error code "eval" msg name)))

	 
