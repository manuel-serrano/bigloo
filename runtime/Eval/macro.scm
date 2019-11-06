;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/macro.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 08:59:04 1994                          */
;*    Last change :  Sun Aug 25 09:15:10 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    La manipulation des macros (de l'interprete et du compilateur).  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __macro
   
   (export  (install-eval-expander <keyword> <expander>)
	    (install-compiler-expander <keyword> <expander>)
	    (install-expander <keyword> <expander>)
	    (get-eval-expander <keyword>)
	    (get-compiler-expander <keyword>))

   (import  __error
	    __hash
	    __everror
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __param
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
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __evenv
	    __evmodule))

;*---------------------------------------------------------------------*/
;*    *macro-mutex* ...                                                */
;*---------------------------------------------------------------------*/
(define *eval-macro-mutex* (make-mutex "eval-macros"))
(define *compiler-macro-mutex* (make-mutex "compiler-macros"))

;*---------------------------------------------------------------------*/
;*    macro                                                            */
;*---------------------------------------------------------------------*/
(define-struct macros key eval-expander compiler-expander)

;*---------------------------------------------------------------------*/
;*    Global hash tables                                               */
;*---------------------------------------------------------------------*/
(define *eval-macro-table* (make-hashtable))
(define *compiler-macro-table* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    module-macro-table ...                                           */
;*---------------------------------------------------------------------*/
(define (module-macro-table)
   (let ((m (eval-module)))
      (when (evmodule? m)
	 (evmodule-macro-table m))))

;*---------------------------------------------------------------------*/
;*    put-macro! ...                                                   */
;*---------------------------------------------------------------------*/
(define (put-macro! table key expander where)
   (hashtable-update! table
		      key
		      (lambda (x)
			 (evwarning #f
				    "install-expander"
				    (string-append
				     "Redefinition of " where
				     " expander -- ")
				    key)
			 expander)
		      expander))
   
;*---------------------------------------------------------------------*/
;*    install-eval-expander ...                                        */
;*    -------------------------------------------------------------    */
;*    On installe une macro pour l'interprete seulement.               */
;*---------------------------------------------------------------------*/
(define (install-eval-expander keyword expander)
   (cond
      ((not (symbol? keyword))
       (error "install-eval-expander" "Illegal expander keyword" keyword))
      ((not (procedure? expander))
       (error "install-eval-expander" "Illegal expander expander" expander))
      (else
       (synchronize *eval-macro-mutex*
	  (put-macro! (or (module-macro-table) *eval-macro-table*)
	     keyword expander "eval")))))

;*---------------------------------------------------------------------*/
;*    install-compiler-expander ...                                    */
;*    -------------------------------------------------------------    */
;*    On installe une macro pour le compilateur seulement.             */
;*---------------------------------------------------------------------*/
(define (install-compiler-expander keyword expander)
   (cond
      ((not (symbol? keyword))
       (error "install-eval-expander" "Illegal expander keyword" keyword))
      ((not (procedure? expander))
       (error "install-eval-expander" "Illegal expander expander" expander))
      (else
       (synchronize *compiler-macro-mutex*
	  (put-macro! *compiler-macro-table* keyword expander "compiler")))))

;*---------------------------------------------------------------------*/
;*    install-expander ...                                             */
;*    -------------------------------------------------------------    */
;*    On installe une macro pour le compilateur *et* l'interprete.     */
;*---------------------------------------------------------------------*/
(define (install-expander keyword expander)
   (install-eval-expander keyword expander)
   (install-compiler-expander keyword expander))

;*---------------------------------------------------------------------*/
;*    get-eval-expander ...                                            */
;*    -------------------------------------------------------------    */
;*    On recupere une macro pour l'interprete.                         */
;*---------------------------------------------------------------------*/
(define (get-eval-expander keyword)
   (synchronize *eval-macro-mutex*
      (let ((mtable (module-macro-table)))
	 (or (and mtable (hashtable-get mtable keyword))
	     (hashtable-get *eval-macro-table* keyword)))))

;*---------------------------------------------------------------------*/
;*    get-compiler-expander ...                                        */
;*    -------------------------------------------------------------    */
;*    On recupere une macro pour le compilateur.                       */
;*    -------------------------------------------------------------    */
;*    Macro compilers are always global so we don't look into          */
;*    the eval module macro.                                           */
;*---------------------------------------------------------------------*/
(define (get-compiler-expander keyword)
   (synchronize *compiler-macro-mutex*
      (hashtable-get *compiler-macro-table* keyword)))



