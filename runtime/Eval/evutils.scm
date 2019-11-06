;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/evutils.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 30 08:07:53 2010                          */
;*    Last change :  Sun Aug 25 09:16:17 2019 (serrano)                */
;*    Copyright   :  2010-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Utility functions for eval                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evutils
   
   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __param
	    __object
	    __thread
	    __dsssl
	    
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
	    __macro
	    __bit)
   
   (export  (parse-formal-ident ::obj ::obj)
	    (args->list ::obj)
	    (bindings->list ::obj)))

;*---------------------------------------------------------------------*/
;*    parse-formal-ident ...                                           */
;*---------------------------------------------------------------------*/
(define (parse-formal-ident ident loc)
   
   (define (parse-typed-ident ident)
      (let* ((str (symbol->string! ident))
	     (len (string-length str)))
	 (let loop ((i 0))
	    (cond
	       ((=fx i len)
		(cons ident '()))
	       ((and (char=? (string-ref str i) #\:)
		     (<fx i (-fx len 1))
		     (char=? (string-ref str (+fx i 1)) #\:))
		(cond
		   ((=fx i (-fx len 2))
		    (error/source-location "parse-formal-ident"
		       "Illegal empty identifier type"
		       ident
		       loc))
		   ((=fx i 0)
		    (cons (string->symbol "") ident))
		   (else
		    (cons (string->symbol (substring str 0 i))
		       (string->symbol (substring str (+fx i 2) len))))))
	       (else
		(loop (+fx i 1)))))))
   
   (cond
      ((dsssl-named-constant? ident)
       (cons (gensym 'dsssl) '()))
      ((and (pair? ident) (symbol? (car ident)))
       (cons ident '()))
      ((not (symbol? ident))
       (error/source-location "parse-formal-ident"
	  "Illegal identifier type"
	  ident
	  loc))
      (else
       (parse-typed-ident ident))))

;*---------------------------------------------------------------------*/
;*    args->list ...                                                   */
;*---------------------------------------------------------------------*/
(define (args->list args)
   (cond
      ((null? args)
       '())
      ((symbol? args)
       (list args))
      ((pair? args)
       (cons (car args) (args->list (cdr args))))
      (else
       (error/source 'args->list "Illegal args list" args args))))

;*---------------------------------------------------------------------*/
;*    bindings->list ...                                               */
;*---------------------------------------------------------------------*/
(define (bindings->list bindings)
   (cond
      ((null? bindings)
       '())
      ((not (pair? bindings))
       (error/source 'bindings->list "Illegal bindings list" bindings bindings))
      ((symbol? (car bindings))
       (cons bindings (bindings->list (cdr bindings))))
      ((not (pair? (car bindings)))
       (error/source 'bindings->list "Illegal bindings list" bindings bindings))
      (else
       (cons (car bindings) (bindings->list (cdr bindings))))))

