;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Eval/expdsrfi0.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 24 15:25:03 1999                          */
;*    Last change :  Sun Aug 25 09:15:32 2019 (serrano)                */
;*    Copyright   :  2001-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The expander for srfi forms.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __expander_srfi0
   
   (import  __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __param
	    __object
	    __thread
	    __library
	    
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
	    
	    __progn
	    __expand)
   
   (use     __type
	    __evenv
	    __configure
	    __bit)
   
   (extern  ($bigloo-backend::string () "bigloo_backend")
	    (export register-eval-srfi! "bgl_register_eval_srfi"))
   
   (java    (class foreign
	       (method static $bigloo-backend::string () "bigloo_backend"))
	    (export register-eval-srfi! "bgl_register_eval_srfi"))
   
   (export  (expand-eval-cond-expand ::pair-nil ::procedure)
	    (expand-compile-cond-expand ::pair-nil ::procedure)
	    (expand-cond-expand ::pair-nil ::procedure ::pair-nil)
	    (register-eval-srfi! ::symbol)
	    (unregister-eval-srfi! ::symbol)
	    (register-compile-srfi! ::symbol)
	    (unregister-compile-srfi! ::symbol)
	    (eval-srfi?::bool ::symbol)
	    (compile-srfi?::bool ::symbol)
	    (register-srfi! ::symbol)
	    (unregister-srfi! ::symbol)))

;*---------------------------------------------------------------------*/
;*    bigloo-major-version ...                                         */
;*---------------------------------------------------------------------*/
(define-macro (bigloo-major-version)
   `',(string->symbol
	 (string-append "bigloo"
	    (substring *bigloo-version*
	       0
	       (-fx (string-length *bigloo-version*) 1)))))
					     
;*---------------------------------------------------------------------*/
;*    bigloo-branch-version ...                                        */
;*---------------------------------------------------------------------*/
(define-macro (bigloo-branch-version)
   `',(string->symbol
	 (string-append "bigloo"
	    (substring *bigloo-version* 0
	       (string-index *bigloo-version* #\.)))))

;*---------------------------------------------------------------------*/
;*    bigloo-version ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (bigloo-version)
   `',(string->symbol (string-append "bigloo" *bigloo-version*)))

;*---------------------------------------------------------------------*/
;*    bigloo-int-size ...                                              */
;*---------------------------------------------------------------------*/
(define (bigloo-int-size)
   (let ((isize (bigloo-config 'int-size)))
      (string->symbol
	 (string-append "bint"
	    (if (number? isize) (number->string isize) "30")))))

;*---------------------------------------------------------------------*/
;*    bigloo-elong-size ...                                            */
;*---------------------------------------------------------------------*/
(define (bigloo-elong-size)
   (let ((esize (bigloo-config 'elong-size)))
      (string->symbol
	 (string-append "elong"
	    (if (number? esize) (number->string esize) "32")))))

;*---------------------------------------------------------------------*/
;*    *srfi-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *srfi-mutex* (make-mutex 'srfi0))

;*---------------------------------------------------------------------*/
;*    *srfi-common-list* ...                                           */
;*    -------------------------------------------------------------    */
;*    The list of supported srfi by the interpreter                    */
;*    -------------------------------------------------------------    */
;*    The four initial supported srfi are:                             */
;*       - srfi-0                                                      */
;*       - srfi-xxx                                                    */
;*       - ...                                                         */
;*       - srfi-xxx                                                    */
;*       - bigloo                                                      */
;*       - bigloo<major-num>                                           */
;*       - bigloo<major-num><minor-num>                                */
;*       - bint<integers-bit-size>                                     */
;*    -------------------------------------------------------------    */
;*    When a library is used for compiling the name of that library    */
;*    is added to the supported srfis.                                 */
;*---------------------------------------------------------------------*/
(define (srfi-common-list)
   (let ((l (cons* (bigloo-version)
		   (bigloo-major-version)
		   (bigloo-branch-version)
		   'bigloo
		   (bigloo-int-size)
		   (bigloo-elong-size)
		   '(srfi-0
		     srfi-2
		     srfi-4
		     srfi-6
		     srfi-8
		     srfi-9
		     srfi-10
		     srfi-22
		     srfi-28
		     srfi-30))))
      (if $configure-auto-finalizer
	  (cons* 'bigloo-finalizer 'bigloo-weakptr l)
	  l)))

;*---------------------------------------------------------------------*/
;*    *srfi-eval-list* ...                                             */
;*---------------------------------------------------------------------*/
(define *srfi-eval-list* #f)
(define *srfi-compile-list* #f)

;*---------------------------------------------------------------------*/
;*    srfi-compile-list ...                                            */
;*---------------------------------------------------------------------*/
(define (srfi-compile-list)
   (unless *srfi-compile-list*
      (set! *srfi-compile-list* (srfi-common-list)))
   *srfi-compile-list*)

;*---------------------------------------------------------------------*/
;*    srfi-eval-list ...                                               */
;*---------------------------------------------------------------------*/
(define (srfi-eval-list)
   (unless *srfi-eval-list*
      (set! *srfi-eval-list* (cons 'bigloo-eval (srfi-common-list))))
   *srfi-eval-list*)

;*---------------------------------------------------------------------*/
;*    register-eval-srfi! ...                                          */
;*---------------------------------------------------------------------*/
(define (register-eval-srfi! srfi::symbol)
   (synchronize *srfi-mutex*
      (set! *srfi-eval-list* (cons srfi (srfi-eval-list)))))

;*---------------------------------------------------------------------*/
;*    unregister-eval-srfi! ...                                        */
;*---------------------------------------------------------------------*/
(define (unregister-eval-srfi! srfi::symbol)
   (synchronize *srfi-mutex*
      (set! *srfi-eval-list* (remq! srfi (srfi-eval-list)))))

;*---------------------------------------------------------------------*/
;*    register-compile-srfi! ...                                       */
;*---------------------------------------------------------------------*/
(define (register-compile-srfi! srfi::symbol)
   (synchronize *srfi-mutex*
      (set! *srfi-compile-list* (cons srfi (srfi-compile-list)))))

;*---------------------------------------------------------------------*/
;*    unregister-compile-srfi! ...                                     */
;*---------------------------------------------------------------------*/
(define (unregister-compile-srfi! srfi::symbol)
   (synchronize *srfi-mutex*
      (set! *srfi-compile-list* (remq! srfi (srfi-compile-list)))))

;*---------------------------------------------------------------------*/
;*    register-srfi! ...                                               */
;*---------------------------------------------------------------------*/
(define (register-srfi! srfi::symbol)
   (register-eval-srfi! srfi)
   (register-compile-srfi! srfi))

;*---------------------------------------------------------------------*/
;*    unregister-srfi! ...                                             */
;*---------------------------------------------------------------------*/
(define (unregister-srfi! srfi::symbol)
   (unregister-eval-srfi! srfi)
   (unregister-compile-srfi! srfi))

;*---------------------------------------------------------------------*/
;*    expand-eval-cond-expand ...                                      */
;*---------------------------------------------------------------------*/
(define (expand-eval-cond-expand x e)
   (expand-cond-expand x e (srfi-eval-list)))

;*---------------------------------------------------------------------*/
;*    expand-compile-cond-expand ...                                   */
;*---------------------------------------------------------------------*/
(define (expand-compile-cond-expand x e)
   (expand-cond-expand x e (srfi-compile-list)))

;*---------------------------------------------------------------------*/
;*    compile-srfi? ...                                                */
;*---------------------------------------------------------------------*/
(define (compile-srfi? srfi)
   (synchronize *srfi-mutex*
      (memq srfi (srfi-compile-list))))
   
;*---------------------------------------------------------------------*/
;*    eval-srfi? ...                                                   */
;*---------------------------------------------------------------------*/
(define (eval-srfi? srfi)
   (synchronize *srfi-mutex*
      (memq srfi (srfi-eval-list))))
   
;*---------------------------------------------------------------------*/
;*    expand-cond-exapnd ...                                           */
;*---------------------------------------------------------------------*/
(define (expand-cond-expand x e features)
   (match-case x
      ((cond-expand)
       #unspecified)
      ((?- ?clause . ?else)
       (match-case clause
	  (((kwote else) . ?body)
	   (if (null? else)
	       (e (evepairify `(begin ,@body) x) e)
	       (expand-error "cond-expand" "Illegal form" x)))
	  ((((kwote and)) . ?body)
	   (e (evepairify `(begin ,@body) x) e))
	  ((((kwote and) ?req1) . ?body)
	   (e (evepairify `(cond-expand
			      (,req1 ,@body)
			      ,@else)
			  x)
	      e))
	  ((((kwote and) ?req1 ?req2 . ?reqs) . ?body)
	   (expand-cond-expand-and x e req1 req2 reqs body else))
	  ((((kwote or)) . ?body)
	   (e (evepairify `(cond-expand ,@else) x) e))
	  ((((kwote or) ?req1) . ?body)
	   (e (evepairify `(cond-expand
			      (,req1 ,@body)
			      ,@else)
			  x)
	      e))
	  ((((kwote or) ?req1 ?req2 . ?reqs) . ?body)
	   (expand-cond-expand-or x e req1 req2 reqs body else))
	  ((((kwote not) ?req) . ?body)
	   (e (evepairify `(cond-expand
			      (,req (cond-expand ,@else))
			      (else ,@body))
			  x)
	      e))
	  (((library (and (? symbol?) ?lib)) . ?body)
	   (e (evepairify (if (library-exists? lib)
			      `(begin ,@body)
			      `(cond-expand ,@else))
			  x)
	      e))
	  (((config ?key ?value) . ?body)
	   (e (evepairify (if (equal? (bigloo-config key) value)
			      `(begin ,@body)
			      `(cond-expand ,@else))
			  x)
	      e))
	  (((and (? symbol?) ?feature) . ?body)
	   (e (evepairify (if (memq feature features)
			      `(begin ,@body)
			      `(cond-expand ,@else))
			  x)
	      e))
	  (else
	   (expand-error "cond-expand" "Illegal form" x))))
      (else
       (expand-error "cond-expand" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-cond-expand-and ...                                       */
;*---------------------------------------------------------------------*/
(define (expand-cond-expand-and x e req1 req2 reqs body else)
   (e (evepairify `(cond-expand
		      (,req1 (cond-expand
				((and ,req2 ,@reqs) ,@body)
				,@else))
		      ,@else)
		  x)
      e))

;*---------------------------------------------------------------------*/
;*    expand-cond-expand-or ...                                        */
;*---------------------------------------------------------------------*/
(define (expand-cond-expand-or x e req1 req2 reqs body else)
   (let ((bd (gensym)))
      (e (evepairify `(cond-expand
			 (,req1 ,(evepairify `(begin ,@body) body))
			 (else
			  (cond-expand
			     ((or ,req2 ,@reqs) ,@body)
			     ,@else)))
		     x)
	 e)))

