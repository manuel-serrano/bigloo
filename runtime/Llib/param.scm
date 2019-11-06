;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/param.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct  8 05:29:58 2004                          */
;*    Last change :  Sun Aug 25 09:10:00 2019 (serrano)                */
;*    Copyright   :  2004-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bigloo global parameters                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __param
   
   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __bignum
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_vectors_6_8
	   __r4_control_features_6_9
	   __r4_pairs_and_lists_6_3
	   __r4_characters_6_6
	   __r4_equivalence_6_2 
	   __r4_strings_6_7
	   __r4_ports_6_10_1
	   __foreign
	   __error
	   __evenv
	   __os
	   __bit)
   
   (import  __object
	    __thread
	    __configure)
   
   (export (bigloo-strict-r5rs-strings::bool)
	   (bigloo-strict-r5rs-strings-set! ::bool)
	   
	   (bigloo-compiler-debug::int)
	   (bigloo-compiler-debug-set! ::int)
	   
	   (bigloo-debug::int)
	   (bigloo-debug-set! ::int)
	   
	   (bigloo-debug-module::int)
	   (bigloo-debug-module-set! ::int)

	   (bigloo-profile::int)
	   (bigloo-profile-set! ::int)
	   
	   (bigloo-warning::int)
	   (bigloo-warning-set! ::int)
	   
	   (bigloo-trace-stack-depth::int)
	   (bigloo-trace-stack-depth-set! ::int)
	   
	   (bigloo-trace-color::bool)
	   (bigloo-trace-color-set! ::bool)

	   (bigloo-trace::pair-nil)
	   (bigloo-trace-set! ::pair-nil)
	   
	   (bigloo-case-sensitive::symbol)
	   (bigloo-case-sensitive-set! ::symbol)
	   
	   (bigloo-initialized!) 
	   (bigloo-initialized?::bool)
	   
	   (bigloo-load-reader::obj)
	   (bigloo-load-reader-set! ::obj)
	   
	   (bigloo-load-module::obj)
	   (bigloo-load-module-set! ::obj)
	   
	   (bigloo-module-extension-handler::obj)
	   (bigloo-module-extension-handler-set! ::obj)
	   
	   (bigloo-eval-strict-module::bool)
	   (bigloo-eval-strict-module-set! ::bool)
	   
	   (bigloo-library-path::pair-nil)
	   (bigloo-library-path-set! ::pair-nil)
	   
	   (bigloo-dns-enable-cache::bool)
	   (bigloo-dns-enable-cache-set! ::bool)
	   
	   (bigloo-dns-cache-validity-timeout::long)
	   (bigloo-dns-cache-validity-timeout-set! ::long))
   
   (extern (export bigloo-dns-enable-cache
		   "bgl_dns_enable_cache")
	   (export bigloo-dns-cache-validity-timeout
		   "bgl_dns_cache_validity_timeout")

	   (export bigloo-debug
		   "bgl_debug")
	   (export bigloo-debug-set!
		   "bgl_debug_set")))

;*---------------------------------------------------------------------*/
;*    *parameter-mutex*                                                */
;*---------------------------------------------------------------------*/
(define *parameter-mutex* (make-mutex "param"))

;*---------------------------------------------------------------------*/
;*    define-parameter ...                                             */
;*---------------------------------------------------------------------*/
(define-macro (define-parameter id default . setter)
   (let ((vid (symbol-append '* id '*)))
      `(begin
	  (define ,vid ,default)
	  (define (,id)
	     ,vid)
	  (define (,(symbol-append id '-set!) v)
	     (synchronize *parameter-mutex*
		,(if (pair? setter)
		     `(set! ,vid (,(car setter) v))
		     `(set! ,vid v)))
	     v))))

;*---------------------------------------------------------------------*/
;*    bigloo-strict-r5rs-strings ...                                   */
;*    -------------------------------------------------------------    */
;*    Set to #t for a pure r5rs interpretation of escape String        */
;*    sequences. Set to #f to evaluates sequences such a \t, \n        */
;*    as C strings.                                                    */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-strict-r5rs-strings #f)

;*---------------------------------------------------------------------*/
;*    bigloo-compiler-debug ...                                        */
;*    -------------------------------------------------------------    */
;*    The compiler debug level (a positive integer).                   */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-compiler-debug
   0
   (lambda (val)
      (if (and (fixnum? val) (<fx val 0))
	  (error 'bigloo-compiler-debug-set! "Illegal debug level" val)
	  val)))
   
;*---------------------------------------------------------------------*/
;*    bigloo-debug ...                                                 */
;*    -------------------------------------------------------------    */
;*    The runtime debug level (a positive integer).                    */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-debug
   0
   (lambda (val)
      (if (and (fixnum? val) (<fx val 0))
	  (error 'bigloo-debug-set! "Illegal debug level" val)
	  val)))

;*---------------------------------------------------------------------*/
;*    bigloo-debug-module ...                                          */
;*    -------------------------------------------------------------    */
;*    The runtime debug module level (a positive integer).             */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-debug-module
   0
   (lambda (val)
      (if (and (fixnum? val) (<fx val 0))
	  (error 'bigloo-debug-module-set! "Illegal debug module level" val)
	  val)))

;*---------------------------------------------------------------------*/
;*    bigloo-warning ...                                               */
;*    -------------------------------------------------------------    */
;*    The runtime warning level (a positive integer).                  */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-warning
   1
   (lambda (val)
      (if (and (fixnum? val) (<fx val 0))
	  (error 'bigloo-warning-set! "Illegal warning level" val)
	  val)))

;*---------------------------------------------------------------------*/
;*    bigloo-profile ...                                               */
;*    -------------------------------------------------------------    */
;*    The runtime profile level (a positive integer).                  */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-profile
   0
   (lambda (val)
      (if (and (fixnum? val) (<fx val 0))
	  (error 'bigloo-profile-set! "Illegal profile level" val)
	  val)))

;*---------------------------------------------------------------------*/
;*    bigloo-trace-color ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-trace-color #t)

;*---------------------------------------------------------------------*/
;*    bigloo-trace ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-trace
   (let ((env-value (getenv "BIGLOOTRACE")))
      (if (string? env-value)
	  (map string->symbol (string-split env-value))
	  '())))

;*---------------------------------------------------------------------*/
;*    bigloo-trace-stack-depth ...                                     */
;*    -------------------------------------------------------------    */
;*    The depth of a stack dump on error.                              */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-trace-stack-depth
   (let ((env-value (getenv "BIGLOOSTACKDEPTH")))
      (if (string? env-value)
	  (string->integer env-value)
	  10)))

;*---------------------------------------------------------------------*/
;*    bigloo-case-sensitive ...                                        */
;*    -------------------------------------------------------------    */
;*    The case sensitivity for Bigloo symbols and keywords. Rgc        */
;*    may produce:                                                     */
;*       - uppercase symbols, the default, denoted by UPCASE.          */
;*       - lowercase symbols, denoted by DOWNCASE.                     */
;*       - case senstive symbols, denoted by SENSITIVE.                */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-case-sensitive
   'sensitive
   (lambda (val)
      (cond
	 ((memq val '(sensitive downcase upcase))
	  val)
	 (else
	  (error "bigloo-sensitivity-set!" "Illegal sensitive value" val)))))

;*---------------------------------------------------------------------*/
;*    *bigloo-initializedp* ...                                        */
;*---------------------------------------------------------------------*/
(define *bigloo-initializedp* #f)
 
;*---------------------------------------------------------------------*/
;*    bigloo-initialized! ...                                          */
;*---------------------------------------------------------------------*/
(define (bigloo-initialized!)
   (set! *bigloo-initializedp* #t))

;*---------------------------------------------------------------------*/
;*    bigloo-initialized? ...                                          */
;*---------------------------------------------------------------------*/
(define (bigloo-initialized?)
   *bigloo-initializedp*)

;*---------------------------------------------------------------------*/
;*    bigloo-load-reader ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-load-reader #f)

;*---------------------------------------------------------------------*/
;*    bigloo-load-module ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-load-module #f)

;*---------------------------------------------------------------------*/
;*    bigloo-module-extension-handler ...                              */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-module-extension-handler #f)

;*---------------------------------------------------------------------*/
;*    bigloo-eval-strict-module ...                                    */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-eval-strict-module #t)

;*---------------------------------------------------------------------*/
;*    bigloo-library-path ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-library-path
   (list "." (bigloo-config 'library-directory))
   (lambda (val)
      (cond
	 ((not (list? val))
	  (error 'bigloo-library-path-set! "Illegal list" val))
	 ((not (every string? val))
	  (error 'bigloo-library-path-set!
		 "Illegal values"
		 (filter (lambda (s) (not (string? s))) val)))
	 (else
	  val))))

;*---------------------------------------------------------------------*/
;*    bigloo-dns-enable-cache ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-dns-enable-cache #t)

;*---------------------------------------------------------------------*/
;*    bigloo-dns-cache-validity-timeout ...                            */
;*---------------------------------------------------------------------*/
(define-parameter bigloo-dns-cache-validity-timeout 20)
