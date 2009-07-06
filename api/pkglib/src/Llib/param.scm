;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pkglib/src/Llib/param.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 13 09:36:33 2006                          */
;*    Last change :  Thu May 10 12:41:47 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Pkglib-bigloo params                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pkglib_param
   
   (include "param.sch")
   
   (export (pkglib-version::bstring)
	   
	   (pkglib-interface-suffix::bstring)
	   (pkglib-interface-suffix-set! ::bstring)

	   (pkglib-package-suffix::bstring)
	   (pkglib-package-tuning-mark::char)
	   
	   (pkglib-meta-filename::bstring)
	   
	   (pkglib-verbose::int)
	   (pkglib-verbose-set! ::int)

	   (pkglib-colors::pair)
	   (pkglib-colors-set! ::pair)))

;*---------------------------------------------------------------------*/
;*    pkglib-version ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter pkglib-version
   "1.0.0")

;*---------------------------------------------------------------------*/
;*    pkglib-interface-suffix ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter pkglib-interface-suffix
   "spi")

;*---------------------------------------------------------------------*/
;*    pkglib-package-suffix ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter pkglib-package-suffix
   "tar.gz")

;*---------------------------------------------------------------------*/
;*    pkglib-package-tuning-mark ...                                   */
;*---------------------------------------------------------------------*/
(define-parameter pkglib-package-tuning-mark
   #\_)

;*---------------------------------------------------------------------*/
;*    pkglib-meta-filename ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter pkglib-meta-filename
   (make-file-name "etc" "meta"))

;*---------------------------------------------------------------------*/
;*    pkglib-verbose ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter pkglib-verbose
   0)

;*---------------------------------------------------------------------*/
;*    pkglib-colors ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter pkglib-colors
   '((error 0)
     (warning 6)
     (ok 3)
     (message 1)
     (command 2)
     (arg0 3)
     (arg1 4)
     (arg2 5)
     (package 6)
     (variable 7)
     (macro 3)
     (function 4)
     (record 5)
     (exception 6))
   (lambda (v)
      (unless (list? v)
	 (error 'pkglib-colors-set! "Illegal colors" v))
      (for-each (lambda (o)
		   (match-case o
		      (((? symbol?) (? integer?))
		       #t)
		      (else
		       (error 'pkglib-colors "Illegal color" o))))
		v)
      v))


