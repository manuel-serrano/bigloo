;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/bvm.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Thu Dec 13 13:35:01 2007 (serrano)                */
;*    Copyright   :  2005-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The bytecode backend class definition                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_bvm
   (import type_type
	   backend_backend)
   (export (abstract-class bvm::backend)))

;*---------------------------------------------------------------------*/
;*    backend-initialize! ::bvm ...                                    */
;*---------------------------------------------------------------------*/
(define-method (backend-initialize! b::bvm)
   (bvm-typed-set! b #t)
   (bvm-callcc-set! b #f)
   (bvm-qualified-types-set! b #t)
   (bvm-effect+-set! b #t)
   (bvm-remove-empty-let-set! b #t)
   (bvm-foreign-closure-set! b #f)
   (bvm-typed-eq-set! b #t)
   (bvm-trace-support-set! b #t)
   (bvm-pragma-support-set! b #f)
   (bvm-tvector-descr-support-set! b #f)
   (bvm-require-tailc-set! b #t))


