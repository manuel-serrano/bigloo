;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/bvm.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 28 11:46:28 2005                          */
;*    Last change :  Mon Nov 14 18:49:06 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The bytecode backend class definition                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_bvm
   (import type_type
	   backend_backend)
   (include "BackEnd/bvm.sch")
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


