;*=====================================================================*/
;*    serrano/prgm/project/bigloo/pthread/src/Llib/pcondvar.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  3 08:14:49 2004                          */
;*    Last change :  Sat Mar  5 14:51:24 2005 (serrano)                */
;*    Copyright   :  2004-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public Posix Condvar implementation.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_condvar

   (include "pcondvar.sch")
   
   (export (condition-variable-specific::obj ::condvar)
	   (condition-variable-specific-set!::obj ::condvar ::obj)))

;*---------------------------------------------------------------------*/
;*    condition-variable-specific ...                                  */
;*---------------------------------------------------------------------*/
(define (condition-variable-specific m)
   ($pcondvar-specific m))

;*---------------------------------------------------------------------*/
;*    condition-variable-specific-set! ...                             */
;*---------------------------------------------------------------------*/
(define (condition-variable-specific-set! m v)
   ($pcondvar-specific-set! m v)
   v)

