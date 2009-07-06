;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Coerce/pproto.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 10:45:07 1995                          */
;*    Last change :  Mon May 15 07:40:41 2000 (serrano)                */
;*    Copyright   :  1995-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We print prototype                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_pproto
   (include "Tools/verbose.sch")
   (import  tools_speek
	    tools_shape
	    type_type
	    type_pptype
	    ast_var)
   (export  (pfunction-proto ::long ::variable)
	    (pvariable-proto ::long ::variable)
	    (reset-ppmarge!)
	    (inc-ppmarge!)
	    (dec-ppmarge!)))

;*---------------------------------------------------------------------*/
;*    reset-ppmarge! ...                                               */
;*---------------------------------------------------------------------*/
(define (reset-ppmarge!)
   (set! *pp-marge* 8))

;*---------------------------------------------------------------------*/
;*    inc-ppmarge! ...                                                 */
;*---------------------------------------------------------------------*/
(define (inc-ppmarge!)
   (set! *pp-marge* (+fx 1 *pp-marge*)))

;*---------------------------------------------------------------------*/
;*    dec-ppmarge! ...                                                 */
;*---------------------------------------------------------------------*/
(define (dec-ppmarge!)
   (set! *pp-marge* (-fx *pp-marge* 1)))

;*---------------------------------------------------------------------*/
;*    *pp-marge* ...                                                   */
;*---------------------------------------------------------------------*/
(define *pp-marge*       8)
(define old-marge        -1)
(define old-marge-string "")

;*---------------------------------------------------------------------*/
;*    pfunction-proto ...                                              */
;*---------------------------------------------------------------------*/
(define (pfunction-proto level variable)
   (let ((marge (if (=fx old-marge *pp-marge*)
		    old-marge-string
		    (let ((marge (make-string *pp-marge* #\space)))
		       (set! old-marge *pp-marge*)
		       (set! old-marge-string marge)
		       marge))))
      (verbose level
	       marge
	       (shape variable) " : "
	       (function-type->string variable)
	       #\Newline)))

;*---------------------------------------------------------------------*/
;*    pvariable-proto ...                                              */
;*---------------------------------------------------------------------*/
(define (pvariable-proto level variable)
   (let ((marge (make-string *pp-marge* #\space)))
      (verbose level
	       marge
	       (shape variable) " : "
	       (variable-type->string variable)
	       #\Newline)))
