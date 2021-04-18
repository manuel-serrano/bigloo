;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/openpgp/src/Llib/error.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 18 18:22:01 2021                          */
;*    Last change :  Sun Apr 18 18:23:03 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    OpenPGP error                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __openpgp-error

   (export (class &openpgp-error::&error)
	   (openpgp-error proc obj msg)))

;*---------------------------------------------------------------------*/
;*    openpgp-error ...                                                */
;*---------------------------------------------------------------------*/
(define (openpgp-error proc obj msg)
   (raise (instantiate::&openpgp-error (proc proc) (obj obj) (msg msg))))
