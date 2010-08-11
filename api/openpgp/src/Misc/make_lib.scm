;*=====================================================================*/
;*    .../prgm/project/bigloo/api/openpgp/src/Misc/make-lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*        adapted :  Florian Loitsch                                   */
;*    Copyright   :  2001-09 Manuel Serrano, Florian Loitsch           */
;*    -------------------------------------------------------------    */
;*    The module is used to build the heap file.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __openpgp-makelib

   (import __openpgp-facade
	   __openpgp-key-manager)

   (eval   (export-all))

   (export (%openpgp-eval)))

;*---------------------------------------------------------------------*/
;*    %openpgp-eval ...                                             */
;*---------------------------------------------------------------------*/
(define (%openpgp-eval)
   #unspecified)
