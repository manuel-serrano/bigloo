;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/openpgp/src/Misc/make_lib.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Florian Loitsch                                   */
;*    Last change :  Sun Apr 18 18:21:52 2021 (serrano)                */
;*    Copyright   :  2001-21 Manuel Serrano, Florian Loitsch           */
;*    -------------------------------------------------------------    */
;*    The module is used to build the heap file.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __openpgp-makelib

   (import __openpgp-facade
	   __openpgp-key-manager
	   __openpgp-error)

   (eval   (export-all)))
