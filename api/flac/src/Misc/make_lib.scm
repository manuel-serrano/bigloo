;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Jun 28 16:56:25 2011 (serrano)                */
;*    Copyright   :  2001-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_makelib

   (import __flac_flac)

   (eval   (export-all)

           (class &flac-error)
           (class flac-decoder)))

