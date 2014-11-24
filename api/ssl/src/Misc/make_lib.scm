;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/ssl/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Mon Nov 24 16:07:01 2014 (serrano)                */
;*    Copyright   :  2001-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ssl_makelib

   (import __ssl_ssl)

   (eval   (export-all)

      (class certificate)
      (class private-key)
      (class secure-context)
      (class ssl-connection)))
