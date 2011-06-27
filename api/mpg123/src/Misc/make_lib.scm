;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/src/Misc/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Fri Jun 24 16:24:20 2011 (serrano)                */
;*    Copyright   :  2001-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_makelib

   (import __mpg123_mpg123)

   (eval   (export-all)

           (class &mpg123-error)
           (class mpg123-handle)))

