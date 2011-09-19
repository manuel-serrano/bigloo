;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/src/Misc/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sun Sep 18 19:26:03 2011 (serrano)                */
;*    Copyright   :  2001-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_makelib

   (library multimedia)
   
   (cond-expand ((library alsa) (library alsa)))
   
   (import __mpg123_mpg123)

   (cond-expand ((library alsa) (import __mpg123_alsadec)))

   (eval   (export-all)

           (class &mpg123-error)
           (class mpg123-handle))

   (cond-expand ((library alsa) (eval (class mpg123-alsadecoder)))))

