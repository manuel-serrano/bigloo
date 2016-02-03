;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/src/Misc/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Jan 26 15:08:07 2016 (serrano)                */
;*    Copyright   :  2001-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_makelib

   (library multimedia)
   
   (cond-expand ((library alsa) (library alsa)))
   (cond-expand ((library pulseaudio) (library pulseaudio)))
   
   (import __mpg123_mpg123
	   __mpg123_decoder)

   (cond-expand ((library alsa) (import __mpg123_alsadec)))
   (cond-expand ((library pulseaudio) (import __mpg123_padec)))

   (eval   (export-all)

           (class &mpg123-error)
           (class mpg123-handle))

   (cond-expand ((library alsa) (eval (class mpg123-alsadecoder))))
   (cond-expand ((library pulseaudio) (eval (class mpg123-pulseaudiodecoder)))))

