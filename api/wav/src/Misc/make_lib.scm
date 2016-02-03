;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/wav/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Wed Jan 27 19:31:29 2016 (serrano)                */
;*    Copyright   :  2001-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __wav_makelib

   (library multimedia)
   
   (cond-expand ((library alsa) (library alsa)))
   (cond-expand ((library pulseaudio) (library pulseaudio)))
   
   (import __wav_wav
	   __wav_decoder)

   (cond-expand ((library alsa) (import __wav_alsadec)))
   (cond-expand ((library pulseaudio) (import __wav_padec)))

   (eval   (export-all)

           (class &wav-error)
           (class wavinfo))

   (cond-expand ((library alsa) (eval (class wav-alsadecoder))))
   (cond-expand ((library pulseaudio) (eval (class wav-pulseaudiodecoder)))))

