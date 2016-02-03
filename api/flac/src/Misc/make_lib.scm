;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Wed Jan 27 15:30:44 2016 (serrano)                */
;*    Copyright   :  2001-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_makelib

   (library multimedia)
   
   (cond-expand ((library alsa) (library alsa)))
   
   (import __flac_flac
	   __flac_decoder
	   __flac_alsadec
	   __flac_padec)

   (cond-expand ((library alsa) (import __flac_alsadec)))

   (eval   (export-all)

           (class &flac-error)
           (class flac-decoder)
	   (class flacdec))

   (cond-expand
      ((library alsa)
       (eval (class flac-alsadecoder))))

   (cond-expand
      ((library pulseaudio)
       (eval (class flac-pulseaudiodecoder)))))

