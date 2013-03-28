;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/wav/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sat Feb 23 19:41:00 2013 (serrano)                */
;*    Copyright   :  2001-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __wav_makelib

   (library multimedia)
   
   (cond-expand ((library alsa) (library alsa)))
   
   (import __wav_wav)

   (cond-expand ((library alsa) (import __wav_alsadec)))

   (eval   (export-all)

           (class &wav-error)
           (class wavinfo))

   (cond-expand ((library alsa) (eval (class wav-alsadecoder)))))

