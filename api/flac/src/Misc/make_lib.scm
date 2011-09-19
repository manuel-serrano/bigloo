;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sun Sep 18 19:25:22 2011 (serrano)                */
;*    Copyright   :  2001-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_makelib

   (library multimedia)
   
   (cond-expand ((library alsa) (library alsa)))
   
   (import __flac_flac)

   (cond-expand ((library alsa) (import __flac_alsadec)))

   (eval   (export-all)

           (class &flac-error)
           (class flac-decoder))

   (cond-expand ((library alsa) (eval (class flac-alsadecoder)))))

