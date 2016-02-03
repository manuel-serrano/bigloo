;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pulseaudio/src/Misc/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Jan 26 17:29:35 2016 (serrano)                */
;*    Copyright   :  2001-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pulseaudio_makelib
   
   (library multimedia)
   
   (import  __pulseaudio_pulseaudio
	    __pulseaudio_music
	    __pulseaudio_simple)

   (eval    (export-all)
      
            (class pulseaudiomusic)
            (class pulseaudio-simple)))
