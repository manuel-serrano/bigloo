;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/wav/src/Llib/wav_padec.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 21 08:15:23 2013                          */
;*    Last change :  Thu Jan 28 15:03:58 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WAV music decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __wav_padec
   
   (cond-expand
      ((library pulseaudio)
       (library pulseaudio)))
   
   (library multimedia)
   
   (import __wav_wav
	   __wav_decoder)
   
   (cond-expand
      ((library pulseaudio)
       (export (class wav-pulseaudiodecoder::wavmusicdecoder)))))

;*---------------------------------------------------------------------*/
;*    pulseaudio dependency                                            */
;*---------------------------------------------------------------------*/
(cond-expand
   ((library pulseaudio)
;;; compile only if pulseaudio available
    
;*---------------------------------------------------------------------*/
;*    musicdecoder-hwparams-set! ::was-alsadecoder ...                 */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-hwparams-set! dec::wav-pulseaudiodecoder am::music buffer::musicbuffer)
   (with-access::pulseaudiomusic am (%status simple name)
      (with-access::wav-pulseaudiodecoder dec (%header %position)
	 (with-access::wavinfo %header (channels audiofmt bps samplerate)
	    (pulseaudiomusic-simple-set! am
	       (instantiate::pulseaudio-simple
		  (name name)
		  (bps bps)
		  (rate samplerate)
		  (channels channels)))))))

;*---------------------------------------------------------------------*/
;*    pulseaudio dependency                                            */
;*---------------------------------------------------------------------*/
))
