;*=====================================================================*/
;*    .../prgm/project/bigloo/api/mpg123/src/Llib/mpg123_padec.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 17 07:53:28 2011                          */
;*    Last change :  Thu Jan 28 05:43:48 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MPG123 PulseAudio decoder                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_padec

   (library multimedia)
   
   (cond-expand
      ((library pulseaudio)
       (library pulseaudio)))
   
   (import __mpg123_mpg123
	   __mpg123_decoder)

   (cond-expand
      ((library pulseaudio)
       (export (class mpg123-pulseaudiodecoder::mpg123-decoder)))))

;*---------------------------------------------------------------------*/
;*    pulseaudio dependency                                            */
;*---------------------------------------------------------------------*/
(cond-expand
   ((library pulseaudio)
;;; compile only if pulseaudio available

;*---------------------------------------------------------------------*/
;*    musicdecoder-hwparams-set! ...                                   */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-hwparams-set! dec::mpg123-pulseaudiodecoder am buffer)
   (with-access::pulseaudiomusic am (%status simple name)
      (with-access::mpg123-pulseaudiodecoder dec (buffer-time-near
						    buffer-size-near-ratio
						    period-size-near-ratio
						    (rate %rate)
						    (channels %channels)
						    (encoding %encoding))
	 (pulseaudiomusic-simple-set! am
	    (instantiate::pulseaudio-simple
	       (name name)
	       (rate rate)
	       (channels channels)
	       (format encoding))))))

;*---------------------------------------------------------------------*/
;*    pulseaudio dependency                                            */
;*---------------------------------------------------------------------*/
))
