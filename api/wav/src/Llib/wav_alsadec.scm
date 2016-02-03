;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/wav/src/Llib/wav_alsadec.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 21 08:15:23 2013                          */
;*    Last change :  Thu Jan 28 15:18:27 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WAV music decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __wav_alsadec

   (cond-expand
      ((library alsa)
       (library alsa)))

   (library multimedia)
   
   (import __wav_wav
	   __wav_decoder)

   (cond-expand
      ((library alsa)
       (export (class wav-alsadecoder::wavmusicdecoder)))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
(cond-expand
   ((library alsa)
;;; compile only if alsa available

;*---------------------------------------------------------------------*/
;*    musicdecoder-hwparams-set! ::was-alsadecoder ...                 */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-hwparams-set! dec::wav-alsadecoder am::music buffer::musicbuffer)
   (with-access::alsamusic am (%status pcm)
      (with-access::wav-alsadecoder dec (%header
					   buffer-time-near
					   buffer-size-near-ratio
					   period-size-near-ratio)
	 (with-access::wavinfo %header (channels audiofmt bps samplerate)
	    (let ((encoding (case bps
			       ((8) 'u8)
			       ((16) 's16)
			       ((24) 's24-3le)
			       ((32) 's32))))
	       (alsa-snd-pcm-hw-set-params! pcm
		  :access 'rw-interleaved
		  :format encoding
		  :channels channels
		  :rate-near samplerate
		  :buffer-size-near-ratio buffer-size-near-ratio
		  :period-size-near-ratio period-size-near-ratio)
 	       (alsa-snd-pcm-sw-set-params! pcm
		  :start-threshold 1
		  :avail-min 1))))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
))
