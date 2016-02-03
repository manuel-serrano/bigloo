;*=====================================================================*/
;*    .../project/bigloo/api/mpg123/src/Llib/mpg123_alsadec.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 17 07:53:28 2011                          */
;*    Last change :  Wed Jan 27 19:27:53 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MPG123 Alsa decoder                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_alsadec

   (library multimedia)
   
   (cond-expand
      ((library alsa)
       (library alsa)))
   
   (import __mpg123_mpg123
	   __mpg123_decoder)

   (cond-expand
      ((library alsa)
       (export (class mpg123-alsadecoder::mpg123-decoder)))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
(cond-expand
   ((library alsa)
;;; compile only if alsa available

;*---------------------------------------------------------------------*/
;*    musicdecoder-hwparams-set! ...                                   */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-hwparams-set! dec::mpg123-decoder am buffer)
   (with-access::alsamusic am (%status pcm)
      (with-access::mpg123-decoder dec (buffer-time-near
					  buffer-size-near-ratio
					  period-size-near-ratio
					  (rate %rate)
					  (channels %channels)
					  (encoding %encoding))
	 (alsa-snd-pcm-reopen pcm)
	 (alsa-snd-pcm-hw-set-params! pcm
	    :access 'rw-interleaved
	    :format encoding
	    :channels channels
	    :rate-near rate
	    :buffer-size-near-ratio buffer-size-near-ratio
	    :period-size-near-ratio period-size-near-ratio)
	 (alsa-snd-pcm-sw-set-params! pcm
	    :start-threshold 1
	    :avail-min 1))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
))
