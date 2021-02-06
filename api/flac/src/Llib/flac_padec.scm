;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Llib/flac_padec.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 18 19:18:08 2011                          */
;*    Last change :  Wed Jan 27 17:29:55 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    FLAC PulseAudio decoder                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_padec

   (library multimedia)
   
   (cond-expand
      ((library pulseaudio)
       (library pulseaudio)))
   
   (import __flac_flac
	   __flac_decoder)
   
   (static (class flacpadec::flacdec))
   
   ;;; Get pa_strerror prototype for inline function pulseaudio-simple-flush
   (cond-expand
     ((library pulseaudio)
      (extern (include "pulse/error.h"))))
   
   (cond-expand
      ((library pulseaudio)
       (export (class flac-pulseaudiodecoder::flacmusicdecoder)))))

;*---------------------------------------------------------------------*/
;*    pulseaudio dependency                                            */
;*---------------------------------------------------------------------*/
(cond-expand
   ((library pulseaudio)
;;; compile only if pulseaudio available

;*---------------------------------------------------------------------*/
;*    musicdecoder-init ::flac-pulseaudiodecoder ...                   */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-init dec::flac-pulseaudiodecoder)
   (call-next-method)
   (with-access::flac-pulseaudiodecoder dec (%flac)
      (unless %flac (set! %flac (instantiate::flacpadec)))))
    
;*---------------------------------------------------------------------*/
;*    musicdecoder-decode ::flac-pulseaudiodecoder ...                 */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-decode dec::flac-pulseaudiodecoder
		  am::musicbuf
		  buffer::musicbuffer)
   (with-access::flac-pulseaudiodecoder dec (%flac)
      (unless %flac (set! %flac (instantiate::flacpadec)))
      (with-access::flacpadec %flac (%buffer %music %decoder)
	 (set! %buffer buffer)
	 (set! %music am)
	 (set! %decoder dec)
	 (with-access::pulseaudiomusic am (%amutex %status simple)
	   (with-access::musicbuffer buffer (url)
	      (unwind-protect
		 (with-access::pulseaudio-simple simple (bps)
		    (if (<=fx bps 16)
			(flac-decoder-decode16 %flac)
			(flac-decoder-decode %flac)))
		 (with-access::musicbuffer %buffer (%eof)
		    (pulseaudio-simple-flush simple)
		    (music-state-set! am (if %eof 'ended 'stop)))))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-metadata ::flacpadec ...                            */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-metadata o::flacpadec total rate channels bps)
   (with-access::flacpadec o (%music)
      (with-access::pulseaudiomusic %music (name)
	 (pulseaudiomusic-simple-set! %music
	    (instantiate::pulseaudio-simple
	       (name name)
	       (rate rate)
	       (channels channels)
	       (bps bps))))))

;*---------------------------------------------------------------------*/
;*    pulseaudio dependency                                            */
;*---------------------------------------------------------------------*/
))
