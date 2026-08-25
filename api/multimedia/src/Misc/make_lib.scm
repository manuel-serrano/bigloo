;*=====================================================================*/
;*    .../bigloo/5.0.x/api/multimedia/src/Misc/make_lib.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Aug 25 22:56:43 2026 (serrano)                */
;*    Copyright   :  2001-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-makelib

   (import __multimedia-exif
	   __multimedia-m3u
	   __multimedia-id3
	   __multimedia-mp3
	   __multimedia-flac
	   __multimedia-mpd
	   __multimedia-music
	   __multimedia-musicproc
	   __multimedia-musicbuf
	   __multimedia-mpc
	   __multimedia-mpg123
	   __multimedia-mplayer
	   __multimedia-mixer
	   __multimedia-soundcard
	   __multimedia-color
	   __multimedia-midi
           __multimedia-jpeg)

   (eval   (export-all)
	   (class exif)
	   (class musictag)
	   (class id3)
	   (class vorbis)
	   (class mp3frame)
	   (class musicinfo)
	   (class mixer)
	   (class music)
	   (class musicstatus)
	   (class mpc)
	   (class musicproc)
	   (class musicbuf)
	   (class musicbuffer)
	   (class musicportbuffer)
	   (class musicmmapbuffer)
	   (class musicdecoder)
	   (class mpg123)
	   (class mplayer)
	   (class soundcard)
 	   (class mpd-database)
	   (class midiscore)
	   (class miditrack)
	   (class midiplayer)
	   (class midireader)
	   (class midisink))

   (export (%multimedia-eval)))

;*---------------------------------------------------------------------*/
;*    %multimedia-eval ...                                             */
;*---------------------------------------------------------------------*/
(define (%multimedia-eval)
   #unspecified)

