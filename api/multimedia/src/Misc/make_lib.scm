;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Misc/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Wed Jan 25 13:44:24 2012 (serrano)                */
;*    Copyright   :  2001-12 Manuel Serrano                            */
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
	   __multimedia-mpd
	   __multimedia-music
	   __multimedia-musicproc
	   __multimedia-mpc
	   __multimedia-mpg123
	   __multimedia-mplayer
	   __multimedia-mixer
	   __multimedia-soundcard
	   __multimedia-color)

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
	   (class mpg123)
	   (class mplayer)
	   (class soundcard)
 	   (class mpd-database))

   (export (%multimedia-eval)))

;*---------------------------------------------------------------------*/
;*    %multimedia-eval ...                                             */
;*---------------------------------------------------------------------*/
(define (%multimedia-eval)
   #unspecified)

