;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/src/Llib/mpg123.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 20 14:46:34 2011                          */
;*    Last change :  Fri Jun 22 15:59:08 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    C mpg123 functions                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (extern
    (include "mpg123.h")

    ;; mpg123-handle
    (type $mpg123-handle void* "mpg123_handle *")

    (macro $string-null::string "((char *)0L)")
    (macro $mpg123-handle-nil::$mpg123-handle "(mpg123_handle *)0L")
    
    (macro $mpg123-init::int
       () "mpg123_init")
    (macro $mpg123-exit::void
       () "mpg123_void")
    (macro $mpg123-open-feed::int
       (::$mpg123-handle) "mpg123_open_feed")
    (macro $mpg123-close::int
       (::$mpg123-handle) "mpg123_close")
    (macro $mpg123-delete::void
       (::$mpg123-handle) "mpg123_delete")
    (macro $mpg123-current-decoder::string
       (::$mpg123-handle) "mpg123_current_decoder")
    (macro $mpg123-plain-strerror::string
       (::int) "(char *)mpg123_plain_strerror")
    (macro $mpg123-strerror::string
       (::$mpg123-handle) "(char *)mpg123_strerror")
    (macro $mpg123-length::long
       (::$mpg123-handle) "mpg123_length")
    (macro $mpg123-tpf::double
       (::$mpg123-handle) "mpg123_tpf")
    (macro $mpg123-tell::long
       (::$mpg123-handle) "mpg123_tell")
    (macro $mpg123-tellframe::long
       (::$mpg123-handle) "mpg123_tellframe")
    (macro $mpg123-tell-stream::long
       (::$mpg123-handle) "mpg123_tell_stream")
    (macro $mpg123-timeframe::long
       (::$mpg123-handle ::double) "mpg123_timeframe")
    (macro $mpg123-seek-frame::long
       (::$mpg123-handle ::long ::int) "mpg123_seek_frame")
    (macro $mpg123-seek::long
       (::$mpg123-handle ::long ::int) "mpg123_seek")
    (macro $mpg123-volume::int
       (::$mpg123-handle ::double) "mpg123_volume")
    (macro $mpg123-param::int
       (::$mpg123-handle ::$mpg123-params ::long ::double) "mpg123_param")
    
    ($bgl-mpg123-decode::int
       (::obj ::string ::long ::long ::string ::long)
       "bgl_mpg123_decode")
    ($bgl-mpg123-new::$mpg123-handle
       (::string) "bgl_mpg123_new")
    ($bgl-mpg123-decoders::pair-nil
       () "bgl_mpg123_decoders")
    ($bgl-mpg123-get-format::long
       (::$mpg123-handle) "bgl_mpg123_getformat")
    ($bgl-mpg123-position::long
       (::$mpg123-handle) "bgl_mpg123_position")
    ($bgl-mpg123-info::long
       (::obj) "bgl_mpg123_info")
    ($bgl-mpg123-getvolume::double
       (::$mpg123-handle) "bgl_mpg123_getvolume")
    ($bgl-mpg123-param::obj
       (::$mpg123-handle ::$mpg123-params ::long ::double) "bgl_mpg123_param")
    ($bgl-mpg123-getparam::obj
       (::obj ::$mpg123-params) "bgl_mpg123_getparam")

    (macro $mpg123-new-format::int "MPG123_NEW_FORMAT")
    (macro $mpg123-err::int "MPG123_ERR")
    (macro $mpg123-need-more::int "MPG123_NEED_MORE")
    (macro $mpg123-done::int "MPG123_DONE")
    (macro $mpg123-ok::int "MPG123_OK")

    (macro $mpg123-enc-signed-16::long "MPG123_ENC_SIGNED_16")
    (macro $mpg123-enc-unsigned-16::long "MPG123_ENC_UNSIGNED_16")
    (macro $mpg123-enc-unsigned-8::long "MPG123_ENC_UNSIGNED_8")
    (macro $mpg123-enc-signed-8::long "MPG123_ENC_SIGNED_8")
    (macro $mpg123-enc-alaw-8::long "MPG123_ENC_ALAW_8")
    (macro $mpg123-enc-ulaw-8::long "MPG123_ENC_ULAW_8")
    (macro $mpg123-enc-signed-32::long "MPG123_ENC_SIGNED_32")
    (macro $mpg123-enc-unsigned-32::long "MPG123_ENC_UNSIGNED_32")
;*     (macro $mpg123-enc-signed-24::long "MPG123_ENC_SIGNED_24")      */
;*     (macro $mpg123-enc-unsigned-24::long "MPG123_ENC_UNSIGNED_24")  */
    (macro $mpg123-enc-float-32::long "MPG123_ENC_FLOAT_32")
    (macro $mpg123-enc-float-64::long "MPG123_ENC_FLOAT_64")

    (type $mpg123-params long "enum mpg123_parms")
    (macro $mpg123-verbose::$mpg123-params "MPG123_VERBOSE")
    (macro $mpg123-flags::$mpg123-params "MPG123_FLAGS")
    (macro $mpg123-add-flags::$mpg123-params "MPG123_ADD_FLAGS")
    (macro $mpg123-force-rate::$mpg123-params "MPG123_FORCE_RATE")
    (macro $mpg123-down-sample::$mpg123-params "MPG123_DOWN_SAMPLE")
    (macro $mpg123-rva::$mpg123-params "MPG123_RVA")
    (macro $mpg123-downspeed::$mpg123-params "MPG123_DOWNSPEED")
    (macro $mpg123-upspeed::$mpg123-params "MPG123_UPSPEED")
    (macro $mpg123-start-frame::$mpg123-params "MPG123_START_FRAME")
    (macro $mpg123-decode-frames::$mpg123-params "MPG123_DECODE_FRAMES")
    (macro $mpg123-icy-interval::$mpg123-params "MPG123_ICY_INTERVAL")
    (macro $mpg123-outscale::$mpg123-params "MPG123_OUTSCALE")
    (macro $mpg123-timeout::$mpg123-params "MPG123_TIMEOUT")
    (macro $mpg123-remove-flags::$mpg123-params "MPG123_REMOVE_FLAGS")
    (macro $mpg123-resync-limit::$mpg123-params "MPG123_RESYNC_LIMIT")
    (macro $mpg123-index-size::$mpg123-params "MPG123_INDEX_SIZE")
;*     (macro $mpg123-preframes::$mpg123-params "MPG123_PREFRAMES")    */
;*     (macro $mpg123-feedpool::$mpg123-params "MPG123_FEEDPOOL")      */
;*     (macro $mpg123-feedbuffer::$mpg123-params "MPG123_FEEDBUFFER")        */

    (macro $mpg123-seek-set::int "SEEK_SET")))

