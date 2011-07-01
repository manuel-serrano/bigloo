;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/src/Llib/mpg123.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 20 14:46:34 2011                          */
;*    Last change :  Thu Jun 30 17:45:48 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
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
       (::int) "mpg123_plain_strerror")
    (macro $mpg123-length::long
       (::$mpg123-handle) "mpg123_length")
    (macro $mpg123-tpf::double
       (::$mpg123-handle) "mpg123_tpf")
    (macro $mpg123-tell::long
       (::$mpg123-handle) "mpg123_tell")
    (macro $mpg123-timeframe::long
       (::$mpg123-handle ::double) "mpg123_timeframe")
    (macro $mpg123-seek-frame::long
       (::$mpg123-handle ::long ::int) "mpg123_seek_frame")
    (macro $mpg123-volume::int
       (::$mpg123-handle ::double) "mpg123_volume")
    
    ($bgl-mpg123-decode::int
       (::$mpg123-handle ::string ::long ::long ::string ::long)
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
       (::$mpg123-handle) "bgl_mpg123_info")
    ($bgl-mpg123-getvolume::long
       (::$mpg123-handle) "bgl_mpg123_getvolume")

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
    (macro $mpg123-enc-signed-24::long "MPG123_ENC_SIGNED_24")
    (macro $mpg123-enc-unsigned-24::long "MPG123_ENC_UNSIGNED_24")
    (macro $mpg123-enc-float-32::long "MPG123_ENC_FLOAT_32")
    (macro $mpg123-enc-float-64::long "MPG123_ENC_FLOAT_64")

    (macro $mpg123-seek-set::int "SEEK_SET")))

