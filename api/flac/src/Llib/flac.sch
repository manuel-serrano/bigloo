;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Llib/flac.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 20 14:46:34 2011                          */
;*    Last change :  Tue Jun 28 16:40:19 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    C flac functions                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (extern
    (include "FLAC/stream_decoder.h")
    (include "bglflac.h")

    ;; flac-decoder
    (type $flac-decoder void* "FLAC__StreamDecoder *")
    (macro $flac-decoder-new::$flac-decoder
       () "FLAC__stream_decoder_new")
    (macro $flac-decoder-delete::void
       (::$flac-decoder) "FLAC__stream_decoder_delete")
    (macro $flac-decoder-set-md5-checking::$flac-bool
       (::$flac-decoder ::$flac-bool) "FLAC__stream_decoder_set_md5_checking")
    (macro $bgl-flac-decoder-init-stream::$flac-decoder-init-status
       (::$flac-decoder ::obj) "bgl_FLAC__stream_decoder_init_stream")

    ;; flac-bool
    (type $flac-bool long "FLAC__bool")
    (macro $flac-true::$flac-bool "true")
    (macro $flac-false::$flac-bool "false")

    (type $flac-decoder-init-status long "FLAC__StreamDecoderInitStatus")
    (macro $flac-decoder-init-status-ok::$flac-decoder-init-status
       "FLAC__STREAM_DECODER_INIT_STATUS_OK")
    (macro $flac-decoder-init-status-unsupported-container::$flac-decoder-init-status
       "FLAC__STREAM_DECODER_INIT_STATUS_UNSUPPORTED-CONTAINER")
    (macro $flac-decoder-init-status-invalid-callbacks::$flac-decoder-init-status
       "FLAC__STREAM_DECODER_INIT_STATUS_INVALID_CALLBACKS")
    (macro $flac-decoder-init-status-allocation-error::$flac-decoder-init-status
       "FLAC__STREAM_DECODER_INIT_STATUS_ALLOCATION_ERROR")
    (macro $flac-decoder-init-status-opening-file::$flac-decoder-init-status
       "FLAC__STREAM_DECODER_INIT_STATUS_OPENING_FILE")
    (macro $flac-decoder-init-status-initialized::$flac-decoder-init-status
       "FLAC__STREAM_DECODER_INIT_STATUS_INITIALIZED")
    ))

