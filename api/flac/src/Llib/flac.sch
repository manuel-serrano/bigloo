;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Llib/flac.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 20 14:46:34 2011                          */
;*    Last change :  Sat Apr  4 08:34:03 2015 (serrano)                */
;*    Copyright   :  2011-15 Manuel Serrano                            */
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
    
    ;; misc
    (macro $flac-max-block-size::long "FLAC__MAX_BLOCK_SIZE")
    (macro $flac-max-channels::long "FLAC__MAX_CHANNELS")
    (macro $flac-make-custom::custom (::long) "create_custom")
    
    ;; flac-decoder
    (type $flac-decoder void* "FLAC__StreamDecoder *")
    (macro $flac-decoder-new::$flac-decoder
       () "FLAC__stream_decoder_new")
    (macro $flac-decoder-delete::void
       (::$flac-decoder) "FLAC__stream_decoder_delete")
    (macro $flac-decoder-set-md5-checking::$flac-bool
       (::$flac-decoder ::$flac-bool) "FLAC__stream_decoder_set_md5_checking")
    (macro $flac-decoder-process-until-end-of-stream::$flac-bool
       (::$flac-decoder) "FLAC__stream_decoder_process_until_end_of_stream")
    (macro $flac-decoder-finish::$flac-bool
       (::$flac-decoder) "FLAC__stream_decoder_finish")
    (macro $flac-decoder-flush::$flac-bool
       (::$flac-decoder) "FLAC__stream_decoder_flush")
    (macro $flac-decoder-reset::$flac-bool
       (::$flac-decoder) "FLAC__stream_decoder_reset")
    (macro $flac-decoder-seek-absolute::$flac-bool
       (::$flac-decoder ::ullong) "FLAC__stream_decoder_seek_absolute")
    (macro $bgl-flac-decoder-init-stream::$flac-decoder-init-status
       (::$flac-decoder ::obj) "bgl_FLAC__stream_decoder_init_stream")
    (macro $bgl-flac-decoder-init-stream16::$flac-decoder-init-status
       (::$flac-decoder ::obj) "bgl_FLAC__stream_decoder_init_stream16")
    (macro $flac-decoder-get-state::$flac-decoder-state
       (::$flac-decoder) "FLAC__stream_decoder_get_state")
    (macro $flac-decoder-get-bits-per-sample::ulong
       (::$flac-decoder) "FLAC__stream_decoder_get_bits_per_sample")
    (macro $flac-decoder-get-sample-rate::ulong
       (::$flac-decoder) "FLAC__stream_decoder_get_sample_rate")
    (macro $flac-decoder-get-total-samples::llong
       (::$flac-decoder) "FLAC__stream_decoder_get_total_samples")
    (macro $flac-decoder-get-blocksize::ulong
       (::$flac-decoder) "FLAC__stream_decoder_get_blocksize")
    
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
    
    (type $flac-decoder-state long "FLAC__StreamDecoderState")
    (macro $flac-decoder-search-for-metata::$flac-decoder-state
       "FLAC__STREAM_DECODER_SEARCH_FOR_METADATA")
    (macro $flac-stream-decoder-read-metadata::$flac-decoder-state
       "FLAC__STREAM_DECODER_READ_METADATA")
    (macro $flac-stream-decoder-search-for-frame-sync::$flac-decoder-state
       "FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC")
    (macro $flac-stream-decoder-read-frame::$flac-decoder-state
       "FLAC__STREAM_DECODER_READ_FRAME")
    (macro $flac-stream-decoder-end-of-stream::$flac-decoder-state
       "FLAC__STREAM_DECODER_END_OF_STREAM")
    (macro $flac-STREAM-DECODER-OGG-ERROR::$flac-decoder-state
       "FLAC__STREAM_DECODER_OGG_ERROR")
    (macro $flac-stream-decoder-seek-error::$flac-decoder-state
       "FLAC__STREAM_DECODER_SEEK_ERROR")
    (macro $flac-stream-decoder-aborted::$flac-decoder-state
       "FLAC__STREAM_DECODER_ABORTED")
    (macro $flac-stream-decoder-memory-allocation-error::$flac-decoder-state
       "FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR")
    (macro $flac-stream-decoder-uninitialized::$flac-decoder-state
       "FLAC__STREAM_DECODER_UNINITIALIZED")
    
    (type $flac-decoder-error-status long "FLAC__StreamDecoderErrorStatus")
    (macro $flac-stream-decoder-error-status-lost-sync::$flac-decoder-error-status
       "FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC")
    (macro $flac-stream-decoder-error-status-bad-header::$flac-decoder-error-status
       "FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER")
    (macro $flac-stream-decoder-error-status-frame-crc-mismatch::$flac-decoder-error-status
       "FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH")
    (macro $flac-stream-decoder-error-status-unparseable-stream::$flac-decoder-error-status
       "FLAC__STREAM_DECODER_ERROR_STATUS_UNPARSEABLE_STREAM")))

