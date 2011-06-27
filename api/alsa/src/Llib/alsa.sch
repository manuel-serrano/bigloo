;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/alsa.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 17:42:08 2011                          */
;*    Last change :  Sat Jun 25 07:12:50 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Direct use of ALSA types and functions                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (extern
      (include "alsa/asoundlib.h")
      ;;(include "bglalsa_config.h")
      (include "bglalsa.h")

      ;; misc
      (macro $snd-lib-version::int "SND_LIB_VERSION")
      
      ;; snd-pcm
      (type $snd-pcm void* "snd_pcm_t *")
      (infix macro $snd-pcm-nil::$snd-pcm () "0L")

      (macro $snd-pcm-name::string
	 (::$snd-pcm) "snd_pcm_name")
      (macro $snd-pcm-close::int
	 (::$snd-pcm) "snd_pcm_close")
      (macro $snd-pcm-get-state::$snd-pcm-state
	 (::$snd-pcm) "snd_pcm_state")
      (macro $snd-pcm-set-params!::int
	 (::$snd-pcm ::$snd-pcm-format ::$snd-pcm-access ::int ::int ::int ::int)
	 "snd_pcm_set_params")
      (macro $snd-pcm-writei::$snd-pcm-sframes
	 (::$snd-pcm ::string ::$snd-pcm-uframes)
	 "snd_pcm_writei")

      ;; snd-pcm-stream
      (type $snd-pcm-stream long "snd_pcm_stream_t")
      (macro $snd-pcm-stream-playback::$snd-pcm-stream
	 "SND_PCM_STREAM_PLAYBACK")
      (macro $snd-pcm-stream-capture::$snd-pcm-stream
	 "SND_PCM_STREAM_CAPTURE")

      ;; snd-pcm-state
      (type $snd-pcm-state long "snd_pcm_state_t")
      (macro $snd-pcm-state-open::$snd-pcm-state
	 "SND_PCM_STATE_OPEN")
      (macro $snd-pcm-state-setup::$snd-pcm-state
	 "SND_PCM_STATE_SETUP")
      (macro $snd-pcm-state-prepared::$snd-pcm-state
	 "SND_PCM_STATE_PREPARED")
      (macro $snd-pcm-state-running::$snd-pcm-state
	 "SND_PCM_STATE_RUNNING")
      (macro $snd-pcm-state-xrun::$snd-pcm-state
	 "SND_PCM_STATE_XRUN")
      (macro $snd-pcm-state-draining::$snd-pcm-state
	 "SND_PCM_STATE_DRAINING")
      (macro $snd-pcm-state-paused::$snd-pcm-state
	 "SND_PCM_STATE_PAUSED")
      (macro $snd-pcm-state-suspended::$snd-pcm-state
	 "SND_PCM_STATE_SUSPENDED")
      (macro $snd-pcm-state-disconnected::$snd-pcm-state
	 "SND_PCM_STATE_DISCONNECTED")

      ;; snd-pcm-format
      (type $snd-pcm-format long "snd_pcm_format_t")
      (macro $snd-pcm-format-unknown::$snd-pcm-format
	 "SND_PCM_FORMAT_UNKNOWN")
      (macro $snd-pcm-format-s8::$snd-pcm-format
	 "SND_PCM_FORMAT_S8")
      (macro $snd-pcm-format-u8::$snd-pcm-format
	 "SND_PCM_FORMAT_U8")
      (macro $snd-pcm-format-s16-le::$snd-pcm-format
	 "SND_PCM_FORMAT_S16_LE")
      (macro $snd-pcm-format-s16-be::$snd-pcm-format
	 "SND_PCM_FORMAT_S16_BE")
      (macro $snd-pcm-format-u16-le::$snd-pcm-format
	 "SND_PCM_FORMAT_U16_LE")
      (macro $snd-pcm-format-u16-be::$snd-pcm-format
	 "SND_PCM_FORMAT_U16_BE")
      (macro $snd-pcm-format-s24-le::$snd-pcm-format
	 "SND_PCM_FORMAT_S24_LE")
      (macro $snd-pcm-format-s24-be::$snd-pcm-format
	 "SND_PCM_FORMAT_S24_BE")
      (macro $snd-pcm-format-u24-le::$snd-pcm-format
	 "SND_PCM_FORMAT_U24_LE")
      (macro $snd-pcm-format-u24-be::$snd-pcm-format
	 "SND_PCM_FORMAT_U24_BE")
      (macro $snd-pcm-format-s32-le::$snd-pcm-format
	 "SND_PCM_FORMAT_S32_LE")
      (macro $snd-pcm-format-s32-be::$snd-pcm-format
	 "SND_PCM_FORMAT_S32_BE")
      (macro $snd-pcm-format-u32-le::$snd-pcm-format
	 "SND_PCM_FORMAT_U32_LE")
      (macro $snd-pcm-format-u32-be::$snd-pcm-format
	 "SND_PCM_FORMAT_U32_BE")
      (macro $snd-pcm-format-float-le::$snd-pcm-format
	 "SND_PCM_FORMAT_FLOAT_LE")
      (macro $snd-pcm-format-float-be::$snd-pcm-format
	 "SND_PCM_FORMAT_FLOAT_BE")
      (macro $snd-pcm-format-float64-le::$snd-pcm-format
	 "SND_PCM_FORMAT_FLOAT64_LE")
      (macro $snd-pcm-format-float64-be::$snd-pcm-format
	 "SND_PCM_FORMAT_FLOAT64_BE")
      (macro $snd-pcm-format-iec958-subframe-le::$snd-pcm-format
	 "SND_PCM_FORMAT_IEC958_SUBFRAME_LE")
      (macro $snd-pcm-format-iec958-subframe-be::$snd-pcm-format
	 "SND_PCM_FORMAT_IEC958_SUBFRAME_BE")
      (macro $snd-pcm-format-mu-law::$snd-pcm-format
	 "SND_PCM_FORMAT_MU_LAW")
      (macro $snd-pcm-format-a-law::$snd-pcm-format
	 "SND_PCM_FORMAT_A_LAW")
      (macro $snd-pcm-format-ima-adpcm::$snd-pcm-format
	 "SND_PCM_FORMAT_IMA_ADPCM")
      (macro $snd-pcm-format-mpeg::$snd-pcm-format
	 "SND_PCM_FORMAT_MPEG")
      (macro $snd-pcm-format-gsm::$snd-pcm-format
	 "SND_PCM_FORMAT_GSM")
      (macro $snd-pcm-format-special::$snd-pcm-format
	 "SND_PCM_FORMAT_SPECIAL")
      (macro $snd-pcm-format-s24-3le::$snd-pcm-format
	 "SND_PCM_FORMAT_S24_3LE")
      (macro $snd-pcm-format-s24-3be::$snd-pcm-format
	 "SND_PCM_FORMAT_S24_3BE")
      (macro $snd-pcm-format-u24-3le::$snd-pcm-format
	 "SND_PCM_FORMAT_U24_3LE")
      (macro $snd-pcm-format-u24-3be::$snd-pcm-format
	 "SND_PCM_FORMAT_U24_3BE")
      (macro $snd-pcm-format-s20-3le::$snd-pcm-format
	 "SND_PCM_FORMAT_S20_3LE")
      (macro $snd-pcm-format-s20-3be::$snd-pcm-format
	 "SND_PCM_FORMAT_S20_3BE")
      (macro $snd-pcm-format-u20-3le::$snd-pcm-format
	 "SND_PCM_FORMAT_U20_3LE")
      (macro $snd-pcm-format-u20-3be::$snd-pcm-format
	 "SND_PCM_FORMAT_U20_3BE")
      (macro $snd-pcm-format-s18-3le::$snd-pcm-format
	 "SND_PCM_FORMAT_S18_3LE")
      (macro $snd-pcm-format-s18-3be::$snd-pcm-format
	 "SND_PCM_FORMAT_S18_3BE")
      (macro $snd-pcm-format-u18-3le::$snd-pcm-format
	 "SND_PCM_FORMAT_U18_3LE")
      (macro $snd-pcm-format-u18-3be::$snd-pcm-format
	 "SND_PCM_FORMAT_U18_3BE")
      (macro $snd-pcm-format-s16::$snd-pcm-format
	 "SND_PCM_FORMAT_S16")
      (macro $snd-pcm-format-u16::$snd-pcm-format
	 "SND_PCM_FORMAT_U16")
      (macro $snd-pcm-format-s24::$snd-pcm-format
	 "SND_PCM_FORMAT_S24")
      (macro $snd-pcm-format-u24::$snd-pcm-format
	 "SND_PCM_FORMAT_U24")
      (macro $snd-pcm-format-s32::$snd-pcm-format
	 "SND_PCM_FORMAT_S32")
      (macro $snd-pcm-format-u32::$snd-pcm-format
	 "SND_PCM_FORMAT_U32")
      (macro $snd-pcm-format-float::$snd-pcm-format
	 "SND_PCM_FORMAT_FLOAT")
      (macro $snd-pcm-format-float64::$snd-pcm-format
	 "SND_PCM_FORMAT_FLOAT64")
      (macro $snd-pcm-format-iec958-subframe::$snd-pcm-format
	 "SND_PCM_FORMAT_IEC958_SUBFRAME")
	 
      ;; snd-pcm-access
      (type $snd-pcm-access long "snd_pcm_access_t")
      (macro $snd-pcm-access-mmap-interleaved::$snd-pcm-access
	 "SND_PCM_ACCESS_MMAP_INTERLEAVED")
      (macro $snd-pcm-access-mmap-noninterleaved::$snd-pcm-access
	 "SND_PCM_ACCESS_MMAP_NONINTERLEAVED")
      (macro $snd-pcm-access-mmap-complex::$snd-pcm-access
	 "SND_PCM_ACCESS_MMAP_COMPLEX")
      (macro $snd-pcm-access-rw-interleaved::$snd-pcm-access
	 "SND_PCM_ACCESS_RW_INTERLEAVED")
      (macro $snd-pcm-access-rw-noninterleaved::$snd-pcm-access
	 "SND_PCM_ACCESS_RW_NONINTERLEAVED")

      ;; snd-pcm-sframes
      (type $snd-pcm-sframes long "snd_pcm_sframes_t")

      ;; snd-pcm-uframes
      (type $snd-pcm-uframes ulong "snd_pcm_uframes_t")
      
      ;; snd-pcm-mode
      (macro $snd-pcm-nonblock::int
	 "SND_PCM_NONBLOCK")
      (macro $snd-pcm-async::int
	 "SND_PCM_ASYNC")

      ;; snd-pcm-hw-params
      (type $snd-pcm-hw-params void* "snd_pcm_hw_params_t *")

      (macro $bgl-snd-pcm-hw-params-malloc::$snd-pcm-hw-params
	 () "bgl_snd_pcm_hw_params_malloc")
      (macro $bgl-snd-pcm-hw-params-free::void
	 (::$snd-pcm-hw-params) "bgl_snd_pcm_hw_params_free")
      (macro $snd-pcm-hw-params-any::int
	 (::$snd-pcm ::$snd-pcm-hw-params) "snd_pcm_hw_params_any")
      (macro $snd-pcm-hw-params-set-access!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::$snd-pcm-access)
	 "snd_pcm_hw_params_set_access")
      (macro $snd-pcm-hw-params-set-format!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::$snd-pcm-format)
	 "snd_pcm_hw_params_set_format")
      (macro $snd-pcm-hw-params-set-channels!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "snd_pcm_hw_params_set_channels")
      (macro $bgl-snd-pcm-hw-params-set-rate-near!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "bgl_snd_pcm_hw_params_set_rate_near")
      (macro $bgl-snd-pcm-hw-params-set-buffer-size-near!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "bgl_snd_pcm_hw_params_set_buffer_size_near")
      (macro $bgl-snd-pcm-hw-params-set-period-size-near!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "bgl_snd_pcm_hw_params_set_period_size_near")
      
      ;; snd-pcm-sw-params
      (type $snd-pcm-sw-params void* "snd_pcm_sw_params_t *")

      (macro $bgl-snd-pcm-sw-params-malloc::$snd-pcm-sw-params
	 () "bgl_snd_pcm_sw_params_malloc")
      (macro $bgl-snd-pcm-sw-params-free::void
	 (::$snd-pcm-sw-params) "bgl_snd_pcm_sw_params_free")
      (macro $snd-pcm-sw-params-set-start-threshold!::int
	 (::$snd-pcm ::$snd-pcm-sw-params ::$snd-pcm-uframes)
	 "snd_pcm_sw_params_set_start_threshold")
      (macro $snd-pcm-sw-params-set-avail-min!::int
	 (::$snd-pcm ::$snd-pcm-sw-params ::$snd-pcm-uframes)
	 "snd_pcm_sw_params_set_avail_min")
      (macro $snd-pcm-sw-params::int
	 (::$snd-pcm ::$snd-pcm-sw-params) "snd_pcm_sw_params")

      ;; helper functions
      (macro $snd-pcm-bytes-to-frames::$snd-pcm-sframes
	 (::$snd-pcm ::long) "snd_pcm_bytes_to_frames")
      (macro $snd-pcm-frames-to-bytes::long
	 (::$snd-pcm ::$snd-pcm-sframes) "snd_pcm_frames_to_bytes")
      (macro $bgl-sframes->uframes::$snd-pcm-uframes
	 (::$snd-pcm-sframes) "(snd_pcm_uframes)")
      (macro $bgl-uframes->sframes::$snd-pcm-sframes
	 (::$snd-pcm-uframes) "(snd_pcm_sframes)")

      ;; bigloo functions
      (macro $bgl-snd-pcm-write::long
	 (::obj ::string ::long)
	 "bgl_snd_pcm_write")
      (macro $bgl-snd-pcm-flush::void
	 (::obj)
	 "bgl_snd_pcm_flush")
      
      ;; snd-error
      (macro $snd-strerror::string
	 (::int) "snd_strerror")))
