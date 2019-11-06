;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/alsa/src/Llib/alsa.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 17:42:08 2011                          */
;*    Last change :  Thu Mar  7 14:26:32 2019 (serrano)                */
;*    Copyright   :  2011-19 Manuel Serrano                            */
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
      (macro $snd-asoundlib-version::string () "(char *)snd_asoundlib_version")

      ;; snd-card
      (macro $snd-card-load::bool
	 (::int) "snd_card_load")
      (macro $bgl-snd-card-get-name::string
	 (::int) "bgl_snd_card_get_name")
      (macro $bgl-snd-card-get-longname::string (::int)
	     "bgl_snd_card_get_longname")
      (macro $bgl-snd-devices-list::pair-nil (::string)
	     "bgl_snd_devices_list")
      
      ;; snd-pcm
      (type $snd-pcm void* "snd_pcm_t *")
      (infix macro $snd-pcm-nil::$snd-pcm () "0L")
      (infix macro $snd-pcm-nil?::bool (::$snd-pcm) " == 0L")

      (macro $bgl-snd-pcm-open::int
	 (::obj ::string ::$snd-pcm-stream ::int)
	 "bgl_snd_pcm_open")
      (macro $bgl-snd-pcm-reopen::int
	 (::obj ::string ::$snd-pcm-stream ::int)
	 "bgl_snd_pcm_reopen")
      (macro $snd-pcm-name::string
	 (::$snd-pcm) "(char *)snd_pcm_name")
      (macro $bgl-snd-pcm-close::int
	 (::obj) "bgl_snd_pcm_close")
      (macro $snd-pcm-hw-free!::int
	 (::$snd-pcm) "snd_pcm_hw_free")
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
      (macro $snd-pcm-pause::int
	 (::$snd-pcm ::int)
	 "snd_pcm_pause")
      (macro $snd-pcm-wait::int
	 (::$snd-pcm ::int) "snd_pcm_wait")
      (macro $snd-pcm-drop::int
	 (::$snd-pcm) "snd_pcm_drop")
      (macro $snd-pcm-drain::int
	 (::$snd-pcm) "snd_pcm_drain")
      (macro $snd-pcm-recover::int
	 (::$snd-pcm ::int ::int) "snd_pcm_recover")
      (macro $snd-pcm-reset::int
	 (::$snd-pcm) "snd_pcm_reset")
      (macro $snd-pcm-prepare::int
	 (::$snd-pcm) "snd_pcm_prepare")
      (macro $snd-pcm-start::int
	 (::$snd-pcm) "snd_pcm_start")
      (macro $snd-pcm-avail::$snd-pcm-sframes
	 (::$snd-pcm) "snd_pcm_avail")
      (macro $snd-pcm-avail-update::$snd-pcm-sframes
	 (::$snd-pcm) "snd_pcm_avail_update")
      (macro $bgl-snd-pcm-write::long
	 (::obj ::string ::long)
	 "bgl_snd_pcm_write")
      (macro $bgl-snd-pcm-flush::void
	 (::obj)
	 "bgl_snd_pcm_flush")
      
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
      (macro $snd-pcm-hw-params::int
	 (::$snd-pcm ::$snd-pcm-hw-params) "snd_pcm_hw_params")
      (macro $snd-pcm-hw-params-any::int
	 (::$snd-pcm ::$snd-pcm-hw-params) "snd_pcm_hw_params_any")
      (macro $snd-pcm-hw-params-current::int
	 (::$snd-pcm ::$snd-pcm-hw-params) "snd_pcm_hw_params_current")
      (macro $snd-pcm-hw-params-set-access!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::$snd-pcm-access)
	 "snd_pcm_hw_params_set_access")
      (macro $snd-pcm-hw-params-test-access?::bool
	 (::$snd-pcm ::$snd-pcm-hw-params ::$snd-pcm-access)
	 "snd_pcm_hw_params_test_access")
      (macro $snd-pcm-hw-params-set-format!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::$snd-pcm-format)
	 "snd_pcm_hw_params_set_format")
      (macro $snd-pcm-hw-params-test-format?::bool
	 (::$snd-pcm ::$snd-pcm-hw-params ::$snd-pcm-format)
	 "snd_pcm_hw_params_test_format")
      (macro $snd-pcm-hw-params-set-channels!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "snd_pcm_hw_params_set_channels")
      (macro $snd-pcm-hw-params-test-channels?::bool
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "snd_pcm_hw_params_test_channels")
      (macro $snd-pcm-hw-params-set-rate!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int ::int)
	 "snd_pcm_hw_params_set_rate")
      (macro $snd-pcm-hw-params-test-rate?::bool
	 (::$snd-pcm ::$snd-pcm-hw-params ::int ::int)
	 "snd_pcm_hw_params_test_rate")
      (macro $bgl-snd-pcm-hw-params-set-rate-near!::uint
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "bgl_snd_pcm_hw_params_set_rate_near")
      (macro $snd-pcm-hw-params-set-rate-resample!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::uint)
	 "snd_pcm_hw_params_set_rate_resample")
      (macro $snd-pcm-hw-params-set-buffer-size!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "snd_pcm_hw_params_set_buffer_size")
      (macro $bgl-snd-pcm-hw-params-get-buffer-size::int
	 (::$snd-pcm)
	 "bgl_snd_pcm_hw_params_get_buffer_size")
      (macro $bgl-snd-pcm-hw-params-get-buffer-time::int
	 (::$snd-pcm)
	 "bgl_snd_pcm_hw_params_get_buffer_time")
      (macro $bgl-snd-pcm-hw-params-set-buffer-size-near!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "bgl_snd_pcm_hw_params_set_buffer_size_near")
      (macro $bgl-snd-pcm-hw-params-set-buffer-time-near!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "bgl_snd_pcm_hw_params_set_buffer_time_near")
      (macro $snd-pcm-hw-params-set-period-size!::int
	 (::$snd-pcm ::$snd-pcm-hw-params ::int ::int)
	 "snd_pcm_hw_params_set_period_size")
      (macro $bgl-snd-pcm-hw-params-get-period-size::int
	 (::$snd-pcm-hw-params)
	 "bgl_snd_pcm_hw_params_get_period_size")
      (macro $bgl-snd-pcm-hw-params-set-period-size-near!::uint
	 (::$snd-pcm ::$snd-pcm-hw-params ::int)
	 "bgl_snd_pcm_hw_params_set_period_size_near")
      (macro $bgl-snd-pcm-hw-params-get-rates::int
	 (::$snd-pcm)
	 "bgl_snd_pcm_hw_params_get_rates")
      
      ;; snd-pcm-sw-params
      (type $snd-pcm-sw-params void* "snd_pcm_sw_params_t *")

      (macro $bgl-snd-pcm-sw-params-malloc::$snd-pcm-sw-params
	 () "bgl_snd_pcm_sw_params_malloc")
      (macro $bgl-snd-pcm-sw-params-free::void
	 (::$snd-pcm-sw-params) "bgl_snd_pcm_sw_params_free")
      (macro $snd-pcm-sw-params::int
	 (::$snd-pcm ::$snd-pcm-sw-params) "snd_pcm_sw_params")
      (macro $snd-pcm-sw-params-set-start-threshold!::int
	 (::$snd-pcm ::$snd-pcm-sw-params ::$snd-pcm-uframes)
	 "snd_pcm_sw_params_set_start_threshold")
      (macro $snd-pcm-sw-params-current::int
	 (::$snd-pcm ::$snd-pcm-sw-params) "snd_pcm_sw_params_current")
      (macro $snd-pcm-sw-params-set-avail-min!::int
	 (::$snd-pcm ::$snd-pcm-sw-params ::$snd-pcm-uframes)
	 "snd_pcm_sw_params_set_avail_min")

      ;; helper functions
      (macro $snd-pcm-bytes-to-frames::$snd-pcm-sframes
	 (::$snd-pcm ::long) "snd_pcm_bytes_to_frames")
      (macro $snd-pcm-frames-to-bytes::long
	 (::$snd-pcm ::$snd-pcm-sframes) "snd_pcm_frames_to_bytes")
      (macro $bgl-sframes->uframes::$snd-pcm-uframes
	 (::$snd-pcm-sframes) "(snd_pcm_uframes_t)")
      (macro $bgl-uframes->sframes::$snd-pcm-sframes
	 (::$snd-pcm-uframes) "(snd_pcm_sframes_t)")

      ;; snd-ctl
      (type $snd-ctl void* "snd_ctl_t *")
      (infix macro $snd-ctl-nil::$snd-ctl () "0L")
      (infix macro $snd-ctl-nil?::bool (::$snd-ctl) " == 0L")

      (macro $bgl-snd-ctl-open::int
	 (::obj ::string ::int) "bgl_snd_ctl_open")
      (macro $snd-ctl-close::int
	 (::$snd-ctl) "snd_ctl_close")

      ;; snd-ctl-mode
      (macro $snd-ctl-nonblock::int
	 "SND_CTL_NONBLOCK")
      (macro $snd-ctl-async::int
	 "SND_CTL_ASYNC")

      ;; snd-ctl
      (type $snd-ctl-card-info void* "snd_ctl_card_info_t *")
      (infix macro $snd-ctl-card-info-nil::$snd-ctl-card-info () "0L")

      (macro $bgl-snd-ctl-card-info-init::void
	 (::obj) "bgl_snd_ctl_card_info_init")
      (macro $bgl-snd-ctl-rawmidi-info-init::void
	 (::obj) "bgl_snd_ctl_rawmidi_info_init")
      (macro $bgl-snd-ctl-rawmidi-next-device::int
	 (::obj ::int) "bgl_snd_ctl_rawmidi_next_device")

      ;; snd-mixer
      (type $snd-mixer void* "snd_mixer_t *")
      (infix macro $snd-mixer-nil::$snd-mixer () "0L")
      (infix macro $snd-mixer-nil?::bool (::$snd-mixer) " == 0L")
      
      (macro $bgl-snd-mixer-open::int
	 (::obj) "bgl_snd_mixer_open")
      (macro $snd-mixer-close::int
	 (::$snd-mixer) "snd_mixer_close")
      (macro $snd-mixer-attach::int
	 (::$snd-mixer ::string)  "snd_mixer_attach")
      (macro $snd-mixer-load::int
	 (::$snd-mixer) "snd_mixer_load")
      (macro $snd-mixer-get-count::uint
	 (::$snd-mixer) "snd_mixer_get_count")

      ;; snd-rawmidi
      (type $snd-rawmidi void* "snd_rawmidi_t *")
      (infix macro $snd-rawmidi-nil::$snd-rawmidi () "0L")
      (infix macro $snd-rawmidi-nil?::bool ($snd-rawmidi) " == 0L")

      (macro $snd-rawmidi-append::int
	 "SND_RAWMIDI_APPEND")
      (macro $snd-rawmidi-nonblock::int
	 "SND_RAWMIDI_NONBLOCK")
      (macro $snd-rawmidi-sync::int
	 "SND_RAWMIDI_SYNC")
      
      (macro $snd-rawmidi-stream-input::int
	 "SND_RAWMIDI_STREAM_INPUT")
      (macro $snd-rawmidi-stream-output::int
	 "SND_RAWMIDI_STREAM_OUTPUT")
      ($bgl-snd-rawmidi-isdir::bool
	 (::obj ::int ::int ::int) "bgl_snd_rawmidi_isdir")

      ($bgl-snd-rawmidi-open-output::int
	 (::obj ::string ::int) "bgl_snd_rawmidi_open_output")
      (macro $snd-rawmidi-close::int
	 (::$snd-rawmidi) "snd_rawmidi_close")

      (macro $snd-rawmidi-write::int
	 (::$snd-rawmidi ::void* ::long) "snd_rawmidi_write")

      (macro $snd-rawmidi-drain::int
	 (::$snd-rawmidi) "snd_rawmidi_drain")
      (macro $snd-rawmidi-drop::int
	 (::$snd-rawmidi) "snd_rawmidi_drop")

      ;; snd-error
      (macro $snd-strerror::string
	 (::int) "(char *)snd_strerror")))
