;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/gstreamer/src/Llib/gst.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 30 15:28:51 2007                          */
;*    Last change :  Tue Jan 24 16:16:01 2012 (serrano)                */
;*    Copyright   :  2007-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Direct use of GSTREAMER functions                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directive                                                    */
;*---------------------------------------------------------------------*/
(directives
   (extern
    (include "gst/gst.h")
    (include "bglgst_config.h")
    (include "bglgst.h")
    (include "bglgst_plugin.h")

    ;; gst-bin
    (type $gst-bin void* "GstBin *")
    (infix macro $gst-bin-nil::$gst-bin () "0L")
    (macro $gst-bin-new::$gst-element
       (::string) "gst_bin_new")
    (macro $gst-bin->object::$gst-object
       (::$gst-bin) "(GstObject *)")
    (macro $gst-bin::$gst-bin
       (::$gst-object) "GST_BIN")
    (macro $gst-bin-add!::bool
       (::$gst-bin ::$gst-element) "gst_bin_add")
    (macro $gst-bin-remove!::bool
       (::$gst-bin ::$gst-element) "gst_bin_remove")
    (macro $gst-bin-get-by-name::$gst-element
       (::$gst-bin ::string) "gst_bin_get_by_name")
    
    ;; gst-buffer
    (type $gst-buffer void* "GstBuffer *")
    (infix macro $gst-buffer-nil::$gst-buffer () "0L")
    (macro $gst-buffer-new::$gst-buffer
       () "gst_buffer_new")
    (macro $gst-buffer-new-allocate::$gst-buffer
       (::void* ::int ::void*) "gst_buffer_new_allocate")
    (macro $gst-buffer-ref!::$gst-buffer
       (::$gst-buffer) "gst_buffer_ref")
    (macro $gst-buffer-unref!::void
       (::$gst-buffer) "gst_buffer_unref")
    (macro $gst-buffer-null?::bool
       (::$gst-buffer) "(bool_t)!")
    (macro $gst-buffer->object::$gst-object
       (::$gst-buffer) "(GstObject *)")
    (macro $gst-buffer-get-size::long
       (::$gst-buffer) "gst_buffer_get_size")
    (macro $gst-buffer-set-size!::void
       (::$gst-buffer ::long) "gst_buffer_set_size")
    (macro $gst-buffer-get-string::obj
       (::$gst-buffer) "bgl_gst_buffer_get_string")
    (macro $gst-buffer-set-string!::void
       (::$gst-buffer ::obj) "bgl_gst_buffer_set_string")
    (macro $gst-buffer-dts::$gst-clock-time
       (::$gst-buffer) "GST_BUFFER_DTS")
    (macro $gst-buffer-pts::$gst-clock-time
       (::$gst-buffer) "GST_BUFFER_PTS")
    (macro $gst-buffer-dts-or-pts::$gst-clock-time
       (::$gst-buffer) "GST_BUFFER_DTS_OR_PTS")
    (macro $gst-buffer-duration::$gst-clock-time
       (::$gst-buffer) "GST_BUFFER_DURATION")
    
     ;; gst-bus
    (type $gst-bus void* "GstBus *")
    (infix macro $gst-bus-nil::$gst-bus () "0L")
    (macro $gst-bus-new::$gst-bus
       () "gst_bus_new")
    (macro $gst-bus-null?::bool
       (::$gst-bus) "(bool_t)!")
    (macro $gst-bus->object::$gst-object
       (::$gst-bus) "(GstObject *)")
    (macro $gst-bus::$gst-bus
       (::$gst-object) "GST_BUS")
    (macro $gst-bus-post::bool
       (::$gst-bus ::$gst-message) "gst_bus_post")
    (macro $gst-bus-peek::$gst-message
       (::$gst-bus) "gst_bus_peek")
    (macro $gst-bus-pop::$gst-message
       (::$gst-bus) "gst_bus_pop")
    (macro $gst-bus-set-sync-handler!::void
       (::$gst-bus ::procedure) "bgl_gst_bus_set_sync_handler")
;*     (macro $gst-bus-pop-filtered::$gst-message                      */
;*        (::$gst-bus ::$gst-message-type) "gst_bus_pop_filtered")     */

    (macro $gst-bus-poll::$gst-message
       (::$gst-bus ::$gst-message-type ::$gst-clock-time-diff) "gst_bus_poll")
    (macro $gst-bus-add-watch!::uint
       (::$gst-bus ::$gst-bus-func ::void*) "gst_bus_add_watch")
    (macro $gst-bus-wait-playing::void
       (::$gst-bus ::obj) "bgl_gst_bus_wait_playing")

    ;; gst-bus-func
    (type $gst-bus-func void* "GstBusFunc")

    ;; gst-caps
    (type $gst-caps void* "GstCaps *")
    (infix macro $gst-caps-nil::$gst-caps () "0L")
    (macro $gst-caps-null?::bool
       (::$gst-caps) "(bool_t)!")
    (macro $gst-caps->object::$gst-object
       (::$gst-caps) "(GstObject *)")
    (macro $gst-caps::$gst-caps
       (::$gst-object) "GST_CAPS")
    (macro $gst-caps-ref!::$gst-caps
       (::$gst-caps) "gst_caps_ref")
    (macro $gst-caps-unref!::void
       (::$gst-caps) "gst_caps_unref")
    (macro $gst-caps-get-size::uint
       (::$gst-caps) "gst_caps_get_size")
    (macro $gst-caps-get-structure::$gst-structure
       (::$gst-caps ::uint) "gst_caps_get_structure")
    (macro $gst-caps-is-always-compatible?::bool
       (::$gst-caps ::$gst-caps) "gst_caps_is_always_compatible")
    (macro $gst-caps-new-simple::obj
       (::bstring ::pair-nil ::procedure) "bgl_gst_caps_new_simple")
    (macro $gst-caps-append::void
       (::$gst-caps ::$gst-caps) "gst_caps_append")
    (macro $gst-caps-merge::$gst-caps
       (::$gst-caps ::$gst-caps) "gst_caps_merge")
    (macro $gst-caps-append-structure::void
       (::$gst-caps ::$gst-structure) "gst_caps_append_structure")
    (macro $gst-caps-merge-structure::$gst-caps
       (::$gst-caps ::$gst-structure) "gst_caps_merge_structure")
    (macro $gst-caps-remove-structure::void
       (::$gst-caps ::uint) "gst_caps_remove_structure")
    (macro $gst-caps-to-string::string
       (::$gst-caps) "gst_caps_to_string")
    (macro $gst-caps-from-string::$gst-caps
       (::string) "gst_caps_from_string")
    
    ;; gst-clock
    (type $gst-clock void* "GstClock *")
    (macro $gst-clock-get-time::$gst-clock-time
       (::$gst-clock) "gst_clock_get_time")
    
    ;; gst-clock-time
    (type $gst-clock-time llong "GstClockTime")
    (macro $gst-clock-time-none::$gst-clock-time "GST_CLOCK_TIME_NONE")
    
    ;; gst-clock-time-diff
    (type $gst-clock-time-diff llong "GstClockTimeDiff")
    
    ;; gst-element
    (type $gst-element void* "GstElement *")
    (infix macro $gst-element-nil::$gst-element () "0L")
    (macro $gst-element-null?::bool
       (::$gst-element) "(bool_t)!")
    (macro $gst-element->object::$gst-object
       (::$gst-element) "(GstObject *)")
    (macro $gst-element::$gst-element
       (::$gst-object) "GST_ELEMENT")
    (macro $gst-element-link!::bool
       (::$gst-element ::$gst-element) "gst_element_link")
    (macro $gst-element-unlink!::void
       (::$gst-element ::$gst-element) "gst_element_unlink")
    (macro $gst-element-link-filtered!::bool
       (::$gst-element ::$gst-element ::$gst-caps) "gst_element_link_filtered")
    (macro $gst-element-get-factory::$gst-element-factory
       (::$gst-element) "gst_element_get_factory")
    (macro $gst-element-get-name::string
       (::$gst-element) "gst_element_get_name")
    (macro $gst-element-set-name!::void
       (::$gst-element ::string) "gst_element_set_name")
    (macro $gst-element-set-state!::$gst-state-change-return
       (::$gst-element ::$gst-state) "gst_element_set_state")
    (macro $gst-element-get-state::$gst-state-change-return
       (::$gst-element ::long ::long ::$gst-clock-time) "gst_element_get_state")
    (macro $gst-element-add-pad!::bool
       (::$gst-element ::$gst-pad) "gst_element_add_pad")
    (macro $gst-element-get-static-pad::$gst-pad
       (::$gst-element ::string) "gst_element_get_static_pad")
    (macro $gst-element-get-request-pad::$gst-pad
       (::$gst-element ::string) "gst_element_get_request_pad")
    (macro $gst-element-get-compatible-pad::$gst-pad
       (::$gst-element ::$gst-pad ::$gst-caps) "gst_element_get_compatible_pad")
    (macro $gst-element-get-start-time::$gst-clock-time
       (::$gst-element) "gst_element_get_start_time")
    (macro $gst-element-get-base-time::$gst-clock-time
       (::$gst-element) "gst_element_get_base_time")
    (macro $gst-element-get-clock::$gst-clock
       (::$gst-element) "gst_element_get_clock")
    (macro $gst-element-query-position::llong
       (::$gst-element) "bgl_gst_element_query_position")
    (macro $gst-element-query-duration::llong
       (::$gst-element) "bgl_gst_element_query_duration")
    (macro $gst-element-seek-simple::bool
       (::$gst-element ::$gst-format ::$gst-seek-flags ::llong)
       "gst_element_seek_simple")
    (macro $gst-element-interface-list::pair-nil
       (::$gst-element) "bgl_gst_element_interface_list")
    (macro $gst-element-release-request-pad!::void
       (::$gst-element ::$gst-pad) "gst_element_release_request_pad")
    
    ;; gst-element-factory
    (type $gst-element-factory void* "GstElementFactory *")
    (infix macro $gst-element-factory-nil::$gst-element-factory () "0L")
    (macro $gst-element-factory-null?::bool
       (::$gst-element-factory) "(bool_t)!")
    (macro $gst-element-factory->object::$gst-object
       (::$gst-element-factory) "(GstObject *)")
    (macro $gst-element-factory::$gst-element-factory
       (::$gst-object) "GST_ELEMENT_FACTORY")
    (macro $gst-element-factory-find::$gst-element-factory
       (::string) "gst_element_factory_find")
    (macro $gst-element-factory-has-interface?::bool
       (::$gst-element-factory ::string) "gst_element_factory_has_interface")
    (macro $gst-element-factory-create::$gst-element
       (::$gst-element-factory ::string) "gst_element_factory_create")
    (macro $gst-element-factory-make::$gst-element
       (::string ::string) "gst_element_factory_make")
    (infix macro $gst-element-factory-name-nil::string () "(char *)0L")
    (macro $gst-element-factory-get-metadata::string
       (::$gst-element-factory ::string) "(char *)gst_element_factory_get_metadata")
    (macro $gst-element-factory-get-uri-protocols::pair-nil
       (::$gst-element-factory) "bgl_gst_element_factory_get_uri_protocols")
    (macro $gst-element-factory-can-sink-all-caps?::bool
       (::$gst-element-factory ::$gst-caps)
       "gst_element_factory_can_sink_all_caps")
    (macro $gst-element-factory-can-src-all-caps?::bool
       (::$gst-element-factory ::$gst-caps)
       "gst_element_factory_can_src_all_caps")
    (macro $gst-element-factory-get-static-pad-templates::pair-nil
       (::$gst-element-factory) "bgl_gst_element_factory_get_static_pad_templates")

    ;; gst-element-metadata
    (macro $gst-element-metadata-author::string
       "GST_ELEMENT_METADATA_AUTHOR")
    (macro $gst-element-metadata-doc-uri::string
       "GST_ELEMENT_METADATA_DOC_URI")
    (macro $gst-element-metadata-description::string
       "GST_ELEMENT_METADATA_DESCRIPTION")
    (macro $gst-element-metadata-icon-name::string
       "GST_ELEMENT_METADATA_ICON_NAME")
    (macro $gst-element-metadata-klass::string
       "GST_ELEMENT_METADATA_KLASS")
    (macro $gst-element-metadata-long-name::string
       "GST_ELEMENT_METADATA_LONGNAME")

    ;; gst-format
    (type $gst-format long "GstFormat")
    (macro $gst-format-undefined::$gst-format
       "GST_FORMAT_UNDEFINED")
    (macro $gst-format-default::$gst-format
       "GST_FORMAT_DEFAULT")
    (macro $gst-format-bytes::$gst-format
       "GST_FORMAT_BYTES")
    (macro $gst-format-time::$gst-format
       "GST_FORMAT_TIME")
    (macro $gst-format-buffers::$gst-format
       "GST_FORMAT_BUFFERS")
    (macro $gst-format-percent::$gst-format
       "GST_FORMAT_PERCENT")

    ;; gst-ghost-pad
    (type $gst-ghost-pad void* "GstGhostPad *")
    (infix macro $gst-ghost-pad-nil::$gst-ghost-pad () "0L")
    (macro $gst-ghost-pad-null?::bool
       (::$gst-ghost-pad) "(bool_t)!")
    (macro $gst-ghost-pad-new::$gst-pad
       (::string ::$gst-pad) "gst_ghost_pad_new")
    (macro $gst-ghost-pad->object::$gst-object
       (::$gst-ghost-pad) "(GstObject *)")
    (macro $gst-ghost-pad::$gst-ghost-pad
       (::$gst-object) "GST_GHOST_PAD")
    (macro $gst-ghost-pad-get-target::$gst-pad
       (::$gst-ghost-pad) "gst_ghost_pad_get_target")
    (macro $gst-ghost-pad-set-target::bool
       (::$gst-ghost-pad ::$gst-pad) "gst_ghost_pad_set_target")
    
    ;; gst-message
    (type $gst-message void* "GstMessage *")
    (infix macro $gst-message-nil::$gst-message () "0L")
    (macro $gst-message-null?::bool
       (::$gst-message) "(bool_t)!")
    (macro $gst-message->object::$gst-object
       (::$gst-message) "(GstObject *)")
    (macro $gst-message::$gst-message
       (::$gst-object) "GST_MESSAGE")
    (macro $gst-message-ref!::$gst-message
       (::$gst-message) "gst_message_ref")
    (macro $gst-message-unref!::void
       (::$gst-message) "gst_message_unref")
    (macro $gst-message-get-type::$gst-message-type
       (::$gst-message) "GST_MESSAGE_TYPE")
    (macro $gst-message-get-type-name::string
       (::$gst-message) "(char *)GST_MESSAGE_TYPE_NAME")
    (macro $gst-message-get-structure::$gst-structure
       (::$gst-message) "(GstStructure *)gst_message_get_structure")
    (macro $gst-message-error-string::string
       (::$gst-message) "bgl_gst_message_error_string")
    (macro $gst-message-info-string::string
       (::$gst-message) "bgl_gst_message_info_string")
    (macro $gst-message-warning-string::string
       (::$gst-message) "bgl_gst_message_warning_string")
    (macro $gst-message-tag-list::pair-nil
       (::$gst-message) "bgl_gst_message_tag_list")
    (macro $gst-message-new-state::$gst-state
       (::$gst-message) "bgl_gst_message_new_state")
    (macro $gst-message-old-state::$gst-state
       (::$gst-message) "bgl_gst_message_old_state")
    (macro $gst-message-pending-state::$gst-state
       (::$gst-message) "bgl_gst_message_pending_state")
    (macro $gst-message-get-src::obj
       (::$gst-message) "bgl_gst_message_get_src")
    (macro $gst-message-stream-status-type::int
       (::$gst-message) "bgl_gst_message_stream_status_type")
    (macro $gst-message-new-application::$gst-message
       (::$gst-object ::$gst-structure) "gst_message_new_application")
    (macro $gst-message-new-custom::$gst-message
       (::$gst-message-type ::$gst-object ::$gst-structure)
       "gst_message_new_custom")
    (macro $gst-message-new-element::$gst-message
       (::$gst-object ::$gst-structure) "gst_message_new_element")
    (macro $gst-message-new-eos::$gst-message
       (::$gst-object) "gst_message_new_eos")
    (macro $gst-message-new-state-changed::$gst-message
       (::$gst-object ::$gst-state ::$gst-state ::$gst-state)
       "gst_message_new_state_changed")
    (macro $gst-message-new-state-dirty::$gst-message
       (::$gst-object) "gst_message_new_state_dirty")
    (macro $gst-message-new-async-done::$gst-message
       (::$gst-object ::$gst-clock-time) "gst_message_new_async_done")
    (macro $gst-message-new-latency::$gst-message
       (::$gst-object) "gst_message_new_latency")

    ;; gst-message-type
    (type $gst-message-type long "GstMessageType")
    (macro $gst-message-unknown::$gst-message-type
       "GST_MESSAGE_UNKNOWN")
    (macro $gst-message-eos::$gst-message-type
       "GST_MESSAGE_EOS")
    (macro $gst-message-error::$gst-message-type
       "GST_MESSAGE_ERROR")
    (macro $gst-message-warning::$gst-message-type
       "GST_MESSAGE_WARNING")
    (macro $gst-message-info::$gst-message-type
       "GST_MESSAGE_INFO")
    (macro $gst-message-tag::$gst-message-type
       "GST_MESSAGE_TAG")
    (macro $gst-message-buffering::$gst-message-type
       "GST_MESSAGE_BUFFERING")
    (macro $gst-message-state-changed::$gst-message-type
       "GST_MESSAGE_STATE_CHANGED")
    (macro $gst-message-state-dirty::$gst-message-type
       "GST_MESSAGE_STATE_DIRTY")
    (macro $gst-message-step-done::$gst-message-type
       "GST_MESSAGE_STEP_DONE")
    (macro $gst-message-clock-provide::$gst-message-type
       "GST_MESSAGE_CLOCK_PROVIDE")
    (macro $gst-message-clock-lost::$gst-message-type
       "GST_MESSAGE_CLOCK_LOST")
    (macro $gst-message-new-clock::$gst-message-type
       "GST_MESSAGE_NEW_CLOCK")
    (macro $gst-message-structure-change::$gst-message-type
       "GST_MESSAGE_STRUCTURE_CHANGE")
    (macro $gst-message-stream-status::$gst-message-type
       "GST_MESSAGE_STREAM_STATUS")
    (macro $gst-message-application::$gst-message-type
       "GST_MESSAGE_APPLICATION")
    (macro $gst-message-element::$gst-message-type
       "GST_MESSAGE_ELEMENT")
    (macro $gst-message-segment-start::$gst-message-type
       "GST_MESSAGE_SEGMENT_START")
    (macro $gst-message-segment-done::$gst-message-type
       "GST_MESSAGE_SEGMENT_DONE")
    (macro $gst-message-duration-changed::$gst-message-type
       "GST_MESSAGE_DURATION_CHANGED")
    (macro $gst-message-latency::$gst-message-type
       "GST_MESSAGE_LATENCY")
    (macro $gst-message-async-start::$gst-message-type
       "GST_MESSAGE_ASYNC_START")
    (macro $gst-message-async-done::$gst-message-type
       "GST_MESSAGE_ASYNC_DONE")
    (macro $gst-message-request-state::$gst-message-type
       "GST_MESSAGE_REQUEST_STATE")
    (macro $gst-message-step-start::$gst-message-type
       "GST_MESSAGE_STEP_START")
    (macro $gst-message-qos::$gst-message-type
       "GST_MESSAGE_QOS")
    (macro $gst-message-progress::$gst-message-type
       "GST_MESSAGE_PROGRESS")
    (macro $gst-message-toc::$gst-message-type
       "GST_MESSAGE_TOC")
    (macro $gst-message-reset-time::$gst-message-type
       "GST_MESSAGE_RESET_TIME")
    (macro $gst-message-stream-start::$gst-message-type
       "GST_MESSAGE_STREAM_START")
    (macro $gst-message-need-context::$gst-message-type
       "GST_MESSAGE_NEED_CONTEXT")
    (macro $gst-message-have-context::$gst-message-type
       "GST_MESSAGE_HAVE_CONTEXT")
    (macro $gst-message-extended::$gst-message-type
       "GST_MESSAGE_EXTENDED")
    (macro $gst-message-device-added::$gst-message-type
       "GST_MESSAGE_DEVICE_ADDED")
    (macro $gst-message-device-removed::$gst-message-type
       "GST_MESSAGE_DEVICE_REMOVED")
    (macro $gst-message-property-notify::$gst-message-type
       "GST_MESSAGE_PROPERTY_NOTIFY")
    (macro $gst-message-stream-collection::$gst-message-type
       "GST_MESSAGE_STREAM_COLLECTION")
    (macro $gst-message-streams-selected::$gst-message-type
       "GST_MESSAGE_STREAMS_SELECTED")
    (macro $gst-message-redirect::$gst-message-type
       "GST_MESSAGE_REDIRECT")
    (macro $gst-message-device-changed::$gst-message-type
       "GST_MESSAGE_DEVICE_CHANGED")
    (macro $gst-message-any::$gst-message-type
       "GST_MESSAGE_ANY")

    ;; gst-stream-status-type
    (type $gst-stream-status-type long "GstStreamStatusType")
    (macro $gst-stream-status-type-create::$gst-stream-status-type
       "GST_STREAM_STATUS_TYPE_CREATE")
    (macro $gst-stream-status-type-enter::$gst-stream-status-type
       "GST_STREAM_STATUS_TYPE_ENTER")
    (macro $gst-stream-status-type-leave::$gst-stream-status-type
       "GST_STREAM_STATUS_TYPE_LEAVE")
    (macro $gst-stream-status-type-destroy::$gst-stream-status-type
       "GST_STREAM_STATUS_TYPE_DESTROY")
    (macro $gst-stream-status-type-start::$gst-stream-status-type
       "GST_STREAM_STATUS_TYPE_START")
    (macro $gst-stream-status-type-pause::$gst-stream-status-type
       "GST_STREAM_STATUS_TYPE_PAUSE")
    (macro $gst-stream-status-type-stop::$gst-stream-status-type
       "GST_STREAM_STATUS_TYPE_STOP")

;*     ;; gst-mixer                                                    */
;*     (type $gst-mixer void* "GstMixer *")                            */
;*     (macro $gst-mixer::$gst-mixer                                   */
;*        (::$gst-object) "(GstMixer *)")                              */
;*     (macro $gst-mixer-get-track::obj                                */
;*        (::$gst-element ::string ::obj) "bgl_gst_mixer_get_track")   */
;*     (macro $gst-mixer-track-list::obj                               */
;*        (::$gst-element ::obj) "bgl_gst_mixer_track_list")           */
;*     (macro $gst-mixer-get-volume::vector                            */
;*        (::$gst-element ::$gst-mixer-track) "bgl_gst_mixer_get_volume") */
;*     (macro $gst-mixer-set-volume!::obj                              */
;*        (::$gst-element ::$gst-mixer-track ::obj) "bgl_gst_mixer_set_volume") */
;*     (macro $gst-mixer-set-mute!::void                               */
;*        (::$gst-element ::$gst-mixer-track ::bool) "gst_mixer_set_mute") */
;*     (macro $gst-mixer-set-record!::void                             */
;*        (::$gst-element ::$gst-mixer-track ::bool) "gst_mixer_set_record") */
;* {*     (macro $gst-mixer-get-mixer-flags::$gst-mixer-flags             *} */
;* {*        (::$gst-element) "gst_mixer_get_mixer_flags")                *} */
;*                                                                     */
;*     ;; gst-mixer-flags                                              */
;*     (type $gst-mixer-flags long "GstMixerFlags")                    */
;*     (macro $gst-mixer-flag-none::$gst-mixer-flags                   */
;*        "GST_MIXER_FLAG_NONE")                                       */
;*     (macro $gst-mixer-flag-auto-notification::$gst-mixer-flags      */
;*        "GST_MIXER_FLAG_AUTO_NOTIFICATIONS")                         */
;*                                                                     */
;*     ;; gst-mixer-track                                              */
;*     (type $gst-mixer-track void* "GstMixerTrack *")                 */
;*     (infix macro $gst-mixer-track-nil::$gst-mixer-track () "0L")    */
;*     (macro $gst-mixer-track::$gst-mixer-track                       */
;*        (::$gst-object) "GST_MIXER_TRACK")                           */
;*     (macro $gst-mixer-track-null?::bool                             */
;*        (::$gst-mixer-track) "(bool_t)!")                            */
;*     (macro $gst-mixer-track->object::$gst-object                    */
;*        (::$gst-mixer-track) "(GstObject *)")                        */
;*     (infix macro $gst-mixer-track-get-flags::long                   */
;* 	   (::$gst-mixer-track) "->flags")                             */
;*     (infix macro $gst-mixer-track-get-label::string                 */
;* 	   (::$gst-mixer-track) "->label")                             */
;*     (macro $gst-mixer-track-has-flag?::bool                         */
;*        (::$gst-mixer-track ::$gst-mixer-track-flags) "GST_MIXER_TRACK_HAS_FLAG") */
;*     (infix macro $gst-mixer-track-get-max-volume::long              */
;* 	   (::$gst-mixer-track) "->max_volume")                        */
;*     (infix macro $gst-mixer-track-get-min-volume::long              */
;* 	   (::$gst-mixer-track) "->min_volume")                        */
;*     (infix macro $gst-mixer-track-get-num-channels::long            */
;* 	   (::$gst-mixer-track) "->num_channels")                      */
;*                                                                     */
;*     ;; gst-mixer-track-flags                                        */
;*     (type $gst-mixer-track-flags long "GstMixerTrackFlags")         */
;*     (macro $gst-mixer-track-input::$gst-mixer-track-flags           */
;*        "GST_MIXER_TRACK_INPUT")                                     */
;*     (macro $gst-mixer-track-output::$gst-mixer-track-flags          */
;*        "GST_MIXER_TRACK_OUTPUT")                                    */
;*     (macro $gst-mixer-track-mute::$gst-mixer-track-flags            */
;*        "GST_MIXER_TRACK_MUTE") 	                               */
;*     (macro $gst-mixer-track-record::$gst-mixer-track-flags          */
;*        "GST_MIXER_TRACK_RECORD")                                    */
;*     (macro $gst-mixer-track-master::$gst-mixer-track-flags          */
;*        "GST_MIXER_TRACK_MASTER") 	                               */
;*     (macro $gst-mixer-track-software::$gst-mixer-track-flags        */
;*        "GST_MIXER_TRACK_SOFTWARE") 	                               */
;*     (macro $gst-mixer-track-readonly::$gst-mixer-track-flags        */
;*        "GST_MIXER_TRACK_READONLY")                                  */
;*     (macro $gst-mixer-track-writeonly::$gst-mixer-track-flags       */
;*        "GST_MIXER_TRACK_WRITEONLY")                                 */
    
    ;; gst-object
    (type $gst-object void* "GstObject *")
    (infix macro $gst-object-nil::$gst-object () "0L")
    (macro $gst-object-null?::bool
	   (::$gst-object) "(bool_t)!")
    (macro $gst-object-ref!::$gst-object
       (::$gst-object) "gst_object_ref")
    (macro $gst-object-unref!::void
       (::$gst-object) "gst_object_unref")
    (macro $gst-object-to-obj::obj
       (::$gst-object ::bool) "bgl_gst_object_to_obj")
    (macro $gst-object-refcount::int
       (::$gst-object) "GST_OBJECT_REFCOUNT")
    (macro $gst-object-property-list::pair-nil
       (::$gst-object) "bgl_gst_object_property_list")
    (macro $gst-object-get-property::bstring
       (::$gst-object ::string) "bgl_gst_object_get_property")
    (macro $gst-object-set-property!::obj
       (::$gst-object ::string ::obj) "bgl_gst_object_set_property")
    (macro $gst-object-connect!::obj
       (::$gst-object ::string ::procedure) "bgl_gst_object_connect")

    ;; gst-pad
    (type $gst-pad void* "GstPad *")
    (infix macro $gst-pad-nil::$gst-pad () "0L")
    (macro $gst-pad-null?::bool
       (::$gst-pad) "(bool_t)!")
    (macro $gst-pad-new::$gst-pad
       (::string ::$gst-pad-direction) "gst_pad_new")
    (macro $gst-pad->object::$gst-object
       (::$gst-pad) "(GstObject *)")
    (macro $gst-pad::$gst-pad
       (::$gst-object) "GST_PAD")
    (macro $gst-pad-get-name::string
       (::$gst-pad) "gst_pad_get_name")
    (macro $gst-pad-get-direction::$gst-pad-direction
       (::$gst-pad) "gst_pad_get_direction")
    (macro $gst-pad-get-parent-element::$gst-element
       (::$gst-pad) "gst_pad_get_parent_element")
    (macro $gst-pad-can-link?::bool
       (::$gst-pad ::$gst-pad) "gst_pad_can_link")
    (macro $gst-pad-link!::$gst-pad-link-return
       (::$gst-pad ::$gst-pad) "gst_pad_link")
    (macro $gst-pad-is-linked?::bool
       (::$gst-pad) "gst_pad_is_linked")
    (macro $gst-pad-unlink!::bool
       (::$gst-pad ::$gst-pad) "gst_pad_unlink")
    (macro $gst-pad-query-caps::$gst-caps
       (::$gst-pad ::$gst-caps) "gst_pad_query_caps")
    (macro $gst-pad-set-caps!::bool
       (::$gst-pad ::$gst-caps) "bgl_gst_pad_set_caps")
    (macro $gst-pad-get-allowed-caps::$gst-caps
       (::$gst-pad) "gst_pad_get_allowed_caps")
    (macro $gst-pad-get-current-caps::$gst-caps
       (::$gst-pad) "gst_pad_get_current_caps")
    (macro $gst-pad-get-pad-template-caps::$gst-caps
       (::$gst-pad) "gst_pad_get_pad_template_caps")
    (macro $gst-pad-add-probe!::ulong
       (::$gst-pad ::$gst-pad-probe-type ::procedure) "bgl_gst_pad_add_probe")
    (macro $gst-pad-remove-probe!::void
       (::$gst-pad ::int) "gst_pad_remove_probe")

    ;; gst-pad-template
    (type $gst-pad-template void* "GstPadTemplate *")
    (infix macro $gst-pad-template-nil::$gst-pad-template () "0L")
    (macro $gst-pad-template-null?::bool
       (::$gst-pad-template) "(bool_t)!")
    (macro $gst-pad-template-new::$gst-pad-template
       (::string ::$gst-pad-direction ::$gst-pad-presence ::$gst-caps)
       "gst_pad_template_new")
    (macro $gst-pad-template->object::$gst-object
       (::$gst-pad-template) "(GstObject *)")
    (macro $gst-pad-template::$gst-pad-template
       (::$gst-object) "GST_PAD_TEMPLATE")
    
    ;; gst-pad-link-return
    (type $gst-pad-link-return long "GstPadLinkReturn")
    (macro $gst-pad-link-ok::$gst-pad-link-return
       "GST_PAD_LINK_OK")
    (macro $gst-pad-link-wrong-hierarchy::$gst-pad-link-return
       "GST_PAD_LINK_WRONG_HIERARCHY")
    (macro $gst-pad-link-was-linked::$gst-pad-link-return
       "GST_PAD_LINK_WAS_LINKED")
    (macro $gst-pad-link-wrong-direction::$gst-pad-link-return
       "GST_PAD_LINK_WRONG_DIRECTION")
    (macro $gst-pad-link-noformat::$gst-pad-link-return
       "GST_PAD_LINK_NOFORMAT")
    (macro $gst-pad-link-nosched::$gst-pad-link-return
       "GST_PAD_LINK_NOSCHED")
    (macro $gst-pad-link-refused::$gst-pad-link-return
       "GST_PAD_LINK_REFUSED")
    
    ;; gst-pad-direction
    (type $gst-pad-direction long "GstPadDirection")
    (macro $gst-pad-unknown::$gst-pad-direction
       "GST_PAD_UNKNOWN")
    (macro $gst-pad-src::$gst-pad-direction
       "GST_PAD_SRC")
    (macro $gst-pad-sink::$gst-pad-direction
       "GST_PAD_SINK")
    
    ;; gst-pad-presence
    (type $gst-pad-presence long "GstPadPresence")
    (macro $gst-pad-always::$gst-pad-presence
       "GST_PAD_ALWAYS")
    (macro $gst-pad-sometimes::$gst-pad-presence
       "GST_PAD_SOMETIMES")
    (macro $gst-pad-request::$gst-pad-presence
       "GST_PAD_REQUEST")

    ;; gst-pad-probe-type
    (type $gst-pad-probe-type long "GstPadProbeType")
    (macro $gst-pad-probe-type-invalid::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_INVALID")
    (macro $gst-pad-probe-type-idle::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_IDLE")
    (macro $gst-pad-probe-type-BLOCK::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_BLOCK")
    (macro $gst-pad-probe-type-buffer::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_BUFFER")
    (macro $gst-pad-probe-type-buffer-list::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_BUFFER_LIST")
    (macro $gst-pad-probe-type-event-downstream::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_EVENT_DOWNSTREAM")
    (macro $gst-pad-probe-type-event-upstream::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_EVENT_UPSTREAM")
    (macro $gst-pad-probe-type-event-flush::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_EVENT_FLUSH")
    (macro $gst-pad-probe-type-query-downstream::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_QUERY_DOWNSTREAM")
    (macro $gst-pad-probe-type-query-upstream::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_QUERY_UPSTREAM")
    (macro $gst-pad-probe-type-push::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_PUSH")
    (macro $gst-pad-probe-type-pull::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_PULL")
    (macro $gst-pad-probe-type-blocking::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_BLOCKING")
    (macro $gst-pad-probe-type-data-downstream::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_DATA_DOWNSTREAM")
    (macro $gst-pad-probe-type-data-upstream::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_DATA_UPSTREAM")
    (macro $gst-pad-probe-type-data-both::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_DATA_BOTH")
    (macro $gst-pad-probe-type-block-downstream::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_BLOCK_DOWNSTREAM")
    (macro $gst-pad-probe-type-block-upstream::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_BLOCK_UPSTREAM")
    (macro $gst-pad-probe-type-event-both::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_EVENT_BOTH")
    (macro $gst-pad-probe-type-query-both::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_QUERY_BOTH")
    (macro $gst-pad-probe-type-all-both::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_ALL_BOTH")
    (macro $gst-pad-probe-type-scheduling::$gst-pad-probe-type
       "GST_PAD_PROBE_TYPE_SCHEDULING")

    ;; gst-parse
    (macro $gst-parse-launch::obj
       (::string) "bgl_gst_parse_launch")
    (macro $gst-parse-launchv::obj
       (::pair-nil) "bgl_gst_parse_launchv")
    
    ;; gst-pipeline
    (type $gst-pipeline void* "GstPipeline *")
    (infix macro $gst-pipeline-nil::$gst-pipeline () "0L")
    (macro $gst-pipeline-new::$gst-element
       (::string) "gst_pipeline_new")
    (macro $gst-pipeline->object::$gst-object
       (::$gst-pipeline) "(GstObject *)")
    (macro $gst-pipeline::$gst-pipeline
       (::$gst-object) "GST_PIPELINE")
    (macro $gst-pipeline-get-bus::$gst-bus
       (::$gst-pipeline) "gst_pipeline_get_bus")
    (macro $gst-pipeline-get-clock::$gst-clock
       (::$gst-pipeline) "gst_pipeline_get_clock")
    (macro $gst-pipeline-set-clock!::bool
       (::$gst-pipeline ::$gst-clock) "gst_pipeline_set_clock")
    
    ;; gst-plugin
    (type $gst-plugin void* "GstPlugin *")
    (infix macro $gst-plugin-nil::$gst-plugin () "0L")
    (macro $gst-plugin->object::$gst-object
       (::$gst-plugin) "(GstObject *)")
    (macro $gst-plugin-null?::bool
       (::$gst-plugin) "(bool_t)!")
    (macro $gst-plugin::$gst-plugin
       (::$gst-object) "GST_PLUGIN")
    (macro $gst-plugin-get-name::string
       (::$gst-plugin) "(char *)gst_plugin_get_name")
    (macro $gst-plugin-get-description::string
       (::$gst-plugin) "(char *)gst_plugin_get_description")
    (macro $gst-plugin-get-filename::string
       (::$gst-plugin) "(char *)gst_plugin_get_filename")
    (macro $gst-plugin-get-license::string
       (::$gst-plugin) "(char *)gst_plugin_get_license")
    (macro $gst-plugin-get-package::string
       (::$gst-plugin) "(char *)gst_plugin_get_package")
    (macro $gst-plugin-get-origin::string
       (::$gst-plugin) "(char *)gst_plugin_get_origin")
    (macro $gst-plugin-get-source::string
       (::$gst-plugin) "(char *)gst_plugin_get_source")
    (macro $gst-plugin-get-version::string
       (::$gst-plugin) "(char *)gst_plugin_get_version")
    
    ;; gst-plugin-feature
    (type $gst-plugin-feature void* "GstPluginFeature *")
    (infix macro $gst-plugin-feature-nil::$gst-plugin-feature () "0L")
    (macro $gst-plugin-feature->object::$gst-object
       (::$gst-plugin-feature) "(GstObject *)")
    (macro $gst-plugin-feature-null?::bool
       (::$gst-plugin-feature) "(bool_t)!")
    (macro $gst-plugin-feature::$gst-plugin-feature
       (::$gst-object) "GST_PLUGIN_FEATURE")
    (macro $gst-plugin-feature-name::string
       (::$gst-plugin-feature) "(char *)gst_plugin_feature_get_name")
    (macro $gst-plugin-feature-get-plugin-name::string
       (::$gst-plugin-feature) "(char *)gst_plugin_feature_get_plugin_name")
    (macro $gst-plugin-feature-set-name!::string
       (::$gst-plugin-feature ::string) "gst_plugin_feature_set_name")
    (macro $gst-plugin-feature-rank::uint
       (::$gst-plugin-feature) "gst_plugin_feature_get_rank")
    (macro $gst-plugin-feature-set-rank!::void
       (::$gst-plugin-feature ::uint) "gst_plugin_feature_set_rank")
    
    ;; gst-state
    (type $gst-state long "GstState")
    (macro $gst-state-void-pending::$gst-state
       "GST_STATE_VOID_PENDING")
    (macro $gst-state-null::$gst-state
       "GST_STATE_NULL")
    (macro $gst-state-ready::$gst-state
       "GST_STATE_READY")
    (macro $gst-state-paused::$gst-state
       "GST_STATE_PAUSED")
    (macro $gst-state-playing::$gst-state
       "GST_STATE_PLAYING")
    
    ;; gst-registry
    (type $gst-registry void* "GstRegistry *")
    (infix macro $gst-registry-nil::$gst-registry () "0L")
    (macro $gst-registry-null?::bool
       (::$gst-registry) "(bool_t)!")
    (macro $gst-registry->object::$gst-object
       (::$gst-registry) "(GstObject *)")
    (macro $gst-registry::$gst-registry
       (::$gst-object) "GST_REGISTRY")
    (macro $gst-registry-get::$gst-registry
       () "gst_registry_get")
    (macro $gst-registry-get-element-factory-list::pair-nil
       (::$gst-registry) "bgl_gst_registry_get_element_factory_list")
    (macro $gst-registry-get-feature-list-by-plugin::pair-nil
       (::$gst-registry ::string) "bgl_gst_registry_get_feature_list_by_plugin")
    (macro $gst-registry-get-plugin-list::pair-nil
       (::$gst-registry) "bgl_gst_registry_get_plugin_list")
    (macro $gst-registry-find-plugin::$gst-plugin
       (::$gst-registry ::string) "gst_registry_find_plugin")
    (macro $gst-registry-find-feature::$gst-plugin-feature
       (::$gst-registry ::string ::long) "gst_registry_find_feature")

    ;; gst-seek-flags
    (type $gst-seek-flags long "GstSeekFlags")
    (macro $gst-seek-flag-none::$gst-seek-flags
       "GST_SEEK_FLAG_NONE")
    (macro $gst-seek-flag-flush::$gst-seek-flags
       "GST_SEEK_FLAG_FLUSH")
    (macro $gst-seek-flag-accurate::$gst-seek-flags
       "GST_SEEK_FLAG_ACCURATE")
    (macro $gst-seek-flag-key-unit::$gst-seek-flags
       "GST_SEEK_FLAG_KEY_UNIT")
    (macro $gst-seek-flag-segment::$gst-seek-flags
       "GST_SEEK_FLAG_SEGMENT")
    (macro $gst-seek-flag-trickmode::$gst-seek-flags
       "GST_SEEK_FLAG_TRICKMODE")
    (macro $gst-seek-flag-snap-before::$gst-seek-flags
       "GST_SEEK_FLAG_SNAP_BEFORE")
    (macro $gst-seek-flag-snap-after::$gst-seek-flags
       "GST_SEEK_FLAG_SNAP_AFTER")
    (macro $gst-seek-flag-snap-nearest::$gst-seek-flags
       "GST_SEEK_FLAG_SNAP_NEAREST")
    (macro $gst-seek-flag-trickmode-key-units::$gst-seek-flags
       "GST_SEEK_FLAG_TRICKMODE_KEY_UNITS")
    (macro $gst-seek-flag-trickmode-no-audio::$gst-seek-flags
       "GST_SEEK_FLAG_TRICKMODE_NO_AUDIO")
    
    ;; gst-state-change-return
    (type $gst-state-change-return long "GstStateChangeReturn")
    (macro $gst-state-change-failure::$gst-state-change-return
       "GST_STATE_CHANGE_FAILURE")
    (macro $gst-state-change-success::$gst-state-change-return
       "GST_STATE_CHANGE_SUCCESS")
    (macro $gst-state-change-async::$gst-state-change-return
       "GST_STATE_CHANGE_ASYNC")
    (macro $gst-state-change-no-preroll::$gst-state-change-return
       "GST_STATE_CHANGE_NO_PREROLL")

    ;; gst-static-pad-template
    (type $gst-static-pad-template void* "GstStaticPadTemplate *")
    (infix macro $gst-static-pad-template-nil::$gst-static-pad-template () "0L")
    (macro $gst-static-pad-template-null?::bool
       (::$gst-static-pad-template) "(bool_t)!")
    (macro $gst-static-pad-template-name-template::string
       (::$gst-static-pad-template) "(char *)GST_PAD_TEMPLATE_NAME_TEMPLATE")
    (infix macro $gst-static-pad-template-direction::$gst-pad-direction
       (::$gst-static-pad-template) "->direction")
    (infix macro $gst-static-pad-template-presence::$gst-pad-presence
       (::$gst-static-pad-template) "->presence")
    
    ;; gst-structure
    (type $gst-structure void* "GstStructure *")
    (infix macro $gst-structure-nil::$gst-structure () "0L")
    (macro $gst-structure-null?::bool
       (::$gst-structure) "(bool_t)!")
    (macro $gst-structure->object::$gst-object
       (::$gst-structure) "(GstObject *)")
    (macro $gst-structure-new-empty::$gst-structure
       (::string) "gst_structure_new_empty")
    (macro $gst-structure::$gst-structure
       (::$gst-object) "GST_STRUCTURE")
    (macro $gst-structure-free!::void
       (::$gst-structure) "gst_structure_free")
    (macro $gst-structure-get-name::string
       (::$gst-structure) "(char *)gst_structure_get_name")
    (macro $gst-structure-set-name!::void
       (::$gst-structure ::string) "gst_structure_set_name")
    (macro $gst-structure-property-list::pair-nil
       (::$gst-structure) "bgl_gst_structure_property_list")
    (macro $gst-structure-get-property::bstring
       (::$gst-structure ::string) "bgl_gst_structure_get_property")
    (macro $gst-structure-set-property!::obj
       (::$gst-structure ::string ::obj) "bgl_gst_structure_set_property")

    ;; gst-type-find
    (type $gst-type-find void* "GstTypeFind *")
    (infix macro $gst-type-find-nil::$gst-type-find () "0L")
    (macro $gst-type-find-null?::bool
       (::$gst-type-find) "(bool_t)!")

    ;; bgl-gst-ports-src
    (type $bgl-port-src void* "BglPortSrc *")
    (macro $bgl-port-src::$bgl-port-src (::$gst-object)
          "BGL_PORT_SRC")
    (macro $bgl-port-src-new::$gst-element (::input-port)
          "bgl_port_src_new")
    
    ;; gst misc functions
    ($gst-init::void (::pair-nil) "bgl_gst_init")
    ($gst-version::string () "gst_version_string")
    ($gst-add-finalizer!::void (::obj ::procedure) "bgl_gst_add_finalizer")
    ($gst-invoke-callbacks::void () "bgl_gst_invoke_callbacks")
    ($gst-invoke-finalizers::void () "bgl_gst_invoke_finalizers")))
