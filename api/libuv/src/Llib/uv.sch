;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/api/libuv/src/Llib/uv.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 11:57:14 2014                          */
;*    Last change :  Tue Oct 23 11:49:02 2018 (serrano)                */
;*    Copyright   :  2014-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV C bindings                                                 */
;*    -------------------------------------------------------------    */
;*    https://github.com/thlorenz/libuv-dox                            */
;*    http://nikhilm.github.io/uvbook/basics.html                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives

   (extern
      (include "uv.h")
      (include "buv.h")

      ;; misc
      (macro $void*_nil::void* "0L")
      (macro $string-nil::string "0L")
      (macro $string-nilp::bool (::string) "((char *)0L) ==" )
      
      (type double* void* "double *")
      (macro &double::double* (::double) "&")
      (macro $f64vector->double*::double* (::f64vector ::int) "&BGL_F64VREF")
      (macro $u8vector->double*::double* (::u8vector ::int) "&BGL_F64VREF")
      
      (macro $uv-strerror::string (::int) "(char *)uv_strerror")
      (macro $uv-err-name::string (::int) "(char *)uv_err_name")

      (macro $uv-version::string () "(char *)uv_version_string")

      ;; uv-state
      (type $uv_stat_t void* "uv_stat_t *")
      (macro $uv-stat-nil::$uv_stat_t "0L")
      
      ;; handle
      (type $uv_handle_t void* "uv_handle_t *")
      (type $uv_close_cb void* "uv_close_cb")
      (macro $uv-handle-t::$uv_handle_t (::void*) "(uv_handle_t *)")
      
      (macro $uv-handle->integer::long (::$uv_handle_t) "(long)")
      (macro $uv_handle_nil::$uv_handle_t "0L")
      (macro $uv_handle_nilp::bool (::$uv_handle_t) "((uv_handle_t *)0L) == ")
      
      (macro $uv-handle-ref::void (::$uv_handle_t) "uv_ref")
      (macro $uv-handle-unref::void (::$uv_handle_t) "uv_unref")
      (macro $uv-handle-has-ref?::bool (::$uv_handle_t) "uv_has_ref")
      (macro $uv-handle-close::void (::$uv_handle_t ::$uv_close_cb) "uv_close")
      (macro $uv-handle-active?::bool (::$uv_handle_t) "uv_is_active")
      
      ($bgl_uv_close_cb::$uv_close_cb (::$uv_handle_t) "bgl_uv_close_cb")
      (macro $BGL_UV_CLOSE_CB::$uv_close_cb "(uv_close_cb)&bgl_uv_close_cb")
      
      ;; loop
      (type $uv_loop_t void* "uv_loop_t *")
      (macro $uv-loop-t::$uv_loop_t (::$uv_handle_t) "(uv_loop_t *)")
      
      (macro $uv_loop_nil::$uv_loop_t "0L")
      (macro $uv_loop_nilp::bool (::$uv_loop_t) "((uv_loop_t *)0L) == ")
      
      (macro $uv_loop_new::$uv_loop_t () "uv_loop_new")
      (macro $uv_default_loop::$uv_loop_t () "uv_default_loop")
      
      (macro $uv-run::int (::$uv_loop_t ::int) "uv_run")
      (macro $uv-stop::void (::$uv_loop_t) "uv_stop")
      (macro $uv-loop-alive?::bool (::$uv_handle_t) "uv_loop_alive")
      (macro $uv-update-time::void (::$uv_handle_t) "uv_update_time")
      (macro $uv-now::uint64 (::$uv_handle_t) "uv_now")
      
      (macro $UV_RUN_DEFAULT::int "UV_RUN_DEFAULT")
      
      ;; queue
      (type $uv_work_t void* "uv_work_t *")
      
      (macro $uv_work_nil::$uv_work_t "0L")
      
      ($bgl_uv_queue_work::void (::UvWork ::UvLoop) "bgl_uv_queue_work")
      (macro $uv_cancel::void (::$uv_req_t) "uv_cancel")
      
      ;; request
      (type $uv_req_t void* "uv_req_t *")
      (macro $uv-req-t::$uv_req_t (::$uv_work_t) "(uv_req_t *)")
      
      ;; fs-event
      (type $uv_fs_event_t void* "uv_fs_event_t *")
      (type $uv_fs_event_cb void* "uv_fs_event_cb")
      (macro $uv-fs-event-t::$uv_fs_event_t (::$uv_handle_t) "(uv_fs_event_t *)")
      
      (macro $uv_fs_event_nil::$uv_fs_event_t "0L")
      
      ($bgl_uv_fs_event_new::$uv_fs_event_t (::UvFsEvent ::UvLoop) "bgl_uv_fs_event_new")
      (macro $uv_fs_event_start::void (::$uv_fs_event_t ::$uv_fs_event_cb ::string ::int) "uv_fs_event_start")
      (macro $uv_fs_event_stop::void (::$uv_fs_event_t) "uv_fs_event_stop")
      
      ($bgl_uv_fs_event_cb::$uv_fs_event_cb (::$uv_fs_event_t ::string ::int ::int) "bgl_uv_fs_event_cb")
      (macro $BGL_UV_FS_EVENT_CB::$uv_fs_event_cb "(uv_fs_event_cb)&bgl_uv_fs_event_cb")
      
      (macro $UV_RENAME::int "UV_RENAME")
      (macro $UV_CHANGE::int "UV_CHANGE")
      
      ;; fs-poll
      (type $uv_fs_poll_t void* "uv_fs_poll_t *")
      (type $uv_fs_poll_cb void* "uv_fs_poll_cb")
      (macro $uv-fs-poll-t::$uv_fs_poll_t (::$uv_handle_t) "(uv_fs_poll_t *)")
      
      (macro $uv_fs_poll_nil::$uv_fs_poll_t "0L")
      
      ($bgl_uv_fs_poll_new::$uv_fs_poll_t (::UvFsPoll ::UvLoop) "bgl_uv_fs_poll_new")
      (macro $uv_fs_poll_start::void (::$uv_fs_poll_t ::$uv_fs_poll_cb ::string ::int) "uv_fs_poll_start")
      (macro $uv_fs_poll_stop::void (::$uv_fs_poll_t) "uv_fs_poll_stop")
      
      ($bgl_uv_fs_poll_cb::$uv_fs_poll_cb (::$uv_fs_poll_t ::int ::$uv_stat_t ::$uv_stat_t) "bgl_uv_fs_poll_cb")
      ($bgl_uv_fs_poll_getpath::bstring (::$uv_fs_poll_t) "bgl_uv_fs_poll_getpath")
      (macro $BGL_UV_FS_POLL_CB::$uv_fs_poll_cb "(uv_fs_poll_cb)&bgl_uv_fs_poll_cb")
      
      ;; poll
      (type $uv_poll_t void* "uv_poll_t *")
      (type $uv_poll_cb void* "uv_poll_cb")
      (macro $uv-poll-t::$uv_poll_t (::$uv_handle_t) "(uv_poll_t *)")
      (macro $uv_readable::int "UV_READABLE")
      (macro $uv_writable::int "UV_WRITABLE")
      ;;(macro $uv_disconnect::int "UV_DISCONNECT")
      
      (macro $uv_poll_nil::$uv_poll_t "0L")
      
      ($bgl_uv_poll_new::$uv_poll_t (::UvPoll ::UvLoop) "bgl_uv_poll_new")
      (macro $uv_poll_start::void (::$uv_poll_t ::int ::$uv_poll_cb) "uv_poll_start")
      (macro $uv_poll_stop::void (::$uv_poll_t) "uv_poll_stop")
      
      ($bgl_uv_poll_cb::$uv_poll_cb (::$uv_poll_t ::int ::int) "bgl_uv_poll_cb")
      (macro $BGL_UV_POLL_CB::$uv_poll_cb "(uv_poll_cb)&bgl_uv_poll_cb")
      
      ;; timer
      (type $uv_timer_t void* "uv_timer_t *")
      (type $uv_timer_cb void* "uv_timer_cb")
      (macro $uv-timer-t::$uv_timer_t (::$uv_handle_t) "(uv_timer_t *)")
      
      (macro $uv_timer_nil::$uv_timer_t "0L")
      
      ($bgl_uv_timer_new::$uv_timer_t (::UvTimer ::UvLoop) "bgl_uv_timer_new")
      (macro $uv_timer_start::void (::$uv_timer_t ::$uv_timer_cb ::uint64 ::uint64) "uv_timer_start")
      (macro $uv_timer_stop::void (::$uv_timer_t) "uv_timer_stop")
      
      ($bgl_uv_timer_cb::$uv_timer_cb (::$uv_timer_t) "bgl_uv_timer_cb")
      (macro $BGL_UV_TIMER_CB::$uv_timer_cb "(uv_timer_cb)&bgl_uv_timer_cb")

      (macro $uv-hrtime::uint64 () "uv_hrtime")
      
      ;; idle
      (type $uv_idle_t void* "uv_idle_t *")
      (type $uv_idle_cb void* "uv_idle_cb")
      (macro $uv-idle-t::$uv_idle_t (::$uv_handle_t) "(uv_idle_t *)")
      
      (macro $uv_idle_nil::$uv_idle_t "0L")
      
      ($bgl_uv_idle_new::$uv_idle_t (::UvIdle ::UvLoop) "bgl_uv_idle_new")
      (macro $uv_idle_start::void (::$uv_idle_t ::$uv_idle_cb) "uv_idle_start")
      (macro $uv_idle_stop::void (::$uv_idle_t) "uv_idle_stop")
      
      ($bgl_uv_idle_cb::$uv_idle_cb (::$uv_idle_t ::int) "bgl_uv_handle_cb")
      (macro $BGL_UV_IDLE_CB::$uv_idle_cb "(uv_idle_cb)&bgl_uv_handle_cb")
      
      ;; check
      (type $uv_check_t void* "uv_check_t *")
      (type $uv_check_cb void* "uv_check_cb")
      (macro $uv-check-t::$uv_check_t (::$uv_handle_t) "(uv_check_t *)")
      
      (macro $uv_check_nil::$uv_check_t "0L")
      
      ($bgl_uv_check_new::$uv_check_t (::UvCheck ::UvLoop) "bgl_uv_check_new")
      (macro $uv_check_start::void (::$uv_check_t ::$uv_check_cb) "uv_check_start")
      (macro $uv_check_stop::void (::$uv_check_t) "uv_check_stop")
      
      ($bgl_uv_check_cb::$uv_check_cb (::$uv_check_t ::int) "bgl_uv_handle_cb")
      (macro $BGL_UV_CHECK_CB::$uv_check_cb "(uv_check_cb)&bgl_uv_handle_cb")
      
      ;; async
      (type $uv_async_t void* "uv_async_t *")
      (type $uv_async_cb void* "uv_async_cb")
      (macro $uv-async-t::$uv_async_t (::$uv_handle_t) "(uv_async_t *)")
      
      (infix macro $uv_async_nil::$uv_async_t () "0L")
      
      ($bgl_uv_async_new::$uv_async_t (::UvAsync ::UvLoop) "bgl_uv_async_new")
      (macro $uv_async_send::void (::$uv_async_t) "uv_async_send")
      
      ;; fs
      (macro $dup::int (::int) "dup")
	 
      ($uv-open-input-file::obj (::obj ::obj ::obj)
	 "bgl_uv_open_input_file")
      
      ($uv-fs-rename::int (::string ::string ::obj ::UvLoop)
	 "bgl_uv_fs_rename")
      ($uv-fs-ftruncate::int (::UvFile ::int64 ::obj ::UvLoop)
	 "bgl_uv_fs_ftruncate")
      ($uv-fs-truncate::int (::string ::long ::obj ::UvLoop)
	 "bgl_uv_fs_truncate")
      ($uv-fs-chown::int (::string ::int ::int ::obj ::UvLoop)
	 "bgl_uv_fs_chown")
      ($uv-fs-fchown::int (::UvFile ::int ::int ::obj ::UvLoop)
	 "bgl_uv_fs_fchown")
      ($uv-fs-lchown::int (::string ::int ::int ::obj ::UvLoop)
	 "bgl_uv_fs_lchown")
      ($uv-fs-chmod::int (::string ::int ::obj ::UvLoop)
	 "bgl_uv_fs_chmod")
      ($uv-fs-fchmod::int (::UvFile ::int ::obj ::UvLoop)
	 "bgl_uv_fs_fchmod")
      ($uv-fs-open::obj (::bstring ::int ::int ::obj ::UvLoop)
	 "bgl_uv_fs_open")
      ($uv-fs-close::int (::UvFile ::obj ::UvLoop)
	 "bgl_uv_fs_close")
      ($uv-fs-fstat::obj (::UvFile ::obj ::UvLoop)
	 "bgl_uv_fs_fstat")
      ($uv-fs-lstat::obj (::string ::obj ::UvLoop)
	 "bgl_uv_fs_lstat")
      ($uv-fs-stat::obj (::string ::obj ::UvLoop)
	 "bgl_uv_fs_stat")
      ($uv-fs-link::int (::string ::string ::obj ::UvLoop)
	 "bgl_uv_fs_link")
      ($uv-fs-symlink::int (::string ::string ::obj ::UvLoop)
	 "bgl_uv_fs_symlink")
      ($uv-fs-readlink::obj (::string ::obj ::UvLoop)
	 "bgl_uv_fs_readlink")
      ($uv-fs-unlink::int (::string ::obj ::UvLoop)
	 "bgl_uv_fs_unlink")
      ($uv-fs-rmdir::int (::string ::obj ::UvLoop)
	 "bgl_uv_fs_rmdir")
      ($uv-fs-mkdir::int (::string ::int ::obj ::UvLoop)
	 "bgl_uv_fs_mkdir")
      ($uv-fs-fsync::int (::UvFile ::obj ::UvLoop)
	 "bgl_uv_fs_fsync")
      ($uv-fs-fdatasync::int (::UvFile ::obj ::UvLoop)
	 "bgl_uv_fs_fdatasync")
      ($uv-fs-futime::int (::UvFile ::double ::double ::obj ::UvLoop)
	 "bgl_uv_fs_futime")
      ($uv-fs-utime::int (::string ::double ::double ::obj ::UvLoop)
	 "bgl_uv_fs_utime")
      ($uv-fs-write::int (::UvFile ::bstring ::long ::long ::int64 ::obj ::UvLoop)
	 "bgl_uv_fs_write")
      ($uv-fs-read::int (::UvFile ::bstring ::long ::long ::int64 ::obj ::UvLoop)
	 "bgl_uv_fs_read")
      (macro $uv-guess-handle::int (::int)
	     "uv_guess_handle")
      
      (macro $uv-handle-tcp::int "UV_TCP")
      (macro $uv-handle-tty::int "UV_TTY")
      (macro $uv-handle-udp::int "UV_UDP")
      (macro $uv-handle-pipe::int "UV_NAMED_PIPE")
      (macro $uv-handle-file::int "UV_FILE")
      (macro $uv-handle-unknown::int "UV_UNKNOWN_HANDLE")
      
      ;; pipe
      (type $uv_pipe_t void* "uv_pipe_t *")
      (macro $uv-pipe-t::$uv_pipe_t (::$uv_handle_t) "(uv_pipe_t *)")
      (infix macro $uv-pipe-ipc?::bool (::$uv_pipe_t) "->ipc")
      
      (macro $uv-pipe-init::int (::$uv_loop_t ::$uv_pipe_t ::int)
	     "uv_pipe_init")
      ($uv-pipe-create::$uv_pipe_t (::$uv_loop_t ::obj ::bool)
	 "bgl_uv_pipe_create")
      (macro $uv-pipe-open::int (::$uv_loop_t ::int)
	     "uv_pipe_open")
      (macro $uv-pipe-bind::int (::$uv_pipe_t ::string)
	     "uv_pipe_bind")
      ($uv-pipe-connect::void (::UvPipe ::string ::obj ::UvLoop)
	 "bgl_uv_pipe_connect")
      (macro $uv-pipe-pending-instances::int (::$uv_pipe_t ::int)
	     "uv_pipe_pending_instances")
      ;; os
      (macro $uv-loadavg::void (::double*) "uv_loadavg")
      (macro $uv-get-free-memory::double () "uv_get_free_memory")
      (macro $uv-get-total-memory::double () "uv_get_total_memory")
      ($uv-resident-memory::long () "bgl_uv_resident_memory")
      ($uv-cpus::vector () "bgl_uv_cpus")
      ($uv-exepath::bstring () "bgl_uv_exepath")
      (macro $uv-setup-args::void (::pair-nil) "bgl_uv_setup_args")
      ($uv-process-title-init!::void () "bgl_uv_process_title_init")
      (macro $uv-get-process-title::int (::string ::int) "uv_get_process_title")
      (macro $uv-set-process-title!::int (::string) "uv_set_process_title")
      
      (macro $uv-uptime::int (::double*) "uv_uptime")
      
      ;; dns
      ($uv-getaddrinfo::int (::string ::string ::int ::obj ::UvLoop)
	 "bgl_uv_getaddrinfo")
      
      ($uv-inet-pton::obj (::string ::int)
	 "bgl_uv_inet_pton")
      
      ;; stream
      (type $uv_stream_t void* "uv_stream_t *")
      (macro $uv-stream-t::$uv_stream_t (::$uv_handle_t) "(uv_stream_t *)")
      
      (infix macro $uv-stream-write-queue-size::long (::$uv_stream_t) "->write_queue_size")
      (infix macro $uv-stream-fd::long (::$uv_stream_t) "->io_watcher.fd")
      ($uv-write::int (::UvHandle ::string ::long ::long ::procedure ::UvLoop)
	 "bgl_uv_write")
      ($uv-write2::int (::UvHandle ::string ::long ::long ::obj ::procedure ::UvLoop)
	 "bgl_uv_write2")
      ($uv-read-start::int (::UvHandle ::procedure ::procedure ::UvLoop)
	 "bgl_uv_read_start")
      ($uv-read-stop::int (::$uv_stream_t)
	 "uv_read_stop")
      ($uv-shutdown::int (::UvStream ::obj ::UvLoop)
	 "bgl_uv_shutdown")
      ($uv-listen::int (::UvStream ::int ::obj ::UvLoop)
	 "bgl_uv_listen")
      (macro $uv-accept::int (::$uv_stream_t ::$uv_stream_t)
	     "uv_accept")
      (macro $uv-is-closing::int (::$uv_stream_t)
	     "uv_is_closing")
      (macro $uv-is-writable::int (::$uv_stream_t)
	     "uv_is_writable")
      (macro $uv-is-readable::int (::$uv_stream_t)
	     "uv_is_readable")
      
      ;; tcp
      (type $uv_tcp_t void* "uv_tcp_t *")
      (macro $uv-tcp-t::$uv_tcp_t (::$uv_handle_t) "(uv_tcp_t *)")
      
      (macro $uv-tcp-init::int (::$uv_loop_t ::$uv_tcp_t)
	     "uv_tcp_init")
      ($uv-tcp-create::$uv_tcp_t (::$uv_loop_t ::obj)
	 "bgl_uv_tcp_create")
      
      ($uv-tcp-connect::int (::UvTcp ::string ::int ::int ::obj ::UvLoop)
	 "bgl_uv_tcp_connect")
      (macro $uv-tcp-open::int (::$uv_tcp_t ::int)
	     "uv_tcp_open")
      ($uv-tcp-bind::int (::$uv_tcp_t ::string ::int ::int)
	 "bgl_uv_tcp_bind")
      (macro $uv-tcp-nodelay::int (::$uv_tcp_t ::bool)
	     "uv_tcp_nodelay")
      (macro $uv-tcp-keepalive::int (::$uv_tcp_t ::bool ::uint)
	     "uv_tcp_keepalive")
      (macro $uv-tcp-simultaneous-accepts::int (::$uv_tcp_t ::bool)
	     "uv_tcp_simultaneous_accepts")
      ($uv-tcp-getsockname::obj (::$uv_tcp_t)
	 "bgl_uv_tcp_getsockname")
      ($uv-tcp-getpeername::obj (::$uv_tcp_t)
	 "bgl_uv_tcp_getpeername")
      
      ;; tty
      (type $uv_tty_t void* "uv_tty_t *")
      (macro $uv-tty-t::$uv_tty_t (::$uv_handle_t) "(uv_tty_t *)")
;*       ;; libuv 1.1.1                                                */
;*       (type $uv_tty_mode_t void* "uv_tty_mode_t")                   */

;*       (infix macro $uv-tty-fd::long (::$uv_tty_t) "->io_watcher.fd") */
;*       (macro $uv-tty-mode-normal::$uv_tty_mode_t "UV_TTY_MODE_NORMAL") */
;*       (macro $uv-tty-mode-raw::$uv_tty_mode_t "UV_TTY_MODE_RAW")    */
;*       (macro $uv-tty-mode-io::$uv_tty_mode_t "UV_TTY_MODE_IO")      */
      
      ($uv-tty-create::$uv_tty_t (::$uv_loop_t ::obj ::int ::bool)
	 "bgl_uv_tty_create")
      (macro $uv-tty-set-mode::int (::$uv_tty_t ::int) "uv_tty_set_mode")
      ($uv-tty-get-winsize::vector (::$uv_tty_t) "bgl_uv_tty_get_winsize")
      
      ;; udp
      (type $uv_udp_t void* "uv_udp_t *")
      (macro $uv-udp-t::$uv_udp_t (::$uv_handle_t) "(uv_udp_t *)")
      (type $uv_membership_t void* "uv_membership")

      (macro $uv-membership-join-group::$uv_membership_t "UV_JOIN_GROUP")
      (macro $uv-membership-leave-group::$uv_membership_t "UV_LEAVE_GROUP")
      (infix macro $uv-udp-fd::long (::$uv_udp_t) "->io_watcher.fd")
      
      ($uv-udp-create::$uv_udp_t (::$uv_loop_t ::obj)
	 "bgl_uv_udp_create")
      ($uv-udp-bind::int (::$uv_udp_t ::string ::int ::int ::int)
	 "bgl_uv_udp_bind")
      ($uv-udp-recv-start::int (::UvHandle ::procedure ::procedure ::UvLoop)
	 "bgl_uv_udp_recv_start")
      (macro $uv-udp-recv-stop::int (::$uv_udp_t)
	     "uv_udp_recv_stop")
      
      ($uv-udp-send::int (::$uv_udp_t ::bstring ::long ::long ::long ::string ::int ::obj ::UvLoop)
	 "bgl_uv_udp_send")
      ($uv-udp-getsockname::obj (::$uv_udp_t)
	 "bgl_uv_udp_getsockname")
      (macro $uv-udp-set-ttl::int (::$uv_udp_t ::int)
	 "uv_udp_set_ttl")
      (macro $uv-udp-set-multicast-ttl::int (::$uv_udp_t ::int)
	 "uv_udp_set_multicast_ttl")
      (macro $uv-udp-set-multicast-loop::int (::$uv_udp_t ::bool)
	 "uv_udp_set_multicast_loop")
      (macro $uv-udp-set-broadcast::int (::$uv_udp_t ::bool)
	     "uv_udp_set_broadcast")
      (macro $uv-udp-set-membership::int (::$uv_udp_t ::string ::string ::$uv_membership_t)
	     "uv_udp_set_membership")
      
      ;; process
      (type $uv_process_t void* "uv_process_t *")
      (type $uv_process_options_t void* "uv_process_options_t *")
      (type $uv_stdio_container_t void* "uv_stdio_container_t *")
      
      (macro $uv-process-t::$uv_process_t (::$uv_handle_t) "(uv_process_t *)")
      
      ($uv-process-new::$uv_process_t (::UvProcess) "bgl_uv_process_new")
      ($uv-process-spawn::int (::UvLoop ::UvProcess ::UvProcessOptions ::obj)
	 "bgl_uv_spawn")
      (macro $uv-process-kill::int (::$uv_process_t ::int)
	     "uv_process_kill")
      (macro $uv-kill::int (::int ::int)
	     "uv_kill")
      
      (infix macro $uv-process-pid::int (::$uv_process_t) "->pid")
      
      (infix macro $uv-process-options-new::$uv_process_options_t ()
	 "(uv_process_options_t *)GC_MALLOC( sizeof( uv_process_options_t ) )")
      
      (macro $uv-process-options-file::string (::$uv_process_options_t)
	     "BGL_UV_PROCESS_OPTIONS_FILE_GET" )
      (macro $uv-process-options-file-set!::void (::$uv_process_options_t ::string)
	     "BGL_UV_PROCESS_OPTIONS_FILE_SET")
      ($uv-process-options-args::vector (::$uv_process_options_t)
	 "bgl_uv_process_options_args_get" )
      ($uv-process-options-args-set!::void (::$uv_process_options_t ::vector)
	 "bgl_uv_process_options_args_set")
      ($uv-process-options-env::vector (::$uv_process_options_t)
	 "bgl_uv_process_options_env_get" )
      ($uv-process-options-env-set!::void (::$uv_process_options_t ::vector)
	 "bgl_uv_process_options_env_set")
      (macro $uv-process-options-cwd::string (::$uv_process_options_t)
	     "BGL_UV_PROCESS_OPTIONS_CWD_GET" )
      (macro $uv-process-options-cwd-set!::void (::$uv_process_options_t ::string)
	     "BGL_UV_PROCESS_OPTIONS_CWD_SET")
      (macro $uv-process-options-flags::int (::$uv_process_options_t)
	     "BGL_UV_PROCESS_OPTIONS_FLAGS_GET" )
      (macro $uv-process-options-flags-set!::void (::$uv_process_options_t ::int)
	     "BGL_UV_PROCESS_OPTIONS_FLAGS_SET")
      (macro $uv-process-options-stdio-count::int (::$uv_process_options_t)
	     "BGL_UV_PROCESS_OPTIONS_STDIO_COUNT_GET" )
      (macro $uv-process-options-stdio-count-set!::void (::$uv_process_options_t ::int)
	     "BGL_UV_PROCESS_OPTIONS_STDIO_COUNT_SET")
      (macro $uv-process-options-stdio::$uv_stdio_container_t (::$uv_process_options_t)
	     "BGL_UV_PROCESS_OPTIONS_STDIO_GET" )
      (macro $uv-process-options-stdio-set!::void (::$uv_process_options_t ::$uv_stdio_container_t)
	     "BGL_UV_PROCESS_OPTIONS_STDIO_SET")
      (macro $uv-process-options-uid::int (::$uv_process_options_t)
	     "BGL_UV_PROCESS_OPTIONS_UID_GET" )
      (macro $uv-process-options-uid-set!::void (::$uv_process_options_t ::int)
	     "BGL_UV_PROCESS_OPTIONS_UID_SET")
      (macro $uv-process-options-gid::int (::$uv_process_options_t)
	     "BGL_UV_PROCESS_OPTIONS_GID_GET" )
      (macro $uv-process-options-gid-set!::void (::$uv_process_options_t ::int)
	     "BGL_UV_PROCESS_OPTIONS_GID_SET")
      
      (macro $uv-process-options-stdio-container-set!::void
	 (::$uv_process_options_t ::int)
	 "BGL_UV_PROCESS_OPTIONS_STDIO_CONTAINER_SET")
      (macro $uv-process-options-stdio-container-stream-set!::void
	 (::$uv_process_options_t ::int ::$uv_stream_t)
	 "BGL_UV_PROCESS_OPTIONS_STDIO_CONTAINER_STREAM_SET")
      (macro $uv-process-options-stdio-container-fd-set!::void
	 (::$uv_process_options_t ::int ::int)
	 "BGL_UV_PROCESS_OPTIONS_STDIO_CONTAINER_FD_SET")
      (macro $uv-process-options-stdio-container-flags-set!::void
	 (::$uv_process_options_t ::int ::int)
	 "BGL_UV_PROCESS_OPTIONS_STDIO_CONTAINER_FLAGS_SET")
      
      (macro $UV_PROCESS_SETUID::int "UV_PROCESS_SETUID")
      (macro $UV_PROCESS_SETGID::int "UV_PROCESS_SETGID")
      (macro $UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS::int "UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS")
      (macro $UV_PROCESS_DETACHED::int "UV_PROCESS_DETACHED")
      (macro $UV_PROCESS_WINDOWS_HIDE::int "UV_PROCESS_WINDOWS_HIDE")
      
      (macro $UV_IGNORE::int "UV_IGNORE")
      (macro $UV_INHERIT-FD::int "UV_INHERIT_FD")
      (macro $UV_INHERIT-STREAM::int "UV_INHERIT_STREAM")
      (macro $UV_CREATE_PIPE::int "UV_CREATE_PIPE")
      (macro $UV_READABLE_PIPE::int "UV_READABLE_PIPE")
      (macro $UV_WRITABLE_PIPE::int "UV_WRITABLE_PIPE")
      
      ))




