;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/uv.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 11:57:14 2014                          */
;*    Last change :  Thu Jul 10 11:38:15 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
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

      ;; handle
      (type $uv_handle_t void* "uv_handle_t*")
      (type $uv_close_cb void* "uv_close_cb")
      (macro $uv-handle-t::$uv_handle_t (::void*) "(uv_handle_t *)")
      
      (macro $uv_handle_nil::$uv_handle_t "0L")
      (macro $uv_handle_nilp::bool (::$uv_handle_t) "((uv_handle_t *)0L) == ")
      
      (macro $uv-handle-ref::void (::$uv_handle_t) "uv_ref")
      (macro $uv-handle-unref::void (::$uv_handle_t) "uv_unref")
      (macro $uv-handle-close::void (::$uv_handle_t ::$uv_close_cb) "uv_close")

      ($bgl_uv_close_cb::$uv_close_cb (::$uv_handle_t) "bgl_uv_close_cb")
      (macro $BGL_UV_CLOSE_CB::$uv_close_cb "(uv_close_cb)&bgl_uv_close_cb")
      
      ;; loop
      (type $uv_loop_t void* "uv_loop_t*")
      (macro $uv-loop-t::$uv_loop_t (::$uv_handle_t) "(uv_loop_t *)")
      
      (macro $uv_loop_nil::$uv_loop_t "0L")
      (macro $uv_loop_nilp::bool (::$uv_loop_t) "((uv_loop_t *)0L) == ")
      
      (macro $uv_loop_new::$uv_loop_t () "uv_loop_new")
      (macro $uv_default_loop::$uv_loop_t () "uv_default_loop")
      
      (macro $uv-run::void (::$uv_loop_t ::int) "uv_run")
      
      (macro $UV_RUN_DEFAULT::int "UV_RUN_DEFAULT")

      ;; timer
      (type $uv_timer_t void* "uv_timer_t*")
      (type $uv_timer_cb void* "uv_timer_cb")
      (macro $uv-timer-t::$uv_timer_t (::$uv_handle_t) "(uv_timer_t *)")
      
      (macro $uv_timer_nil::$uv_timer_t "0L")
      
      ($bgl_uv_timer_new::$uv_timer_t (::UvTimer ::UvLoop) "bgl_uv_timer_new")
      (macro $uv_timer_start::void (::$uv_timer_t ::$uv_timer_cb ::uint64 ::uint64) "uv_timer_start")
      (macro $uv_timer_stop::void (::$uv_timer_t) "uv_timer_stop")

      ($bgl_uv_timer_cb::$uv_timer_cb (::$uv_timer_t ::int) "bgl_uv_timer_cb")
      (macro $BGL_UV_TIMER_CB::$uv_timer_cb "(uv_timer_cb)&bgl_uv_timer_cb")

      ;; async
      (type $uv_async_t void* "uv_async_t*")
      (type $uv_async_cb void* "uv_async_cb")
      (macro $uv-async-t::$uv_async_t (::$uv_handle_t) "(uv_async_t *)")
      
      (infix macro $uv_async_nil::$uv_async_t () "0L")
      
      ($bgl_uv_async_new::$uv_async_t (::UvAsync ::UvLoop) "bgl_uv_async_new")
      (macro $uv_async_send::void (::$uv_async_t) "uv_async_send")

      ;; fs
      (macro $uv-rename-file::void (::string ::string ::procedure ::UvLoop)
	     "bgl_uv_rename_file")
      ))




