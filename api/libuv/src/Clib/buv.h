/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/libuv/src/Clib/buv.h             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 17 17:34:44 2014                          */
/*    Last change :  Thu Jan  1 10:17:19 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C macros for the Bigloo UV binding                               */
/*=====================================================================*/

#define BGL_UV_PROCESS_OPTIONS_FILE_GET( o ) \
   ((char *)((o)->file))
#define BGL_UV_PROCESS_OPTIONS_FILE_SET( o, v ) \
   (((o)->file) = (v))

#define BGL_UV_PROCESS_OPTIONS_CWD_GET( o ) \
   ((char *)((o)->cwd))
#define BGL_UV_PROCESS_OPTIONS_CWD_SET( o, v ) \
   (((o)->cwd) = (v))

#define BGL_UV_PROCESS_OPTIONS_FLAGS_GET( o ) \
   (((o)->flags))
#define BGL_UV_PROCESS_OPTIONS_FLAGS_SET( o, v ) \
   (((o)->flags) = (v))

#define BGL_UV_PROCESS_OPTIONS_STDIO_COUNT_GET( o ) \
   (((o)->stdio_count))
#define BGL_UV_PROCESS_OPTIONS_STDIO_COUNT_SET( o, v ) \
   (((o)->stdio_count) = (v))

#define BGL_UV_PROCESS_OPTIONS_STDIO_GET( o ) \
   (((o)->stdio))
#define BGL_UV_PROCESS_OPTIONS_STDIO_SET( o, v ) \
   (((o)->stdio) = (v))

#define BGL_UV_PROCESS_OPTIONS_UID_GET( o ) \
   (((o)->uid))
#define BGL_UV_PROCESS_OPTIONS_UID_SET( o, v ) \
   (((o)->uid) = (v))

#define BGL_UV_PROCESS_OPTIONS_GID_GET( o ) \
   (((o)->gid))
#define BGL_UV_PROCESS_OPTIONS_GID_SET( o, v ) \
   (((o)->gid) = (v))

#define BGL_UV_PROCESS_OPTIONS_STDIO_CONTAINER_SET( o, len ) \
   ((o)->stdio = ((uv_stdio_container_t *)GC_MALLOC( sizeof( uv_stdio_container_t ) * len )), \
    (o)->stdio_count = len)

#define BGL_UV_PROCESS_OPTIONS_STDIO_CONTAINER_STREAM_SET( o, i, v ) \
   ((o)->stdio[ i ].data.stream = (v))

#define BGL_UV_PROCESS_OPTIONS_STDIO_CONTAINER_FD_SET( o, i, v ) \
   ((o)->stdio[ i ].data.fd = (v))

#define BGL_UV_PROCESS_OPTIONS_STDIO_CONTAINER_FLAGS_SET( o, i, v ) \
   ((o)->stdio[ i ].flags = (v))
