/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/libuv/src/Clib/bgluv.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May  6 13:53:14 2014                          */
/*    Last change :  Mon Jul 21 10:13:32 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    LIBUV Bigloo C binding                                           */
/*=====================================================================*/
#include <bigloo.h>

#include <uv.h>

#include "bgluv.h"

/*---------------------------------------------------------------------*/
/*    bgl_uv_mutex                                                     */
/*---------------------------------------------------------------------*/
extern obj_t bgl_uv_mutex;
extern obj_t bgl_make_input_port( obj_t, FILE *, obj_t, obj_t );

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    gc_marks ...                                                     */
/*---------------------------------------------------------------------*/
obj_t gc_marks = BNIL;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    gc_mark ...                                                      */
/*---------------------------------------------------------------------*/
static void
gc_mark( obj_t obj ) {
   BGL_MUTEX_LOCK( bgl_uv_mutex );
   gc_marks = MAKE_PAIR( obj, gc_marks );
   BGL_MUTEX_UNLOCK( bgl_uv_mutex );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    gc_unmark ...                                                    */
/*---------------------------------------------------------------------*/
void
gc_unmark( obj_t obj ) {
   BGL_MUTEX_LOCK( bgl_uv_mutex );
   gc_marks = bgl_remq( obj, gc_marks );
   BGL_MUTEX_UNLOCK( bgl_uv_mutex );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_close_cb ...                                              */
/*---------------------------------------------------------------------*/
void
bgl_uv_close_cb( uv_handle_t *handle ) {
   uv_timer_t *t = (uv_timer_t *)handle;
   BgL_uvhandlez00_bglt o = (BgL_uvhandlez00_bglt)handle->data;
   obj_t p = o->BgL_onclosez00;

   if( PROCEDUREP( p ) ) PROCEDURE_ENTRY( p )( p, BEOA );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_timer_cb ...                                              */
/*---------------------------------------------------------------------*/
void
bgl_uv_timer_cb( uv_timer_t *handle, int status ) {
   BgL_uvwatcherz00_bglt o = (BgL_uvwatcherz00_bglt)handle->data;
   obj_t p = o->BgL_cbz00;
   
   PROCEDURE_ENTRY( p )( p, o, BINT( status ), BEOA );
}

/*---------------------------------------------------------------------*/
/*    uv_timer_t *                                                     */
/*    bgl_uv_timer_new ...                                             */
/*---------------------------------------------------------------------*/
uv_timer_t *
bgl_uv_timer_new( BgL_uvtimerz00_bglt o, BgL_uvloopz00_bglt loop ) {
   uv_timer_t *new = (uv_timer_t *)GC_MALLOC( sizeof( uv_timer_t ) );
   new->data = o;
   new->close_cb = &bgl_uv_close_cb;

   uv_timer_init( (uv_loop_t *)loop->BgL_z42builtinz42, new );
   return new;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_async_cb ...                                              */
/*---------------------------------------------------------------------*/
static void
bgl_uv_async_cb( uv_async_t *handle ) {
   BgL_uvwatcherz00_bglt o = (BgL_uvwatcherz00_bglt)handle->data;
   obj_t p = o->BgL_cbz00;

   if( PROCEDUREP( p ) ) {
      PROCEDURE_ENTRY( p )( p, o, BEOA );
   }
}

/*---------------------------------------------------------------------*/
/*    uv_async_t *                                                     */
/*    bgl_uv_async_new ...                                             */
/*---------------------------------------------------------------------*/
uv_async_t *
bgl_uv_async_new( BgL_uvasyncz00_bglt o, BgL_uvloopz00_bglt loop ) {
   uv_async_t *new = (uv_async_t *)GC_MALLOC( sizeof( uv_async_t ) );
   new->data = o;

   uv_async_init( (uv_loop_t *)loop->BgL_z42builtinz42, new, &bgl_uv_async_cb );
   return new;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_cb ...                                                 */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_cb( uv_fs_t *req ) {
   obj_t p = (obj_t)req->data;

   gc_unmark( p );

   if( PROCEDUREP( p ) ) {
      PROCEDURE_ENTRY( p )( p, BINT( req->result ), string_to_bstring( (char *)req->path ), BEOA );
   }
   
   uv_fs_req_cleanup( req );
   free( req );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_rename_file ...                                           */
/*---------------------------------------------------------------------*/
void
bgl_uv_rename_file( char *oldp, char *newp, obj_t proc, BgL_uvloopz00_bglt loop ) {
   uv_fs_t *req = (uv_fs_t *)malloc( sizeof( uv_fs_t ) );
   req->data = proc;

   gc_mark( proc );
   
   uv_fs_rename( (uv_loop_t *)loop->BgL_z42builtinz42, req, oldp, newp, &bgl_uv_fs_cb );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    _irq ...                                                         */
/*---------------------------------------------------------------------*/
static obj_t _irq, _idle, _sys, _nice, _user, _times, _speed, _model;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    uv_init_cpus ...                                                 */
/*---------------------------------------------------------------------*/
static void
uv_init_cpus() {
   _irq = string_to_symbol( "irq" );
   _idle = string_to_symbol( "idle" );
   _sys = string_to_symbol( "sys" );
   _nice = string_to_symbol( "nice" );
   _user = string_to_symbol( "user" );
   _times = string_to_symbol( "times" );
   _speed = string_to_symbol( "speed" );
   _model = string_to_symbol( "model" );
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_cpu ...                                                   */
/*---------------------------------------------------------------------*/
static obj_t
bgl_uv_cpu( uv_cpu_info_t cpu ) {
   obj_t res = BNIL;
   obj_t times = BNIL;

   times = MAKE_PAIR(
      MAKE_PAIR( _irq, DOUBLE_TO_REAL( (double)cpu.cpu_times.irq ) ),
      times );
   times = MAKE_PAIR(
      MAKE_PAIR( _idle, DOUBLE_TO_REAL( (double)cpu.cpu_times.idle ) ),
      times );
   times = MAKE_PAIR(
      MAKE_PAIR( _sys, DOUBLE_TO_REAL( (double)cpu.cpu_times.sys ) ),
      times );
   times = MAKE_PAIR(
      MAKE_PAIR( _nice, DOUBLE_TO_REAL( (double)cpu.cpu_times.nice) ),
      times );
   times = MAKE_PAIR(
      MAKE_PAIR( _user, DOUBLE_TO_REAL( (double)cpu.cpu_times.user ) ),
      times );

   res = MAKE_PAIR( MAKE_PAIR( _times, times ), res );
   res = MAKE_PAIR( MAKE_PAIR( _speed, BINT( cpu.speed ) ), res );
   res = MAKE_PAIR( MAKE_PAIR( _model, string_to_bstring( cpu.model ) ), res );

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_cpus ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_cpus() {
   int count;
   uv_cpu_info_t *cpus;

   if( uv_cpu_info( &cpus, &count ) ) {
      return create_vector( 0 );
   } else {
      obj_t vec = create_vector( count );
      int i;

      uv_init_cpus();
      
      for( i = 0; i < count; i++ ) {
	 VECTOR_SET( vec, i, bgl_uv_cpu( cpus[ i ] ) );
      }

      uv_free_cpu_info( cpus, count );
      return vec;
   }
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_uv_close_file ...                                            */
/*---------------------------------------------------------------------*/
static int
bgl_uv_close_file( int file ) {
   uv_fs_t *req = (uv_fs_t *)malloc( sizeof( uv_fs_t ) );
   uv_loop_t *loop = uv_default_loop();
   int res = uv_fs_close( loop, req, (uv_file)file, 0L );

   uv_fs_req_cleanup( req );
   free( req );
   return res;
}

static void read_cb( uv_stream_t* stream,
			ssize_t nread,
			const uv_buf_t* buf ) {
   fprintf( stderr, "UV_READ_CB stream=%d nread=%d buf=%p\n",
	    stream, nread, buf );
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_uv_read ...                                                  */
/*---------------------------------------------------------------------*/
static long
bgl_uv_read( obj_t port, void *buf, size_t count ) {
   uv_fs_t *req = (uv_fs_t *)malloc( sizeof( uv_fs_t ) );
   uv_loop_t *loop = uv_default_loop();
   uv_file file = (uv_file)(long)PORT_FILE( port );
   int res = uv_fs_read( loop, req, (uv_file)file, buf, count, 0, &read_cb );

   if( res == -1 ) {
      fprintf( stderr, "bgl_uv_read.1 file=%d count=%d res=%d\n", file, count, res );
   }

   res = req->result;

   if( res < 0 ) {
      fprintf( stderr, "bgl_uv_read.2 file=%d count=%d res=%d %s %s\n", file, count, res,
	       uv_strerror( res ), uv_err_name( res ) );
   }
      
   uv_fs_req_cleanup( req );
   free( req );
   return res;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_open_input_file ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_open_input_file( obj_t name, obj_t buffer, obj_t proc ) {
   uv_fs_t *req = (uv_fs_t *)malloc( sizeof( uv_fs_t ) );
   char *path = BSTRING_TO_STRING( name );
   uv_loop_t *loop = uv_default_loop();
   obj_t res;
   int r;
   
   req->data = proc;

   if( r = uv_fs_open( loop, req, path, O_RDONLY, 0, 0 ) < 0 ) {
      uv_fs_req_cleanup( req );
      free( req );
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"open-input-uv-file",
			"Cannot open file for input",
			name );
   } else {
      obj_t port = bgl_make_input_port( name, (FILE *)req->result, KINDOF_FILE, buffer );

      fprintf( stderr, "open=%d fd=%d\n", r, req->result );
      if( proc == BFALSE ) {
	 uv_fs_req_cleanup( req );
	 free( req );
      }
      
      INPUT_PORT( port ).sysread = &bgl_uv_read;
      INPUT_PORT( port ).sysseek = 0L;
      INPUT_PORT( port ).port.sysclose = &bgl_uv_close_file;
      
      return port;
   }
}
