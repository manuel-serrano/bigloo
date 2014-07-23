/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/libuv/src/Clib/bgluv.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May  6 13:53:14 2014                          */
/*    Last change :  Wed Jul 23 12:39:19 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    LIBUV Bigloo C binding                                           */
/*=====================================================================*/
#include <bigloo.h>

#include <uv.h>

#include "bgluv.h"

/*---------------------------------------------------------------------*/
/*    type alias                                                       */
/*---------------------------------------------------------------------*/
typedef BgL_uvloopz00_bglt bgl_uv_loop_t;
typedef BgL_uvfilez00_bglt bgl_uv_file_t;
typedef BgL_uvhandlez00_bglt bgl_uv_handle_t;
typedef BgL_uvwatcherz00_bglt bgl_uv_watcher_t;
typedef BgL_uvasyncz00_bglt bgl_uv_async_t;

/*---------------------------------------------------------------------*/
/*    bgl_uv_mutex                                                     */
/*---------------------------------------------------------------------*/
extern obj_t bgl_uv_mutex;
extern obj_t bgl_make_input_port( obj_t, FILE *, obj_t, obj_t );
extern obj_t bgl_uv_new_file( int, obj_t );

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
   bgl_uv_handle_t o = (bgl_uv_handle_t)handle->data;
   obj_t p = o->BgL_onclosez00;

   if( PROCEDUREP( p ) ) PROCEDURE_ENTRY( p )( p, BEOA );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_timer_cb ...                                              */
/*---------------------------------------------------------------------*/
void
bgl_uv_timer_cb( uv_timer_t *handle, int status ) {
   bgl_uv_watcher_t o = (bgl_uv_watcher_t)handle->data;
   obj_t p = o->BgL_cbz00;
   
   PROCEDURE_ENTRY( p )( p, o, BINT( status ), BEOA );
}

/*---------------------------------------------------------------------*/
/*    uv_timer_t *                                                     */
/*    bgl_uv_timer_new ...                                             */
/*---------------------------------------------------------------------*/
uv_timer_t *
bgl_uv_timer_new( BgL_uvtimerz00_bglt o, bgl_uv_loop_t loop ) {
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
   bgl_uv_watcher_t o = (bgl_uv_watcher_t)handle->data;
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
bgl_uv_async_new( bgl_uv_async_t o, bgl_uv_loop_t loop ) {
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
/*    int                                                              */
/*    bgl_uv_fs_rename ...                                             */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_rename( char *oldp, char *newp, obj_t proc, bgl_uv_loop_t loop ) {
   if( PROCEDUREP( proc ) ) {
      if( PROCEDURE_CORRECT_ARITYP( proc, 2 ) ) {
	 uv_fs_t *req = (uv_fs_t *)malloc( sizeof( uv_fs_t ) );
   
	 req->data = proc;
	 gc_mark( proc );
	 uv_fs_rename( (uv_loop_t *)loop->BgL_z42builtinz42,
		       req, oldp, newp, &bgl_uv_fs_cb );

	 return 0;
      } else {
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-rename-file",
			   "wrong callback arity",
			   proc );
      }
   } else {
      uv_fs_t req;
      int res;
      
      uv_fs_rename( (uv_loop_t *)loop->BgL_z42builtinz42,
		    &req, oldp, newp, 0L );

      res = req.result;
      uv_fs_req_cleanup( &req );
      
      return res;
   }
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
      MAKE_PAIR( _irq, BGL_INT64_TO_BINT64( cpu.cpu_times.irq ) ),
      times );
   times = MAKE_PAIR(
      MAKE_PAIR( _idle, BGL_INT64_TO_BINT64( cpu.cpu_times.idle ) ),
      times );
   times = MAKE_PAIR(
      MAKE_PAIR( _sys, BGL_INT64_TO_BINT64( cpu.cpu_times.sys ) ),
      times );
   times = MAKE_PAIR(
      MAKE_PAIR( _nice, BGL_INT64_TO_BINT64( cpu.cpu_times.nice) ),
      times );
   times = MAKE_PAIR(
      MAKE_PAIR( _user, BGL_INT64_TO_BINT64( cpu.cpu_times.user ) ),
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

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_uv_sync_read ...                                             */
/*---------------------------------------------------------------------*/
static long
bgl_uv_sync_read( obj_t port, void *ptr, size_t num ) {
   int fd = (long)PORT_FILE( port );
   long n;

 loop:
   if( (n = read( fd, ptr, num ) ) <= 0 ) {
      if( n == 0 ) {
	 INPUT_PORT( port ).eof = 1;
      } else if( errno == EINTR ) {
	 goto loop;
      }
   }

   return n;
}
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_file_seek ...                                             */
/*---------------------------------------------------------------------*/
static void
bgl_uv_file_seek( obj_t port, long pos ) {
   int fd = (long)PORT_FILE( port );

   if( lseek( fd, pos, SEEK_SET ) == -1 ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "set-input-port-position!",
			strerror( errno ),
			port );
   }
      
   INPUT_PORT( port ).filepos = pos;
   INPUT_PORT( port ).eof = 0;
   INPUT_PORT( port ).matchstart = 0;
   INPUT_PORT( port ).matchstop = 0;
   INPUT_PORT( port ).forward = 0;
   INPUT_PORT( port ).bufpos = 0;
   INPUT_PORT( port ).lastchar = '\n';
   
   RGC_BUFFER_SET( port, 0, '\0' );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_open_input_file_cb ...                                 */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_open_input_file_cb( uv_fs_t* req ) {
   obj_t proc = CAR( (obj_t)req->data );
   obj_t buffer = CDR( (obj_t)req->data );
   obj_t port;

   gc_unmark( req->data );

   if( req->result == -1 ) {
      port = BINT( req->result );
   } else {
      port = bgl_make_input_port(
	 string_to_bstring( ( char*)(req->path) ), (FILE *)req->result, KINDOF_FILE, buffer );

      INPUT_PORT( port ).port.userdata = GC_MALLOC( sizeof( uv_fs_t ) );
      INPUT_PORT( port ).sysread = &bgl_uv_sync_read;
      INPUT_PORT( port ).sysseek = &bgl_uv_file_seek;
      INPUT_PORT( port ).port.sysclose = &bgl_uv_close_file;
      BGL_INPUT_PORT_LENGTH_SET( port, bgl_file_size( (char *)req->path ) );
   }
   
   uv_fs_req_cleanup( req );
   free( req );

   PROCEDURE_ENTRY( proc )( proc, port, BEOA );
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
   
   req->data = MAKE_PAIR( proc, buffer );
   gc_mark( req->data );

   if( r = uv_fs_open( loop, req, path, O_RDONLY, 0,
		       PROCEDUREP( proc ) ? bgl_uv_fs_open_input_file_cb : 0L ) < 0 ) {
      uv_fs_req_cleanup( req );
      free( req );
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-open-input-file",
			(char *)uv_strerror( req->result ),
			name );
   } else {
      obj_t port = bgl_make_input_port( name, (FILE *)req->result, KINDOF_FILE, buffer );

      if( proc == BFALSE ) {
	 uv_fs_req_cleanup( req );
	 free( req );
      }

      INPUT_PORT( port ).port.userdata = GC_MALLOC( sizeof( uv_fs_t ) );
      INPUT_PORT( port ).sysread = &bgl_uv_sync_read;
      INPUT_PORT( port ).sysseek = &bgl_uv_file_seek;
      INPUT_PORT( port ).port.sysclose = &bgl_uv_close_file;
      
      return port;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_open_cb ...                                            */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_open_cb( uv_fs_t* req ) {
   obj_t proc = (obj_t)req->data;
   obj_t port;

   gc_unmark( req->data );

   if( req->result == -1 ) {
      port = BINT( req->result );
   } else {
      port =
	 bgl_uv_new_file( req->result, string_to_bstring( (char *)req->path ) );
      ((bgl_uv_file_t)port)->BgL_z52readreqz52 =
	 GC_MALLOC( sizeof( uv_fs_t ) );
   }
   
   uv_fs_req_cleanup( req );
   free( req );

   PROCEDURE_ENTRY( proc )( proc, port, BEOA );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_open ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_open( obj_t bpath, int flags, int mode, obj_t proc, bgl_uv_loop_t bloop ) {
   char *path = BSTRING_TO_STRING( bpath );
   uv_loop_t *loop = (uv_loop_t *)bloop->BgL_z42builtinz42;
   
   if( PROCEDUREP( proc ) ) {
      if( PROCEDURE_CORRECT_ARITYP( proc, 1 ) ) {
	 uv_fs_t *req = (uv_fs_t *)malloc( sizeof( uv_fs_t ) );
   
	 req->data = proc;
	 gc_mark( req->data );

	 if( uv_fs_open( loop, req, path, flags, mode, bgl_uv_fs_open_cb ) < 0 ) {
	    uv_fs_req_cleanup( req );
	    free( req );
	    C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-open-input-file",
			      "Cannot open file for input",
			      bpath );
	 }

	 return BUNSPEC;
      } else {
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-fs-open",
			   "wrong callback arity",
			   proc );
      }
   } else {
      uv_fs_t req;

      if( uv_fs_open( loop, &req, path, flags, mode, 0L ) < 0 ) {
	 uv_fs_req_cleanup( &req );
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-open-input-file",
			   (char *)uv_strerror( req.result ),
			   bpath );
      } else {
	 obj_t res = bgl_uv_new_file( req.result, bpath );
	 ((bgl_uv_file_t)res)->BgL_z52readreqz52 =
	    GC_MALLOC( sizeof( uv_fs_t ) );
	 
	 uv_fs_req_cleanup( &req );

	 return res;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_fs_close_cb ...                                           */
/*---------------------------------------------------------------------*/
void
bgl_uv_fs_close_cb( uv_fs_t *req ) {
   obj_t p = (obj_t)req->data;

   gc_unmark( p );

   if( req->result < 0 ) {
      PROCEDURE_ENTRY( p )( p, BFALSE, BEOA );
   } else {
      PROCEDURE_ENTRY( p )( p, BTRUE, BEOA );
   }

   uv_fs_req_cleanup( req );
   free( req );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_close ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_close( obj_t port, obj_t proc, bgl_uv_loop_t bloop ) {
   uv_loop_t *loop = (uv_loop_t *)bloop->BgL_z42builtinz42;
   int fd = ((bgl_uv_file_t)port)->BgL_fdz00;

   if( PROCEDUREP( proc ) ) {
      uv_fs_t *req = (uv_fs_t *)malloc( sizeof( uv_fs_t ) );
      req->data = proc;
      gc_mark( proc );
      
      if( uv_fs_close( loop, req, fd, &bgl_uv_fs_close_cb ) < 0 ) {
	 uv_fs_req_cleanup( req );
	 free( req );
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-fs-close",
			   (char *)uv_strerror( req->result ),
			   port );
      }
   } else {
      uv_fs_t req;

      if( uv_fs_close( loop, &req, fd, 0L ) < 0 ) {
	 uv_fs_req_cleanup( &req );
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-fs-clsoe",
			   (char *)uv_strerror( req.result ),
			   port );
      } else {
	 return BTRUE;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    statbuf ...                                                      */
/*---------------------------------------------------------------------*/
static obj_t _dev, _mode, _nlink, _uid, _gid, _rdev, _ino, _size;
static obj_t _blksize, _blocks, _flags, _gen, _atime, _mtime, _ctime;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    uv_init_stat ...                                                 */
/*---------------------------------------------------------------------*/
static void
uv_init_stat() {
   _dev = string_to_symbol( "dev" );
   _mode = string_to_symbol( "mode" );
   _nlink = string_to_symbol( "nlink" );
   _uid = string_to_symbol( "uid" );
   _gid = string_to_symbol( "gid" );
   _rdev = string_to_symbol( "rdev" );
   _ino = string_to_symbol( "ino" );
   _size = string_to_symbol( "size" );
   _blksize = string_to_symbol( "blksize" );
   _blocks = string_to_symbol( "blocks" );
   _flags = string_to_symbol( "flags" );
   _gen = string_to_symbol( "gen" );
   _atime = string_to_symbol( "atime" );
   _mtime = string_to_symbol( "mtime" );
   _ctime = string_to_symbol( "ctime" );
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fstat ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fstat( uv_stat_t buf ) {
   uv_init_stat();
   obj_t res = BNIL;

   res = MAKE_PAIR(
      MAKE_PAIR( _ctime, ELONG_TO_BELONG( buf.st_ctim.tv_sec ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _mtime, ELONG_TO_BELONG( buf.st_mtim.tv_sec ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _atime, ELONG_TO_BELONG( buf.st_atim.tv_sec ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _gen, BGL_INT64_TO_BINT64( buf.st_gen ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _flags, BGL_INT64_TO_BINT64( buf.st_flags ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _blocks, BGL_INT64_TO_BINT64( buf.st_blocks ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _blksize, BGL_INT64_TO_BINT64( buf.st_blksize ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _size, BGL_INT64_TO_BINT64( buf.st_size ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _ino, BGL_INT64_TO_BINT64( buf.st_ino ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _rdev, BGL_INT64_TO_BINT64( buf.st_rdev ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _gid, BGL_INT64_TO_BINT64( buf.st_gid ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _uid, BGL_INT64_TO_BINT64( buf.st_uid ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _nlink, BGL_INT64_TO_BINT64( buf.st_nlink ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _mode, BGL_INT64_TO_BINT64( buf.st_mode ) ),
      res );
   res = MAKE_PAIR(
      MAKE_PAIR( _dev, BGL_INT64_TO_BINT64( buf.st_dev ) ),
      res );

   return res;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_uv_fs_fstat_cb ...                                           */
/*---------------------------------------------------------------------*/
static void
bgl_uv_fs_fstat_cb( uv_fs_t *req ) {
   obj_t p = (obj_t)req->data;

   gc_unmark( p );

   if( req->result < 0 ) {
      PROCEDURE_ENTRY( p )( p, BFALSE, BEOA );
   } else {
      PROCEDURE_ENTRY( p )( p, bgl_uv_fstat( req->statbuf ), BEOA );
   }

   uv_fs_req_cleanup( req );
   free( req );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_uv_fs_fstat ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_uv_fs_fstat( obj_t port, obj_t proc, bgl_uv_loop_t bloop ) {
   uv_loop_t *loop = (uv_loop_t *)bloop->BgL_z42builtinz42;
   int fd = ((bgl_uv_file_t)port)->BgL_fdz00;

   if( PROCEDUREP( proc ) ) {
      if( PROCEDURE_CORRECT_ARITYP( proc, 1 ) ) {
	 uv_fs_t *req = (uv_fs_t *)malloc( sizeof( uv_fs_t ) );
	 req->data = proc;
	 gc_mark( proc );
      
	 if( uv_fs_fstat( loop, req, fd, &bgl_uv_fs_fstat_cb ) < 0 ) {
	    uv_fs_req_cleanup( req );
	    free( req );
	    C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-fs-fstat",
			      (char *)uv_strerror( req->result ),
			      port );
	 }
      } else {
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-fs-fstat",
			   "wrong callback arity",
			   proc );
      }
   } else {
      uv_fs_t req;

      if( uv_fs_fstat( loop, &req, fd, 0L ) < 0 ) {
	 uv_fs_req_cleanup( &req );
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-fs-fstat",
			   (char *)uv_strerror( req.result ),
			   port );
      } else {
	 obj_t res = bgl_uv_fstat( req.statbuf );

	 uv_fs_req_cleanup( &req );

	 return res;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_uv_fs_read_cb ...                                            */
/*---------------------------------------------------------------------*/
void
bgl_uv_fs_read_cb( uv_fs_t *req ) {
   obj_t p = (obj_t)req->data;

   gc_unmark( p );

   PROCEDURE_ENTRY( p )( p, BINT( req->result ), BEOA );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_uv_fs_read ...                                               */
/*---------------------------------------------------------------------*/
int
bgl_uv_fs_read( obj_t port, obj_t buffer, long offset, long length, long position, obj_t proc, bgl_uv_loop_t bloop ) {
   uv_loop_t *loop = (uv_loop_t *)bloop->BgL_z42builtinz42;
   bgl_uv_file_t file = (bgl_uv_file_t)port;
   uv_fs_t *req = file->BgL_z52readreqz52;
   uv_buf_t *buf = (uv_buf_t *)(&(file->BgL_z52bufz52));
   int fd = file->BgL_fdz00;

   if( length + offset > STRING_LENGTH( buffer ) ) {
      C_SYSTEM_FAILURE( BGL_INDEX_OUT_OF_BOUND_ERROR, "uv-fs-read",
			"offset+length out of buffer range",
			BINT( STRING_LENGTH( buffer ) ) );
   }
			
   if( PROCEDUREP( proc ) ) {
      if( PROCEDURE_CORRECT_ARITYP( proc, 1 ) ) {
      
	 /* uv_buf_init inlined */
	 file->BgL_z52bufz52 = &(STRING_REF( buffer, offset ));
	 file->BgL_z52buflenz52 = length;

/*    void* buf = (void *)&(STRING_REF( buffer, offset ));             */
/*    uv_buf_t iov;                                                    */
/*                                                                     */
/*    iov = uv_buf_init( buf, length );                                */

	 req->data = proc;
	 gc_mark( proc );

	 uv_fs_read( loop, req, fd, buf, 1, position, &bgl_uv_fs_read_cb );
	 uv_fs_req_cleanup( req );
      } else {
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, "uv-fs-read",
			   "wrong callback arity",
			   proc );
      }
   } else {
      return pread( fd, &(STRING_REF( buffer, offset )), length, offset );
   }
}
      
