/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cports.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Jul 23 15:34:53 1992                          */
/*    Last change :  Wed Nov 24 09:58:49 2010 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Input ports handling                                             */
/*=====================================================================*/
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#ifndef _MSC_VER
#   include <dirent.h>
#   include <sys/time.h>
#else
#   include <io.h>
#   include <windows.h>
#   include <winsock2.h>
#   define pclose _pclose
#   define popen _popen
#   define S_ISDIR( mode ) ((mode & _S_IFDIR) != 0)
#endif
#include <string.h>
#if !defined( sony_news ) && \
    !(defined( NeXT ) && (defined( mc68000 ) || defined( i386 ))) && \
    !defined( _MSC_VER )
#   include <unistd.h>
#endif
#ifndef _MSC_VER
#  include <sys/file.h>
#endif
#include <bigloo.h>
#if( defined( sony_news ) || (defined( NeXT ) && defined( mc68000 )) )
#   include <ctype.h>
#endif

#if POSIX_FILE_OPS
#   include <unistd.h>
#endif

#if HAVE_TERMIOS
#  include <termios.h>
#else 
#  if HAVE_TERMIO
#    include <termio.h>
#  endif
#endif

#if( defined( S_IRUSR ) && \
     defined( S_IWUSR ) && \
     defined( S_IRGRP ) && \
     defined( S_IWGRP ) && \
     defined( S_IROTH ) && \
     defined( S_IWOTH ))
#  define OMOD (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#else
#  define OMOD 0666
#endif

#if( !defined( EWOULDBLOCK ) )
#  define EWOULDBLOCK 0
#endif

#if( !defined( EAGAIN ) )
#  define EAGAIN 0
#endif

#if( !defined( EINTR ) )
#  define EINTR 0
#endif

/*---------------------------------------------------------------------*/
/*    Default IO operations                                            */
/*---------------------------------------------------------------------*/
#if POSIX_FILE_OPS
#  define _WRITE write
#  define _LSEEK lseek
#  define _CLOSE close
#  define _FILENO fileno
#  define _FD int
#  define _CREAT( name, mod ) creat( name, mod )
#  define _OPEN( name, flag, mod ) open( name, flag, mod )
#  define _FSTAT fstat
#  define _READ read
#else
#  define _WRITE posix_write
#  define _LSEEK posix_lseek
#  define _CLOSE posix_close
#  define _FILENO
#  define _FD FILE * 
#  define _CREAT( name, mod ) fopen( name, "wb" )
#  define _OPEN( name, flag, mod ) fopen( name, "a+b" )
#  define _FSTAT( fd, buf ) fstat( fileno( fd ), buf )
#  define _READ posix_read
#endif

/*---------------------------------------------------------------------*/
/*    sendfile compatibility kit                                       */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_SENDFILE )
#  if( BGL_SENDFILE_BRAND == BGL_SENDFILE_LINUX )
extern ssize_t sendfile( int, int, off_t *, size_t );
#    define BGL_SENDFILE sendfile
#  else
#    if( BGL_SENDFILE_BRAND == BGL_SENDFILE_BSD )
#      include <sys/types.h>
#      include <sys/socket.h>
#      include <sys/uio.h>
#      define BGL_SENDFILE bsd_sendfile
#    else
       -> Error: no implementation for sendfile!
#    endif
#  endif       
#endif

/*---------------------------------------------------------------------*/
/*    compatibility kit ISO C / POSIX 2001.                            */
/*---------------------------------------------------------------------*/
static size_t posix_write( FILE *f, void *buf, size_t count ) {
   return fwrite( buf, 1, count, f );
}

static long posix_lseek( FILE *f, long offset, int whence ) {
   return fseek( f, offset, whence );
}

static int posix_close( FILE *f ) {
   return fclose( f );
}

static ssize_t posix_read( FILE *f, void *buf, size_t count ) {
   int n = fread( buf, 1, count, f );

   if( n != 0 )
      return n;

   if( ferror( f ) ) {
      return -1;
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    BSD compatibility kit                                            */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_SENDFILE && ( BGL_SENDFILE_BRAND == BGL_SENDFILE_BSD ) )
static int bsd_sendfile(int out_fd, int in_fd, off_t *offset, size_t count) {
   off_t sz = count;
   int n = sendfile( in_fd, out_fd, offset ? *offset : 0, &sz, 0, 0 );

   return n ? n : sz;
}
#endif

/*---------------------------------------------------------------------*/
/*    DEBUG_SENDCHARS                                                  */
/*---------------------------------------------------------------------*/
#define DEBUG_SENDCHARS 1
#undef DEBUG_SENDCHARS

/*---------------------------------------------------------------------*/
/*    isascii                                                          */
/*---------------------------------------------------------------------*/
#if( !defined( isascii ) )
#   define isascii( c ) (!((c) & ~0177))
#endif

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long default_io_bufsiz;

/*---------------------------------------------------------------------*/
/*    External definitions.                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL obj_t bgl_make_input_port( obj_t, FILE *, obj_t, obj_t );
extern obj_t make_string();
extern obj_t make_string_sans_fill();

/*---------------------------------------------------------------------*/
/*    Prototypes                                                       */
/*---------------------------------------------------------------------*/
static bool_t pipe_name_p( char * );
static char *pipe_name(char *);

static size_t strwrite( obj_t, void *, size_t );
static long strseek( void *, long, int );

static size_t procwrite( obj_t, void *, size_t );
static obj_t procflush( obj_t );
static int procclose( obj_t );

static obj_t output_flush( obj_t, char*, size_t, int, bool_t );

/*---------------------------------------------------------------------*/
/*    standard ports ...                                               */
/*---------------------------------------------------------------------*/
static obj_t _stdin, _stdout, _stderr;

/*---------------------------------------------------------------------*/
/*    stdout-variables                                                 */
/*---------------------------------------------------------------------*/
static obj_t stdout_mutex = BUNSPEC;
DEFINE_STRING( stdout_mutex_name, _2, "stdout-mutex", 12 );

static size_t stdout_from = 0;

/*---------------------------------------------------------------------*/
/*    struct sendfile_info_t ...                                       */
/*    -------------------------------------------------------------    */
/*    This structure is used for the Boehm's collection version        */
/*    7 and higher. The collector require a "blocking" function        */
/*    to be wrapping sendfile invokation. This function receives       */
/*    a sendfile_info_t as parameter and fills the "res" field.        */
/*---------------------------------------------------------------------*/
struct sendfile_info_t {
   int out;
   int in;
   size_t sz;
   off_t *off;
   long res;
   obj_t port;
};

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bglerror ...                                                     */
/*---------------------------------------------------------------------*/
static int
bglerror( int err ) {
   switch( err ) {
      case EPIPE: return BGL_IO_SIGPIPE_ERROR;

#if( defined( EBADF ) )
      case EBADF: return BGL_IO_PORT_ERROR;
#endif	 
#if( defined( EBADFD ) )
      case EBADFD: return BGL_IO_PORT_ERROR;
#endif
#if( defined( ENAMETOOLONG ) )
      case ENAMETOOLONG: return BGL_IO_PORT_ERROR;
#endif
#if( defined( ENFILE ) )
      case ENFILE: return BGL_IO_PORT_ERROR;
#endif
#if( defined( ENODEV ) )
      case ENODEV: return BGL_IO_PORT_ERROR;
#endif

#if( defined( ENOMEM ) )
      case ENOMEM: return BGL_IO_WRITE_ERROR;
#endif	 
#if( defined( ENOSPC ) )
      case ENOSPC: return BGL_IO_WRITE_ERROR;
#endif	 
	 
      default: return BGL_IO_ERROR;
   }
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_eof_read ...                                                 */
/*    -------------------------------------------------------------    */
/*    Used, for instance, on input string ports.                       */
/*---------------------------------------------------------------------*/
static long
bgl_eof_read( obj_t port, char *ptr, long num ) {
   return 0;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_read ...                                                     */
/*---------------------------------------------------------------------*/
long
bgl_read( obj_t port, char *ptr, long num ) {
   FILE *stream = PORT_STREAM( port );
   long n;

 loop:
   if( (n = _READ( fileno( stream ), ptr, num ) ) < 0 )
      if( errno == EINTR ) goto loop;

   return n;
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_console_read ...                                             */
/*    -------------------------------------------------------------    */
/*    In constrast to read, this function does not block on input, if  */
/*    not enough characters are available.                             */
/*---------------------------------------------------------------------*/
static long
bgl_console_read( obj_t port, char *ptr, long num ) {
   FILE *stream = PORT_STREAM( port );
   char *buf = ptr;
   int c;

   /* in order to mimic C behavior, flushes the output port */
   /* when reading on the console.                          */
   /* Note that _stdout does not have a sysflush.           */
   output_flush( _stdout, 0, 0, 1, 1 );

   /* now stdout is flushed out, read the charcters */
   while( ((c = getc( stream )) != EOF) ) {
      *buf++ = c;

      if( c == '\n' ) break;
      if( --num <= 0 ) break;
   }

   return (long)(buf - ptr);
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_proc_read ...                                                */
/*---------------------------------------------------------------------*/
static long
bgl_proc_read( obj_t port, char *b, long l ) {
   obj_t buf = CREF( port )->input_procedure_port_t.pbuffer;

 loop:

   if( STRINGP( buf ) ) {
      /* won't read because the proc buffer is already filled */
      char *s = BSTRING_TO_STRING( buf );
      long p = CREF( port )->input_procedure_port_t.pbufpos;
      long r = STRING_LENGTH( buf ) - p;

      if( r <= l ) {
	 memmove( b, &s[ p ], r );
	 CREF( port )->input_procedure_port_t.pbuffer = BFALSE;
	 CREF( port )->input_procedure_port_t.pbufpos = 0;
	 return r;
      } else {
	 memmove( b, &s[ p ], l );
	 CREF( port )->input_procedure_port_t.pbufpos += l;
	 return l;
      }
   } else {
      /* invoke the procedure to fill the buffer */
      obj_t proc = CREF( port )->input_port_t.port.name;
      obj_t nbuf = PROCEDURE_ENTRY( proc )( proc, BEOA );

      if( STRINGP( nbuf ) ) {
	 buf = CREF( port )->input_procedure_port_t.pbuffer = nbuf;
	 goto loop;
      } else {
	 if( nbuf == BFALSE ) {
	    /* eof has been reached */
	    CREF( port )->input_port_t.eof = 1;
	    return 0;
	 } else {
	    C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			      "input-procedure-port",
			      "Procedure result must be #f, or a string",
			      nbuf);
	    return -1;
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    timeout_set_port_blocking ...                                    */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_FCNTL )
static void
timeout_set_port_blocking( char *fun, int fd, int bool ) {
   int val;

   if( (val = fcntl( fd, F_GETFL, 0 ) ) < 0 ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR, fun, strerror( errno ), BINT( fd ) );
   }

   if( !bool ) 
      val |= O_NONBLOCK;
   else 
      val &= ~O_NONBLOCK;

   if( fcntl( fd, F_SETFL, val ) < 0 ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR, fun, strerror( errno ), BINT( fd ) );
   }
}
#endif 

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    sysread_with_timeout ...                                         */
/*    -------------------------------------------------------------    */
/*    In constrast to read, this function does not block on input if   */
/*    insufficient characters are available.                           */
/*---------------------------------------------------------------------*/
static long
sysread_with_timeout( obj_t port, char *ptr, long num ) {
#if( defined( POSIX_FILE_OPS ) && BGL_HAVE_SELECT && BGL_HAVE_FCNTL )
   int fd = fileno( (FILE *)PORT_STREAM( port ) );
   struct bgl_input_timeout *tmt = PORT( port ).timeout;
   long (*sysread)() = tmt->sysread;
   long n, to;
   fd_set readfds;
   struct timeval timeout;

   if( (n = sysread( port, ptr, num )) >= 0 ) {
      return n;
   } else {
      if( errno != EAGAIN ) return n;
   }

   /* wait for characters to be available */
   to = tmt->timeout;
   
   FD_ZERO( &readfds );
   FD_SET( fd, &readfds );

   timeout.tv_sec = to / 1000000;
   timeout.tv_usec = to % 1000000;

loop:   
   if( (n = select( fd + 1, &readfds, NULL, NULL, &timeout )) <= 0 ) {
      if( n == 0 ) {
	 C_SYSTEM_FAILURE( BGL_IO_TIMEOUT_ERROR,
			   "read/timeout",
			   "time limit exceeded",
			   port );
      } else {
	 /* MS: 23 Jan 2008. No attention was paid to EINTR */
	 /* I'm not 100% sure that this new test is correct */
	 if( errno == EINTR ) {
	    FD_ZERO( &readfds );
	    FD_SET( fd, &readfds );

	    timeout.tv_sec = to / 1000000;
	    timeout.tv_usec = to % 1000000;
	    goto loop;
	 }
	 
	 C_SYSTEM_FAILURE( BGL_IO_READ_ERROR,
			   "read/timeout",
			   strerror( errno ),
			   port );
      }
   } else {
      return sysread( port, ptr, num );
   }
#else
   return INPUT_PORT( port ).sysread( port, ptr, num );
#endif
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    strseek ...                                                      */
/*---------------------------------------------------------------------*/
static long
strseek( void *port, long offset, int whence ) {
   obj_t buf = OUTPUT_PORT( port ).buf;
   int len = STRING_LENGTH( buf );
   int cnt = OUTPUT_PORT( port ).cnt;

   if( (offset < 0) || (offset >= (len - cnt)) )
      return 1;

   OUTPUT_PORT( port ).ptr = (char *)&STRING_REF( buf, offset );
   OUTPUT_PORT( port ).cnt = len - offset;

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    write_string ...                                                 */
/*---------------------------------------------------------------------*/
#define write_string( port, s, l, err ) {	\
   void *_stream = PORT_STREAM( port ); \
   size_t (*_syswrite)(void *, void *, size_t) = OUTPUT_PORT( port ).syswrite; \
   long _n; \
   char *_str = s; \
   size_t _slen = l; \
   \
   while( _slen > 0 ) { \
      if( (_n = _syswrite( _stream, _str, _slen )) < 0 ) { \
	 if( errno == EWOULDBLOCK || errno == EAGAIN || errno == EINTR ) { \
	    continue; \
	 } else { \
  	    if( err ) { \
	       C_SYSTEM_FAILURE( bglerror( errno ), \
			         "write/display", \
			         strerror( errno ), \
			         port ); \
            } else break; \
	 } \
      } else { \
	 _slen -= _n; \
	 _str += _n; \
      } \
   } \
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    invoke_flush_hook ...                                            */
/*---------------------------------------------------------------------*/
static void
invoke_flush_hook( obj_t fhook, obj_t port, size_t slen, bool_t err ) {
   obj_t s = PROCEDURE_ENTRY( fhook )( fhook, port, BINT( slen ), BEOA );

   if( STRINGP( s ) ) {
      write_string( port, BSTRING_TO_STRING( s ), STRING_LENGTH( s ), err );
   } else {
      obj_t buf = BGL_OUTPUT_PORT_FLUSHBUF( port );
      
      if( INTEGERP( s ) &&
	  STRINGP( buf ) &&
	  (CINT( s ) <= STRING_LENGTH( buf )) &&
	  (CINT( s ) > 0) ) {
	 write_string( port, BSTRING_TO_STRING( buf ), CINT( s ), err );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    output_flush ...                                                 */
/*    -------------------------------------------------------------    */
/*    Read automatically invokes a flush on stdout.                    */
/*    In order to avoid locks during writes we need to handle the      */
/*    flushes differently for stdout.                                  */
/*    Invariant: stdout-buffer is already flushed up to stdout_from.   */
/*---------------------------------------------------------------------*/
obj_t
output_flush( obj_t port, char *str, size_t slen, int is_read_flush, bool_t err ) {
   if( port == _stdout ) bgl_mutex_lock ( stdout_mutex );

   if( PORT( port ).kindof == KINDOF_CLOSED ) {
      return BFALSE;
   } else {
      obj_t buf = OUTPUT_PORT( port ).buf;
      int len = STRING_LENGTH( buf );
      size_t cnt = OUTPUT_PORT( port ).cnt;
      long use = len - cnt;
      obj_t fhook = BGL_OUTPUT_PORT_FHOOK( port );

      /* flush out the buffer, if needed */
      if( OUTPUT_PORT( port ).bufmode != BGL_IOEBF ) {
	 char *buf_start = BSTRING_TO_STRING( buf );

	 if ( port == _stdout ) {
	    /* take into account stdout_from */
	    use -= stdout_from;
	    buf_start = (char *)&STRING_REF( buf, stdout_from );
	 }

	 /* invoke the flush hook, if any attached to the port */
	 if( PROCEDUREP( fhook ) )
	    invoke_flush_hook( fhook, port, use + slen, err );

	 /* write the buffer */
	 write_string( port, buf_start, use, err );
	 
	 /* write the string that raises the buffer overflow */
	 write_string( port, str, slen, err );

	 /* Update the port */
	 if ( port == _stdout ) {
	    if ( is_read_flush ) {
	       /* slen must be zero with is_read_flush==1 */
	       stdout_from += use;
	    } else {
	       stdout_from = 0;
	       OUTPUT_PORT( port ).ptr = (char *)&STRING_REF( buf, 0 );
	       OUTPUT_PORT( port ).cnt = STRING_LENGTH( buf );
	    }
	 } else {
	    OUTPUT_PORT( port ).ptr = (char *)&STRING_REF( buf, 0 );
	    OUTPUT_PORT( port ).cnt = STRING_LENGTH( buf );
	 }
      } else {
	 /* invoke the flush hook, if any attached to the port */
	 if( PROCEDUREP( fhook ) )
	    invoke_flush_hook( fhook, port, slen, err );
	 
	 /* this is an exensible buffer, that we increase iff it is full */
	 if( (slen > 0) || (cnt == 0) ) {
	    size_t (*syswrite)(void *, void *, size_t) =
	       OUTPUT_PORT( port ).syswrite;
	    long n = syswrite( PORT_STREAM( port ), str, slen );
	    
	    if( n < 0 && err ) {
	       C_SYSTEM_FAILURE( bglerror( errno ),
				 "write/display",
				 strerror( errno ),
				 port );
	    }
	 }
      }
      
      if( port == _stdout ) bgl_mutex_unlock ( stdout_mutex );

      return port;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_output_flush ...                                             */
/*    -------------------------------------------------------------    */
/*    There is no room for str and cnt has not been decremented.       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_output_flush( obj_t port, char *str, size_t slen ) {
   return output_flush( port, str, slen, 0, 1 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_write ...                                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_write( obj_t op, unsigned char *str, size_t sz ) {
   if( OUTPUT_PORT( op ).cnt > sz ) {
      if( OUTPUT_PORT( op ).bufmode == BGL_IOLBF ) {
	 while( sz-- > 0 ) {
	    char c = *str++;
	    
	    *OUTPUT_PORT( op ).ptr++ = c;
	    OUTPUT_PORT( op ).cnt--;
	    
	    if( c == '\n' ) bgl_output_flush( op, 0, 0 );
	 }
      } else {
	 memcpy( OUTPUT_PORT( op ).ptr, str, sz );
	 OUTPUT_PORT( op ).ptr += sz;
	 OUTPUT_PORT( op ).cnt -= sz;
      }
      
      return op;
   } else {
      return bgl_output_flush( op, str, sz );
   }
}

/*---------------------------------------------------------------------*/
/*     bgl_make_output_port ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_output_port( obj_t name,
		      void *stream,
		      obj_t kindof,
		      obj_t buf,
		      size_t (*write)(),
		      long (*seek)(),
		      int (*close)() ) {
   obj_t new_output_port;

   if( !STRINGP( buf ) ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"make-output-port",
			"Illegal buffer",
			buf );
   }
   
   new_output_port = GC_MALLOC( OUTPUT_PORT_SIZE );
   
   new_output_port->output_port_t.port.header =
      MAKE_HEADER( OUTPUT_PORT_TYPE, 0 );
   
   new_output_port->port_t.name = name;
   new_output_port->port_t.stream = stream;
   new_output_port->port_t.kindof = kindof;
   new_output_port->port_t.chook = BUNSPEC;
   new_output_port->port_t.userdata = BUNSPEC;
   new_output_port->port_t.timeout = 0L;
   new_output_port->port_t.sysseek = seek;
   new_output_port->port_t.sysclose = close;
   new_output_port->output_port_t.syswrite = write;
   new_output_port->output_port_t.sysflush = 0L;
   new_output_port->output_port_t.fhook = BUNSPEC;
   new_output_port->output_port_t.flushbuf = BUNSPEC;

   new_output_port->output_port_t.bufmode = BGL_IOFBF;
   bgl_output_port_buffer_set( BREF( new_output_port ), buf );
   
   return BREF( new_output_port );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_output_port_buffer_set ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_output_port_buffer_set( obj_t port, obj_t buf ) {
   if( !STRINGP( buf ) ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"output-port-buffer-set!",
			"Illegal buffer",
			buf );
   }
   
   OUTPUT_PORT( port ).buf = buf;
   OUTPUT_PORT( port ).ptr = (char *)&STRING_REF( buf, 0 );
   OUTPUT_PORT( port ).cnt = STRING_LENGTH( buf );
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_output_port_timeout_set ...                                  */
/*---------------------------------------------------------------------*/
bool_t
bgl_output_port_timeout_set( obj_t port, long timeout ) {
#if defined( POSIX_FILE_OPS ) && BGL_HAVE_SELECT && BGL_HAVE_FCNTL
   if( PORT_IS_OS( port ) && (timeout >= 0) ) {
      if( timeout == 0 ) {
	 /* always for blocking mode, see bgl_input_port_timeout_set */
	 timeout_set_port_blocking( "output-port-timeout-set!",
				    (int)PORT_STREAM( port ),
				    1 );
	 PORT( port ).timeout = 0;
      } else {
	 if( !PORT( port ).timeout ) {
	    timeout_set_port_blocking( "output-port-timeout-set!",
				       (int)PORT_STREAM( port ),
				       0 );
	 }
	 PORT( port ).timeout = (void *)timeout;
      }

      return 1;
   }
#endif

   return 0;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_file_to_output_port ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_file_to_output_port( FILE *f, obj_t buf ) {
   return bgl_make_output_port( string_to_bstring( "<c-port>" ),
				(void *)_FILENO( f ),
				KINDOF_FILE, 
				buf, 
				(size_t (*)())_WRITE,
				(long (*)())_LSEEK,
				_CLOSE );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_open_output_file ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_output_file( obj_t name, obj_t buf ) {
   char *cname = BSTRING_TO_STRING( name );

#if( HAVE_PIPE )
   if( pipe_name_p( cname ) ) {
      FILE *file = popen( pipe_name( cname ), "w" );
      
      if( !file )
         return BFALSE;

      setvbuf( file, 0, _IONBF, 0 );
	       
      return bgl_make_output_port( name, file,
				   KINDOF_PIPE,
				   buf,
				   posix_write,
				   posix_lseek,
				   pclose );
   } else
#endif
   {
      _FD fd;
      
      if( strcmp( cname, "null:" ) == 0 ) {
#        ifndef _MSC_VER
            cname= "/dev/null";
#        else
            cname= "NUL:";
#        endif
      }

      if( (fd = _CREAT( cname, OMOD )) < 0 )
         return BFALSE;
      else
	 return bgl_make_output_port( name, (void *)fd,
				      KINDOF_FILE,
				      buf,
				      (size_t (*)())_WRITE,
				      (long (*)())_LSEEK,
				      _CLOSE );
   }
}

/*---------------------------------------------------------------------*/
/*    bgl_append_output_file ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_append_output_file( obj_t name, obj_t buf ) {
   _FD fd;
   
   if( (fd = _OPEN( BSTRING_TO_STRING( name ), O_CREAT | O_WRONLY, OMOD )) < 0 )
      return BFALSE;
   else {
      if( _LSEEK( fd, 0, SEEK_END ) < 0 ) {
	 _CLOSE( fd );
	 return BFALSE;
      } else {
	 return bgl_make_output_port( name, (void *)fd,
				      KINDOF_FILE,
				      buf, 
				      (size_t (*)())_WRITE,
				      (long (*)())_LSEEK,
				      _CLOSE );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_open_output_string ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_output_string( obj_t buf ) {
   obj_t port = bgl_make_output_port( string_to_bstring( "string" ), 0L,
				      KINDOF_STRING,
				      buf,
				      strwrite, strseek, 0 );
   PORT_STREAM( port ) = port;
   OUTPUT_PORT( port ).bufmode = BGL_IOEBF;
   OUTPUT_PORT( port ).sysflush = get_output_string;

   return port;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_open_output_procedure ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_output_procedure( obj_t proc, obj_t flush, obj_t close, obj_t buf ) {
   obj_t port = bgl_make_output_port( string_to_bstring( "procedure" ), 0L,
				      KINDOF_PROCEDURE,
				      make_string_sans_fill( 0 ),
				      procwrite, 0L, 0L );
   /* MS, 9 apri 2009: used to be create_vector( 5 )! */
   obj_t udata = create_vector( 4 );
   
   PORT_STREAM( port ) = port;
   OUTPUT_PORT( port ).bufmode = BGL_IONB;
   OUTPUT_PORT( port ).sysflush = procflush;
   PORT( port ).sysclose = procclose;
   PORT( port ).userdata = udata;
   VECTOR_SET( udata, 0, proc );
   VECTOR_SET( udata, 1, buf );
   VECTOR_SET( udata, 2, flush );
   VECTOR_SET( udata, 3, close );

   return port;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    get_output_string ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
get_output_string( obj_t port ) {
   if( PORT( port ).kindof == KINDOF_STRING ) {
      obj_t buf = OUTPUT_PORT( port ).buf;
      int cnt = OUTPUT_PORT( port ).cnt;
	 
      return string_to_bstring_len( BSTRING_TO_STRING( buf ),
				    STRING_LENGTH( buf ) - cnt );
   } else {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"get-output-string",
			"Not a string port",
			port );
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_reset_output_string_port ...                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_reset_output_string_port( obj_t port ) {
   obj_t res = get_output_string( port );
   
   bgl_output_port_buffer_set( port, OUTPUT_PORT( port ).buf );

   return res;
}
   
/*---------------------------------------------------------------------*/
/*    bgl_close_output_port ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_close_output_port( obj_t port ) {
   if( PORT( port ).kindof == KINDOF_CLOSED )
      return port;
      
   if( (port == _stdout) || (port == _stderr) ) {
      output_flush( port, 0, 0, 0, 0 );
      return port;
   } else {
      obj_t res = port;
      obj_t chook = PORT_CHOOK( port );

      if( PORT( port ).kindof == KINDOF_STRING ) {
	 obj_t buf = OUTPUT_PORT( port ).buf;
	 int cnt = OUTPUT_PORT( port ).cnt;

	 res = bgl_string_shrink( buf, STRING_LENGTH( buf ) - cnt );
      } else {
	 output_flush( port, 0, 0, 0, 0 );
      }
      
      PORT( port ).kindof = KINDOF_CLOSED;
      
      if( PORT( port ).sysclose )
	 PORT( port ).sysclose( PORT_STREAM( port ) );

      if( PROCEDUREP( chook ) ) {
	 if( PROCEDURE_ARITY( chook ) == 1 ) {
	    PROCEDURE_ENTRY( chook )( chook, port, BEOA );
	 } else {
	    C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			      "close-output-port",
			      "illegal close hook arity",
			      chook );
	 }
      }

      OUTPUT_PORT( port ).buf = BFALSE;

      return res;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_input_port ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_input_port( obj_t name, FILE *file, obj_t kindof, obj_t buf ) {
   /* An input port cannot be allocated as an atomic object    */
   /* because it holds a buffer and a name that are GC objects */
   obj_t new_input_port;

   switch( (long)kindof ) {
      case (long)KINDOF_PROCEDURE:
	 new_input_port = GC_MALLOC( INPUT_PROCEDURE_PORT_SIZE );
	 break;
      case (long)KINDOF_GZIP:
	 new_input_port = GC_MALLOC( INPUT_GZIP_PORT_SIZE );
	 break;
      default:
	 new_input_port = GC_MALLOC( INPUT_PORT_SIZE );
   }

   new_input_port->port_t.header = MAKE_HEADER( INPUT_PORT_TYPE, 0 );
   new_input_port->port_t.kindof = kindof;
   new_input_port->port_t.name = name;
   new_input_port->port_t.stream = file;
   new_input_port->port_t.timeout = 0L;
   new_input_port->port_t.sysseek = 0L;
   new_input_port->port_t.chook = BUNSPEC;
   new_input_port->port_t.userdata = BUNSPEC;
   new_input_port->input_port_t.filepos = 0;
   new_input_port->input_port_t.fillbarrier = -1;
   new_input_port->input_port_t.length = -1;
   new_input_port->input_port_t.eof = 0;
   new_input_port->input_port_t.matchstart = 0;
   new_input_port->input_port_t.matchstop = 0;
   new_input_port->input_port_t.forward = 0;
   new_input_port->input_port_t.bufpos = 1;
   new_input_port->input_port_t.lastchar = '\n';
   new_input_port->input_port_t.buf = buf;

   switch( (long) kindof ) {
      case (long)KINDOF_CONSOLE:
	 new_input_port->port_t.sysclose = 0;
	 new_input_port->input_port_t.sysread = bgl_console_read;
	 STRING_SET( new_input_port->input_port_t.buf, 0, '\0' );
	 break;

#if( HAVE_PIPE )
      case (long)KINDOF_PIPE:
	 new_input_port->port_t.sysclose = pclose;
	 new_input_port->input_port_t.sysread = bgl_read;
	 STRING_SET( new_input_port->input_port_t.buf, 0, '\0' );
	 break;

      case (long)KINDOF_PROCPIPE:
#endif
      case (long)KINDOF_SOCKET:
	 new_input_port->port_t.sysclose = fclose;
	 new_input_port->input_port_t.sysread = bgl_read;
	 STRING_SET( new_input_port->input_port_t.buf, 0, '\0' );
	 break;

      case (long)KINDOF_FILE:
	 new_input_port->port_t.sysclose = fclose;
	 new_input_port->input_port_t.sysread = bgl_read;
	 STRING_SET( new_input_port->input_port_t.buf, 0, '\0' );
	 break;

      case (long)KINDOF_PROCEDURE:
      case (long)KINDOF_GZIP:
	 new_input_port->port_t.sysclose = 0;
	 new_input_port->input_port_t.sysread = bgl_proc_read;
	 STRING_SET( new_input_port->input_port_t.buf, 0, '\0' );
	 break;

      case (long)KINDOF_STRING:
	 new_input_port->port_t.sysclose = 0;
	 new_input_port->input_port_t.sysread = bgl_eof_read;
	 break;
	 
      default:
	 new_input_port->port_t.sysclose = 0;
	 new_input_port->input_port_t.sysread = bgl_read;
	 STRING_SET( new_input_port->input_port_t.buf, 0, '\0' );
   }


   return BREF( new_input_port );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_input_port_buffer_set ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_input_port_buffer_set( obj_t ip, obj_t buffer ) {
   BGL_INPUT_PORT_BUFFER( ip ) = buffer;
      
   INPUT_PORT( ip ).matchstart = 0;
   INPUT_PORT( ip ).matchstop = 0;
   INPUT_PORT( ip ).forward = 0;
   INPUT_PORT( ip ).bufpos = 1;
   INPUT_PORT( ip ).lastchar = '\n';

   if( PORT( ip ).kindof != KINDOF_STRING ) {
      STRING_SET( buffer, 0 , '\0' );
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_input_port_timeout_set ...                                   */
/*---------------------------------------------------------------------*/
bool_t
bgl_input_port_timeout_set( obj_t port, long timeout ) {
#if defined( POSIX_FILE_OPS ) && BGL_HAVE_SELECT && BGL_HAVE_FCNTL
   if( (timeout >= 0) &&
       ((PORT(port).kindof == KINDOF_FILE) ||
	(PORT(port).kindof == KINDOF_PIPE) ||
	(PORT(port).kindof == KINDOF_PROCPIPE) ||
	(PORT(port).kindof == KINDOF_CONSOLE) ||
	(PORT(port).kindof == KINDOF_SOCKET)) ) {
      if( timeout == 0 ) {
	 struct bgl_input_timeout *to = PORT( port ).timeout;

	 if( to ) INPUT_PORT( port ).sysread = to->sysread;

	 /* for socket and pipe ports the input-port and output-port */
	 /* are dupped, hence, if the output-port has been switched  */
	 /* to non-blocking, the structure of the input-port does    */
	 /* not reflect the change but it must however be forced to  */
	 /* non blocking.                                            */
	 timeout_set_port_blocking( "input-port-timeout-set!",
				    fileno( (FILE *)PORT_STREAM( port ) ),
				    1 );

	 return 0;
      } else {
	 if( PORT( port ).timeout ) {
	    ((struct bgl_input_timeout *)PORT( port ).timeout)->timeout =
	       timeout;
	 } else {
	    struct bgl_input_timeout *to =
	       (struct bgl_input_timeout *)
	       GC_MALLOC( sizeof( struct bgl_input_timeout ) );

	    to->timeout = timeout;
	    to->sysread = INPUT_PORT( port ).sysread;

	    if( (int)PORT_STREAM( port ) == -1 ) {
	       C_SYSTEM_FAILURE( bglerror( errno ),
				 "input-port-timeout-set!",
				 "Illegal input-port",
				 port );
	    }

	    PORT( port ).timeout = (FILE *)to;
	 }

	 INPUT_PORT( port ).sysread = &sysread_with_timeout;
	 timeout_set_port_blocking( "input-port-timeout-set!",
				    fileno( (FILE *)PORT_STREAM( port ) ),
				    0);
	 return 1;
      }
   }
#endif

   return 0;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_open_input_pipe ...                                          */
/*---------------------------------------------------------------------*/
obj_t
bgl_open_input_pipe( obj_t name, obj_t buffer ) {
#if( HAVE_PIPE )
   FILE *file;
   char *cname = BSTRING_TO_STRING( name );

   if( !(file = popen( cname, "r" )) )
      return BFALSE;

   /* we use our own buffer */
   setvbuf( file, NULL, _IONBF, 0 );  

   return bgl_make_input_port( name, file, KINDOF_PIPE, buffer );
#else
   return BFALSE;
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_open_input_resource ...                                      */
/*    -------------------------------------------------------------    */
/*    Resources are only meaning in Java.                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_open_input_resource( obj_t name, obj_t buffer ) {
   return BFALSE;
}

/*---------------------------------------------------------------------*/
/*    bgl_open_input_file ...                                          */
/*    -------------------------------------------------------------    */
/*    !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  */
/*    We fill up its associated buffer when opening an input port.     */
/*    -------------------------------------------------------------    */
/*    This function open two kind of files. Regular file and Unix      */
/*    like pipes when the file name is something like "| ...".         */
/*---------------------------------------------------------------------*/
obj_t
bgl_open_input_file( obj_t name, obj_t buffer ) {
   FILE *file;
   char *cname = BSTRING_TO_STRING( name );

#if( HAVE_PIPE )
   if( pipe_name_p( cname ) ) {
      if( !(file = popen( pipe_name( cname ), "r" ) ) )
	 return BFALSE;

      /* we use our own buffer */
      setvbuf( file, NULL, _IONBF, 0 );  

      return bgl_make_input_port( name, file, KINDOF_PIPE, buffer );
   } else
#endif
   {
      if (strcmp( cname, "null:" ) == 0)
#        ifndef _MSC_VER
            cname= "/dev/null";
#        else
            cname= "NUL:";
#        endif

       if( !(file = fopen( cname, "rb" )) ) {
	  return BFALSE;
       } else {
	  /* we use our own buffer */
	  setvbuf( file, NULL, _IONBF, 0 );  

	  return bgl_make_input_port( name, file, KINDOF_FILE, buffer );
       }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_file_to_buffered_input_port ...                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_file_to_buffered_input_port( obj_t name, FILE *file, obj_t buffer ) {
   if( file == stdin )
      return bgl_make_input_port( name, stdin, KINDOF_CONSOLE, buffer );
   else {
      return bgl_make_input_port( name, file, KINDOF_FILE, buffer );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_file_to_input_port ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_file_to_input_port( FILE *file ) {
   obj_t buffer = make_string_sans_fill( default_io_bufsiz );
   
   return bgl_file_to_buffered_input_port( string_to_bstring( "file" ),
					   file,
					   buffer );
}

/*---------------------------------------------------------------------*/
/*    bgl_open_input_string ...                                        */
/*    -------------------------------------------------------------    */
/*    The start index is in bounds.                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_input_string( obj_t string, int start ) {
   obj_t port;
   long bufsiz = STRING_LENGTH( string ) - start;
   obj_t buffer = make_string_sans_fill( bufsiz );

   
   memcpy( &STRING_REF( buffer, 0 ), &(STRING_REF( string, start )), bufsiz );
   port = bgl_make_input_port( string_to_bstring( "[string]" ),
			       0L,
			       KINDOF_STRING,
			       buffer );

   CREF( port )->input_port_t.eof = 1;
   CREF( port )->input_port_t.bufpos = bufsiz + 1;
   CREF( port )->input_port_t.length = bufsiz;

   return port;
}

/*---------------------------------------------------------------------*/
/*    bgl_open_input_string_bang ...                                   */
/*    -------------------------------------------------------------    */
/*    The start index is in bounds.                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_input_string_bang( obj_t buffer ) {
   obj_t port;
   long bufsiz = STRING_LENGTH( buffer );
   
   port = bgl_make_input_port( string_to_bstring( "[string]" ),
			       0L,
			       KINDOF_STRING,
			       buffer );

   CREF( port )->input_port_t.eof = 1;
   CREF( port )->input_port_t.bufpos = bufsiz + 1;
   CREF( port )->input_port_t.length = bufsiz;

   return port;
}

/*---------------------------------------------------------------------*/
/*    bgl_open_input_procedure ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_input_procedure( obj_t fun, obj_t buffer ) {
   if( PROCEDURE_CORRECT_ARITYP( fun, 0 ) ) {
      obj_t port = bgl_make_input_port( string_to_bstring( "[procedure]" ),
					0L,
					KINDOF_PROCEDURE,
					buffer );

      CREF( port )->port_t.stream = port;
      CREF( port )->port_t.name = fun;
      CREF( port )->input_procedure_port_t.pbuffer = BUNSPEC;
      CREF( port )->input_procedure_port_t.pbufpos = 0;

      return port;
   } else {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"open-input-procedure",
			"Illegal procedure arity",
			fun );
   }
}

/*---------------------------------------------------------------------*/
/*    bgl_open_input_gzip_port ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_input_gzip_port( obj_t fun, obj_t in, obj_t buffer ) {
   if( PROCEDURE_CORRECT_ARITYP( fun, 0 ) ) {
      obj_t port = bgl_make_input_port( PORT( in ).name,
					0L,
					KINDOF_GZIP,
					buffer );

      CREF( port )->port_t.stream = port;
      CREF( port )->port_t.name = fun;
      CREF( port )->input_procedure_port_t.pbuffer = BUNSPEC;
      CREF( port )->input_procedure_port_t.pbufpos = 0;
      CREF( port )->input_gzip_port_t.gzip = in;

      return port;
   } else {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"open-input-gzip-port",
			"Illegal procedure arity",
			fun );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_open_input_c_string ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_open_input_c_string( char *c_string ) {
   obj_t port;
   long bufsiz = (long)strlen( c_string );
   obj_t buffer = string_to_bstring_len( c_string, bufsiz );
   char *new_string = 0;

   port = bgl_make_input_port( string_to_bstring( "[c_string]" ),
			       0L,
			       KINDOF_STRING,
			       buffer );

   CREF(port)->input_port_t.eof = 1;
   /* MS 25 Aug 2008, used to be bufpos = bufsiz + 1 */
   CREF(port)->input_port_t.bufpos = bufsiz;

   return port;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_reopen_input_c_string ...                                    */
/*    -------------------------------------------------------------    */
/*    Simply changes the input buffer of an input string. Does not     */
/*    re-allocate a brand new input-port.                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_reopen_input_c_string( obj_t port, char *c_string ) {
   long bufsiz = (long)strlen( c_string );

   if( BGL_INPUT_PORT_BUFSIZ( port ) < (bufsiz + 1) ) {
      CREF(port)->input_port_t.buf = make_string_sans_fill( bufsiz + 1 );
   }

   CREF(port)->input_port_t.bufpos = bufsiz + 1;
   CREF(port)->input_port_t.matchstart = 0;
   CREF(port)->input_port_t.matchstop = 0;
   CREF(port)->input_port_t.forward = 0;
   CREF(port)->input_port_t.lastchar = '\n';
   strcpy( (char *)&RGC_BUFFER_REF( port, 0 ), c_string );

   return port;
}

/*---------------------------------------------------------------------*/
/*    bgl_close_input_port ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_close_input_port( obj_t port ) {
   if( INPUT_PORTP( port ) ) {
      if( (PORT( port ).kindof != KINDOF_CLOSED) &&
	  (PORT( port ).kindof != KINDOF_CONSOLE) ) {
	 obj_t chook = PORT_CHOOK( port );

	 if( PORT( port ).sysclose )
	    PORT( port ).sysclose( PORT_STREAM( port ) );
	 
	 INPUT_PORT( port ).eof = 1;
      	 PORT( port ).kindof = KINDOF_CLOSED;
	 PORT( port ).sysclose = 0L;
	 
	 if( PROCEDUREP( chook ) ) {
	    if( PROCEDURE_ARITY( chook ) == 1 ) {
	       PROCEDURE_ENTRY( chook )( chook, port, BEOA );
	    } else {
	       C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
				 "close-input-port",
				 "illegal close hook arity",
				 chook );
	    }
	 }
      }

   }

   return port;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_input_port_seek ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_input_port_seek( obj_t port, long pos ) {
   if( INPUT_PORT_ON_FILEP( port ) ) {
      if( fseek( PORT_STREAM( port ), pos, SEEK_SET ) )
	 return BFALSE;
      INPUT_PORT( port ).filepos = pos;
      INPUT_PORT( port ).eof = 0;
      INPUT_PORT( port ).matchstart = 0;
      INPUT_PORT( port ).matchstop = 0;
      INPUT_PORT( port ).forward = 0;
      INPUT_PORT( port ).bufpos = 1;
      INPUT_PORT( port ).lastchar = '\n';
      RGC_BUFFER_SET( port, 0, '\0' );

      return BTRUE;
   }

   if( INPUT_PORT_ON_STRINGP( port ) && (pos < BGL_INPUT_PORT_BUFSIZ(port)) ) {
      INPUT_PORT( port ).filepos = pos;
      INPUT_PORT( port ).matchstart = pos;
      INPUT_PORT( port ).matchstop = pos;
      INPUT_PORT( port ).forward = pos;

      return BTRUE;
   }

   return BFALSE;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_output_port_filepos ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_output_port_filepos( obj_t port ) {
   long (*sysseek)() = PORT( port ).sysseek;

   if( sysseek ) {
      return (long)sysseek( PORT_STREAM( port ), 0, SEEK_CUR );
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_output_port_seek ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_output_port_seek( obj_t port, long pos ) {
   long (*sysseek)() = PORT( port ).sysseek;

   if( sysseek ) {
      return sysseek( PORT_STREAM( port ), pos, SEEK_SET ) ? BFALSE : BTRUE;
   } else {
      return BFALSE;
   }
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_input_port_reopen ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_input_port_reopen( obj_t port ) {
   FILE *nf;

   if( !INPUT_PORT_ON_FILEP( port ) ) {
      if( INPUT_STRING_PORTP( port ) ) {
 	 return bgl_input_port_seek( port, 0 );
      } else {
	 return BFALSE;
      }
   }
   
   nf = freopen( BSTRING_TO_STRING( PORT( port ).name ),
		 "r",
		 (FILE *)PORT_STREAM( port ) );

   if( !nf ) return BFALSE;

   PORT_STREAM( port ) = nf;
      
   /* we use our own buffer */
   setvbuf( (FILE *)nf, NULL, _IONBF, 0 );

   INPUT_PORT( port ).filepos = 0;
   INPUT_PORT( port ).eof = 0;
   INPUT_PORT( port ).matchstart = 0;
   INPUT_PORT( port ).matchstop = 0;
   INPUT_PORT( port ).forward = 0;
   INPUT_PORT( port ).bufpos = 1;
   INPUT_PORT( port ).lastchar = '\n';
   RGC_BUFFER_SET( port, 0, '\0' );

   return BTRUE;
}

/*---------------------------------------------------------------------*/
/*    obj                                                              */
/*    reset_console ...                                                */
/*    -------------------------------------------------------------    */
/*    We flush input port, for ^C to work correctly within the         */
/*    interpreter. The only place this function is called is in the    */
/*    REPL (see Eval/eval.scm).                                        */
/*---------------------------------------------------------------------*/
obj_t
reset_console( obj_t port ) {
   if( PORT( port ).kindof == KINDOF_CONSOLE ) {
      INPUT_PORT( port ).matchstart = 0;
      INPUT_PORT( port ).matchstop = 0;
      INPUT_PORT( port ).bufpos = 1;
      INPUT_PORT( port ).lastchar = '\n';
      RGC_BUFFER_SET( port, 0, '\0' );
   }

   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*     bgl_init_io ...                                                 */
/*---------------------------------------------------------------------*/
void
bgl_init_io() {
#if( !defined( _SBFSIZ ) )
#   define _SBFSIZ 1
#endif
   obj_t denv = BGL_CURRENT_DYNAMIC_ENV();

   default_io_bufsiz = BUFSIZ * _SBFSIZ;

   stdout_mutex = bgl_make_mutex( stdout_mutex_name );
   _stdout = bgl_make_output_port( string_to_bstring( "stdout" ),
				   (void *)_FILENO( stdout ),
				   KINDOF_CONSOLE,
				   make_string_sans_fill( 512 ),
				   (size_t (*)())_WRITE,
				   (long (*)())_LSEEK,
				   _CLOSE );
   _stderr = bgl_make_output_port( string_to_bstring( "stderr" ),
				   (void *)_FILENO( stderr ),
				   KINDOF_CONSOLE,
				   make_string_sans_fill( 0 ), 
				   (size_t (*)())_WRITE,
				   (long (*)())_LSEEK,
				   _CLOSE );
   _stdin = bgl_make_input_port( string_to_bstring( "stdin" ),
				 stdin,
				 KINDOF_CONSOLE,
				 make_string_sans_fill( default_io_bufsiz ) );

   /* in order for the flush to work (with concurrent reads) */
   /* bufmode must never be BGL_IOEBF.                       */
   OUTPUT_PORT( _stdout ).bufmode = BGL_IOLBF;
   
   BGL_ENV_CURRENT_OUTPUT_PORT_SET( denv, _stdout );
   BGL_ENV_CURRENT_ERROR_PORT_SET( denv, _stderr );
   BGL_ENV_CURRENT_INPUT_PORT_SET( denv, _stdin );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_end_io ...                                                   */
/*---------------------------------------------------------------------*/
void
bgl_end_io() {
   bgl_close_output_port( _stdout );
   bgl_close_output_port( _stderr );
}

/*---------------------------------------------------------------------*/
/*    fexists ...                                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
fexists( char *name ) {
#if( HAVE_PIPE )
   if( pipe_name_p( name ) ) {
      return 1;
   }
#else
   if( pipe_name_p(name) ) {
      return 0;
   }
#endif

# ifndef _MSC_VER
    return !access( name, F_OK );
# else
    /* !!!!! verify semantics of Unix' access */
    return !_access( name, 0 );        
# endif
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    reset_eof ...                                                    */
/*    -------------------------------------------------------------    */
/*    The function erase the end-of-file of input console port.        */
/*    This allows, restart reading after a ^D.                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
reset_eof( obj_t port ) {
   if( PORT( port ).kindof == KINDOF_CONSOLE ) {
      /* we forget about EOF */
      INPUT_PORT( port ).eof = 0;

      /* we cleanup buffer   */
      reset_console( port );

      /* we clear errors.    */
      clearerr( stdin );

      return 1;
   }
   else
      return 0;
}

/*---------------------------------------------------------------------*/
/*    static bool_t                                                    */
/*    pipe_name_p ...                                                  */
/*    -------------------------------------------------------------    */
/*    Is a file name a pipe name ? A pipe name start by the            */
/*    sequence "| ".                                                   */
/*---------------------------------------------------------------------*/
static bool_t
pipe_name_p( char *name ) {
   return( (name[ 0 ] == '|') && (name[ 1 ] == ' ') );
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    pipe_name ...                                                    */
/*    -------------------------------------------------------------    */
/*    Pipe name to name translation.                                   */
/*---------------------------------------------------------------------*/
static char *
pipe_name( char *pipe_name ) {
   return (pipe_name + 1);
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    directoryp ...                                                   */
/*    -------------------------------------------------------------    */
/*    Is a file a directory?                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
directoryp( char *name ) { 
   struct stat buf;

   if( stat( name, &buf ) == -1 )
      return 0;

   return S_ISDIR( buf.st_mode & S_IFMT );
}

/*---------------------------------------------------------------------*/
/*    REGULAR_FILE_NAMEP ...                                           */
/*---------------------------------------------------------------------*/
#define REGULAR_FILE_NAMEP( f ) \
   ((f[ 0 ] != '.') || ((f[ 1 ] != 0) && ((f[ 1 ] != '.') || f[ 2 ] != 0)))

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    directory_to_list ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
directory_to_list( char *name ) {
   obj_t res = BNIL;
#ifndef _MSC_VER
   DIR *dir;
   struct dirent *dirent;

   if( (dir = opendir( name )) ) {
      while( (dirent = readdir( dir )) ) {
	 char *fname = dirent->d_name;

	 if( REGULAR_FILE_NAMEP( fname ) ) 
	    res = MAKE_PAIR( string_to_bstring( fname ), res );
      }
      closedir( dir );
   }
#else
   char *const path = (char *)malloc( strlen( name ) + 2 + 1 );

   strcpy( path, name );
   strcat( path, "\\*" );

   {
      WIN32_FIND_DATA find_data;
      HANDLE hSearch = FindFirstFile( path, &find_data );

      if( hSearch != INVALID_HANDLE_VALUE ) {
         BOOL keep_going;

         do {
	    if( REGULAR_FILE_NAMEP( find_data.cFileName ) ) 
               res = MAKE_PAIR( string_to_bstring( find_data.cFileName ), res );
            keep_going= FindNextFile( hSearch, &find_data );
         } while( keep_going );

         FindClose( hSearch );
      }
   }
#endif
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_directory_to_path_list ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_directory_to_path_list( char *name, int len, char sep ) {
   obj_t res = BNIL;
#ifndef _MSC_VER
   DIR *dir;
   struct dirent *dirent;

   if( (dir = opendir( name )) ) {
      while( (dirent = readdir( dir )) ) {
	 char *fname = dirent->d_name;

	 if( REGULAR_FILE_NAMEP( fname ) ) { 
	    obj_t bs = make_string_sans_fill( strlen( fname ) + len + 1 );
	    char *s = BSTRING_TO_STRING( bs );

	    strcpy( s, name );
	    s[ len ] = sep;
	    strcpy( s + len + 1, fname );

	    res = MAKE_PAIR( bs, res );
	 }
      }
      closedir( dir );
   }
#else
   char *const path = (char *)malloc( len + 2 + 1 );

   strcpy( path, name );
   strcat( path, "\\*" );

   {
      WIN32_FIND_DATA find_data;
      HANDLE hSearch = FindFirstFile( path, &find_data );

      if( hSearch != INVALID_HANDLE_VALUE ) {
         BOOL keep_going;

         do {
	    char *fname = find_data.cFileName; 
	    if( REGULAR_FILE_NAMEP( fname ) ) { 
	       obj_t bs = make_string_sans_fill( strlen( fname ) + len + 1 );
	       char *s = BSTRING_TO_STRING( bs );
	       strcpy( s, name );
	       s[ len ] = sep;
	       strcpy( s + len + 1, fname );
	       
	       res = MAKE_PAIR( bs, res );
	    }
            keep_going= FindNextFile( hSearch, &find_data );
         } while( keep_going );

         FindClose( hSearch );
      }
   }
#endif
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_file_to_string ...                                           */
/*---------------------------------------------------------------------*/
obj_t
bgl_file_to_string( char *path ) {
   int fd = open( path, O_RDONLY );

   if( !fd ) {
      C_SYSTEM_FAILURE( bglerror( errno ), 
			"file->string",
			strerror( errno ),
			string_to_bstring( path ) );
   } else {
      struct stat sin;
      
      if( fstat( fd, &sin ) ) {
	 close( fd );
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			   "file->string",
			   strerror( errno ),
			   string_to_bstring( path ) );
      } else {
	 obj_t res = make_string_sans_fill( sin.st_size );
	 int n = read( fd, BSTRING_TO_STRING( res ), sin.st_size );

	 close( fd );
	 
	 if( sin.st_size != n ) {
	    C_SYSTEM_FAILURE( BGL_IO_READ_ERROR,
			      "file->string",
			      strerror( errno ),
			      string_to_bstring( path ) );
	 } else {
	    close( fd );
	    return res;
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    copyfile ...                                                     */
/*    -------------------------------------------------------------    */
/*    This function is a replacement for sendfile. In particular       */
/*    it is used when the INPUT-PORT is a socket descriptor or when    */
/*    the host system does not support the sendfile system call.       */
/*---------------------------------------------------------------------*/
static long
copyfile( obj_t op, void *ip, long sz, long (*sysread)() ) {
   long rsz = 0, o;

   if( sz < 0 ) {
#ifdef __GNUC__
      char _buf[ default_io_bufsiz + 1 ];
      char *buf = _buf;
#else
      char *buf = alloca( default_io_bufsiz + 1 );
#endif
      long n;
      size_t m;

#ifdef DEBUG_SENDCHARS
      fprintf( stderr, "copyfile op=%p ip=%p sz=%d\n", op, ip, sz );
#endif
      
   loopr:
      while( (n = sysread( ip, buf, default_io_bufsiz )) > 0 ) {
	 o = 0;
	 
	 bgl_write( op, (unsigned char *)&buf[ o ], n );
	 rsz += n;
      }

      if( (n < 0) && (errno == EINTR) ) {
	 goto loopr;
      }

#ifdef DEBUG_SENDCHARS
      fprintf( stderr, "copyfile_with_timeout rsz=%d\n", rsz );
#endif

      bgl_output_flush( op, 0, 0 );
      return rsz;
   } else {
      long n = 0, m;
      long s = (default_io_bufsiz > sz) ? sz : default_io_bufsiz;
#ifdef __GNUC__
      char buf[ s + 1 ];
#else
      char *buf = alloca( s + 1 );
#endif

#ifdef DEBUG_SENDCHARS
      fprintf( stderr, "copyfile_with_timeout op=%p ip=%p sz=%d\n", op, ip, sz );
#endif
   loopr2:
      while( (sz > 0) && ((n = sysread( ip, buf, s )) > 0) ) {
	 bgl_write( op, (unsigned char *)&buf[ 0 ], n );
	 rsz += n;
	 sz -= n;
	 
	 if( sz < s ) s = sz;
      }

      if( n < 0) {
	 if( errno == EINTR ) {
	    goto loopr2;
	 } else {
	    return n;
	 }
      }

#ifdef DEBUG_SENDCHARS
      fprintf( stderr, "copyfile_with_timeout rsz=%d\n", rsz );
#endif
      
      bgl_output_flush( op, 0, 0 );
      return rsz;
   }
}  

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    gc_sendfile ...                                                  */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_SENDFILE && BGL_GC_HAVE_DO_BLOCKING )
static void
gc_sendfile( struct sendfile_info_t *si ) {
   size_t sz = si->sz;
   off_t *offset = si->off;
   ssize_t res = 0;
   ssize_t n = 0;
   fd_set writefds;
   
#if DEBUG_SENDCHARS
   fprintf(stderr, "gc_sendfile out=%p in=%p offset=%d sz=%d\n", si->out, si->in, offset ? *offset : 0, si->sz);
#endif

   while( sz > 0 ) {
      if( (n = BGL_SENDFILE( si->out, si->in, offset, sz )) < 0 ) {
	 if( errno == EAGAIN || errno == EINTR ) {
	    FD_ZERO( &writefds );
	    FD_SET( si->out, &writefds );

	    if( select( si->out + 1, NULL, &writefds, NULL, 0 ) <= 0 ) {
	       si->res = -1;
	       return;
	    } else {
	       continue;
	    }
	 } else {
	    si->res = -1;
	    return;
	 }
      } else {
	 sz -= n;
	 
#if DEBUG_SENDCHARS
	 fprintf( stderr, "gc_sendfile read chars n=%d remaining=%d\n", n, sz );
#endif
      }
   }

   si->res = si->sz;
   return;
}
#endif

/*---------------------------------------------------------------------*/
/*    bgl_sendchars ...                                                */
/*    -------------------------------------------------------------    */
/*    uses sendfile to "copy" the input-port to the output-port        */
/*    flushes output-port!                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_sendchars( obj_t ip, obj_t op, long sz, long offset ) {
#define inp INPUT_PORT( ip )
#define outp OUTPUT_PORT( op )
   long dsz;
   long ws = 0;
   struct stat in;
   struct stat out;
   long n;
   void *fd = PORT_STREAM( op );

   if( (PORT( op ).kindof == KINDOF_CLOSED) ||
       (PORT( ip ).kindof == KINDOF_CLOSED) ||
       (PORT( ip ).kindof == KINDOF_GZIP) )
      return BFALSE;
      
   if( offset >= 0 ) bgl_input_port_seek( ip, offset );

   dsz = inp.bufpos - inp.matchstop - 1;
#ifdef DEBUG_SENDCHARS   
   fprintf( stderr, "bgl_sendchars.1: sz=%d offset=%d dsz=%d\n", sz, offset, dsz );
#endif	 
   bgl_output_flush( op, 0, 0 );
#ifdef DEBUG_SENDCHARS   
   fprintf( stderr, "bgl_sendchars.1b: sz=%d offset=%d dsz=%d\n", sz, offset, dsz );
#endif	 

   if( dsz > 0 ) {
      /* flush the input buffer when it contains characters */
      size_t w;

      ws = ((sz > 0) && (dsz > sz)) ? sz : dsz;
      w = outp.syswrite( fd, &STRING_REF( inp.buf, inp.matchstop ), ws );
	 
#ifdef DEBUG_SENDCHARS   
      fprintf( stderr, "bgl_sendchars.3: sz=%d offset=%d w=%d ws=%d\n", sz, offset, w, ws );
#endif	 
      inp.matchstop += (long)w;
      inp.forward = inp.matchstop;

      if( w < ws ) {
	 C_SYSTEM_FAILURE( bglerror( errno ),
			   "send-chars",
			   strerror( errno ),
			   MAKE_PAIR( ip, op ) );
	 return BINT( w );
      }

      if( sz > 0 ) {
	 if( dsz >= sz ) {
#ifdef DEBUG_SENDCHARS   
	    fprintf( stderr, "bgl_sendchars.4: RETURN ws=%d\n", ws );
#endif	       
	    return BINT( ws );
	 }

	 sz -= ws;
      }
   }

#if( BGL_HAVE_SENDFILE )
   if( 
      /* Some operating systems (such as Linux 2.6.10) are demanding */
      /* on the input and output ports. These requirements are set   */
      /* in the configuration files and used to determine what has   */
      /* to be checked before invoking the actual sendfile sys call  */
#  if( BGL_SENDFILE_REQUIRE_INPUT_FILE )
      (inp.port.kindof != KINDOF_FILE)
      || fstat( fileno( (FILE *)PORT_STREAM( ip ) ), &in )
      || !S_ISREG( in.st_mode )
#  else
      0
#  endif
#  if( BGL_SENDFILE_REQUIRE_OUTPUT_SOCKET )
      || (outp.port.kindof != KINDOF_SOCKET)
      || _FSTAT( (int)PORT_STREAM( op ), &out )
      || ((out.st_mode & S_IFSOCK) != S_IFSOCK)
#  endif       
      || 0 ) {
#endif /* BGL_HAVE_SENDFILE */
      if( sz != 0 ) {
	 n = copyfile( op, ip, sz, INPUT_PORT( ip ).sysread );
      }
      if( n < 0 ) {
	 C_SYSTEM_FAILURE( bglerror( errno ), 
			   "send-chars",
			   strerror( errno ),
			   MAKE_PAIR( ip, op ) );
	 return BINT( 0 );
      }
#if( BGL_HAVE_SENDFILE )
   } else {
      if( sz == -1 ) sz = in.st_size;

      if( sz != 0 ) {
#if( BGL_GC_HAVE_BLOCKING )
	 bgl_gc_start_blocking();

	 n = BGL_SENDFILE( (int)PORT_STREAM( op ),
			   fileno( (FILE *)PORT_STREAM( ip ) ),
			   0L,
			   sz );
	 
	 bgl_gc_stop_blocking();
#else
#  if( BGL_GC_HAVE_DO_BLOCKING )
	 struct sendfile_info_t si;
	 si.out = (int)PORT_STREAM( op );
	 si.in = fileno( (FILE *)(PORT_STREAM( ip ) ) );
	 si.sz = sz;
	 si.port = op;
	 si.off = 0L;

#ifdef DEBUG_SENDCHARS   
	 fprintf( stderr, "bgl_sendchars.5: sz=%d offset=%d\n", sz, offset );
#endif	    
	 bgl_gc_do_blocking( &gc_sendfile, &si );

	 n = si.res;
#ifdef DEBUG_SENDCHARS   
	 fprintf( stderr, "bgl_sendchars.6: n=%d\n", n );
#endif	    
#  else
      -> error BGL_GC_HAVE_BLOCKING or BGL_GC_HAVE_DO_BLOCKING required
#  endif  /* BGL_HAVE_DO_BLOCKING */
#endif  /* BGL_HAVE_BLOCKING */
      }

      if( n < 0 )
	 C_SYSTEM_FAILURE( bglerror( errno ),
			   "send-chars",
			   strerror( errno ),
			   MAKE_PAIR( ip, op ) );
   }
   if( offset > 0 ) {
      if( INPUT_PORT_ON_FILEP( ip ) )
	 fseek( (FILE *)PORT_STREAM( ip ), offset + n + ws, SEEK_SET );
   }
#endif /* BGL_HAVE_SENDFILE */
   
   inp.filepos += n + ws;

   return BINT( n + ws );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_sendfile ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_sendfile( obj_t name, obj_t op, long sz, long offset ) {
   struct stat sin;
   _FD fd = (_FD)PORT_STREAM( op );
   int n;
   int in;

   if( PORT( op ).kindof == KINDOF_CLOSED )
      return BFALSE;

      /* Some operating systems (such as Linux 2.6.10) are demanding */
      /* on the input and output ports. These requirements are set   */
      /* in the configuration files and used to determine what has   */
      /* to be checked before invoking the actual sendfile sys call  */
#  if( BGL_SENDFILE_REQUIRE_OUTPUT_SOCKET )
   if( outp.port.kindof != KINDOF_SOCKET)
      return BFALSE;
#  endif

   bgl_output_flush( op, 0, 0 );

   if( sz == -1 ) {
      if( fstat( in, &sin ) ) {
	 C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			   "send-file",
			   strerror( errno ),
			   name );
      }
      sz = sin.st_size;
   }

   if( !(in = open( BSTRING_TO_STRING( name ), O_RDONLY, OMOD )) ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"send-file",
			strerror( errno ),
			name );
   }

#if( !BGL_HAVE_SENDFILE )
   // fprintf( stderr, "bgl_sendfile(%s:%d) copyfile must be protected otherwise it  left its file opened on error\n", __FILE__, __LINE__ );
   /* care, copy file must be installed when the unwind-protect pbm is fixed */
   // n = copyfile( op, (void *)in, sz, (long (*)())&read );
   return BFALSE;
#else
   if( sz != 0 ) {
#  if( BGL_GC_HAVE_BLOCKING )
      bgl_gc_start_blocking();

      n = BGL_SENDFILE( fd, in, (offset > 0 ? (off_t *)(&offset) : 0), sz );

      bgl_gc_stop_blocking();
#  else
#    if( BGL_GC_HAVE_DO_BLOCKING )
      {
	 struct sendfile_info_t si;
	 si.out = fd;
	 si.in = in;
	 si.sz = sz;
	 si.port = op;
	 si.off = (offset > 0 ? (off_t *)(&offset) : 0);

	 bgl_gc_do_blocking( &gc_sendfile, &si );

	 n = si.res;
      }
#    else
      -> error BGL_GC_HAVE_BLOCKING or BGL_GC_HAVE_DO_BLOCKING required
#    endif
#  endif /* BGL_HAVE_BLOCKING */

   }
#endif

   close( in );

   if( n < 0 )
      C_SYSTEM_FAILURE( bglerror( errno ),
			"send-file",
			strerror( errno ),
			MAKE_PAIR( name, op ) );
   return BINT( n );
}

/*---------------------------------------------------------------------*/
/*    static size_t                                                    */
/*    strwrite ...                                                     */
/*---------------------------------------------------------------------*/
static size_t
strwrite( obj_t port, void *str, size_t count ) {
   obj_t buf = OUTPUT_PORT( port ).buf;
   long cnt = OUTPUT_PORT( port ).cnt;
   long used = STRING_LENGTH( buf ) - cnt;
   long nlen = (STRING_LENGTH( buf ) + count) * 2;
   obj_t nbuf = make_string_sans_fill( nlen );

#if DEBUG   
   fprintf( stderr, "%s:%d>>> strwrite count=%d\n", __FILE__, __LINE__, count );
#endif

   memcpy( &STRING_REF( nbuf, 0 ), &STRING_REF( buf, 0 ), used );
   memcpy( &STRING_REF( nbuf, used ), str, count );

   OUTPUT_PORT( port ).ptr = (char *)&STRING_REF( nbuf, used + count );
   OUTPUT_PORT( port ).cnt = nlen - (used + count);
   OUTPUT_PORT( port ).buf = nbuf;

   return count;
}

/*---------------------------------------------------------------------*/
/*    static size_t                                                    */
/*    procwrite ...                                                    */
/*---------------------------------------------------------------------*/
static size_t
procwrite( obj_t port, void *str, size_t sz ) {
   obj_t proc = VECTOR_REF( PORT( port ).userdata, 0 );
   obj_t buf = VECTOR_REF( PORT( port ).userdata, 1 );
   int len = STRING_LENGTH( buf );

   if( sz > len ) {
      buf = make_string_sans_fill( sz + 1 );
      len = sz + 1;
      VECTOR_SET( PORT( port ).userdata, 1, buf );
   }
      
   memcpy( &STRING_REF( buf, 0 ), str, sz );
   STRING_SET( buf, sz, '\0' );
   STRING_LENGTH( buf ) = sz;
   
   PROCEDURE_ENTRY( proc )( proc, buf, BEOA );
   
   STRING_LENGTH( buf ) = len;
   return sz;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    procflush ...                                                    */
/*---------------------------------------------------------------------*/
static obj_t
procflush( obj_t port ) {
   obj_t flush = VECTOR_REF( PORT( port ).userdata, 2 );

   return PROCEDURE_ENTRY( flush )( flush, BEOA );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    procclose ...                                                    */
/*---------------------------------------------------------------------*/
static int
procclose( obj_t port ) {
   obj_t close = VECTOR_REF( PORT( port ).userdata, 3 );

   PROCEDURE_ENTRY( close )( close, BEOA );

   return 0;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_password ...                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_password( char *prompt ) {
   int max_len = 80;
   char *s = alloca( max_len + 1 );
   int i = 0;
   int c;
   FILE *tty = fopen( "/dev/tty", "w" );
   FILE *out = tty ? tty : stderr;
   
#if HAVE_TERMIO || HAVE_TERMIOS
   struct termios t;
   tcflag_t lflag;

   fputs( prompt, out );
   fflush( out );

   /* set the terminal in no echo mode */
   tcgetattr( 0, &t );
   lflag = t.c_lflag;
   t.c_lflag &= ~(ICANON|ECHO);
   t.c_cc[ VMIN ] = 1;
   t.c_cc[ VTIME ] = 0;
   tcsetattr( 0, TCSANOW, &t );
#endif

   while( (c = getchar()) != '\n' ) {
      if( i == max_len ) {
	 char *ns = alloca( max_len * 2 );
	 memcpy( ns, s, max_len );
	 max_len *= 2;
	 s = ns;
      }
      
      s[ i++ ] = c;
      putc( '*', out );
      fflush( out );
   }

   s[ i ] = 0;
   
#if HAVE_TERMIO || HAVE_TERMIOS
   /* restore the terminal */
   t.c_lflag = lflag;
   tcsetattr( 0, TCSANOW, &t );
#endif

   putc( '\n', out );
   fflush( out );
   
   if( tty ) fclose( tty );
   
   return string_to_bstring_len( s, i );
}

