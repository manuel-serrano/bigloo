/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/csystem.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jan 20 08:45:23 1993                          */
/*    Last change :  Wed Sep 25 13:47:21 2019 (serrano)                */
/*    Copyright   :  2002-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    System interface                                                 */
/*=====================================================================*/
#include <sys/types.h>
#include <time.h>
#include <utime.h>
#include <sys/time.h>
#include <signal.h>
#include <string.h>

#ifdef _MINGW_VER
#  define _BGL_WIN32_VER
#  include <io.h>
#  include <winsock2.h>
#  define lstat stat
#else
#  include <sys/times.h>
#  ifdef _MSC_VER
#    define _BGL_WIN32_VER
#    include <io.h>
#    include <winsock2.h>
#    define lstat stat
#  else
#    include <unistd.h>
#    include <sys/socket.h>
#    include <netinet/in.h>
#    include <arpa/inet.h>
#    include <netdb.h>
#  endif
#endif
#include <bigloo.h>

#if BGL_HAVE_GETUID
#  include <pwd.h>
#else
#define uid_t int
#define gid_t int
#endif

#if BGL_HAVE_IOCTL
#   include <sys/ioctl.h>
#endif

#if BGL_HAVE_GETRLIMIT
#include <sys/resource.h>
#endif

/*---------------------------------------------------------------------*/
/*    stack overflow constants                                         */
/*---------------------------------------------------------------------*/
#define BGL_STACKOVERFLOW_SIZE_THRESHOLD 8192;

/*---------------------------------------------------------------------*/
/*    imports                                                          */
/*---------------------------------------------------------------------*/
extern obj_t bgl_stack_overflow_error();

/*---------------------------------------------------------------------*/
/*    Signal mutex                                                     */
/*---------------------------------------------------------------------*/
static obj_t signal_mutex = BUNSPEC;
DEFINE_STRING( signal_mutex_name, _1, "signal-mutex", 12 );
static obj_t getuid_mutex = BUNSPEC;
DEFINE_STRING( getuid_mutex_name, _2, "getuid-mutex", 12 );

/*---------------------------------------------------------------------*/
/*    thread or process sigprocmask                                    */
/*---------------------------------------------------------------------*/
#if HAVE_SIGPROCMASK
extern int (*bgl_sigprocmask)( int, const sigset_t *, sigset_t * );
#endif

/*---------------------------------------------------------------------*/
/*    bgl_init_signal ...                                              */
/*---------------------------------------------------------------------*/
void
bgl_init_signal() {
   if( signal_mutex == BUNSPEC ) {
      signal_mutex = bgl_make_mutex( signal_mutex_name );
   }
   if( getuid_mutex == BUNSPEC ) {
      getuid_mutex = bgl_make_mutex( getuid_mutex_name );
   }
}
          
/*---------------------------------------------------------------------*/
/*    signal_handler ...                                               */
/*---------------------------------------------------------------------*/
static obj_t
signal_handler( int num ) {
   obj_t handler = BGL_SIG_HANDLERS()[ num ];

   /* Re-install the signal handler because some OS (such as Solaris) */
   /* de-install it when the signal is raised.                        */
#if !BGL_HAVE_SIGACTION
   signal( num, (void (*)(int))(signal_handler) );
#endif

   if( PROCEDUREP( handler ) ) {
      return ((obj_t (*)())PROCEDURE_ENTRY(handler))( handler, BINT( num ), BEOA );
   } else {
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    stackov_heuristic_getrlimitp ...                                 */
/*    -------------------------------------------------------------    */
/*    getrlimit heuristic for stack overflow detection.                */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_GETRLIMIT
static int
stackov_heuristic_getrlimitp( siginfo_t *siginfo ) {
   struct rlimit rlimit;
   const obj_t env = BGL_CURRENT_DYNAMIC_ENV();
#if( defined( STACK_GROWS_DOWN ) )
   long stksz = BGL_ENV_STACK_BOTTOM( env ) - (char *)siginfo->si_addr;
#else
   long stksz = (char *)siginfo->si_addr - BGL_ENV_STACK_BOTTOM( env );
#endif   
   long delta;

   getrlimit( RLIMIT_STACK, &rlimit );

   delta = rlimit.rlim_cur - stksz;

   return delta < BGL_STACKOVERFLOW_SIZE_THRESHOLD;
}
#endif

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    stackov_heuristic_sbrk ...                                       */
/*---------------------------------------------------------------------*/
static int
stackov_heuristic_sbrk() {
#if !defined( _BGL_WIN32_VER )
   return ( (long)sbrk( 8192 ) == -1 );
#else
   return 0;
#endif   
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    stackov_handler ...                                              */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_SIGINFO
static void
stackov_handler( int sig, siginfo_t *siginfo, void *ucontext ) {
#if BGL_HAVE_GETRLIMIT
   if( stackov_heuristic_getrlimitp( siginfo ) ) {
      bgl_stack_overflow_error();
   } else
#endif      
   if( stackov_heuristic_sbrk() ) {
      bgl_stack_overflow_error();
   } else {
      /* re-execute the instruction that caused the trap */
      /* to raise the sigsegv normal error handling      */
      signal( SIGSEGV , SIG_DFL) ;
   }
}
#else
stackov_handler( int sig ) {
   if( stackov_heuristic_sbrk() ) {
      bgl_stack_overflow_error();
   } else {
      /* re-execute the instruction that caused the trap */
      /* to raise the sigsegv normal error handling      */
      signal( SIGSEGV , SIG_DFL) ;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    bgl_signal ...                                                   */
/*---------------------------------------------------------------------*/
obj_t
bgl_signal( int sig, obj_t obj ) {
   BGL_MUTEX_LOCK( signal_mutex );

   /* store the obj in the signal table */
   if( obj != BUNSPEC ) {
      BGL_SIG_HANDLERS()[ sig ] = obj;
   }
   
   if( PROCEDUREP( obj ) || obj == BUNSPEC ) {
#if BGL_HAVE_SIGACTION
      {
	 struct sigaction sigact;
	 sigemptyset( &(sigact.sa_mask) );
	 sigact.sa_handler = (void (*)( int ))signal_handler;
	 sigact.sa_flags = SA_RESTART;

	 if( sig == SIGSEGV ) {
	    /* create an alternate stack for SEGV */
	    sigact.sa_flags |= SA_ONSTACK;
	    stack_t ss;

	    ss.ss_flags = 0L;
	    ss.ss_sp = malloc( SIGSTKSZ );
	    ss.ss_size = SIGSTKSZ;

	    if( obj == BUNSPEC ) {
	       /* this is a fake stack overflow detection (see cmain.c) */
#if BGL_HAVE_SIGINFO
	       sigact.sa_flags |= SA_SIGINFO;
	       sigact.sa_handler = NULL;
	       sigact.sa_sigaction = stackov_handler;
#else
	       sigact.sa_handler = stackov_handler;
#endif	    	    
	    }
	    
	    sigaltstack( &ss, 0L );
	 }

	 sigaction( sig, &sigact, NULL );
      }
#else
      if( obj != BUNSPEC ) {
	 signal( (int)sig, (void (*)( int ))signal_handler );
      }
#endif      
      
   } else {
      if( obj == BTRUE ) {
	 signal( (int)sig, SIG_IGN );
      } else {
	 if( obj == BFALSE ) {
	    signal( (int)sig, SIG_DFL );
	 }
      }
   }
   
   BGL_MUTEX_UNLOCK( signal_mutex );
   
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_get_signal_handler ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_get_signal_handler( int sig ) {
   return BGL_SIG_HANDLERS()[ sig ];
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_restore_signal_handlers ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_restore_signal_handlers() {
#if HAVE_SIGPROCMASK
   sigset_t set;

   sigemptyset( &set );
   bgl_sigprocmask( SIG_SETMASK, &set, 0 );
#endif
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_sigsetmask ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_sigsetmask( int set ) {
#if HAVE_SIGPROCMASK
   if( !set ) {
      sigset_t mask;
      bgl_sigprocmask( SIG_SETMASK, 0, &mask );

      return bgl_sigprocmask( SIG_UNBLOCK, &mask, 0 );
   } else {
      return bgl_sigprocmask( SIG_SETMASK, (const sigset_t *)&set, 0 );
   }
#else
   return 0;
#endif
}

/*---------------------------------------------------------------------*/
/*    c_date ...                                                       */
/*---------------------------------------------------------------------*/
char *
c_date() {
#if( defined( sony_news ) )
   long now;
#else      
   time_t now;
#endif

   now = time( 0L );
   return ctime( &now );
}
      
/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_last_modification_time ...                                   */
/*---------------------------------------------------------------------*/
long
bgl_last_modification_time( char *file ) {
   struct stat _stati;

   if( lstat( file, &_stati ) )
      return -1;
   else
      return (long)(_stati.st_mtime);
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_last_change_time ...                                         */
/*---------------------------------------------------------------------*/
long
bgl_last_change_time( char *file ) {
   struct stat _stati;

   if( lstat( file, &_stati ) )
      return -1;
   else
      return (long)(_stati.st_ctime);
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_last_access_time ...                                         */
/*---------------------------------------------------------------------*/
long
bgl_last_access_time( char *file ) {
   struct stat _stati;

   if( lstat( file, &_stati ) )
      return -1;
   else
      return (long)(_stati.st_atime);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_utime ...                                                    */
/*---------------------------------------------------------------------*/
int
bgl_utime( char *file, long atime, long mtime ) {
   struct utimbuf buf = { .actime = (time_t)atime, .modtime= (time_t)mtime };
   int r = utime( file, &buf );
   
   if( r < 0 ) {
      C_SYSTEM_FAILURE( BGL_ERROR, "file-times-set!",
			strerror( errno ),
			string_to_bstring( file ) );
   }
   return r;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_file_size ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_file_size( char *file ) {
   struct stat _stati;

   if( stat( file, &_stati ) )
      return -1;
   else
      return (long)_stati.st_size;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_file_uid ...                                                 */
/*---------------------------------------------------------------------*/
long
bgl_file_uid( char *file ) {
   struct stat _stati;

   if( lstat( file, &_stati ) )
      return -1;
   else
      return _stati.st_uid;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_file_gid ...                                                 */
/*---------------------------------------------------------------------*/
long
bgl_file_gid( char *file ) {
   struct stat _stati;

   if( lstat( file, &_stati ) )
      return -1;
   else
      return _stati.st_gid;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_file_mode ...                                                */
/*---------------------------------------------------------------------*/
long
bgl_file_mode( char *file ) {
   struct stat _stati;

   if( stat( file, &_stati ) )
      return -1;
   else
      return _stati.st_mode;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_file_type ...                                                */
/*---------------------------------------------------------------------*/
obj_t
bgl_file_type( char *file ) {
   struct stat _stati;

   if( lstat( file, &_stati ) ) {
      return string_to_symbol( "does-not-exist" );
   }

#if( defined( S_ISLNK ) )
   if( S_ISLNK( _stati.st_mode ) ) {
      return string_to_symbol( "link" );
   }
#endif   

#if( defined( S_ISREG ) )
   if( S_ISREG( _stati.st_mode ) ) {
      static obj_t reg = 0L;

      if( !reg ) reg = string_to_symbol( "regular" );
      return reg;
   }
#endif   

#if( defined( S_ISDIR ) )
   if( S_ISDIR( _stati.st_mode ) ) {
      static obj_t dir = 0L;

      if( !dir ) dir = string_to_symbol( "directory" );
      return dir;
   }
#endif   

#if( defined( S_ISBLK ) )
   if( S_ISBLK( _stati.st_mode ) ) {
      return string_to_symbol( "block" );
   }
#endif   

#if( defined( S_ISCHR ) )
   if( S_ISCHR( _stati.st_mode ) ) {
      return string_to_symbol( "character" );
   }
#endif   

#if( defined( S_ISFIFO ) )
   if( S_ISFIFO( _stati.st_mode ) ) {
      return string_to_symbol( "fifo" );
   }
#endif   

#if( defined( S_ISSOCK ) )
   if( S_ISSOCK( _stati.st_mode ) ) {
      return string_to_symbol( "socket" );
   }
#endif

   return string_to_symbol( "unknown" );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_chmod ...                                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
int
bgl_chmod( char *file, int read, int write, int exec ) {
# ifndef _BGL_WIN32_VER
    return chmod( file,
                  (read ? S_IRUSR : 0) |
                  (write ? S_IWUSR : 0) |
                  (exec ? S_IXUSR : 0) );
# else
    return _chmod( file,
                   (read ? S_IREAD : 0) |
                   (write ? S_IWRITE : 0) );
# endif
}
		 
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_setenv ...                                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_setenv( char *id, char *val ) {
   size_t l1 = strlen( id ), l2 = strlen( val );
   char *s = malloc( l1 + l2 + 2 );
   
   strcpy( s, id );
   s[ l1 ] = '=';
   strcpy( &s[ l1 + 1 ], val );

   return putenv( s );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_time ...                                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_time( obj_t thunk ) {
#ifdef _MINGW_VER
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   
   BGL_ENV_MVALUES_NUMBER_SET( env, 4 );
   BGL_ENV_MVALUES_VAL_SET( env, 1, 0 );
   BGL_ENV_MVALUES_VAL_SET( env, 2, 0 );
   BGL_ENV_MVALUES_VAL_SET( env, 3, 0 );

   return PROCEDURE_ENTRY( thunk )( thunk, BEOA );
#else   
   static long ctick = 0;
   obj_t env = BGL_CURRENT_DYNAMIC_ENV();
   struct tms buf1, buf2;
   clock_t t1, t2;
   obj_t res;

   if( !ctick ) ctick = sysconf( _SC_CLK_TCK );

   t1 = times( &buf1 );
   res = PROCEDURE_ENTRY( thunk )( thunk, BEOA );
   t2 = times( &buf2 );
      
   BGL_ENV_MVALUES_NUMBER_SET( env, 4 );

#  define BTICK( v ) BINT( (v) * 1000 / ctick )
   BGL_ENV_MVALUES_VAL_SET( env, 1, BTICK( t2 - t1 ) );
   BGL_ENV_MVALUES_VAL_SET( env, 2, BTICK( buf2.tms_stime - buf1.tms_stime ) );
   BGL_ENV_MVALUES_VAL_SET( env, 3, BTICK( (buf2.tms_cutime - buf1.tms_cutime)
					   + (buf2.tms_utime - buf1.tms_utime) ) );
#  undef BTICK   

   return res;
#endif   
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_getuid ...                                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_getuid() {
#if BGL_HAVE_GETUID
   return getuid();
#else
   return 0;
#endif
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_setuid ...                                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_setuid( uid_t uid ) {
#if BGL_HAVE_GETUID
   if( !setuid( uid ) ) {
      return uid;
   } else {
      C_SYSTEM_FAILURE( BGL_ERROR, "setuid", strerror( errno ), BINT( uid ) );
      return uid;
   }
#else
      C_SYSTEM_FAILURE( BGL_ERROR, "setuid",
			"operation not supported", BINT( uid ) );
      return uid;
#endif
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_getgid ...                                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_getgid() {
#if BGL_HAVE_GETGID
   return getgid();
#else
   return 0;
#endif
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_setgid ...                                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_setgid( gid_t gid ) {
#if BGL_HAVE_GETGID
   if( !setgid( gid ) ) {
      return gid;
   } else {
      C_SYSTEM_FAILURE( BGL_ERROR, "setgid", strerror( errno ), BINT( gid ) );
      return gid;
   }
#else
      C_SYSTEM_FAILURE( BGL_ERROR, "setgid",
			"operation not supported", BINT( gid ) );
      return gid;
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    passwd2list ...                                                  */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_GETUID
static obj_t
passwd2list( struct passwd *pw ) {
   if( !pw ) {
      return BFALSE;
   } else {
      obj_t res;

      /* the shell */
      res = MAKE_PAIR( string_to_bstring( pw->pw_shell ), BNIL );
      /* the home directory */
      res = MAKE_PAIR( string_to_bstring( pw->pw_dir ), res );
      /* the real name */
#if BGL_HAVE_GECOS
      res = MAKE_PAIR( string_to_bstring( pw->pw_gecos ), res );
#endif   
      /* the group id */
      res = MAKE_PAIR( BINT( pw->pw_gid ), res );
      /* the user id */
      res = MAKE_PAIR( BINT( pw->pw_uid ), res );
      /* the password */
      res = MAKE_PAIR( string_to_bstring( pw->pw_passwd ), res );
      /* the name */
      res = MAKE_PAIR( string_to_bstring( pw->pw_name ), res );

      return res;
   }
}
#endif   

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_getpwnam ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_getpwnam( char *name ) {
#if BGL_HAVE_GETUID
   struct passwd *pw;
   obj_t res;

   BGL_MUTEX_LOCK( getuid_mutex );
   pw = getpwnam( name );
   res = passwd2list( pw );
   BGL_MUTEX_UNLOCK( getuid_mutex );

   return res;
#else
   return BFALSE;
#endif   
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_getpwuid ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_getpwuid( uid_t uid ) {
#if BGL_HAVE_GETUID
   struct passwd *pw;
   obj_t res;
   
   BGL_MUTEX_LOCK( getuid_mutex );
   pw = getpwuid( uid );
   res = passwd2list( pw );
   BGL_MUTEX_UNLOCK( getuid_mutex );

   return res;
#else
   return BFALSE;
#endif   
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_make_symlink ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_symlink( char *s1, char *s2 ) {
#if BGL_HAVE_SYMLINK   
   if( symlink( s1, s2 ) ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "make-symlink", strerror( errno ),
			string_to_bstring( s2 ) );
   }

   return 0;
#else
   C_SYSTEM_FAILURE( BGL_IO_ERROR, "make-symlink", "Not supported",
		     string_to_bstring( s2 ) );
   return 1;
#endif   
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_getgroups ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_getgroups() {
#if BGL_HAVE_GETGROUPS
   int ngroups = getgroups( 0, 0L );

  if( ngroups == -1 ) {
     C_SYSTEM_FAILURE( BGL_IO_ERROR, "getgroups", strerror( errno ), BFALSE );
  } else {
     gid_t* groups = alloca( sizeof( gid_t ) * ngroups );

     ngroups = getgroups( ngroups, groups );

     if( ngroups == -1 ) {
	C_SYSTEM_FAILURE( BGL_IO_ERROR, "getgroups", strerror( errno ), BFALSE );
     } else {
	obj_t res = create_vector( ngroups + 1 );
	gid_t egid = getegid();
	int seen_egid = 0;
	int i;

	for( i = 0; i < ngroups; i++ ) {
	   VECTOR_SET( res, i, BINT( groups[ i ] ) );
	   if( groups[ i ] == egid ) seen_egid = 1;
	}
	
	if( seen_egid ) {
	   VECTOR( res ).length--;
	} else {
	   VECTOR_SET( res, i, BINT( egid ) );
	}

	return res;
     }
  }
#else
      return create_vector( 0 );
#endif
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    dev2fd ...                                                       */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_IOCTL
static int
dev2fd( obj_t port ) {
   if( INTEGERP( port ) ) {
      return CINT( port );
   }
   if( INPUT_PORTP( port ) && PORT( port ).kindof == KINDOF_FILE ) {
#if POSIX_FILE_OPS
      return PORT_FD( port );
#else
      return fileno( PORT_FILE( p ) );
#endif
   }

   C_SYSTEM_FAILURE( BGL_TYPE_ERROR, "ioctl", "port or integer expected", port );
   return -1;
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ioctl ...                                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ioctl( obj_t dev, long request, long val ) {
#if BGL_HAVE_IOCTL
   unsigned long req = (unsigned long)request;
   int res = ioctl( dev2fd( dev ), req, (unsigned long)val );
	 
   if( !res ) {
      return 1;
   } else {
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "ioctl", strerror( errno ), dev );
      return 0;
   }
#else
   C_SYSTEM_FAILURE( BGL_ERROR, "ioctl", "ioctl not supported by architecture", dev );
   return 0;
#endif   
}

/*---------------------------------------------------------------------*/
/*    bits conversions (see bigloo.h for GCC versions).                */
/*---------------------------------------------------------------------*/
#if( !defined( __GNUC__ ) )

BGL_RUNTIME_DEF BGL_LONGLONG_T
DOUBLE_TO_LLONG_BITS( double dd ) {
   __DOUBLE_TO_LLONG_BITS( dd );
   return result;
}

BGL_RUNTIME_DEF double
LLONG_BITS_TO_DOUBLE( BGL_LONGLONG_T ll ) {
   __LLONG_BITS_TO_DOUBLE( ll );
   return result;
}

BGL_RUNTIME_DEF int
FLOAT_TO_INT_BITS( float f ) {
   __FLOAT_TO_INT_BITS( f );
   return result;
}

BGL_RUNTIME_DEF float
INT_BITS_TO_FLOAT( int i ) {
   __INT_BITS_TO_FLOAT( i );
   return result;
}

#endif

