/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/csystem.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jan 20 08:45:23 1993                          */
/*    Last change :  Tue Dec 28 12:31:39 2010 (serrano)                */
/*    Copyright   :  2002-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    System interface                                                 */
/*=====================================================================*/
#include <time.h>
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
#  include <sys/types.h>
#  include <pwd.h>
#else
#define uid_t int
#endif

/*---------------------------------------------------------------------*/
/*    Signal mutex                                                     */
/*---------------------------------------------------------------------*/
static obj_t signal_mutex = BUNSPEC;
DEFINE_STRING( signal_mutex_name, _1, "signal-mutex", 12 );
static obj_t getuid_mutex = BUNSPEC;
DEFINE_STRING( getuid_mutex_name, _2, "getuid-mutex", 12 );

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
/*    get_handler ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
get_handler( int num ) {
   obj_t handler = BGL_SIG_HANDLERS()[ num ];
   
   /* Re-install the signal handler because some OS (such as Solaris) */
   /* de-install it when the signal is raised.                        */
#if !HAVE_SIGACTION
   signal( num, (void (*)(int))(get_handler) );
#endif

   return ((obj_t (*)())PROCEDURE_ENTRY(handler))( handler, BINT( num ), BEOA );
}
    
/*---------------------------------------------------------------------*/
/*    obj_t ...                                                        */
/*---------------------------------------------------------------------*/
obj_t
c_signal( int sig, obj_t obj ) {
   bgl_mutex_lock( signal_mutex );

   if( PROCEDUREP( obj ) ) {
      /* store the obj in the signal table */
      BGL_SIG_HANDLERS()[ sig ] = obj;

#if HAVE_SIGACTION
      {
	 struct sigaction sigact;

	 sigemptyset( &(sigact.sa_mask) );
	 sigact.sa_handler = (void (*)( int ))get_handler;
	 sigact.sa_flags = SA_RESTART;
	 sigaction( sig, &sigact, NULL );
      }
#else      
      signal( (int)sig, (void (*)( int ))get_handler );
#endif      
      
   } else {
      /* store the obj in the signal table */
      BGL_SIG_HANDLERS()[ sig ] = obj;
      
      if( obj == BTRUE ) {
	 signal( (int)sig, SIG_IGN );
      } else {
	 if( obj == BFALSE ) {
	    signal( (int)sig, SIG_DFL );
	 }
      }
   }
   
   bgl_mutex_unlock( signal_mutex );
   
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    get_signal_handler ...                                           */
/*---------------------------------------------------------------------*/
obj_t
get_signal_handler( int sig ) {
   return BGL_SIG_HANDLERS()[ sig ];
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_sigprocmask ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_sigprocmask( int set ) {
#if HAVE_SIGPROCMASK
   if( !set ) {
      sigset_t mask;
      sigprocmask( SIG_SETMASK, 0, &mask );

      return sigprocmask( SIG_UNBLOCK, &mask, 0 );
   } else {
      return sigprocmask( SIG_SETMASK, (const sigset_t *)&set, 0 );
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
   struct stat _stat;

   if( lstat( file, &_stat ) )
      return -1;
   else
      return (long)(_stat.st_mtime);
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_file_size ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_file_size( char *file ) {
   struct stat _stat;

   if( stat( file, &_stat ) )
      return -1;
   else
      return (long)_stat.st_size;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_file_uid ...                                                 */
/*---------------------------------------------------------------------*/
long
bgl_file_uid( char *file ) {
   struct stat _stat;

   if( lstat( file, &_stat ) )
      return -1;
   else
      return _stat.st_uid;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_file_gid ...                                                 */
/*---------------------------------------------------------------------*/
long
bgl_file_gid( char *file ) {
   struct stat _stat;

   if( lstat( file, &_stat ) )
      return -1;
   else
      return _stat.st_gid;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_file_mode ...                                                */
/*---------------------------------------------------------------------*/
long
bgl_file_mode( char *file ) {
   struct stat _stat;

   if( stat( file, &_stat ) )
      return -1;
   else
      return _stat.st_mode;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_chmod ...                                                    */
/*---------------------------------------------------------------------*/
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
int
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
   BGL_ENV_MVALUES_VAL_SET( env, 3, BTICK( buf2.tms_utime - buf1.tms_utime ) );
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

   bgl_mutex_lock( getuid_mutex );
   pw = getpwnam( name );
   res = passwd2list( pw );
   bgl_mutex_unlock( getuid_mutex );

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
   
   bgl_mutex_lock( getuid_mutex );
   pw = getpwuid( uid );
   res = passwd2list( pw );
   bgl_mutex_unlock( getuid_mutex );

   return res;
#else
   return BFALSE;
#endif   
}
