/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cdlopen.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Feb 17 14:34:53 2000                          */
/*    Last change :  Thu Mar 14 15:09:40 2019 (serrano)                */
/*    -------------------------------------------------------------    */
/*    The dlopen interface.                                            */
/*=====================================================================*/
#include <string.h>
#if defined( _MINGW_VER )
#  include <windows.h>
#endif
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    bgl_dlsym_custom_t                                               */
/*---------------------------------------------------------------------*/
struct bgl_dlsym_custom_t {
   struct custom custom;
   obj_t *addr;
};

/*---------------------------------------------------------------------*/
/*    static char *bgl_error;                                          */
/*---------------------------------------------------------------------*/
#define DLOAD_ERROR_LEN 256
static char dload_error[ DLOAD_ERROR_LEN + 1 ];

static obj_t dload_list = BNIL;

/*---------------------------------------------------------------------*/
/*    dload mutex                                                      */
/*---------------------------------------------------------------------*/
static obj_t dload_mutex = BUNSPEC;
DEFINE_STRING( dload_mutex_name, _1, "dload-mutex", 12 );
static obj_t __dload_noarch = BUNSPEC;
static obj_t __dload_noinit = BUNSPEC;
static obj_t __dload_error = BUNSPEC;

/*---------------------------------------------------------------------*/
/*    bgl_init_dload ...                                               */
/*---------------------------------------------------------------------*/
void
bgl_init_dload() {
   dload_mutex = bgl_make_spinlock( dload_mutex_name );
   __dload_noarch = string_to_symbol( "__dload_noarch" );
   __dload_error = string_to_symbol( "__dload_error" );
   __dload_noinit = string_to_symbol( "__dload_noinit" );
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_dload_error ...                                              */
/*---------------------------------------------------------------------*/
char *
bgl_dload_error() {
   dload_error[ DLOAD_ERROR_LEN ] = 0;
   return dload_error;
}

#if( (!defined( _MSC_VER ) && !defined( _MINGW_VER ) ) )
#   if HAVE_DLOPEN
#      include <dlfcn.h>
#      ifndef RTLD_GLOBAL
#          define RTLD_GLOBAL 0
#      endif
#   endif

#define BGL_DLSYM( dlopen, symbol ) dlsym( dlopen, symbol )

/*---------------------------------------------------------------------*/
/*    static void *                                                    */
/*    dload_init_call ...                                              */
/*---------------------------------------------------------------------*/
static void *
dload_init_call( void *handle, char *sym ) {
   void *(*init)() = BGL_DLSYM( handle, sym );
   char *error;

   if( init == NULL ) {
      strncpy( dload_error, dlerror(), DLOAD_ERROR_LEN );
	 
      return BFALSE;
   } else {
      return init( 0, "dynamic-load" );
   }
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_dload ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_dload( char *filename, char *init_sym, char *init_mod ) {
#if !HAVE_DLOPEN
   strcpy( dload_error, "Feature not supported" );

   return __dload_noarch;
#else
   void *handle = dlopen( filename, RTLD_LAZY | RTLD_GLOBAL );
   obj_t p;

   if( !handle ) {
      char *error;
      
      if( (error = dlerror()) != NULL ) {
	 strncpy( dload_error, error, DLOAD_ERROR_LEN );
      } else {
	 strcpy( dload_error, "dlopen error" );
      }

      return __dload_error;
   } else {
      p = MAKE_PAIR( string_to_bstring( filename ), handle );

      BGL_MUTEX_LOCK( dload_mutex );
      dload_list = MAKE_PAIR( p, dload_list );
      BGL_MUTEX_UNLOCK( dload_mutex );
      
      if( *init_sym ) {
	 return dload_init_call( handle, init_sym );
      }

      if( *init_mod ) {
	 return dload_init_call( handle, init_mod );
      }
      
      return __dload_noinit;
   }
#endif
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_dunload ...                                                  */
/*---------------------------------------------------------------------*/
int
bgl_dunload( obj_t filename ) {
#if HAVE_DLOPEN
   extern bool_t bigloo_strcmp( obj_t, obj_t );

   obj_t p = dload_list;

   BGL_MUTEX_LOCK( dload_mutex );
   
   if ( NULLP( dload_list) ) {
      BGL_MUTEX_UNLOCK( dload_mutex );
      return 0;
   }
   
   if( bigloo_strcmp( CAR( CAR( p ) ), filename ) ) {
      dload_list = CDR( dload_list );
      dlclose( CDR( CAR( p ) ) );
      
      BGL_MUTEX_UNLOCK( dload_mutex );
      return 0;
   } else {
      obj_t r = CDR( p );

      while( PAIRP( r ) ) {
	 if( bigloo_strcmp( CAR( CAR( r ) ), filename ) ) {
	    SET_CDR( p, CDR( r ) );
	    dlclose( CDR( CAR( r ) ) );
	    
	    BGL_MUTEX_UNLOCK( dload_mutex );
	    return 0;
	 }
      }
   }
   
   BGL_MUTEX_UNLOCK( dload_mutex );
#endif   
   return 1;
}

#else
#  if( !defined( _MINGW_VER ) )
#    include <windows.h>
#  endif

#define BGL_DLSYM( dlopen, symbol ) GetProcAddress( dlopen, symbol )

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    dload_init_call ...                                              */
/*---------------------------------------------------------------------*/
static int
dload_init_call( HMODULE hLibrary, char *sym ) {
   void (*init)() = (void (*)())GetProcAddress( hLibrary, sym );
   char *error;
   
   if( !init ) {
      /* failed */
      char *error;

      if( FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER
			 | FORMAT_MESSAGE_FROM_SYSTEM,
			 NULL,
			 GetLastError(),
			 0,
			 (LPTSTR)&error,
			 0,
			 NULL ) > 0) {
	 strncpy( dload_error, error, DLOAD_ERROR_LEN );
	 LocalFree( error );
      } else {
	 strcpy( dload_error, "dlopen error" );
      }
      //FreeLibrary( hLibrary );
      return 2;
   } else {
      init( 0, "dymamic-load" );
      //FreeLibrary( hLibrary );
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_dload ...                                                    */
/*---------------------------------------------------------------------*/
int
bgl_dload( char *filename, char *init_sym, char *init_mod ) {
   HMODULE hLibrary = LoadLibrary( filename );

   if( !hLibrary ) {
      /* failed */
      char *error;

      if( FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER
			 | FORMAT_MESSAGE_FROM_SYSTEM,
                         NULL,
			 GetLastError(),
			 0,
			 (LPTSTR)&error,
			 0,
			 NULL ) > 0 ) {
         strncpy( dload_error, error, DLOAD_ERROR_LEN );
         LocalFree( error );
      } else {
         strcpy( dload_error, "dlopen error" );
      }
      return 1;
   } else {
      /* LoadLibrary succeeded */
      BGL_MUTEX_LOCK( dload_mutex );
      dload_list = MAKE_PAIR( p, dload_list );
      BGL_MUTEX_UNLOCK( dload_mutex );
      
      if( *init_sym ) {
	 int r = dload_init_call( hLibrary, init_sym );
	 if( r ) return r;
      }

      if( *init_mod ) {
	 int r = dload_init_call( hLibrary, init_mod );
	 if( r ) return r;
      }
      
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_dunload ...                                                  */
/*---------------------------------------------------------------------*/
int
bgl_dunload( obj_t filename ) {
   return 1;
}
#endif

/*---------------------------------------------------------------------*/
/*    static obj_t *                                                   */
/*    dload_get_symbol_addr ...                                        */
/*---------------------------------------------------------------------*/
static obj_t *
dload_get_symbol_addr( obj_t filename, obj_t name, obj_t symbol ) {
   obj_t h;
   void *dlopen = 0L;

   /* file the dload structure */
   BGL_MUTEX_LOCK( dload_mutex );
   h = dload_list;

   while( PAIRP( h ) && !dlopen ) {
      if( bigloo_strcmp( CAR( CAR( h ) ), filename ) ) {
	 dlopen = CDR( CAR( h ) );
      } else {
	 h = CDR( h );
      }
   }
   BGL_MUTEX_UNLOCK( dload_mutex );

   if( !dlopen ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"dload-get-symbol",
			"dynamic library not loaded",
			filename );
      return 0L;
   } else {
      return BGL_DLSYM( dlopen, BSTRING_TO_STRING( symbol ) );
   }
}

/*---------------------------------------------------------------------*/
/*    static char *                                                    */
/*    dlsym_to_string ...                                              */
/*---------------------------------------------------------------------*/
static char *
dlsym_to_string( obj_t obj, char *buffer, int len ) {
   obj_t id = (obj_t)CUSTOM_IDENTIFIER( obj );
   
   if( len > STRING_LENGTH( id ) + 10 ) {
      sprintf( buffer, "<dlsym:%s>", BSTRING_TO_STRING( id ) );
      return buffer;
   } else {
      return BSTRING_TO_STRING( id );
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    dlsym_output ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
dlsym_output( obj_t obj, obj_t op ) {
   obj_t id = (obj_t)CUSTOM_IDENTIFIER( obj );
   bgl_write( op, "<dlsym:", 8 );
   bgl_write( op, BSTRING_TO_STRING( id ), STRING_LENGTH( id ) );
   bgl_write( op, ">", 1 );
   return obj;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_dlsym ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_dlsym( obj_t filename, obj_t name, obj_t symbol ) {
   obj_t *addr = dload_get_symbol_addr( filename, name, symbol );

   if( addr ) {
      obj_t res = create_custom( sizeof( obj_t * ) );
      struct bgl_dlsym_custom_t *obj = (struct bgl_dlsym_custom_t *)CREF( res );

      CUSTOM_IDENTIFIER_SET( res, (char *)( name ) );
      CUSTOM_TO_STRING( res ) = dlsym_to_string;
      CUSTOM_OUTPUT( res ) = dlsym_output;
      obj->addr = addr;

      return res;
   } else {
      return BFALSE;
   }
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_dlsym_get ...                                                */
/*---------------------------------------------------------------------*/
obj_t
bgl_dlsym_get( obj_t dlsym ) {
   obj_t *ptr = ((struct bgl_dlsym_custom_t *)CREF( dlsym ))->addr;
   return *ptr;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_dlsym_set ...                                                */
/*---------------------------------------------------------------------*/
obj_t
bgl_dlsym_set( obj_t dlsym, obj_t val ) {
   obj_t *ptr = ((struct bgl_dlsym_custom_t *)CREF( dlsym ))->addr;
   return *ptr = val;
}
