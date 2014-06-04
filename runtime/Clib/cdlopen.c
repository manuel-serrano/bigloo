/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cdlopen.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Feb 17 14:34:53 2000                          */
/*    Last change :  Wed Jun  4 15:37:06 2014 (serrano)                */
/*    -------------------------------------------------------------    */
/*    The dlopen interface.                                            */
/*=====================================================================*/
#include <string.h>
#if defined( _MINGW_VER )
#  include <windows.h>
#endif
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    static char *bgl_error;                                          */
/*---------------------------------------------------------------------*/
#define DLOAD_ERROR_LEN 256
static char dload_error[ DLOAD_ERROR_LEN + 1 ];

static obj_t dload_list = BNIL;

/*---------------------------------------------------------------------*/
/*    Dload mutex                                                      */
/*---------------------------------------------------------------------*/
static obj_t dload_mutex = BUNSPEC;
DEFINE_STRING( dload_mutex_name, _1, "dload-mutex", 12 );

/*---------------------------------------------------------------------*/
/*    bgl_init_dload ...                                               */
/*---------------------------------------------------------------------*/
void
bgl_init_dload() {
   dload_mutex = bgl_make_spinlock( dload_mutex_name );
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

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    dload_init_call ...                                              */
/*---------------------------------------------------------------------*/
static int
dload_init_call( void *handle, char *sym ) {
   void *(*init)() = dlsym( handle, sym );
   char *error;

   if( init == NULL ) {
      strncpy( dload_error, dlerror(), DLOAD_ERROR_LEN );
	 
      return 2;
   } else {
      init( 0, "dynamic-load" );
      return 0;
   }
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_dload ...                                                    */
/*---------------------------------------------------------------------*/
int
bgl_dload( char *filename, char *init_sym, char *init_mod ) {
#if !HAVE_DLOPEN
   strcpy( dload_error, "Feature not supported" );

   return 3;
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

      return 1;
   } else {
      p = MAKE_PAIR( string_to_bstring( filename ), handle );

      BGL_MUTEX_LOCK( dload_mutex );
      dload_list = MAKE_PAIR( p, dload_list );
      BGL_MUTEX_UNLOCK( dload_mutex );
      
      if( *init_sym ) {
	 int r = dload_init_call( handle, init_sym );
	 if( r ) return r;
      }

      if( *init_mod ) {
	 int r = dload_init_call( handle, init_mod );
	 if( r ) return r;
      }
      
      return 0;
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
#if( !defined( _MINGW_VER ) )
#  include <windows.h>
#endif

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
