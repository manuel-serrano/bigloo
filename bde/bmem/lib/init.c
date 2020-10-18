/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/init.c           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:28:06 2003                          */
/*    Last change :  Fri Jan 10 17:08:01 2020 (serrano)                */
/*    Copyright   :  2003-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Allocation profiling initialization                              */
/*=====================================================================*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>

#include <bigloo.h>
#include <bmem.h>

#include <esymbol.h>

#include <dlfcn.h>
#ifndef RTLD_LAZY
#   define RTLD_LAZY 0
#endif

extern void alloc_dump_statistics( FILE *f );
extern void alloc_dump_statistics_json( FILE *f );
extern void alloc_reset_statistics();
extern void declare_type( int tnum, char *tname );
extern void GC_dump_statistics( FILE *f );
extern void GC_dump_statistics_json( FILE *f );
extern void GC_reset_statistics();
extern void thread_dump_statistics( FILE *f );
extern void thread_reset_statistics();
extern void type_dump( FILE *f );

extern long long GC_alloc_total();

static void bmem_init();

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
int bmem_debug = 0;
int bmem_thread = 0;
int bmem_verbose = 2;

pthread_key_t bmem_key;
pthread_key_t bmem_key2;
pthread_key_t bmem_key3;
pthread_mutex_t bmem_mutex;

/* garbage collector */
void *(*____GC_malloc)( size_t ) = 0;
void *(*____GC_realloc)( void *, size_t ) = 0;
void *(*____GC_malloc_atomic)( size_t ) = 0;
void *(*____GC_malloc_uncollectable)( size_t ) = 0;
void (*____GC_gcollect)() = 0;
void *(*____GC_add_gc_hook)( void (*)() ) = 0;
char **____executable_name = 0;
void *____command_line = 0;
void (*____GC_reset_allocated_bytes)() = 0;
BGL_LONGLONG_T (*____bgl_current_nanoseconds)() = 0;

/* inline allocations */
void *(*____make_pair)( void *, void * ) = 0;
void *(*____make_cell)( void * ) = 0;
#if( !BGL_NAN_TAGGING )
void *(*____make_real)( double ) = 0;
#endif
void *(*____make_belong)( long ) = 0;
void *(*____make_bllong)( BGL_LONGLONG_T ) = 0;
#if( defined( BGL_INT32_SIZE ) )
void *(*____bgl_make_bint32)( int32_t ) = 0;
#endif
#if( defined( BGL_UINT32_SIZE ) )
void *(*____bgl_make_buint32)( uint32_t ) = 0;
#endif
void *(*____bgl_make_bint64)( int64_t ) = 0;
void *(*____bgl_make_buint64)( uint64_t ) = 0;

/* string */
void *(*____string_to_bstring)( char * ) = 0;
void *(*____string_to_bstring_len)( char *, int ) = 0;
void *(*____make_string)( int, char ) = 0;
void *(*____make_string_sans_fill)( long ) = 0;
void *(*____string_append)( void *, void * ) = 0;
void *(*____string_append_3)( void *, void *, void * ) = 0;
void *(*____c_substring)( void *, int, int ) = 0;
void *(*____bgl_escape_C_string)( unsigned char *, long, long ) = 0;
void *(*____bgl_escape_scheme_string)( unsigned char *, long, long ) = 0;
void *(*____create_string_for_read)( void *, int ) = 0;
void *(*____bgl_make_keyword)( void * ) = 0;

/* vector */
void *(*____create_vector)( int ) = 0;
void *(*____create_vector_uncollectable)( int ) = 0;
void *(*____make_vector)( int, void * ) = 0;
void *(*____make_vector_uncollectable)( int, void * ) = 0;

/* procedure */
void *(*____make_fx_procedure)( void *(*)(), int, int );
void *(*____make_va_procedure)( void *(*)(), int, int );

/* output port */
void *(*____bgl_make_output_port)( void *, bgl_stream_t, int, void *, void *, ssize_t (*)(), long (*)(), int (*)() );
void *(*____bgl_output_port_timeout_set)( void *, long );

/* input port */
void *(*____bgl_make_input_port)( void *, FILE *, void *, void * );
void *(*____bgl_open_input_file)( void *, void * );
//void *(*____bgl_file_to_buffered_input_port)( void *, FILE *, void * );
void *(*____bgl_file_to_input_port)( FILE * );
void *(*____bgl_open_input_pipe)( void *, void * );
void *(*____bgl_open_input_resource)( void *, void * );
void *(*____bgl_open_input_string)( void *, long );
void *(*____bgl_open_input_substring)( void *, long, long );
void *(*____bgl_open_input_substring_bang)( void *, long, long );
void *(*____bgl_open_input_c_string)( char * );
void *(*____bgl_reopen_input_c_string)( void *, char * );
void *(*____bgl_input_port_timeout_set)( void *, long );
void *bgl_make_input_port_symbol;

/* thread */
void *(*____bglthread_new)( void * );
void *(*____bglthread_new_with_name)( void *, void * );
void *(*____scheduler_start)( void * );
void *(*_____scheduler_start)( void *, void * );
void *(*____scheduler_react)( void * );
void *(*_____scheduler_react)( void *, void * );
void (*____bglthread_switch)( void *, void * );
void (*____bglasync_scheduler_notify)( void * );

void *(*____pthread_getspecific)( pthread_key_t );
int (*____pthread_setspecific)( pthread_key_t, void * );
int (*____pthread_key_create)( pthread_key_t *, void (*)( void *) );
int (*____pthread_mutex_init)( pthread_mutex_t *, void * );

void * (*____bgl_make_mutex)( void * );
void * (*____bgl_make_nil_mutex)( void * );
void * (*____bgl_make_spinlock)( void * );
void * (*____bgl_make_condvar)( void * );
void * (*____bgl_make_nil_condvar)( void * );

/* regexp */
void *(*____bgl_make_regexp)( void * );

/* dynamic environment */
void *(*____make_dynamic_env)();
void (*____bgl_init_dynamic_env)();
void *(*____bgl_dup_dynamic_env)( void * );

/* struct */
void *(*____create_struct)( void *, int );
void *(*____make_struct)( void *, int, void * );

/* socket */
void *(*____bgl_make_client_socket)( void *, int, int, void *, void * );
void *(*____bgl_make_server_socket)( void *, int, int, bool_t );
void *(*____bgl_socket_accept)( void *, int, void *, void * );
long (*____bgl_socket_accept_many)( void *, int, void *, void *, void * );
void *(*____bgl_host)( void * );
void *bgl_socket_accept_symbol, *bgl_socket_accept_many_symbol;

/* date */
void *(*____bgl_seconds_to_date)( long );
void *(*____bgl_nanoseconds_to_date)( long );
void *(*____bgl_make_date)( BGL_LONGLONG_T, int, int, int, int, int, int, long, bool_t, int );
void *(*____bgl_seconds_format)( long, void * );

/* bignum */
obj_t (*____bgl_string_to_bignum)( char *, int );
obj_t (*____bgl_long_to_bignum)( long );
obj_t (*____bgl_llong_to_bignum)( long long );
obj_t (*____bgl_uint64_to_bignum)( uint64_t );
obj_t (*____bgl_flonum_to_bignum)( double );
       
/* classes */
void *(*____register_class )( void *, void *, void *, long, void *, void *, void *, void *, void *, void *, void * );
int (*____bgl_types_number)();
long (*____get_hash_power_number)( char *, unsigned long );
long (*____get_hash_power_number_len)( char *, unsigned long, long );
void *(*____bgl_get_symtab)() = 0;
void (*____bgl_init_objects)() = 0;
void (*____bglpth_setup)() = 0;
void *unknown_ident;

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    unbound ...                                                      */
/*---------------------------------------------------------------------*/
void *
unbound() {
   fprintf( stderr, "unbound function\n" );
   exit( -2 );
}

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    open_shared_library ...                                          */
/*---------------------------------------------------------------------*/
void *open_shared_library( char *lib ) {
   void *handle;
   
   if( !(handle = dlopen( lib, RTLD_LAZY ) ) ) {
      FAIL( IDENT, "Can't open library", lib );
      exit( -1 );
   }
   
   return handle;
}

/*---------------------------------------------------------------------*/
/*    void *(*)()                                                      */
/*    get_function ...                                                 */
/*---------------------------------------------------------------------*/
fun_t
get_function( void *handle, char *id ) {
   char *err;
   fun_t fun = dlsym( handle, id );

   if( bmem_verbose >= 2 ) {
      fprintf( stderr, "  %s...", id );
   }
   if( !fun || (err = dlerror()) ) {
      FAIL( IDENT, "Can't find function", id );
      exit( -2 );
   } else {
      if( bmem_verbose >= 2 ) {
	 fprintf( stderr, "ok\n" );
      }
      return fun;
   }
}

/*---------------------------------------------------------------------*/
/*    void *(*)()                                                      */
/*    find_function ...                                                */
/*---------------------------------------------------------------------*/
fun_t
find_function( void *handle, char *id ) {
   char *err;
   fun_t fun = dlsym( handle, id );

   if( bmem_verbose >= 2 ) {
      fprintf( stderr, "  %s...", id );
   }
   if( !fun || (err = dlerror()) ) {
      if( bmem_verbose >= 2 ) {
	 fprintf( stderr, "no\n" );
      }
      return (fun_t)&unbound;
   } else {
      if( bmem_verbose >= 2 ) {
	 fprintf( stderr, "ok\n" );
      }
      return fun;
   }
}

/*---------------------------------------------------------------------*/
/*    void *                                                           */
/*    get_variable ...                                                 */
/*---------------------------------------------------------------------*/
void *
get_variable( void *handle, char *id ) {
   char *err;
   fun_t fun = dlsym( handle, id );

   if( bmem_verbose >= 2 ) {
      fprintf( stderr, "  %s...", id );
   }
   if( !fun || (err = dlerror()) ) {
      FAIL( IDENT, "Can't find variable", id );
      exit( -2 );
   } else {
      if( bmem_verbose >= 2 ) {
	 fprintf( stderr, "ok\n" );
      }
      return fun;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    dump_statistics ...                                              */
/*---------------------------------------------------------------------*/
static void
dump_statistics() {
   char *n = getenv( "BMEMMON" );
   char *fmt = getenv( "BMEMFORMAT" );
   char *e = 0L;
   FILE *f;
   int bmemdumpfmt = BMEMDUMPFORMAT_SEXP;

   if( !n ) {
      if( ____executable_name && *____executable_name ) {
	 char *s1 = strrchr( *____executable_name, '/' );
	 char *s2 = strrchr( s1 ? s1 + 1: *____executable_name,  '.' );
	 char *s = s1 ? s1 + 1: *____executable_name;
	 int l = strlen( s );
	 char *r = malloc( l + 6 );

	 e = *____executable_name;
	 
	 if( s2 ) {
	    strcpy( r, s );
	    strcpy( &r[ s2 - s ], ".bmem" );
	 } else {
	    sprintf( r, "%s.bmem", s );
	 }

	 n = r;
      } else {
	 n = "a.bmem";
	 e = "???";
      }
   }

   if( fmt && !strcmp( fmt, "json" ) ) {
      bmemdumpfmt = BMEMDUMPFORMAT_JSON;
      n = "a.json";
   }
      
   if( bmem_verbose >= 1 ) {
      fprintf( stderr, "\nDumping file \"%s\"...", n );
      fflush( stderr );
   }
   
   if( !(f = fopen( n, "w" )) ) {
      FAIL( IDENT, "Can't open output file", n );
   }

   if( bmemdumpfmt == BMEMDUMPFORMAT_JSON ) {
      // json dump
      fprintf( f, "{\"monitor\":\n  { \"info\": { \"exec\": \"%s\", \"version\": \"%s\", \"sizeWord\": %d },\n", e, VERSION, BMEMSIZEOFWORD );
      GC_dump_statistics_json( f );
      fprintf( f, "   ,\n" );
      alloc_dump_statistics_json( f );
      fprintf( f, "}}\n" );
   } else {
      // text dump
      fprintf( f, ";; sizes are expressed in word (e.g., 4 bytes)\n" );
      fprintf( f, "(monitor\n" );
      fprintf( f, "  (info (exec \"%s\") (version \"%s\") (sizeof-word %d))\n",
	       e, VERSION, BMEMSIZEOFWORD );
      GC_dump_statistics( f );
      alloc_dump_statistics( f );
      type_dump( f );
      thread_dump_statistics( f );
      fprintf( f, ")\n" );
   }
   if( bmem_verbose >= 1 ) {
      fprintf( stderr, " done\n\n" );
   }
   
   fprintf( stderr, "Total size: %lldMB (%lldKB)\n",
	    GC_alloc_total() / 1024 / 1024, GC_alloc_total() / 1024 );
   
   if( bmem_verbose >= 1 ) {
      fprintf( stderr, "\n(export \"BMEMVERBOSE=0\" to disable bmem messages)\n" );
      fprintf( stderr, "(export \"BMEMFORMAT=json\" to generate json format)\n" );
      fflush( stderr );
      fflush( stdout );
   }
   fclose( f );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_bmem_reset ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_bmem_reset() {
   GC_reset_statistics();
   alloc_reset_statistics();
   thread_reset_statistics();

   fprintf( stderr, "bmem reset\n" );
   
   return BTRUE;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bigloo_abort ...                                                 */
/*---------------------------------------------------------------------*/
int
bigloo_abort( long n ) {
   return n;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bmem_dump ...                                                    */
/*---------------------------------------------------------------------*/
static void
bmem_dump( int _ ) {
   static int indump = 0;

   if( !indump ) {
      indump = 1;
      
      ____GC_gcollect();
      dump_statistics();
      indump = 0;
   }
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bglpth_setup_bmem ...                                            */
/*---------------------------------------------------------------------*/
void
bglpth_setup_bmem() {
   void *hdl;
   char bigloothread_lib[ 1000 ];
   static void (*____bglthread_setup_bmem)();
   
   bmem_thread = 2;

   /* Hello world */
   if( getenv( "BMEMVERBOSE" ) ) {
      bmem_verbose = atoi( getenv( "BMEMVERBOSE" ) );
   }
   
   if( getenv( "BMEMLIBBIGLOOTHREAD" ) ) {
      strcpy( bigloothread_lib, getenv( "BMEMLIBBIGLOOTHREAD" ) );
   } else {
      sprintf( bigloothread_lib, "%s/libbigloopthread_s_mt-%s.%s",
	       LIBRARY_DIRECTORY, BGL_RELEASE_NUMBER,
	       SHARED_LIB_SUFFIX );
   }

   if( bmem_verbose >= 2 ) {
      fprintf( stderr, "Loading thread library %s...\n", bigloothread_lib );
   }

   hdl = open_shared_library( bigloothread_lib );

   ____bglthread_setup_bmem = (void (*)())get_function( hdl, "bglpth_setup_bmem" );
   ____bglthread_new = (void *(*)( void * ))get_function( hdl, "bglpth_thread_new" );
   ____pthread_getspecific = get_function( hdl, "bglpth_pthread_getspecific" );
   ____pthread_setspecific = (int (*)())get_function( hdl, "bglpth_pthread_setspecific" );
   ____pthread_key_create = (int (*)())get_function( hdl, "bglpth_pthread_key_create" );
   ____pthread_mutex_init = (int (*)())get_function( hdl, "bglpth_pthread_mutex_init" );

   if( ____pthread_key_create( &bmem_key, 0L ) ) {
      FAIL( IDENT, "Can't get thread key", "bmem_key" );
      exit( -2 );
   }

   if( ____pthread_key_create( &bmem_key2, 0L ) ) {
      FAIL( IDENT, "Can't get thread key", "bmem_key2" );
      exit( -2 );
   }

   if( ____pthread_key_create( &bmem_key3, 0L ) ) {
      FAIL( IDENT, "Can't get thread key", "bmem_key3" );
      exit( -2 );
   }

   if( ____pthread_mutex_init( &bmem_mutex, 0L ) ) {
      FAIL( IDENT, "Can't get thread key", "bmem_key" );
      exit( -2 );
   }

   ____bglthread_setup_bmem();


   bmem_init();
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bmem_init_inner ...                                              */
/*    -------------------------------------------------------------    */
/*    This is the standard initialization point for the Bigloo         */
/*    C runtime.                                                       */
/*---------------------------------------------------------------------*/
static void
bmem_init_inner() {
   void *hdl;
   char bigloo_lib[ 1000 ];
   char gc_lib[ 1000 ];
   char *bgllibsuffix, *bglgcsuffix = "_u";

   /* Hello world */
   if( getenv( "BMEMVERBOSE" ) ) {
      bmem_verbose = atoi( getenv( "BMEMVERBOSE" ) );
   }
   
   if( !getenv( "BMEMTHREAD" ) ) {
      if( bmem_verbose >= 1 ) {
	 fprintf( stderr, "Bmem initialization...\n" );
      }
   } else {
      if( bmem_verbose >= 1 ) {
	 fprintf( stderr, "Bmem mt initialization...\n" );
      }

      bglpth_setup_bmem();
   }
   
   if( getenv( "BMEMLIBBIGLOO" ) ) {
      strcpy( bigloo_lib, getenv( "BMEMLIBBIGLOO" ) );
   } else {
      if( getenv( "BMEMLIBSUFFIX" ) ) {
	 bgllibsuffix = getenv( "BMEMLIBSUFFIX" );
      } else {
	 bgllibsuffix = "_s";
      }
   
      sprintf( bigloo_lib, "%s/libbigloo%s-%s.%s",
	       LIBRARY_DIRECTORY, bgllibsuffix, BGL_RELEASE_NUMBER,
	       SHARED_LIB_SUFFIX );
   }

#if( BGL_GC_CUSTOM == 1 )
   if( getenv( "BMEMLIBBIGLOOGC" ) ) {
      strcpy( gc_lib, getenv( "BMEMLIBBIGLOOGC" ) );
   } else {
      if( getenv( "BMEMGCSUFFIX" ) ) {
	 bglgcsuffix = getenv( "BMEMGCSUFFIX" );
      } else {
	 bglgcsuffix = "";
      }
      sprintf( gc_lib, "%s/lib%s%s-%s.%s",
	       LIBRARY_DIRECTORY,
	       BGL_GC_LIBRARY,
	       bglgcsuffix,
	       BGL_RELEASE_NUMBER,
	       SHARED_LIB_SUFFIX );
   }
#else
   strcpy( gc_lib, BGL_GC_LIBRARY );
#endif
   if( getenv( "BMEMDEBUG" ) )
      bmem_debug = atoi( getenv( "BMEMDEBUG" ) );

   /* The GC library */
   if( bmem_verbose >= 1 ) {
      fprintf( stderr, "Loading library %s...\n", gc_lib );
   }
   hdl = open_shared_library( gc_lib );
   ____GC_malloc = get_function( hdl, "GC_malloc" );
   ____GC_realloc = get_function( hdl, "GC_realloc" );
   ____GC_malloc_atomic = get_function( hdl, "GC_malloc_atomic" );
   ____GC_malloc_uncollectable = get_function( hdl, "GC_malloc_uncollectable" );
   ____GC_add_gc_hook = get_function( hdl, "GC_add_gc_hook" );
   ____GC_gcollect = (void (*)())get_function( hdl, "GC_gcollect" );
   
   ____make_pair = get_function( hdl, "make_pair" );
   ____make_cell = get_function( hdl, "make_cell" );
#if( !BGL_NAN_TAGGING )
   ____make_real = get_function( hdl, "make_real" );
#endif
   ____make_belong = get_function( hdl, "make_belong" );
   ____make_bllong = get_function( hdl, "make_bllong" );
#if( defined( BGL_INT32_SIZE ) )
   ____bgl_make_bint32 = get_function( hdl, "bgl_make_bint32" );
#endif
#if( defined( BGL_UINT32_SIZE ) )
   ____bgl_make_buint32 = get_function( hdl, "bgl_make_buint32" );
#endif   
   ____bgl_make_bint64 = get_function( hdl, "bgl_make_bint64" );
   ____bgl_make_buint64 = get_function( hdl, "bgl_make_buint64" );
   ____GC_add_gc_hook( GC_collect_hook );
   ____GC_reset_allocated_bytes = (void (*)())get_function( hdl, "GC_reset_allocated_bytes" );

   /* The Bigloo library */
   if( bmem_verbose >= 1 ) {
      fprintf( stderr, "Loading library %s...\n", bigloo_lib );
   }
   hdl = open_shared_library( bigloo_lib );
   ____executable_name = get_variable( hdl, "executable_name" );
   ____command_line = get_variable( hdl, "command_line" );
   ____bgl_init_objects = (void (*)())get_function( hdl, "bgl_init_objects" );
   ____get_hash_power_number = (long (*)())get_function( hdl, "get_hash_power_number" );
   ____get_hash_power_number_len = (long (*)())get_function( hdl, "get_hash_power_number_len" );
   ____bgl_get_symtab = get_function( hdl, "bgl_get_symtab" );
   ____bgl_current_nanoseconds = (BGL_LONGLONG_T (*)())get_function( hdl, "bgl_current_nanoseconds" );
   /* string */
   ____string_to_bstring = get_function( hdl, "string_to_bstring" );
   ____string_to_bstring_len = get_function( hdl, "string_to_bstring_len" );
   ____make_string = (void *(*)(int, char))get_function( hdl, "make_string" );
   ____make_string_sans_fill = get_function( hdl, "make_string_sans_fill" );
   ____string_append = get_function( hdl, "string_append" );
   ____string_append_3 = get_function( hdl, "string_append_3" );
   ____c_substring = get_function( hdl, "c_substring" );
   ____bgl_escape_C_string = get_function( hdl, "bgl_escape_C_string" );
   ____bgl_escape_scheme_string = get_function( hdl, "bgl_escape_scheme_string" );
   ____create_string_for_read = get_function( hdl, "create_string_for_read" );
   ____bgl_make_keyword = get_function( hdl, "bgl_make_keyword" );
   /* vector */
   ____create_vector = get_function( hdl, "create_vector" );
   ____create_vector_uncollectable = get_function( hdl, "create_vector_uncollectable" );
   ____make_vector = get_function( hdl, "make_vector" );
   ____make_vector_uncollectable = get_function( hdl, "make_vector_uncollectable" );
   /* procedure */
   ____make_fx_procedure = get_function( hdl, "make_fx_procedure" );
   ____make_va_procedure = get_function( hdl, "make_va_procedure" );
   /* output port */
   ____bgl_make_output_port = (void *(*)( void *, bgl_stream_t, int, void *, void *, ssize_t (*)(), long (*)(), int (*)() ))get_function( hdl, "bgl_make_output_port" );
   ____bgl_output_port_timeout_set = (void *(*)( void *, long ))get_function( hdl, "bgl_output_port_timeout_set" );

   /* input port */
   ____bgl_make_input_port = (void *(*)( void *, FILE *, void *, void * ))get_function( hdl, "bgl_make_input_port" );
   ____bgl_open_input_file = (void *(*)( void *, void * ))get_function( hdl, "bgl_open_input_file" );
   ____bgl_open_input_pipe = (void *(*)( void *, void * ))get_function( hdl, "bgl_open_input_pipe" );
   ____bgl_open_input_resource = (void *(*)( void *, void * ))get_function( hdl, "bgl_open_input_pipe" );
   ____bgl_open_input_string = (void *(*)( void *, long ))get_function( hdl, "bgl_open_input_string" );
   ____bgl_open_input_substring = (void *(*)( void *, long, long ))get_function( hdl, "bgl_open_input_substring" );
   ____bgl_open_input_c_string = (void *(*)( char * ))get_function( hdl, "bgl_open_input_c_string" );
   ____bgl_reopen_input_c_string = (void *(*)( void *, char * ))get_function( hdl, "bgl_reopen_input_c_string" );
   ____bgl_input_port_timeout_set = (void *(*)( void *, long ))get_function( hdl, "bgl_input_port_timeout_set" );

   /* struct */
   ____create_struct = (void *(*)( void *, int ))get_function( hdl, "create_struct" );
   ____make_struct = (void *(*)( void *, int, void * ))get_function( hdl, "make_struct" );
   
   /* socket */
   ____bgl_make_client_socket = (void *(*)( void *, int, int, void *, void * ))get_function( hdl, "bgl_make_client_socket" );
   ____bgl_make_server_socket = (void *(*)( void *, int, int, bool_t ))get_function( hdl, "bgl_make_server_socket" );
   ____bgl_socket_accept = (void *(*)( void *, int, void *, void * ))get_function( hdl, "bgl_socket_accept" );
   ____bgl_socket_accept_many = (long (*)( void *, int, void *, void *, void * ))get_function( hdl, "bgl_socket_accept_many" );
   ____bgl_host = (void *(*)( void * ))get_function( hdl, "bgl_host" );
   
   /* date */
   ____bgl_seconds_to_date = (void *(*)( long ))get_function( hdl, "bgl_seconds_to_date" );
   ____bgl_nanoseconds_to_date = (void *(*)( long ))get_function( hdl, "bgl_nanoseconds_to_date" );
   ____bgl_make_date = (void *(*)( BGL_LONGLONG_T, int, int, int, int, int, int, long, bool_t, int ))get_function( hdl, "bgl_make_date" );
   ____bgl_seconds_format = (void *(*)( long, void * ))get_function( hdl, "bgl_seconds_format" );

   /* bignum */
   ____bgl_string_to_bignum = (obj_t (*)( char *, int ))get_function( hdl, "bgl_string_to_bignum" );
   ____bgl_long_to_bignum = (obj_t (*)( long ))get_function( hdl, "bgl_long_to_bignum" );
   ____bgl_llong_to_bignum = (obj_t (*)( long long ))get_function( hdl, "bgl_llong_to_bignum" );
   ____bgl_uint64_to_bignum = (obj_t (*)( uint64_t ))get_function( hdl, "bgl_uint64_to_bignum" );
   ____bgl_flonum_to_bignum = (obj_t (*)( double ))get_function( hdl, "bgl_flonum_to_bignum" );
   
   /* class */
   ____register_class = get_function( hdl, "BGl_registerzd2classz12zc0zz__objectz00" );
   ____bgl_types_number = (int (*)())get_function( hdl, "bgl_types_number" );

   /* regexp */
   ____bgl_make_regexp = (void *(*)( void * ))get_function( hdl, "bgl_make_regexp" );
   
   /* dynamic environment */
   ____make_dynamic_env = (void *(*)())get_function( hdl, "make_dynamic_env" );
   ____bgl_init_dynamic_env = (void (*)())get_function( hdl, "bgl_init_dynamic_env" );
   ____bgl_dup_dynamic_env = (void *(*)( void *))get_function( hdl, "bgl_dup_dynamic_env" );

   /* thread (common) */
   ____bgl_make_mutex = (void *(*)( void * ))get_function( hdl, "bgl_make_mutex" );
   ____bgl_make_nil_mutex = (void *(*)( void * ))get_function( hdl, "bgl_make_nil_mutex" );
   ____bgl_make_spinlock = (void *(*)( void * ))get_function( hdl, "bgl_make_spinlock" );
   ____bgl_make_condvar = (void *(*)( void * ))get_function( hdl, "bgl_make_condvar" );
   ____bgl_make_nil_condvar = (void *(*)( void * ))get_function( hdl, "bgl_make_nil_condvar" );

   /* declare types */
   declare_type( UNKNOWN_TYPE_NUM, "byte" );
   declare_type( UNKNOWN_ATOMIC_TYPE_NUM, "atomic byte" );
   declare_type( UNKNOWN_UNCOLLECTABLE_TYPE_NUM, "uncollectable byte" );
   declare_type( UNKNOWN_REALLOC_TYPE_NUM, "realloc4 byte" );
   declare_type( _DYNAMIC_ENV_TYPE_NUM, "%dynamic-env" );
   declare_type( _THREAD_TYPE_NUM, "%native-thread" );
   declare_type( ROWSTRING_TYPE_NUM, "char *" );
   declare_type( LLONG_TYPE_NUM, "llong" );
   declare_type( ELONG_TYPE_NUM, "elong" );
   declare_type( PROCEDURE_LIGHT_TYPE_NUM, "procedure-light" );
   declare_type( TSTRUCT_TYPE_NUM, "tstruct" );
   declare_type( TVECTOR_TYPE_NUM, "tvector" );
   declare_type( EXTENDED_PAIR_TYPE_NUM, "epair" );
   declare_type( BINARY_PORT_TYPE_NUM, "binary-port" );
   declare_type( OUTPUT_STRING_PORT_TYPE_NUM, "output-string-port" );
   declare_type( FOREIGN_TYPE_NUM, "foreign" );
   declare_type( PROCESS_TYPE_NUM, "process" );
   declare_type( REAL_TYPE_NUM, "real" );
   declare_type( STRUCT_TYPE_NUM, "struct" );
   declare_type( SOCKET_TYPE_NUM, "socket" );
   declare_type( CELL_TYPE_NUM, "cell" );
   declare_type( DATE_TYPE_NUM, "date" );
   declare_type( OUTPUT_PORT_TYPE_NUM, "output-port" );
   declare_type( INPUT_PORT_TYPE_NUM, "input-port" );
   declare_type( STACK_TYPE_NUM, "stack" );
   declare_type( SYMBOL_TYPE_NUM, "symbol" );
   declare_type( KEYWORD_TYPE_NUM, "keyword" );
   declare_type( CUSTOM_TYPE_NUM, "custom" );
   declare_type( OPAQUE_TYPE_NUM, "opaque" );
   declare_type( UCS2_STRING_TYPE_NUM, "ucs2-string" );
   declare_type( PROCEDURE_TYPE_NUM, "procedure" );
   declare_type( VECTOR_TYPE_NUM, "vector" );
   declare_type( STRING_TYPE_NUM, "string" );
   declare_type( PAIR_TYPE_NUM, "pair" );
   declare_type( HOSTENT_TYPE_NUM, "hostent" );
   declare_type( PORT_TIMEOUT_TYPE_NUM, "port-timeout" );
   declare_type( CLASS_TYPE_NUM, "class" );
   declare_type( DATAGRAM_SOCKET_TYPE_NUM, "datagram-socket" );
   declare_type( REGEXP_TYPE_NUM, "regexp" );
   declare_type( INT32_TYPE_NUM, "int32" );
   declare_type( UINT32_TYPE_NUM, "uint32" );
   declare_type( INT64_TYPE_NUM, "int64" );
   declare_type( UINT64_TYPE_NUM, "uint64" );
   declare_type( MUTEX_TYPE_NUM, "mutex" );
   declare_type( SPINLOCK_TYPE_NUM, "spinlock" );
   declare_type( CONDVAR_TYPE_NUM, "condvar" );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bmem_init ...                                                    */
/*---------------------------------------------------------------------*/
static void
bmem_init() {
   static int initp = 0;

   if( !initp ) {
      initp = 1;
      bmem_init_inner();
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_objects ...                                             */
/*    -------------------------------------------------------------    */
/*    This is the standard initialization point for the Bigloo         */
/*    C runtime.                                                       */
/*---------------------------------------------------------------------*/
void
bgl_init_objects() {
   /* initialize the preloading */
   bmem_init();

   /* initialize the runtime system */
   ____bgl_init_objects();

   unknown_ident = string_to_symbol( "unknown_function" );
   mark_function( unknown_ident, 0, ante_bgl_init_dsz, 0, -1, -1, -1 );

   bgl_socket_accept_symbol = string_to_symbol( "$socket-accept" );
   ((esymbol_t *)(CSYMBOL(bgl_socket_accept_symbol)))->class_alloc = HOSTENT_TYPE_NUM;

   bgl_socket_accept_many_symbol = string_to_symbol( "$socket-accept-many" );
   ((esymbol_t *)(CSYMBOL(bgl_socket_accept_many_symbol)))->class_alloc = HOSTENT_TYPE_NUM;

   bgl_make_input_port_symbol = string_to_symbol( "$make-input-port" );
   ((esymbol_t *)(CSYMBOL(bgl_make_input_port_symbol)))->class_alloc = UNKNOWN_ATOMIC_TYPE_NUM;

   /* signal registration */
   signal( 2, bmem_dump );

   /* exit registration */
   atexit( (void (*)(void))bmem_dump );
}


