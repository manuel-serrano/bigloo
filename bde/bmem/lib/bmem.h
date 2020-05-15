/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/bmem.h           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:29:17 2003                          */
/*    Last change :  Fri Jan 10 08:45:51 2020 (serrano)                */
/*    Copyright   :  2003-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The allocation profiler include                                  */
/*=====================================================================*/
#include <bigloo_config.h>
#include <stdio.h>
#include <sys/types.h>
#include <pthread.h>

/*---------------------------------------------------------------------*/
/*    Identity                                                         */
/*---------------------------------------------------------------------*/
#define IDENT "bmem"
#define VERSION "0.0.3"

/*---------------------------------------------------------------------*/
/*    BMEMDUMPFORMAT                                                   */
/*---------------------------------------------------------------------*/
#define BMEMDUMPFORMAT_SEXP 0
#define BMEMDUMPFORMAT_JSON 1

/*---------------------------------------------------------------------*/
/*    FAIL                                                             */
/*---------------------------------------------------------------------*/
#define FAIL( proc, msg, obj ) \
   fprintf( stderr, "\n*** ERROR:%s\n%s -- %s\n", proc, msg, obj ), exit( -1 )

/*---------------------------------------------------------------------*/
/*    MEMSIZE                                                          */
/*---------------------------------------------------------------------*/
/* #define BMEMSIZEOFWORD (4)                                          */
/* #define BMEMSIZE( v )  ((v)/BMEMSIZEOFWORD)                         */

#define BMEMSIZEOFWORD (1)
#define BMEMSIZE( v ) v

/*---------------------------------------------------------------------*/
/*    Various types                                                    */
/*---------------------------------------------------------------------*/
typedef void *(*fun_t)();

typedef struct gc_info {
   unsigned long number;
   unsigned long alloc_size;
   unsigned long heap_size;
   unsigned long live_size;
   void *lastfun;
   BGL_LONGLONG_T time;
} gc_info_t;

typedef struct fun_alloc_info {
   unsigned long gc_num;
   unsigned long dsize;
   unsigned long isize;
   void *dtypes;
   void *itypes;
} fun_alloc_info_t;

typedef struct type_alloc_info {
   unsigned long num;
   unsigned long size;
} type_alloc_info_t;
   
typedef struct fun_info {
   void *ident;
   fun_alloc_info_t *allocs;
} fun_info_t;

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
extern int bmem_debug;
extern int bmem_thread;
extern void *unknown_ident;
extern void *bgl_socket_accept_symbol, *bgl_socket_accept_many_symbol;
extern void *bgl_make_input_port_symbol;
extern unsigned long gc_number;
extern void bmem_set_alloc_type( long, long );
extern long bmem_get_alloc_index();
extern void bmem_set_alloc_index( long );
extern unsigned long ante_bgl_init_dsz;

extern pthread_key_t bmem_key;
extern pthread_key_t bmem_key2;
extern pthread_key_t bmem_key3;
extern pthread_mutex_t bmem_mutex;

extern void mark_function( void *, long, long, long, int, int, long );

extern void (*____GC_reset_allocated_bytes)();
extern void *(*____make_pair)( void *, void * );
extern void *(*____make_cell)( void * );
extern void *(*____make_real)( double );
extern void *(*____make_belong)( long );
extern void *(*____make_bllong)( BGL_LONGLONG_T );
extern void *(*____make_cell)( void * );
extern void *(*____GC_malloc)( size_t );
extern void *(*____GC_realloc)( void *, size_t );
extern void *(*____GC_malloc_atomic)( size_t );
extern void *(*____GC_malloc_uncollectable)( size_t );
extern void *(*____GC_add_gc_hook)( void (*)() );
extern BGL_LONGLONG_T (*____bgl_current_nanoseconds)();

extern void (*____bgl_init_objects)();

extern void *(*____string_to_bstring)( char * );
extern void *(*____string_to_bstring_len)( char *, int );
extern void *(*____make_string)( int, char );
extern void *(*____make_string_sans_fill)( long );
extern void *(*____string_append)( void *, void * );
extern void *(*____string_append_3)( void *, void *, void * );
extern void *(*____c_substring)( void *, int, int );
extern void *(*____bgl_escape_C_string)( unsigned char *, long, long );
extern void *(*____bgl_escape_scheme_string)( unsigned char *, long, long );
extern void *(*____create_string_for_read)( void *, int );
extern void *(*____bgl_make_keyword)( void * );
extern void *(*____string_to_llong)( char * );
extern void *(*____string_to_elong)( char * );

extern void *(*____create_vector)( int );
extern void *(*____create_vector_uncollectable)( int );
extern void *(*____make_vector)( int, void * );
extern void *(*____make_vector_uncollectable)( int, void * );

extern void *(*____make_fx_procedure)( void *(*)(), int, int );
extern void *(*____make_va_procedure)( void *(*)(), int, int );

extern void *(*____bgl_make_output_port)( void *, bgl_stream_t, int, void *, void *, ssize_t (*)(), long (*)(), int (*)() );
                                        
extern void *(*____bgl_output_port_timeout_set)( void *, long );

extern void *(*____bgl_make_input_port)( void *, FILE *, void *, void * );
extern void *(*____bgl_open_input_file)( void *, void * );
extern void *(*____bgl_file_input_port)( FILE * );
extern void *(*____bgl_open_input_pipe)( void *, void * );
extern void *(*____bgl_open_input_resource)( void *, void * );
extern void *(*____bgl_open_input_string)( void *, long );
extern void *(*____bgl_open_input_substring)( void *, long, long );
extern void *(*____bgl_open_input_c_string)( char * );
extern void *(*____bgl_reopen_input_c_string)( void *, char * );
extern void *(*____bgl_input_port_timeout_set)( void *, long );

extern void *(*____bgl_make_regexp)( void * );

extern void *(*____bgl_dynamic_env)();
extern void *(*____make_dynamic_env)();
extern void (*____bgl_init_dynamic_env)();
extern void *(*____bgl_dup_dynamic_env)( void * );

extern void *(*____bglthread_new )( void * );
extern void *(*____bglthread_new_with_name )( void *, void * );
	      
extern void *(*____create_struct )( void *, int );
extern void *(*____make_struct )( void *, int, void * );

extern void *(*____bgl_make_client_socket )( void *, int, int, void *, void * );
extern void *(*____bgl_make_server_socket )( void *, int, int, bool_t );
extern void *(*____bgl_socket_accept )( void *, int, void *, void * );
extern long (*____bgl_socket_accept_many )( void *, int, void *, void *, void * );
extern void *(*____bgl_host )( void * );

extern void *(*____bgl_seconds_to_date )( long );
extern void *(*____bgl_nanoseconds_to_date )( long );
extern void *(*____bgl_make_date )( BGL_LONGLONG_T, int, int, int, int, int, int, long, bool_t, int );
extern void *(*____bgl_seconds_format )( long, void * );

extern obj_t(*____bgl_string_to_bignum)( char *s, int r );
extern obj_t(*____bgl_long_to_bignum)( long );
extern obj_t(*____bgl_llong_to_bignum)( long long );
extern obj_t(*____bgl_uint64_to_bignum)( uint64_t );
extern obj_t(*____bgl_flonum_to_bignum)( double );

extern void *(*____scheduler_start)( void * );
extern void *(*____scheduler_react)( void * );
extern void (*____bglthread_switch)( void *, void * );
extern void (*____bglasync_scheduler_notify)( void * );

extern void *(*____register_class )( void *, void *, void *,
				     long,
				     void *, void *, void *,
				     void *, void *,
				     void *, void * );
extern int (*____bgl_types_number)();

extern void *(*____pthread_getspecific)( pthread_key_t );
extern int (*____pthread_setspecific)( pthread_key_t, void * );
extern int (*____pthread_key_create)( pthread_key_t *, void (*)( void *) );
extern int (*____pthread_mutex_init)( pthread_mutex_t *, void * );

extern void *(*____bgl_make_mutex)( void * );
extern void *(*____bgl_make_nil_mutex)( void * );
extern void *(*____bgl_make_spinlock)( void * );
extern void *(*____bgl_make_condvar)( void * );
extern void *(*____bgl_make_nil_condvar)( void * );

extern long (*____get_hash_power_number)( char *, unsigned long );
extern long (*____get_hash_power_number_len)( char *, unsigned long, long );
extern void *(*____bgl_get_symtab)();

/*---------------------------------------------------------------------*/
/*    Functions                                                        */
/*---------------------------------------------------------------------*/
extern void GC_collect_hook( int, long );

/*---------------------------------------------------------------------*/
/*    Lists                                                            */
/*---------------------------------------------------------------------*/
typedef struct pa_pair {
   void *car;
   void *cdr;
} pa_pair_t;

#define PA_PAIRP( l ) (l)
#define PA_CAR( l ) ((l)->car)
#define PA_CDR( l ) ((l)->cdr)

extern void for_each( void (*)(void *, void *), pa_pair_t *, void * );
extern void for_each_json( void (*)(void *, void *), pa_pair_t *, void * );
extern pa_pair_t *pa_cons( void *, pa_pair_t * );
extern pa_pair_t *pa_reverse( pa_pair_t * );
extern pa_pair_t *pa_assq( void *, pa_pair_t * );

/*---------------------------------------------------------------------*/
/*    Trace                                                            */
/*---------------------------------------------------------------------*/
extern void *bgl_debug_trace_top( int );
extern char *bgl_debug_trace_top_name( int );
extern char *bgl_debug_trace_symbol_name( void * );
extern char *bgl_debug_trace_symbol_name_json( void * );
extern void for_each_trace( void (*)(void *, void *), int, int, void * );

/*---------------------------------------------------------------------*/
/*    Types                                                            */
/*---------------------------------------------------------------------*/
#define PAIR_TYPE_NUM                  0
#define STRING_TYPE_NUM                1
#define VECTOR_TYPE_NUM                2
#define PROCEDURE_TYPE_NUM             3
#define UCS2_STRING_TYPE_NUM           4
#define OPAQUE_TYPE_NUM                5
#define CUSTOM_TYPE_NUM                6
#define KEYWORD_TYPE_NUM               7
#define SYMBOL_TYPE_NUM                8
#define STACK_TYPE_NUM                 9
#define INPUT_PORT_TYPE_NUM            10
#define OUTPUT_PORT_TYPE_NUM           11
#define DATE_TYPE_NUM                  12
#define CELL_TYPE_NUM                  13
#define SOCKET_TYPE_NUM                14
#define STRUCT_TYPE_NUM                15
#define REAL_TYPE_NUM                  16
#define PROCESS_TYPE_NUM               17
#define FOREIGN_TYPE_NUM               18
#define OUTPUT_STRING_PORT_TYPE_NUM    19
#define BINARY_PORT_TYPE_NUM           20
#define EXTENDED_PAIR_TYPE_NUM         21
#define TVECTOR_TYPE_NUM               22
#define TSTRUCT_TYPE_NUM               23
#define PROCEDURE_LIGHT_TYPE_NUM       24
#define ELONG_TYPE_NUM                 25
#define LLONG_TYPE_NUM                 26
#define ROWSTRING_TYPE_NUM             27
#define _THREAD_TYPE_NUM               28
#define _DYNAMIC_ENV_TYPE_NUM          29
#define UNKNOWN_TYPE_NUM               30
#define UNKNOWN_ATOMIC_TYPE_NUM        31
#define UNKNOWN_REALLOC_TYPE_NUM       32
#define HOSTENT_TYPE_NUM               33
#define PORT_TIMEOUT_TYPE_NUM          34
#define DATAGRAM_SOCKET_TYPE_NUM       44
#define REGEXP_TYPE_NUM                45
#define INT32_TYPE_NUM                 48
#define UINT32_TYPE_NUM                49
#define INT64_TYPE_NUM                 50
#define UINT64_TYPE_NUM                51
#define UNKNOWN_UNCOLLECTABLE_TYPE_NUM 52
#define MUTEX_TYPE_NUM                 53
#define SPINLOCK_TYPE_NUM              54
#define CONDVAR_TYPE_NUM               55
#define BIGNUM_TYPE_NUM                56

/* a fake type */
#define CLASS_TYPE_NUM                 99
