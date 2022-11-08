/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/bmem.h           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:29:17 2003                          */
/*    Last change :  Sat Mar  5 09:45:04 2022 (serrano)                */
/*    Copyright   :  2003-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The allocation profiler include                                  */
/*=====================================================================*/
#ifndef BMEM_H 
#define BMEM_H

#include <bigloo_config.h>
#include <stdio.h>
#include <sys/types.h>
#include <pthread.h>
#if BGL_HAVE_BACKTRACE
#  include <backtrace.h>
#endif

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

typedef struct alloc_info {
   long typenum;
   long size;
   char *function;
   char *filename;
   long lineno;
   int depth;
} alloc_info_t;

typedef struct line_alloc {
   long lineno;
   long size;
   long count;
   unsigned int typecount;
   long *typenums;
} line_alloc_t;

typedef struct file_alloc {
   char *filename;
   long size;
   line_alloc_t *lines;
} file_alloc_t;

/*---------------------------------------------------------------------*/
/*    LOADF ...                                                        */
/*---------------------------------------------------------------------*/
#define LOADF(hdl, ident) \
   ____##ident = get_function(hdl, #ident)

/*---------------------------------------------------------------------*/
/*    WRAP ...                                                         */
/*---------------------------------------------------------------------*/
#define WRAP(ident, tnum, proto, call) \
obj_t ident proto { \
   obj_t __res; \
   long otnum = bmem_get_alloc_type(); \
   bmem_set_alloc_type(tnum); \
   __res =  ____##ident call ; \
   bmem_set_alloc_type(otnum); \
   return __res; \
}

#define WRAPR(ident, identr, tnum, proto, call)	\
obj_t ident proto { \
   obj_t __res; \
   long otnum = bmem_get_alloc_type(); \
   bmem_set_alloc_type(tnum); \
   __res =  ____##identr call ; \
   bmem_set_alloc_type(otnum); \
   return __res; \
}

#define WRAP_DEBUG(ident, tnum, proto, call) \
obj_t ident proto { \
   obj_t __res; \
   long otnum = bmem_get_alloc_type(); \
   fprintf( stderr, ">>> wrap " #ident "\n..."); \
   bmem_set_alloc_type(tnum); \
   __res =  ____##ident call ; \
   bmem_set_alloc_type(otnum); \
   fprintf( stderr, "<<< wrap " #ident "\n..."); \
   return __res; \
}

#define MARKF(ident, tnum) \
   backtrace_alloc_name_put(ident, tnum)

/*---------------------------------------------------------------------*/
/*    shared libraries handling                                        */
/*---------------------------------------------------------------------*/
fun_t get_function(void *, char *);
extern void declare_type(int, char *, char *);

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
extern int bmem_debug;
extern int bmem_thread;
extern int bmem_color;
extern int bmem_backtrace;
extern int bmem_verbose;
extern void *unknown_ident;
extern void *bgl_socket_accept_symbol, *bgl_socket_accept_many_symbol;
extern void *bgl_make_input_port_symbol;
extern unsigned long gc_number;
extern unsigned long ante_bgl_init_dsz;

extern pthread_key_t bmem_key;
extern pthread_key_t bmem_key2;
extern pthread_key_t bmem_key3;
extern pthread_mutex_t bmem_mutex;

extern void mark_function( void *, long, long, long, int, int, long );

extern void *(*____pthread_getspecific)( pthread_key_t );
extern int (*____pthread_setspecific)( pthread_key_t, void * );
extern int (*____pthread_key_create)( pthread_key_t *, void (*)( void *) );
extern int (*____pthread_mutex_init)( pthread_mutex_t *, void * );

extern void (*____GC_reset_allocated_bytes)();
extern void *(*____GC_malloc)( size_t );
extern void *(*____GC_realloc)( void *, size_t );
extern void *(*____GC_malloc_atomic)( size_t );
extern void *(*____GC_malloc_uncollectable)( size_t );
extern void *(*____GC_add_gc_hook)( void (*)() );
extern BGL_LONGLONG_T (*____bgl_current_nanoseconds)();

extern void (*____bgl_init_objects)();

extern void (*____bgl_init_trace_register)();
extern obj_t (*____bgl_get_trace_stack)(int);

extern void *(*____register_class )( void *, void *, void *,
				     long,
				     void *, void *, void *,
				     void *, void *,
				     void *, void * );
extern int (*____bgl_types_number)();

/*---------------------------------------------------------------------*/
/*    alloc                                                            */
/*---------------------------------------------------------------------*/
extern void alloc_init(char **);
extern int alloc_is_native(const char*);
extern void bmem_set_alloc_type(long);
extern long bmem_get_alloc_type();

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

#if !BGL_HAVE_BACKTRACE
typedef int (*backtrace_full_callback)();
#endif

extern void backtrace_for_each(backtrace_full_callback, int, void *);
extern int backtrace_alloc_cb(void *, uintptr_t, const char *, int, const char *);
extern void backtrace_alloc_name_put(char *, int);

/*---------------------------------------------------------------------*/
/*    Hash                                                             */
/*---------------------------------------------------------------------*/
typedef struct hashbucketentry {
   char *key;
   void *data;
} hashbucketentry_t;

typedef struct hashtable {
   long size;
   hashbucketentry_t *buckets;
} hashtable_t;

extern hashtable_t *hashtable_create(long);
extern void *hashtable_get(hashtable_t *, const char *);
extern int hashtable_put(hashtable_t *, const char *, void *);
extern void hashtable_foreach(hashtable_t *, void (*)(const char *, void*));

/*---------------------------------------------------------------------*/
/*    Types                                                            */
/*---------------------------------------------------------------------*/
#define NO_TYPE_NUM                    0 
#define PAIR_TYPE_NUM                  1
#define STRING_TYPE_NUM                2
#define VECTOR_TYPE_NUM                3
#define PROCEDURE_TYPE_NUM             4
#define UCS2_STRING_TYPE_NUM           5
#define OPAQUE_TYPE_NUM                6
#define CUSTOM_TYPE_NUM                7
#define KEYWORD_TYPE_NUM               8
#define SYMBOL_TYPE_NUM                9
#define STACK_TYPE_NUM                 11
#define INPUT_PORT_TYPE_NUM            12
#define OUTPUT_PORT_TYPE_NUM           13
#define DATE_TYPE_NUM                  14
#define CELL_TYPE_NUM                  15
#define SOCKET_TYPE_NUM                16
#define STRUCT_TYPE_NUM                17
#define REAL_TYPE_NUM                  18
#define PROCESS_TYPE_NUM               19
#define FOREIGN_TYPE_NUM               20
#define OUTPUT_STRING_PORT_TYPE_NUM    21
#define BINARY_PORT_TYPE_NUM           22
#define EXTENDED_PAIR_TYPE_NUM         23
#define TVECTOR_TYPE_NUM               24
#define TSTRUCT_TYPE_NUM               25
#define PROCEDURE_LIGHT_TYPE_NUM       26
#define ELONG_TYPE_NUM                 27
#define LLONG_TYPE_NUM                 28
#define ROWSTRING_TYPE_NUM             29
#define _THREAD_TYPE_NUM               30
#define _DYNAMIC_ENV_TYPE_NUM          31
#define UNKNOWN_TYPE_NUM               32
#define UNKNOWN_ATOMIC_TYPE_NUM        33
#define UNKNOWN_REALLOC_TYPE_NUM       34
#define HOSTENT_TYPE_NUM               35
#define PORT_TIMEOUT_TYPE_NUM          36
#define DATAGRAM_SOCKET_TYPE_NUM       37
#define REGEXP_TYPE_NUM                38
#define INT32_TYPE_NUM                 39
#define UINT32_TYPE_NUM                40
#define INT64_TYPE_NUM                 41
#define UINT64_TYPE_NUM                42
#define UNKNOWN_UNCOLLECTABLE_TYPE_NUM 43
#define MUTEX_TYPE_NUM                 44
#define SPINLOCK_TYPE_NUM              45
#define CONDVAR_TYPE_NUM               46
#define BIGNUM_TYPE_NUM                47

#define IGNORE_TYPE_NUM                50

/* a fake type */
#define CLASS_TYPE_NUM                 99

#endif
