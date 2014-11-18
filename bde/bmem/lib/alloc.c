/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bde/bmem/lib/alloc.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:42:57 2003                          */
/*    Last change :  Tue Nov 18 11:55:52 2014 (serrano)                */
/*    Copyright   :  2003-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Allocation replacement routines                                  */
/*=====================================================================*/
#define BGL_GC NO_GC
#include <bigloo.h>
#include <bmem.h>
#include <esymbol.h>
#include <stdlib.h>
#include <string.h>

extern void gc_alloc_size_add( int size );

/*---------------------------------------------------------------------*/
/*    static pa_pair_t *                                               */
/*    all_functions ...                                                */
/*---------------------------------------------------------------------*/
static pa_pair_t *all_functions = 0;
static int stamp = 1;
static int alloc_type = -1;
static int alloc_type_offset = 0;
static long max_stack_size = 100000;
unsigned long ante_bgl_init_dsz = 0;

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    all_types ...                                                    */
/*---------------------------------------------------------------------*/
static char **all_types = 0;
static int types_number = 0;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    set_alloc_type ...                                               */
/*---------------------------------------------------------------------*/
void
set_alloc_type( int t, int o ) {
   if( bmem_thread ) {
      ____pthread_setspecific( bmem_key, (void *)t );
      ____pthread_setspecific( bmem_key2, (void *)o );
   } else {
      alloc_type = t;
      alloc_type_offset = o;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    get_alloc_type ...                                               */
/*---------------------------------------------------------------------*/
static int
get_alloc_type() {
   if( bmem_thread ) {
      return (int)____pthread_getspecific( bmem_key );
   } else {
      return alloc_type;
   }
}
    
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    get_alloc_type_offset ...                                        */
/*---------------------------------------------------------------------*/
static int
get_alloc_type_offset() {
   if( bmem_thread ) {
      return (int)____pthread_getspecific( bmem_key2 );
   } else {
      return alloc_type_offset;
   }
}
    
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    type_dump ...                                                    */
/*---------------------------------------------------------------------*/
void
type_dump( FILE *f ) {
   int i;
   
   fprintf( f, "  (type" );
   for( i = 0; i < types_number; i++ ) {
      if( all_types[ i ] )
         fprintf( f, "\n    (%d \"%s\")", i, all_types[ i ] );
   }
   fprintf( f, ")\n" );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    declare_type ...                                                 */
/*---------------------------------------------------------------------*/
void
declare_type( int tnum, char *tname ) {
   if( (tnum + 1) > types_number ) {
      all_types = (char **)realloc( all_types, (tnum + 1) * sizeof(char *)  );
      memset( &all_types[ types_number ],
              0,
              (tnum-types_number) * sizeof(char *) );
      types_number = tnum + 1;
   }

   all_types[ tnum ] = tname;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    alloc_dump_type ...                                              */
/*---------------------------------------------------------------------*/
void
alloc_dump_type( pa_pair_t *i, FILE *f ) {
   type_alloc_info_t *tai = (type_alloc_info_t *)PA_CDR( i );
   
   fprintf( f, "\n          (%ld #l%ld #l%ld)", (long)PA_CAR( i ),
	    tai->num, BMEMSIZE( tai->size ) );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    alloc_dump ...                                                   */
/*---------------------------------------------------------------------*/
void
alloc_dump( fun_alloc_info_t *i, FILE *f ) {
   fprintf( f, "      (%lu #l%lu #l%lu\n", i->gc_num,
	    BMEMSIZE( i->dsize ), BMEMSIZE( i->isize ) );
   fprintf( f, "        (dtype" );
   for_each( (void (*)(void *, void *))alloc_dump_type, i->dtypes, f );
   fprintf( f, ")\n" );
   fprintf( f, "        (itype" );
   for_each( (void (*)(void *, void *))alloc_dump_type, i->itypes, f );
   fprintf( f, "))\n" );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    fun_dump ...                                                     */
/*---------------------------------------------------------------------*/
void
fun_dump( void *ident, FILE *f ) {
   esymbol_t *fun = (esymbol_t *)ident;

   fprintf( f, "\n    (|%s|\n", ESYMBOL_TO_STRING( (obj_t)fun ) );
   for_each( (void (*)(void *, void *))alloc_dump, CESYMBOL( fun )->alloc_info, f );
   fprintf( f, "      )" );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    alloc_dump_statistics ...                                        */
/*---------------------------------------------------------------------*/
void
alloc_dump_statistics( FILE *f ) {
   fprintf( f, "  (function" );
   for_each( (void (*)(void *, void *))fun_dump, all_functions, (void *)f );
   fprintf( f, ")\n" );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    alloc_reset_statistics ...                                       */
/*---------------------------------------------------------------------*/
void
alloc_reset_statistics() {
   pa_pair_t *aux = all_functions;
   int i;

   while( PA_PAIRP( aux ) ) {
      void *id = PA_CAR( aux );
      esymbol_t *fun = (esymbol_t *)CSYMBOL( id );

      fun->alloc_info = 0L;
      
      aux = PA_CDR( aux );
   }

   all_functions = 0;
   stamp = 1;
   alloc_type = -1;
}

/*---------------------------------------------------------------------*/
/*    fun_alloc_info_t *                                               */
/*    make_fun_alloc_info ...                                          */
/*---------------------------------------------------------------------*/
fun_alloc_info_t *
make_fun_alloc_info( long gc, long dsz, long isz ) {
   fun_alloc_info_t *i = malloc( sizeof( struct fun_alloc_info ) );

   if( !i ) FAIL( IDENT, "Can't alloc fun_alloc_info", "0" );
   
   i->gc_num = gc;
   
   i->dsize = dsz;
   i->isize = isz;
   
   i->dtypes = 0;
   i->itypes = 0;

   return i;
}
  
/*---------------------------------------------------------------------*/
/*    type_alloc_info_t *                                              */
/*    make_type_alloc_info ...                                         */
/*---------------------------------------------------------------------*/
type_alloc_info_t *
make_type_alloc_info() {
   type_alloc_info_t *new = calloc( sizeof( struct type_alloc_info ), 1 );
   return new;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    mark_type ...                                                    */
/*---------------------------------------------------------------------*/
void
mark_type( fun_alloc_info_t *i, int dtype, long dsize, int itype, long isize ) {
   if( dtype >=  0 ) {
      pa_pair_t *cell = pa_assq( (void *)dtype, (pa_pair_t *)(i->dtypes) );

      if( cell ) {
	 type_alloc_info_t *tai = (type_alloc_info_t *)PA_CDR( cell );
	 tai->num += 1;
	 tai->size += dsize;
      } else {
	 type_alloc_info_t *new = make_type_alloc_info();
	 new->num = 1;
	 new->size = dsize;
	 i->dtypes = pa_cons( pa_cons( (void *)dtype, (void *)new ),
			      (pa_pair_t *)(i->dtypes) );
      }
   }

   if( itype >=  0 ) {
      pa_pair_t *cell = pa_assq( (void *)itype, (pa_pair_t *)(i->itypes) );

      if( cell ) {
	 type_alloc_info_t *tai = (type_alloc_info_t *)PA_CDR( cell );
	 tai->num += 1;
	 tai->size += isize;
      } else {
	 type_alloc_info_t *new = make_type_alloc_info();
	 new->num = 1;
	 new->size = isize;
	 i->itypes = pa_cons( pa_cons( (void *)itype, (void *)new ),
			      (pa_pair_t *)(i->itypes) );
      }
   }
}
      
      
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    mark_function ...                                                */
/*---------------------------------------------------------------------*/
void
mark_function( void *id, long gc, long dsz, long isz, int dt, int it, long stamp ) {
   esymbol_t *fun;

   if( !SYMBOLP( id ) ) {
      if( unknown_ident ) {
	 mark_function( unknown_ident, gc, dsz, isz, dt, it, stamp );
      } else {
	 if( !gc ) {
	    ante_bgl_init_dsz += dsz;
	 } else
	    fprintf( stderr,
		     "*** WARNING: giving up with some allocations: %ld\n",
		     dsz );
      }
      return;
   }

   fun = (esymbol_t *)CSYMBOL( id );

   if( !dsz && (fun->stamp == stamp) ) {
      return;
   } else {
      fun->stamp = stamp;
   }

   if( !fun->alloc_info ) {
      fun_alloc_info_t *nfai = make_fun_alloc_info( gc, dsz, isz );

      mark_type( nfai, dt, dsz, it, isz );
      all_functions = pa_cons( id, all_functions );
      fun->alloc_info = pa_cons( nfai, 0 );
   } else {
      fun_alloc_info_t *ofai = ((fun_alloc_info_t *)(PA_CAR( ((pa_pair_t *)fun->alloc_info) )));

      if( ofai->gc_num != gc ) {
	 fun_alloc_info_t *nfai = make_fun_alloc_info( gc, dsz, isz );

	 mark_type( nfai, dt, dsz, it, isz );
	 fun->alloc_info = pa_cons( nfai, fun->alloc_info );
      } else {

	 mark_type( ofai, dt, dsz, it, isz );

	 ofai->dsize += dsz;
	 ofai->isize += isz;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    mark_rest_functions ...                                          */
/*---------------------------------------------------------------------*/
void
mark_rest_functions( void *id, void *isize ) {
   mark_function( id, gc_number, 0, (int)isize, -1, get_alloc_type(), stamp );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_pair ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
make_pair( obj_t car, obj_t cdr ) {
   obj_t pair;

   set_alloc_type( PAIR_TYPE_NUM, 0 );

   gc_alloc_size_add( PAIR_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  PAIR_SIZE, 0,
		  PAIR_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)PAIR_SIZE );

   pair = ____GC_malloc( PAIR_SIZE );

#if( !defined( TAG_PAIR ) )
   pair->pair_t.header = MAKE_HEADER( PAIR_TYPE, PAIR_SIZE );
#endif
   pair->pair_t.car = car;
   pair->pair_t.cdr = cdr;

   set_alloc_type( -1, 0 );
   return BPAIR( pair );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_cell ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
make_cell( obj_t val ) {
   obj_t cell;
   set_alloc_type( CELL_TYPE_NUM, 0 );

   gc_alloc_size_add( CELL_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  CELL_SIZE, 0,
		  CELL_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)CELL_SIZE );

   cell = ____GC_malloc( CELL_SIZE );
#if( !defined( TAG_CELL ) )
   cell->cell_t.header = MAKE_HEADER( CELL_TYPE, CELL_SIZE );
#endif
   cell->cell_t.val = val;

   set_alloc_type( -1, 0 );
   return BCELL( cell );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_real ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
make_real( double d ) {
   obj_t a_real;
   set_alloc_type( REAL_TYPE_NUM, 0 );

   gc_alloc_size_add( REAL_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  REAL_SIZE, 0,
		  REAL_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)REAL_SIZE );

   a_real = ____GC_malloc_atomic( REAL_SIZE );

#if( !defined( TAG_REAL ) )
   a_real->real_t.header = MAKE_HEADER( REAL_TYPE, REAL_SIZE );
#endif
   a_real->real_t.real = d;

   set_alloc_type( -1,0  );
   return BREAL( a_real );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_belong ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
make_belong( long l ) {
   obj_t a_elong;

   set_alloc_type( ELONG_TYPE_NUM, 0 );

   gc_alloc_size_add( ELONG_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  ELONG_SIZE, 0,
		  ELONG_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)ELONG_SIZE );

   a_elong = ____GC_malloc_atomic( ELONG_SIZE );

   a_elong->elong_t.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
   a_elong->elong_t.elong = l;

   set_alloc_type( -1, 0 );
   return BREF( a_elong );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_bllong ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
make_bllong( BGL_LONGLONG_T l ) {
   obj_t a_llong;

   set_alloc_type( LLONG_TYPE_NUM, 0 );

   gc_alloc_size_add( LLONG_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  LLONG_SIZE, 0,
		  LLONG_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)LLONG_SIZE );

   a_llong = ____GC_malloc_atomic( LLONG_SIZE );

   a_llong->llong_t.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   a_llong->llong_t.llong = l;

   set_alloc_type( -1, 0 );
   return BREF( a_llong );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    GC_malloc_find_type ...                                          */
/*---------------------------------------------------------------------*/
static void
GC_malloc_find_type( int lb, int unknown ) {
   void *top = bgl_debug_trace_top( get_alloc_type_offset() );

   if( SYMBOLP( top ) ) {
      int ty = ((esymbol_t *)CSYMBOL( top ))->class_alloc;
      int to = ((esymbol_t *)CSYMBOL( top ))->class_offset;

      set_alloc_type( ty == -1 ? unknown : ty, to );
#if BMEMDEBUG
      if( bmem_debug >= 10 ) {
	 fprintf( stderr, "UNKNOWN_TYPE_NUM(debug>=10) GC_malloc(%d): %s ty=%d type=%d\n",
		  lb,
		  bgl_debug_trace_top_name( get_alloc_type_offset() ),
		  ty, get_alloc_type() );
      }
#endif
   } else {
      set_alloc_type( unknown, 0 );
#if BMEMDEBUG
      if( bmem_debug >= 10 ) {
	 fprintf( stderr, "UNKNOWN_TYPE_NUM(debug>=10) GC_malloc(%d): ???? type=%d\n",
		  lb, get_alloc_type() );
      }
#endif
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_malloc ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
GC_malloc( size_t lb ) {
   gc_alloc_size_add( lb );

   if( get_alloc_type() == -1 )
      GC_malloc_find_type( lb, UNKNOWN_TYPE_NUM );

#if BMEMDEBUG
   if( bmem_debug ) {
      fprintf( stderr, "GC_malloc(%zu): %s %d\n",
	       lb,
	       bgl_debug_trace_top_name( get_alloc_type_offset() ),
	       get_alloc_type() );
   }
#endif
   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  lb, 0,
		  get_alloc_type(), -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)lb );
   set_alloc_type( -1, 0 );

   return ____GC_malloc( lb );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_realloc ...                                                   */
/*---------------------------------------------------------------------*/
obj_t
GC_realloc( obj_t old, size_t lb ) {
   gc_alloc_size_add( lb );

   set_alloc_type( UNKNOWN_REALLOC_TYPE_NUM, 0 );

#if BMEMDEBUG
   if( bmem_debug ) {
      fprintf( stderr, "GC_realloc(%zu): top=%s type=%d\n",
	       lb,
	       bgl_debug_trace_top_name( get_alloc_type_offset() ),
	       get_alloc_type() );
   }
#endif
   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  lb, 0,
		  UNKNOWN_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)lb );
   set_alloc_type( -1, 0 );

   return ____GC_realloc( old, lb );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_malloc_atomic ...                                             */
/*---------------------------------------------------------------------*/
obj_t
GC_malloc_atomic( size_t lb ) {
   gc_alloc_size_add( lb );

   if( get_alloc_type() == -1 )
      GC_malloc_find_type( lb, UNKNOWN_ATOMIC_TYPE_NUM );

#if BMEMDEBUG
   if( bmem_debug ) {
      fprintf( stderr, "GC_malloc_atomic(%zu): top=%s type=%d\n",
	       lb,
	       bgl_debug_trace_top_name( get_alloc_type_offset() ),
	       get_alloc_type() );
   }
#endif
   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  lb, 0,
		  get_alloc_type(), -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)lb );
   set_alloc_type( -1, 0 );

   return ____GC_malloc_atomic( lb );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_local_malloc ...                                              */
/*    -------------------------------------------------------------    */
/*    We have to disable the GC_local_malloc function otherwise        */
/*    we get confused in function such as make_pair in                 */
/*    multithreaded environment.                                       */
/*---------------------------------------------------------------------*/
obj_t
GC_local_malloc( size_t lb ) {
   return GC_malloc( lb );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_local_malloc_atomic ...                                       */
/*---------------------------------------------------------------------*/
obj_t
GC_local_malloc_atomic( size_t lb ) {
   return GC_malloc_atomic( lb );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    register_class ...                                               */
/*---------------------------------------------------------------------*/
obj_t
BGl_registerzd2classz12zc0zz__objectz00( obj_t name, obj_t super,
					 int abstract,
					 obj_t creator, obj_t allocate,
					 obj_t nil, obj_t predicate,
					 long hash, obj_t def,
					 obj_t constructor, obj_t virt ) {
   static int init = 0;
   char tmp[ 256 ];
   obj_t alloc;
   char *cname = BSTRING_TO_STRING( SYMBOL_TO_STRING( name ) );
   int tnum = ____bgl_types_number();
   obj_t class;

   if( !init ) {
      fprintf( stderr, "Defining classes...\n" );
      init = 1;
   }

   fprintf( stderr, "  %s (%d)...", cname, tnum );
   fflush( stderr );
   declare_type( tnum, cname );

   sprintf( tmp, "%%allocate-%s", cname );
   alloc = string_to_symbol( tmp );
   ((esymbol_t *)(CSYMBOL(alloc)))->class_alloc = tnum;
   ((esymbol_t *)(CSYMBOL(alloc)))->class_offset = 1;

   sprintf( tmp, "widening-%s", cname );
   alloc = string_to_symbol( tmp );
   ((esymbol_t *)(CSYMBOL(alloc)))->class_alloc = tnum;
   ((esymbol_t *)(CSYMBOL(alloc)))->class_offset = 1;

   class = ____register_class( name, super, abstract, creator, allocate,
			       nil, predicate,
			       hash, def,
			       constructor, virt );

   fprintf( stderr, "ok\n" );

   return class;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_dynamic_env ...                                         */
/*---------------------------------------------------------------------*/
void
bgl_init_dynamic_env() {
   set_alloc_type( _DYNAMIC_ENV_TYPE_NUM, 0 );
   ____bgl_init_dynamic_env();
}

/*---------------------------------------------------------------------*/
/*    WRAPPER ...                                                      */
/*---------------------------------------------------------------------*/
#define WRAPPER( ident, tnum, proto, call ) \
obj_t ident proto { \
   set_alloc_type( tnum, 0 );  \
   return ____##ident call ; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER_DBG ...                                                  */
/*---------------------------------------------------------------------*/
#define WRAPPER_DBG( level, ident, tnum, proto, call )	\
obj_t ident proto { \
   int lvl = bmem_debug; \
   obj_t res; \
   bmem_debug = level; \
   set_alloc_type( tnum, 0 ); \
   fprintf( stderr, "***WRAPPER_DEBUG: " #ident " type=%d\n", get_alloc_type() ); \
   res = ____##ident call ; \
   bmem_debug = lvl; \
   return res; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER_PUSH ...                                                 */
/*---------------------------------------------------------------------*/
#define WRAPPER_PUSH( ident, tnum, typeres, proto, call ) \
typeres ident proto { \
   typeres res; \
   BGL_PUSH_TRACE( ident##_symbol, BFALSE ); \
   set_alloc_type( tnum, 0 ); \
   res = ____##ident call ; \
   BGL_POP_TRACE(); \
   return res; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER2 ...                                                     */
/*---------------------------------------------------------------------*/
#define WRAPPER2( ident, tnum1, tnum2, proto, call ) \
obj_t ident proto { \
   obj_t aux; \
   set_alloc_type( tnum1, 0 ); \
   aux = ____##ident call ; \
   set_alloc_type( tnum2, 0 ); \
   return aux; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER2_PUSH ...                                                */
/*---------------------------------------------------------------------*/
#define WRAPPER2_PUSH( ident, tnum1, tnum2, proto, call ) \
obj_t ident proto { \
   obj_t aux; \
   BGL_PUSH_TRACE( ident##_symbol, BFALSE ); \
   set_alloc_type( tnum1, 0 ); \
   aux = ____##ident call ; \
   set_alloc_type( tnum2, 0 ); \
   BGL_POP_TRACE(); \
   return aux; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER3 ...                                                     */
/*---------------------------------------------------------------------*/
#define WRAPPER3( type, ident, tnum, proto, call ) \
type ident proto { \
   set_alloc_type( tnum, 0 );  \
   return ____##ident call ; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER4 ...                                                     */
/*---------------------------------------------------------------------*/
#define WRAPPER4( ident, var, sym, proto, call ) \
obj_t ident proto { \
   static obj_t s = 0; \
   if( !s ) s = string_to_symbol( sym ); \
   { \
     obj_t res; \
     BGL_PUSH_TRACE( s, BFALSE ); \
     res = var call ; \
     BGL_POP_TRACE(); \
     return res; \
   } \
}
 
/* string */
WRAPPER( string_to_bstring_len, STRING_TYPE_NUM, (char *s, int l), (s ,l) )
WRAPPER( string_to_bstring, STRING_TYPE_NUM, (char *s), (s) )
WRAPPER( make_string, STRING_TYPE_NUM, (int l, char c), (l,c)  )
WRAPPER( make_string_sans_fill, STRING_TYPE_NUM, (int l), (l) )
WRAPPER( string_append, STRING_TYPE_NUM, (void *s1, void *s2), (s1, s2) )
WRAPPER( string_append_3, STRING_TYPE_NUM, (void *s1, void *s2, void *s3), (s1, s2, s3) )
WRAPPER( c_substring, STRING_TYPE_NUM, (void *s, int l1, int l2), (s, l1, l2) )
WRAPPER( bgl_escape_C_string, STRING_TYPE_NUM, (unsigned char *s, long l1, long l2), (s, l1, l2) )
WRAPPER( bgl_escape_scheme_string, STRING_TYPE_NUM, (unsigned char *s, long l1, long l2), (s, l1, l2) )
WRAPPER( create_string_for_read, STRING_TYPE_NUM, (void *o, int s), (o, s) )
WRAPPER( string_to_keyword, KEYWORD_TYPE_NUM, (char *s), (s) )
WRAPPER( bstring_to_keyword, KEYWORD_TYPE_NUM, (void *s), (s) )

/* vector */
WRAPPER( create_vector, VECTOR_TYPE_NUM, (int len), (len) )
WRAPPER( make_vector, VECTOR_TYPE_NUM, (int len, void *init), (len, init) )
WRAPPER( make_vector_uncollectable, VECTOR_TYPE_NUM, (int len, void *init), (len, init) )

/* create_vector_uncollectable is only */ 
/* used to allocate class objects.     */
WRAPPER( create_vector_uncollectable, CLASS_TYPE_NUM, (int len), (len) )

/* procedure */
WRAPPER( make_fx_procedure, PROCEDURE_TYPE_NUM, (obj_t (*e)(), int a, int s), ((void *(*)())e, a, s) )
WRAPPER( make_va_procedure, PROCEDURE_TYPE_NUM, (obj_t (*e)(), int a, int s), ((void *(*)())e, a, s) )

/* output port */
WRAPPER( bgl_make_output_port, OUTPUT_PORT_TYPE_NUM, (obj_t n, bgl_stream_t d, int y, obj_t t, obj_t b, ssize_t (*w)(), long (*s)(), int (*c)()), (n, d, y, t, b, w, s, c) )
WRAPPER( bgl_open_output_string, OUTPUT_PORT_TYPE_NUM, (obj_t o), (o) )
WRAPPER2( bgl_output_port_timeout_set, PORT_TIMEOUT_TYPE_NUM, -1, (void *o, long t), (o, t) )

/* input port */
WRAPPER( bgl_make_input_port, INPUT_PORT_TYPE_NUM, (obj_t s, FILE *f, obj_t o, obj_t b), (s, f, o, b) )
WRAPPER( bgl_open_input_file, INPUT_PORT_TYPE_NUM, (obj_t s, obj_t b), (s, b) )
WRAPPER( bgl_open_input_pipe, INPUT_PORT_TYPE_NUM, (obj_t s, obj_t b), (s, b) )
WRAPPER( bgl_open_input_resource, INPUT_PORT_TYPE_NUM, (obj_t s, obj_t b), (s, b) )
WRAPPER( bgl_open_input_string, INPUT_PORT_TYPE_NUM, (obj_t o, long i), (o, i) )
WRAPPER( bgl_open_input_substring, INPUT_PORT_TYPE_NUM, (obj_t o, long i, long e), (o, i, e) )
WRAPPER( bgl_open_input_substring_bang, INPUT_PORT_TYPE_NUM, (obj_t o, long i, long e), (o, i, e) )
WRAPPER( bgl_open_input_c_string, INPUT_PORT_TYPE_NUM, (char *s), (s) )
WRAPPER( bgl_reopen_input_c_string, INPUT_PORT_TYPE_NUM, (void *o, char *s), (o, s) )
WRAPPER2( bgl_input_port_timeout_set, PORT_TIMEOUT_TYPE_NUM, -1, (void *o, long t), (o, t) )

/* struct */
WRAPPER( create_struct, STRUCT_TYPE_NUM, (obj_t k, int l), (k, l) )
WRAPPER( make_struct, STRUCT_TYPE_NUM, (obj_t k, int l, obj_t i), (k, l, i) )

/* socket */
WRAPPER( bgl_make_client_socket, SOCKET_TYPE_NUM, (obj_t h, int p, int s, obj_t ib, obj_t ob ), (h, p, s, ib, ob) )
WRAPPER( bgl_make_server_socket, SOCKET_TYPE_NUM, (obj_t h, int p, int b), (h, p, b) )
WRAPPER_PUSH( bgl_socket_accept, SOCKET_TYPE_NUM, obj_t, (obj_t s, int e, obj_t ib, obj_t ob), (s, e, ib, ob) )
WRAPPER_PUSH( bgl_socket_accept_many, SOCKET_TYPE_NUM, long, (obj_t s, bool_t e, obj_t ib, obj_t ob, obj_t vec), (s, e, ib, ob, vec) )
WRAPPER( bgl_host, HOSTENT_TYPE_NUM, (obj_t s), (s) )

/* date */
WRAPPER( bgl_seconds_to_date, DATE_TYPE_NUM, (long s), (s) )
WRAPPER( bgl_nanoseconds_to_date, DATE_TYPE_NUM, (long s), (s) )
WRAPPER( bgl_make_date, DATE_TYPE_NUM, (int s, int m, int hr, int mday, int mon, int year, long tz, bool_t istz, int isdst), (s, m, hr, mday, mon, year, tz, istz, isdst) )
WRAPPER( bgl_seconds_format, STRING_TYPE_NUM, (long s, obj_t f), (s, f) )

/* dynamic environment */
WRAPPER3( obj_t, make_dynamic_env, _DYNAMIC_ENV_TYPE_NUM, (), () )
WRAPPER3( obj_t, bgl_dup_dynamic_env, _DYNAMIC_ENV_TYPE_NUM, (obj_t o), (o) )

/* thread */
WRAPPER( bglthread_new, _THREAD_TYPE_NUM, (obj_t p), (p) )
WRAPPER( bglthread_new_with_name, _THREAD_TYPE_NUM, (obj_t p, obj_t n), (p,n) )
WRAPPER4( BGl_schedulerzd2startz12zc0zz__ft_schedulerz00, ____scheduler_start, "scheduler-start!", (obj_t o), (o) )
WRAPPER4( BGl_schedulerzd2reactz12zc0zz__ft_schedulerz00, ____scheduler_react, "scheduler-react!", (obj_t o), (o) )
