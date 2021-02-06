/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/bde/bmem/lib/alloc.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:42:57 2003                          */
/*    Last change :  Fri Jan 10 13:54:19 2020 (serrano)                */
/*    Copyright   :  2003-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Allocation replacement routines                                  */
/*=====================================================================*/
#define BGL_GC BGL_NO_GC
#include <bigloo.h>
#include <bmem.h>
#include <esymbol.h>
#include <stdlib.h>
#include <string.h>

extern void gc_alloc_size_add( int size );
extern int bmem_verbose;

/*---------------------------------------------------------------------*/
/*    static pa_pair_t *                                               */
/*    all_functions ...                                                */
/*---------------------------------------------------------------------*/
static pa_pair_t *all_functions = 0;
static int stamp = 1;
static long alloc_types[] = {-1, -1, -1, -1, -1, -1};
static long alloc_type_offsets[] = {0, 0, 0, 0, 0};
static long alloc_index = -1;
static long max_stack_size = 100000;
unsigned long ante_bgl_init_dsz = 0;

#define DBG_INDEX_START( name ) \
   { long __idx = bmem_thread ?			 \
      (long)____pthread_getspecific( bmem_key3 ) \
      : alloc_index; \
      ((__idx < 0 || __idx >= 5) ? (fprintf( stderr, "*** bmem: stack overflow/underflow \"%s\" [%ld]\n", name, __idx ), exit( -2 )) : 0)

#define DBG_INDEX_STOP( name ) \
   (bmem_thread ? (long)____pthread_getspecific( bmem_key3 ) : alloc_index) != (__idx -1) \
      ? fprintf( stderr, "*** bmem: illegal stack after \"%s\" [%ld/%ld]\n", name, (bmem_thread ? (long)____pthread_getspecific( bmem_key3 ) : alloc_index), __idx - 1), -1 : 0; } 0

#define DBG_INDEX_RESET() \
   (alloc_index = __idx)

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    all_types ...                                                    */
/*---------------------------------------------------------------------*/
static char **all_types = 0;
static int types_number = 0;

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bmem_get_alloc_index ...                                         */
/*---------------------------------------------------------------------*/
long
bmem_get_alloc_index() {
   if( bmem_thread ) {
      return (long)____pthread_getspecific( bmem_key3 );
   } else {
      return alloc_index;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bmem_set_alloc_index ...                                         */
/*---------------------------------------------------------------------*/
void
bmem_set_alloc_index( long idx ) {
   if( bmem_thread ) {
      ____pthread_setspecific( bmem_key3, (void *)idx );
   } else {
      alloc_index = idx;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bmem_set_alloc_type ...                                          */
/*---------------------------------------------------------------------*/
void
bmem_set_alloc_type( long t, long o ) {
   if( bmem_thread ) {
      long *alloc_types = (long *)____pthread_getspecific( bmem_key );
      long *alloc_type_offsets = (long *)____pthread_getspecific( bmem_key2 );
      long alloc_index = (long)____pthread_getspecific( bmem_key3 );
      
      if( !alloc_types ) {
	 alloc_types = malloc( sizeof( long ) * 5 );
	 alloc_type_offsets = malloc( sizeof( long ) * 5 );
	 alloc_index = -1;

	 ____pthread_setspecific( bmem_key, alloc_types );
	 ____pthread_setspecific( bmem_key2, alloc_type_offsets );
      }

      alloc_index++;
      alloc_types[ alloc_index ] = t;
      alloc_type_offsets[ alloc_index ] = o;
      
      ____pthread_setspecific( bmem_key3, (void *)alloc_index );
   } else {
      alloc_index++;
      alloc_types[ alloc_index ] = t;
      alloc_type_offsets[ alloc_index ] = o;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bmem_set_allocation_type ...                                     */
/*---------------------------------------------------------------------*/
void
bmem_set_allocation_type( long t, long o ) {
   /* this overrides the the standard Bigloo library definition */
   bmem_set_alloc_type( t, o );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bmem_pop_type ...                                                */
/*---------------------------------------------------------------------*/
static void
bmem_pop_type() {
   if( bmem_thread ) {
      long alloc_index = (long)____pthread_getspecific( bmem_key3 );
      
      alloc_index--;
      ____pthread_setspecific( bmem_key3, (void *)alloc_index );
   } else {
      alloc_index--;
   }
}
   
/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    get_alloc_type ...                                               */
/*---------------------------------------------------------------------*/
static long
get_alloc_type() {
   if( bmem_thread ) {
      long *alloc_types = (long *)____pthread_getspecific( bmem_key );

      if( !alloc_types ) {
	 // something is wrong fall back
	 return -1;
      } else {
	 long *alloc_type_offsets = (long *)____pthread_getspecific( bmem_key2 );
	 long alloc_index = (long)____pthread_getspecific( bmem_key3 );

	 return (alloc_index == -1) ? -1 : alloc_types[ alloc_index ];
      }
   } else {
      return (alloc_index == -1) ? -1 : alloc_types[ alloc_index ];
   }
}
    
/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    get_alloc_type_offset ...                                        */
/*---------------------------------------------------------------------*/
static long
get_alloc_type_offset() {
   if( bmem_thread ) {
      long *alloc_types = (long *)____pthread_getspecific( bmem_key );
      
      if( !alloc_types ) {
	 // something is wrong fall back
	 return 0;
      } else {
	 long *alloc_type_offsets = (long *)____pthread_getspecific( bmem_key2 );
	 long alloc_index = (long)____pthread_getspecific( bmem_key3 );

	 return alloc_type_offsets[ alloc_index ];
      }
   } else {
      return alloc_type_offsets[ alloc_index ];
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
/*    alloc_dump_type_json ...                                         */
/*---------------------------------------------------------------------*/
void
alloc_dump_type_json( pa_pair_t *i, FILE *f ) {
   type_alloc_info_t *tai = (type_alloc_info_t *)PA_CDR( i );
   
   fprintf( f, "            { \"type\": %ld, \"cnt\": %ld, \"size\": %ld }",
	    (long)PA_CAR( i ),
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
/*    alloc_dump_json ...                                              */
/*---------------------------------------------------------------------*/
void
alloc_dump_json( fun_alloc_info_t *i, FILE *f ) {
   fprintf( f, "      { \"gc\": %lu, \"dsize\": %lu, \"isize\": %lu,\n",
	    i->gc_num,
	    BMEMSIZE( i->dsize ), BMEMSIZE( i->isize ) );
   fprintf( f, "        \"dtype\": " );
   for_each_json( (void (*)(void *, void *))alloc_dump_type_json, i->dtypes, f );
   fprintf( f, " ,\n" );
   fprintf( f, "        \"itype\": " );
   for_each_json( (void (*)(void *, void *))alloc_dump_type_json, i->itypes, f );
   fprintf( f, " }" );
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
/*    fun_dump ...                                                     */
/*---------------------------------------------------------------------*/
void
fun_dump_json( void *ident, FILE *f ) {
   esymbol_t *fun = (esymbol_t *)ident;

   fprintf( f, "   { \"function\": %s, \"allocs\": ", bgl_debug_trace_symbol_name_json( (obj_t)fun ) );
   for_each_json( (void (*)(void *, void *))alloc_dump_json, CESYMBOL( fun )->alloc_info, f );
   fprintf( f, " }" );
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
/*    alloc_dump_statistics_json ...                                   */
/*---------------------------------------------------------------------*/
void
alloc_dump_statistics_json( FILE *f ) {
   fprintf( f, "  \"function\": " );
   for_each_json( (void (*)(void *, void *))fun_dump_json, all_functions, (void *)f );
   fprintf( f, "\n" );
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
   
   if( bmem_thread ) {
      ____pthread_setspecific( bmem_key3, (void *)0 );
   } else {
      alloc_index = 0;
   }
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
      pa_pair_t *cell = pa_assq( (void *)(long)dtype, (pa_pair_t *)(i->dtypes) );

      if( cell ) {
	 type_alloc_info_t *tai = (type_alloc_info_t *)PA_CDR( cell );
	 tai->num += 1;
	 tai->size += dsize;
      } else {
	 type_alloc_info_t *new = make_type_alloc_info();
	 new->num = 1;
	 new->size = dsize;
	 i->dtypes = pa_cons( pa_cons( (void *)(long)dtype, (void *)new ),
			      (pa_pair_t *)(i->dtypes) );
      }
   }

   if( itype >=  0 ) {
      pa_pair_t *cell = pa_assq( (void *)(long)itype, (pa_pair_t *)(i->itypes) );

      if( cell ) {
	 type_alloc_info_t *tai = (type_alloc_info_t *)PA_CDR( cell );
	 tai->num += 1;
	 tai->size += isize;
      } else {
	 type_alloc_info_t *new = make_type_alloc_info();
	 new->num = 1;
	 new->size = isize;
	 i->itypes = pa_cons( pa_cons( (void *)(long)itype, (void *)new ),
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
   mark_function( id, gc_number, 0, (long)isize, -1, get_alloc_type(), stamp );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_pair ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
make_pair( obj_t car, obj_t cdr ) {
   obj_t pair;

   bmem_set_alloc_type( PAIR_TYPE_NUM, 0 );

   DBG_INDEX_START( "make_pair" );
   gc_alloc_size_add( PAIR_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  PAIR_SIZE, 0,
		  PAIR_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)PAIR_SIZE );

   pair = ____make_pair( car, cdr );

   if( !bmem_thread ) {
      DBG_INDEX_RESET();
      bmem_pop_type();
      //bmem_set_alloc_type( -1, 0 );
   }
   DBG_INDEX_STOP( "make_pair" );
   
   return pair;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_cell ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
make_cell( obj_t val ) {
   obj_t cell;
   
   bmem_set_alloc_type( CELL_TYPE_NUM, 0 );

   DBG_INDEX_START( "make_cell" );
   gc_alloc_size_add( CELL_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  CELL_SIZE, 0,
		  CELL_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)CELL_SIZE );

   cell = ____make_cell( val );

   // bmem_pop_type();
   if( !bmem_thread ) {
      DBG_INDEX_RESET();
      bmem_pop_type();
      // bmem_set_alloc_type( -1, 0 );
   }
   DBG_INDEX_STOP( "make_cell" );
   
   return cell;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_real ...                                                    */
/*---------------------------------------------------------------------*/
#if( !BGL_NAN_TAGGING )
obj_t
make_real( double d ) {
   obj_t a_real;
   
   bmem_set_alloc_type( REAL_TYPE_NUM, 0 );
   
   DBG_INDEX_START( "make_real" );

   gc_alloc_size_add( REAL_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  REAL_SIZE, 0,
		  REAL_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)REAL_SIZE );

   a_real = ____make_real( d );

   if( !bmem_thread ) {
      DBG_INDEX_RESET();
      bmem_pop_type();
      //bmem_set_alloc_type( -1,0  );
   }
   DBG_INDEX_STOP( "make_real" );
      
   return a_real;
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_belong ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
make_belong( long l ) {
   obj_t a_elong;
   bmem_set_alloc_type( ELONG_TYPE_NUM, 0 );
   
   DBG_INDEX_START( "make_belong" );

   gc_alloc_size_add( ELONG_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  ELONG_SIZE, 0,
		  ELONG_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)ELONG_SIZE );

   a_elong = ____GC_malloc_atomic( ELONG_SIZE );

   a_elong->elong.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
   a_elong->elong.val = l;

   bmem_pop_type();
   DBG_INDEX_STOP( "make_belong" );
   
   return BREF( a_elong );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_bllong ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
make_bllong( BGL_LONGLONG_T l ) {
   obj_t a_llong;

   bmem_set_alloc_type( LLONG_TYPE_NUM, 0 );

   DBG_INDEX_START( "make_bllong" );
   gc_alloc_size_add( LLONG_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  LLONG_SIZE, 0,
		  LLONG_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)LLONG_SIZE );

   a_llong = ____GC_malloc_atomic( LLONG_SIZE );

   a_llong->llong.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   a_llong->llong.val = l;


   bmem_pop_type();
   DBG_INDEX_STOP( "make_bllong" );
   return BREF( a_llong );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_bint32 ...                                              */
/*---------------------------------------------------------------------*/
#if( defined( BGL_INT32_SIZE ) )
obj_t
bgl_make_bint32( int32_t l ) {
   obj_t a_int32;

   bmem_set_alloc_type( INT32_TYPE_NUM, 0 );
   DBG_INDEX_START( "bgl_make_bint32" );

   gc_alloc_size_add( BGL_INT32_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  BGL_INT32_SIZE, 0,
		  INT32_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)BGL_INT32_SIZE );

   a_int32 = ____GC_malloc_atomic( BGL_INT32_SIZE );

   a_int32->sint32.header = MAKE_HEADER( INT32_TYPE_NUM, BGL_INT32_SIZE );
   a_int32->sint32.val = l;

   bmem_pop_type();
   DBG_INDEX_STOP( "bgl_make_bint32" );
   return BREF( a_int32 );
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_buint32 ...                                             */
/*---------------------------------------------------------------------*/
#if( defined( BGL_UINT32_SIZE ) )
obj_t
bgl_make_buint32( uint32_t l ) {
   obj_t a_uint32;

   bmem_set_alloc_type( UINT32_TYPE_NUM, 0 );
   DBG_INDEX_START( "bgl_make_buint32" );

   gc_alloc_size_add( BGL_UINT32_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  BGL_UINT32_SIZE, 0,
		  UINT32_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)BGL_UINT32_SIZE );

   a_uint32 = ____GC_malloc_atomic( BGL_UINT32_SIZE );

   a_uint32->uint32.header = MAKE_HEADER( UINT32_TYPE_NUM, BGL_UINT32_SIZE );
   a_uint32->uint32.val = l;

   bmem_pop_type();
   DBG_INDEX_STOP( "bgl_make_buint32" );
   return BREF( a_uint32 );
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_bint64 ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_bint64( int64_t l ) {
   obj_t a_int64;

   bmem_set_alloc_type( INT64_TYPE_NUM, 0 );
   DBG_INDEX_START( "bgl_make_bint64" );

   gc_alloc_size_add( BGL_INT64_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  BGL_INT64_SIZE, 0,
		  INT64_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)BGL_INT64_SIZE );

   a_int64 = ____GC_malloc_atomic( BGL_INT64_SIZE );

   a_int64->sint64.header = MAKE_HEADER( INT64_TYPE_NUM, BGL_INT64_SIZE );
   a_int64->sint64.val = l;

   bmem_pop_type();
   DBG_INDEX_STOP( "bgl_make_bint64" );
   return BREF( a_int64 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_buint64 ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_buint64( uint64_t l ) {
   obj_t a_uint64;

   bmem_set_alloc_type( UINT64_TYPE_NUM, 0 );
   DBG_INDEX_START( "bgl_make_buint64" );

   gc_alloc_size_add( BGL_UINT64_SIZE );

   mark_function( bgl_debug_trace_top( get_alloc_type_offset() ),
		  gc_number,
		  BGL_UINT64_SIZE, 0,
		  UINT64_TYPE_NUM, -1,
		  ++stamp );
   for_each_trace( mark_rest_functions, 1, max_stack_size, (void *)BGL_UINT64_SIZE );

   a_uint64 = ____GC_malloc_atomic( BGL_UINT64_SIZE );

   a_uint64->uint64.header = MAKE_HEADER( UINT64_TYPE_NUM, BGL_UINT64_SIZE );
   a_uint64->uint64.val = l;

   bmem_pop_type();
   if( !bmem_thread ) {
      DBG_INDEX_RESET();
      //bmem_set_alloc_type( -1, 0 );
   }
   
   DBG_INDEX_STOP( "bgl_make_buint64" );
   return BREF( a_uint64 );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    GC_malloc_unknown ...                                            */
/*---------------------------------------------------------------------*/
int
#if __GNUC__
__attribute__ ((noinline))
#endif
GC_malloc_unknown( int ty, int to, int unknown ) {
   // for gdb breakpoing
   return ty == -1 ? unknown : ty;   
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

      bmem_set_alloc_type( ty == -1 ? GC_malloc_unknown( ty, to, unknown ) : ty, to );
#if BMEMDEBUG
      if( bmem_debug >= 10 ) {
	 fprintf( stderr, "UNKNOWN_TYPE_NUM(debug>=10) GC_malloc(%d): %s ty=%d type=%ld\n",
		  lb,
		  bgl_debug_trace_top_name( get_alloc_type_offset() ),
		  ty, get_alloc_type() );
      }
#endif
   } else {
      bmem_set_alloc_type( GC_malloc_unknown( -1, -1, unknown ), 0 );
#if BMEMDEBUG
      if( bmem_debug >= 10 ) {
	 fprintf( stderr, "UNKNOWN_TYPE_NUM(debug>=10) GC_malloc(%d): ???? type=%ld\n",
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

   if( get_alloc_type() == -1 ) {
      GC_malloc_find_type( lb, UNKNOWN_TYPE_NUM );
   }

#if BMEMDEBUG
   if( bmem_debug ) {
      fprintf( stderr, "GC_malloc(%zu): %s %ld\n",
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

   if( get_alloc_type_offset() >= 0 ) bmem_pop_type();
   // bmem_set_alloc_type( -1, 0 );

   return ____GC_malloc( lb );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_realloc ...                                                   */
/*---------------------------------------------------------------------*/
obj_t
GC_realloc( obj_t old, size_t lb ) {
   gc_alloc_size_add( lb );

   bmem_set_alloc_type( UNKNOWN_REALLOC_TYPE_NUM, 0 );

#if BMEMDEBUG
   if( bmem_debug ) {
      fprintf( stderr, "GC_realloc(%zu): top=%s type=%ld\n",
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
   if( get_alloc_type_offset() >= 0 ) bmem_pop_type();
   // bmem_set_alloc_type( -1, 0 );

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
      fprintf( stderr, "GC_malloc_atomic(%zu): top=%s type=%ld\n",
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
   
   if( get_alloc_type_offset() >= 0 ) bmem_pop_type();
   // bmem_set_alloc_type( -1, 0 );

   return ____GC_malloc_atomic( lb );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    GC_malloc_uncollectable ...                                      */
/*---------------------------------------------------------------------*/
obj_t
GC_malloc_uncollectable( size_t lb ) {
   gc_alloc_size_add( lb );

   if( get_alloc_type() == -1 )
      GC_malloc_find_type( lb, UNKNOWN_UNCOLLECTABLE_TYPE_NUM );

#if BMEMDEBUG
   if( bmem_debug ) {
      fprintf( stderr, "GC_malloc_uncollectable(%zu): top=%s type=%ld\n",
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
   
   if( get_alloc_type_offset() >= 0 ) bmem_pop_type();
   // bmem_set_alloc_type( -1, 0 );

   return ____GC_malloc_uncollectable( lb );
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
BGl_registerzd2classz12zc0zz__objectz00( obj_t name, obj_t module, obj_t super,
					 long hash,
					 obj_t creator, obj_t ator, obj_t ctor,
					 obj_t nil, obj_t shrink,
					 obj_t plain, obj_t virtual ) {
   static int init = 0;
   char tmp[ 256 ];
   obj_t alloc;
   char *cname = BSTRING_TO_STRING( SYMBOL_TO_STRING( name ) );
   int tnum = ____bgl_types_number();
   obj_t class;

   if( !init ) {
      if( bmem_verbose >= 1 ) {
	 fprintf( stderr, "Defining classes...\n" );
      }
      init = 1;
   }

   if( bmem_verbose >= 2 ) {
      fprintf( stderr, "  %s@%s (%d)...",
	       cname,
	       BSTRING_TO_STRING( SYMBOL_TO_STRING( module ) ),
	       tnum );
   }
	       
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

   class = ____register_class( name, module, super,
			       hash,
			       creator, ator, ctor,
			       nil, shrink,
			       plain, virtual );

   if( bmem_verbose >= 2 ) {
      fprintf( stderr, "ok\n" );
   }

   return class;
}

/*---------------------------------------------------------------------*/
/*    WRAPPER ...                                                      */
/*---------------------------------------------------------------------*/
#define WRAPPER( ident, tnum, proto, call ) \
obj_t ident proto { \
   obj_t __res; \
   bmem_set_alloc_type( tnum, 0 ); \
   DBG_INDEX_START( "" #ident ); \
   __res =  ____##ident call ; \
   DBG_INDEX_STOP( "" #ident ); \
   return __res; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER_DBG ...                                                  */
/*---------------------------------------------------------------------*/
#define WRAPPER_DBG( ident, tnum, proto, call ) \
obj_t ident proto { \
   obj_t __res; \
   bmem_set_alloc_type( tnum, 0 ); \
   DBG_INDEX_START( "" #ident ); \
   fprintf( stderr, ">>> wrapper %s tnum=%d stk=%d/%d\n", "" #ident, tnum, bmem_get_alloc_index(), __idx ); \
   __res = ____##ident call ; \
   fprintf( stderr, "<<< wrapper %s tnum=%d stk=%d/%d\n", "" #ident, tnum, bmem_get_alloc_index(), __idx - 1); \
   DBG_INDEX_STOP( "" #ident ); \
   return __res; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER_TRES ...                                                 */
/*---------------------------------------------------------------------*/
#define WRAPPER_TRES( ident, tnum, tres, proto, call ) \
obj_t ident proto { \
   tres __res; \
   DBG_INDEX_START( "" #ident ); \
   bmem_set_alloc_type( tnum, 0 ); \
   __res = ____##ident call; \
   DBG_INDEX_STOP( "" #ident ); \
   return __res; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER_PUSH ...                                                 */
/*---------------------------------------------------------------------*/
#define WRAPPER_PUSH( ident, tnum, typeres, proto, call ) \
typeres ident proto { \
   typeres res; \
   BGL_PUSH_TRACE( ident##_symbol, BFALSE ); \
   bmem_set_alloc_type( tnum, 0 ); \
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
   bmem_set_alloc_type( tnum1, 0 ); \
   aux = ____##ident call ; \
   bmem_set_alloc_type( tnum2, 0 ); \
   return aux; \
}

/*---------------------------------------------------------------------*/
/*    WRAPPER3 ...                                                     */
/*---------------------------------------------------------------------*/
#define WRAPPER3( type, ident, tnum, proto, call ) \
type ident proto { \
   bmem_set_alloc_type( tnum, 0 );  \
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
WRAPPER( make_string, STRING_TYPE_NUM, (int l, char c), (l,c)  )
WRAPPER( make_string_sans_fill, STRING_TYPE_NUM, (long l), (l) )
WRAPPER( string_append, STRING_TYPE_NUM, (void *s1, void *s2), (s1, s2) )
WRAPPER( string_append_3, STRING_TYPE_NUM, (void *s1, void *s2, void *s3), (s1, s2, s3) )
WRAPPER( c_substring, STRING_TYPE_NUM, (void *s, int l1, int l2), (s, l1, l2) )
WRAPPER( bgl_escape_C_string, STRING_TYPE_NUM, (unsigned char *s, long l1, long l2), (s, l1, l2) )
WRAPPER( bgl_escape_scheme_string, STRING_TYPE_NUM, (unsigned char *s, long l1, long l2), (s, l1, l2) )
WRAPPER( create_string_for_read, STRING_TYPE_NUM, (void *o, int s), (o, s) )
WRAPPER( bgl_make_keyword, KEYWORD_TYPE_NUM, (void *s), (s) )

/* vector */
WRAPPER( create_vector, VECTOR_TYPE_NUM, (long len), (len) )

/* create_vector_uncollectable is only */ 
/* used to allocate class objects.     */
WRAPPER( create_vector_uncollectable, CLASS_TYPE_NUM, (long len), (len) )

/* procedure */
WRAPPER( make_fx_procedure, PROCEDURE_TYPE_NUM, (obj_t (*e)(), int a, int s), ((void *(*)())e, a, s) )
WRAPPER( make_va_procedure, PROCEDURE_TYPE_NUM, (obj_t (*e)(), int a, int s), ((void *(*)())e, a, s) )

/* regexp */
WRAPPER( bgl_make_regexp, REGEXP_TYPE_NUM, (obj_t pat), (pat) );

/* output port */
WRAPPER( bgl_make_output_port, OUTPUT_PORT_TYPE_NUM, (obj_t n, bgl_stream_t d, int y, obj_t t, obj_t b, ssize_t (*w)(), long (*s)(), int (*c)()), (n, d, y, t, b, w, s, c) )
WRAPPER2( bgl_output_port_timeout_set, PORT_TIMEOUT_TYPE_NUM, -1, (void *o, long t), (o, t) )

/* input port */
WRAPPER( bgl_make_input_port, INPUT_PORT_TYPE_NUM, (obj_t s, FILE *f, obj_t o, obj_t b), (s, f, o, b) )
WRAPPER( bgl_open_input_file, INPUT_PORT_TYPE_NUM, (obj_t s, obj_t b), (s, b) )
WRAPPER( bgl_open_input_pipe, INPUT_PORT_TYPE_NUM, (obj_t s, obj_t b), (s, b) )
WRAPPER( bgl_open_input_resource, INPUT_PORT_TYPE_NUM, (obj_t s, obj_t b), (s, b) )
WRAPPER( bgl_open_input_string, INPUT_PORT_TYPE_NUM, (obj_t o, long i), (o, i) )
WRAPPER( bgl_open_input_c_string, INPUT_PORT_TYPE_NUM, (char *s), (s) )
WRAPPER( bgl_reopen_input_c_string, INPUT_PORT_TYPE_NUM, (void *o, char *s), (o, s) )
WRAPPER2( bgl_input_port_timeout_set, PORT_TIMEOUT_TYPE_NUM, -1, (void *o, long t), (o, t) )

/* struct */
WRAPPER( create_struct, STRUCT_TYPE_NUM, (obj_t k, int l), (k, l) )
WRAPPER( make_struct, STRUCT_TYPE_NUM, (obj_t k, int l, obj_t i), (k, l, i) )

/* socket */
WRAPPER( bgl_make_client_socket, SOCKET_TYPE_NUM, (obj_t h, int p, int s, obj_t ib, obj_t ob ), (h, p, s, ib, ob) )
WRAPPER( bgl_make_server_socket, SOCKET_TYPE_NUM, (obj_t h, int p, int b, bool_t f), (h, p, b, f) )
WRAPPER_PUSH( bgl_socket_accept, SOCKET_TYPE_NUM, obj_t, (obj_t s, int e, obj_t ib, obj_t ob), (s, e, ib, ob) )
WRAPPER_PUSH( bgl_socket_accept_many, SOCKET_TYPE_NUM, long, (obj_t s, bool_t e, obj_t ib, obj_t ob, obj_t vec), (s, e, ib, ob, vec) )
WRAPPER( bgl_host, HOSTENT_TYPE_NUM, (obj_t s), (s) )

/* date */
WRAPPER( bgl_seconds_to_date, DATE_TYPE_NUM, (long s), (s) )
WRAPPER( bgl_nanoseconds_to_date, DATE_TYPE_NUM, (long s), (s) )
WRAPPER( bgl_seconds_format, STRING_TYPE_NUM, (long s, obj_t f), (s, f) )

/* bignum */
WRAPPER( bgl_string_to_bignum, BIGNUM_TYPE_NUM, (char *s, int r), (s, r) )
WRAPPER( bgl_long_to_bignum, BIGNUM_TYPE_NUM, (long l), (l) )
WRAPPER( bgl_llong_to_bignum, BIGNUM_TYPE_NUM, (long long l), (l) )
WRAPPER( bgl_uint64_to_bignum, BIGNUM_TYPE_NUM, (uint64_t l), (l) )
WRAPPER( bgl_flonum_to_bignum, BIGNUM_TYPE_NUM, (double l), (l) )

/* dynamic environment */
WRAPPER3( obj_t, make_dynamic_env, _DYNAMIC_ENV_TYPE_NUM, (), () )
WRAPPER3( obj_t, bgl_dup_dynamic_env, _DYNAMIC_ENV_TYPE_NUM, (obj_t o), (o) )

/* thread */
WRAPPER( bglthread_new, _THREAD_TYPE_NUM, (obj_t p), (p) )
WRAPPER( bglthread_new_with_name, _THREAD_TYPE_NUM, (obj_t p, obj_t n), (p,n) )
WRAPPER4( BGl_schedulerzd2startz12zc0zz__ft_schedulerz00, ____scheduler_start, "scheduler-start!", (obj_t o), (o) )
WRAPPER4( BGl_schedulerzd2reactz12zc0zz__ft_schedulerz00, ____scheduler_react, "scheduler-react!", (obj_t o), (o) )

WRAPPER( bgl_make_mutex, MUTEX_TYPE_NUM, (obj_t n), (n) )
WRAPPER( bgl_make_nil_mutex, MUTEX_TYPE_NUM, (obj_t n), (n) )
WRAPPER( bgl_make_spinlock, SPINLOCK_TYPE_NUM, (obj_t n), (n) )
WRAPPER( bgl_make_condvar, CONDVAR_TYPE_NUM, (obj_t n), (n) )
WRAPPER( bgl_make_nil_condvar, CONDVAR_TYPE_NUM, (obj_t n), (n) )
