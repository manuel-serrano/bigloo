/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/inline_alloc.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Sep 21 15:33:10 1994                          */
/*    Last change :  Sat Nov 20 05:57:57 2010 (serrano)                */
/*    -------------------------------------------------------------    */
/*    On fait des fonctions d'allocations specialisees pour les cons   */
/*    et les flottants.                                                */
/*=====================================================================*/
#ifndef GC_PRIVATE_H
#  include <private/gc_priv.h>
#endif
#undef abs

#include <bigloo.h>

#if( (BGL_GC == BGL_BOEHM_GC) && BGL_GC_CUSTOM && !defined( BGL_GC_THREADS ))

#if( BGL_GC_VERSION >= 700 )
#  define GRANULE_SIZE 1
#endif
  
/*---------------------------------------------------------------------*/
/*    GC_INLINE_ALLOC_6xx                                              */
/*    -------------------------------------------------------------    */
/*    Inline allocation for GC <= 6.8                                  */
/*---------------------------------------------------------------------*/
#define GC_INLINE_ALLOC_6xx( res, size, default_alloc ) \
   ptr_t op; \
   ptr_t *opp; \
   DCL_LOCK_STATE; \
   \
   opp = (void **)&(GC_objfreelist[ (long)ALIGNED_WORDS( size ) ]); \
   FASTLOCK(); \
   if( !FASTLOCK_SUCCEEDED() || (op = *opp) == 0 ) { \
      FASTUNLOCK(); \
      return default_alloc; \
   } \
   *opp = obj_link( op ); \
   GC_words_allocd += (long)ALIGNED_WORDS( size ); \
   FASTUNLOCK(); \
   \
   res = (obj_t)op;

/*---------------------------------------------------------------------*/
/*    GC_INLINE_ALLOC_7xx ...                                          */
/*    -------------------------------------------------------------    */
/*    Inline allocation for GC >= 7.0                                  */
/*---------------------------------------------------------------------*/
#define GC_INLINE_ALLOC_7xx( res, size, default_alloc ) \
   void *op; \
   void **opp; \
   size_t lg; \
   DCL_LOCK_STATE; \
   \
   lg = GC_size_map[ size ]; \
   opp = (void **)&(GC_objfreelist[ lg ]); \
   LOCK(); \
   \
   if( EXPECT((op = *opp) == 0, 0) ) { \
      UNLOCK(); \
      return default_alloc; \
   }  \
   *opp = obj_link( op ); \
   GC_bytes_allocd += GRANULES_TO_BYTES( lg ); \
   UNLOCK(); \
   \
   res = (obj_t)op;

/*---------------------------------------------------------------------*/
/*    GC_INLINE_ALLOC ...                                              */
/*---------------------------------------------------------------------*/
#if( BGL_GC_VERSION < 700 )
#  define GC_INLINE_ALLOC GC_INLINE_ALLOC_6xx
#else
#  define GC_INLINE_ALLOC GC_INLINE_ALLOC_7xx
#endif

/*---------------------------------------------------------------------*/
/*    alloc_make_pair ...                                              */
/*---------------------------------------------------------------------*/
static obj_t 
alloc_make_pair( obj_t car, obj_t cdr ) {
   obj_t pair;
   
   pair = (obj_t)GC_MALLOC( PAIR_SIZE );

#if( !defined( TAG_PAIR ) )
   pair->pair_t.header = MAKE_HEADER( PAIR_TYPE, PAIR_SIZE );
#endif
   pair->pair_t.car = car;
   pair->pair_t.cdr = cdr;
   
   return BPAIR( pair );
}   

/*---------------------------------------------------------------------*/
/*    make_pair ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t 
make_pair( obj_t car, obj_t cdr ) {
   obj_t pair;
   
   GC_INLINE_ALLOC( pair, PAIR_SIZE, alloc_make_pair( car, cdr ) );

#if( !defined( TAG_PAIR ) )
   pair->pair_t.header = MAKE_HEADER( PAIR_TYPE, PAIR_SIZE );
#endif
   pair->pair_t.car = car;
   pair->pair_t.cdr = cdr;
   
   return BPAIR( pair );
}

/*---------------------------------------------------------------------*/
/*    alloc_make_extended_pair ...                                     */
/*---------------------------------------------------------------------*/
static obj_t 
alloc_make_extended_pair( obj_t car, obj_t cdr, obj_t cer ) {
   obj_t pair;

   pair = (obj_t)GC_MALLOC( EXTENDED_PAIR_SIZE );

#if( !defined( TAG_PAIR ) )
   pair->pair_t.header = MAKE_HEADER( PAIR_TYPE, PAIR_SIZE );
#endif
   pair->extended_pair_t.car = car;
   pair->extended_pair_t.cdr = cdr;
   pair->extended_pair_t.cer = cer;
   pair->extended_pair_t.eheader = BINT( EXTENDED_PAIR_TYPE );
   
   return BPAIR( pair );
}   

/*---------------------------------------------------------------------*/
/*    make_extended_pair ...                                           */
/*---------------------------------------------------------------------*/
GC_API obj_t 
make_extended_pair( obj_t car, obj_t cdr, obj_t cer ) {
   obj_t pair;
   
   GC_INLINE_ALLOC( pair,
		    EXTENDED_PAIR_SIZE,
		    alloc_make_extended_pair( car, cdr, cer ) );

#if( !defined( TAG_PAIR ) )
   pair->pair_t.header = MAKE_HEADER( PAIR_TYPE, EXTENDED_PAIR_SIZE );
#endif
   pair->extended_pair_t.car = car;
   pair->extended_pair_t.cdr = cdr;
   pair->extended_pair_t.cer = cer;
   pair->extended_pair_t.eheader = BINT( EXTENDED_PAIR_TYPE );
   
   return BPAIR( pair );
}

/*---------------------------------------------------------------------*/
/*    alloc_make_cell ...                                              */
/*---------------------------------------------------------------------*/
static obj_t 
alloc_make_cell( obj_t val ) {
   obj_t cell;

   cell = (obj_t)GC_MALLOC( CELL_SIZE );

#if( !defined( TAG_CELL ) )
   cell->cell_t.header = MAKE_HEADER( CELL_TYPE, CELL_SIZE );
#endif
   cell->cell_t.val = val;
   
   return BCELL( cell );
}   

/*---------------------------------------------------------------------*/
/*    make_cell ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t 
make_cell( obj_t val ) {
   obj_t cell;
   
   GC_INLINE_ALLOC( cell, CELL_SIZE, alloc_make_cell( val ) );

#if( !defined( TAG_CELL ) )
   cell->cell_t.header = MAKE_HEADER( CELL_TYPE, CELL_SIZE );
#endif
   cell->cell_t.val = val;
   
   return BCELL( cell );
}

/*---------------------------------------------------------------------*/
/*    alloc_make_real ...                                              */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_real( double d ) {
   obj_t real;

   real = (obj_t)GC_MALLOC_ATOMIC( REAL_SIZE );
   
#if( !defined( TAG_REAL ) )
   real->real_t.header = MAKE_HEADER( REAL_TYPE, REAL_SIZE );
#endif
   real->real_t.real = d;

   return BREAL( real );
}

/*---------------------------------------------------------------------*/
/*    make_real ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_real( double d ) {
   obj_t real;

   GC_INLINE_ALLOC( real, REAL_SIZE, alloc_make_real( d ) );

#if( !defined( TAG_REAL ) )
   real->real_t.header = MAKE_HEADER( REAL_TYPE, REAL_SIZE );
#endif
   real->real_t.real = d;

   return BREAL( real );
}

/*---------------------------------------------------------------------*/
/*    alloc_make_belong ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_belong( long l ) {
   obj_t elong;

   elong = (obj_t)GC_MALLOC_ATOMIC( ELONG_SIZE );
   
   elong->elong_t.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
   elong->elong_t.elong = l;

   return BREF( elong );
}

/*---------------------------------------------------------------------*/
/*    make_belong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_belong( long l ) {
   obj_t elong;

   GC_INLINE_ALLOC( elong, ELONG_SIZE, alloc_make_belong( l ) );

   elong->elong_t.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
   elong->elong_t.elong = l;

   return BREF( elong );
}

/*---------------------------------------------------------------------*/
/*    alloc_make_bllong ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_bllong( BGL_LONGLONG_T l ) {
   obj_t llong;

   llong = (obj_t)GC_MALLOC_ATOMIC( LLONG_SIZE );
   
   llong->llong_t.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   llong->llong_t.llong = l;

   return BREF( llong );
}

/*---------------------------------------------------------------------*/
/*    make_bllong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_bllong( BGL_LONGLONG_T l ) {
   obj_t llong;

   GC_INLINE_ALLOC( llong, LLONG_SIZE, alloc_make_bllong( l ) );

   llong->llong_t.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   llong->llong_t.llong = l;

   return BREF( llong );
}

#else /* BGL_GC == BGL_BOEHM_GC && BGL_GC_CUSTOM && !defined(BGL_GC_THREADS) */

/*---------------------------------------------------------------------*/
/*    make_pair ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_pair( obj_t car, obj_t cdr ) {
   obj_t pair;

#if( defined( GC_THREADS ) && defined( THREAD_LOCAL_ALLOC ) )
   pair = GC_THREAD_MALLOC( PAIR_SIZE );
#else      
   pair = GC_MALLOC( PAIR_SIZE );
#endif      
#if( !defined( TAG_PAIR ) )
   pair->pair_t.header = MAKE_HEADER( PAIR_TYPE, PAIR_SIZE );
#endif
   pair->pair_t.car = car;
   pair->pair_t.cdr = cdr;
   
   return BPAIR( pair );
}

/*---------------------------------------------------------------------*/
/*    make_extended_pair ...                                           */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_extended_pair( obj_t car, obj_t cdr, obj_t cer ) {
   obj_t pair;

#if( defined( GC_THREADS ) && defined( THREAD_LOCAL_ALLOC ) )
   pair = GC_THREAD_MALLOC( EXTENDED_PAIR_SIZE );
#else      
   pair = GC_MALLOC( EXTENDED_PAIR_SIZE );
#endif      
#if( !defined( TAG_PAIR ) )
   pair->pair_t.header = MAKE_HEADER( PAIR_TYPE, EXTENEDED_PAIR_SIZE );
#endif
   pair->extended_pair_t.car = car;
   pair->extended_pair_t.cdr = cdr;
   pair->extended_pair_t.cer = cer;
   pair->extended_pair_t.eheader = BINT( EXTENDED_PAIR_TYPE );
   
   return BPAIR( pair );
}

/*---------------------------------------------------------------------*/
/*    make_cell ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_cell( obj_t val ) {
   obj_t cell;

#if( defined( GC_THREADS ) && defined( THREAD_LOCAL_ALLOC ) )
   cell = GC_THREAD_MALLOC( CELL_SIZE );
#else      
   cell = GC_MALLOC( CELL_SIZE );
#endif      
#if( !defined( TAG_CELL ) )
   cell->cell_t.header = MAKE_HEADER( CELL_TYPE, CELL_SIZE );
#endif
   cell->cell_t.val = val;
   
   return BCELL( cell );
}

/*---------------------------------------------------------------------*/
/*    make_real ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_real( double real ) {
   obj_t a_real;

#if( defined( GC_THREADS ) && defined( THREAD_LOCAL_ALLOC ) )
   a_real = GC_THREAD_MALLOC_ATOMIC( REAL_SIZE );
#else
   a_real = GC_MALLOC_ATOMIC( REAL_SIZE );
#endif
   
#if( !defined( TAG_REAL ) )
   a_real->real_t.header = MAKE_HEADER( REAL_TYPE, REAL_SIZE );
#endif
   a_real->real_t.real = real;
	
   return BREAL( a_real );
}

/*---------------------------------------------------------------------*/
/*    make_belong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_belong( long elong ) {
   obj_t a_elong;

#if( defined( GC_THREADS ) && defined( THREAD_LOCAL_ALLOC ) )
   a_elong = GC_THREAD_MALLOC_ATOMIC( ELONG_SIZE );
#else
   a_elong = GC_MALLOC_ATOMIC( ELONG_SIZE );
#endif
   
   a_elong->elong_t.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
   a_elong->elong_t.elong = elong;
	
   return BREF( a_elong );
}

/*---------------------------------------------------------------------*/
/*    make_bllong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_bllong( BGL_LONGLONG_T llong ) {
   obj_t a_llong;

#if( defined( GC_THREADS ) && defined( THREAD_LOCAL_ALLOC ) )
   a_llong = GC_THREAD_MALLOC_ATOMIC( LLONG_SIZE );
#else
   a_llong = GC_MALLOC_ATOMIC( LLONG_SIZE );
#endif
   
   a_llong->llong_t.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   a_llong->llong_t.llong = llong;
	
   return BREF( a_llong );
}
#endif
