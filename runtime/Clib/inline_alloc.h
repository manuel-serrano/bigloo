/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/runtime/Clib/inline_alloc.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 26 15:43:27 2017                          */
/*    Last change :  Thu Apr 19 08:07:07 2018 (serrano)                */
/*    Copyright   :  2017-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Single-threaded Boehm allocations                                */
/*=====================================================================*/
#if( BGL_GC_VERSION >= 700 )
#  define GRANULE_SIZE 1
#endif

/*---------------------------------------------------------------------*/
/*    GC_INLINE_MALLOC_6xx                                             */
/*    -------------------------------------------------------------    */
/*    Inline allocation for GC <= 6.8                                  */
/*---------------------------------------------------------------------*/
#define GC_INLINE_MALLOC_6xx( res, size, default_alloc ) \
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
/*    GC_INLINE_MALLOC_7xx ...                                         */
/*    -------------------------------------------------------------    */
/*    Inline allocation for GC >= 7.0                                  */
/*---------------------------------------------------------------------*/
#define GC_INLINE_MALLOC_7xx( res, size, default_alloc ) \
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
/*    GC_INLINE_MALLOC ...                                             */
/*---------------------------------------------------------------------*/
#if( BGL_GC_VERSION < 700 )
#  define GC_INLINE_MALLOC GC_INLINE_MALLOC_6xx
#else
#  define GC_INLINE_MALLOC GC_INLINE_MALLOC_7xx
#endif

#if( BGL_GC_VERSION >= 750 && BGL_GC_VERSION < 762 )
#  define GC_objfreelist GC_freelists
#endif

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_pair ...                                                    */
/*---------------------------------------------------------------------*/
#ifndef BGL_MAKE_PAIR
#define BGL_MAKE_PAIR

static obj_t
alloc_make_pair( obj_t car, obj_t cdr ) {
   obj_t pair;

   pair = (obj_t)GC_MALLOC( PAIR_SIZE );
   BGL_INIT_PAIR( pair, car, cdr );

   return BPAIR( pair );
}

GC_API obj_t 
make_pair( obj_t car, obj_t cdr ) {
   obj_t pair;

   GC_INLINE_MALLOC( pair, PAIR_SIZE, alloc_make_pair( car, cdr ) );
   BGL_INIT_PAIR( pair, car, cdr );

   return BPAIR( pair );
}

#endif

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_epair ...                                                   */
/*---------------------------------------------------------------------*/
#ifndef BGL_MAKE_EPAIR
#define BGL_MAKE_EPAIR

static obj_t 
alloc_make_epair( obj_t car, obj_t cdr, obj_t cer ) {
   obj_t pair;

   pair = (obj_t)GC_MALLOC( EPAIR_SIZE );
   BGL_INIT_EPAIR( pair, car, cdr, cer );

   return BPAIR( pair );
}   

GC_API obj_t 
make_epair( obj_t car, obj_t cdr, obj_t cer ) {
   obj_t pair;
   
   GC_INLINE_MALLOC( pair, EPAIR_SIZE, alloc_make_epair( car, cdr, cer ) );
   BGL_INIT_EPAIR( pair, car, cdr, cer );

   return BPAIR( pair );
}

#endif

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_cell ...                                                    */
/*---------------------------------------------------------------------*/
#ifndef BGL_MAKE_CELL
#define BGL_MAKE_CELL

static obj_t
alloc_make_cell( obj_t val ) {
   obj_t cell;

   cell = (obj_t)GC_MALLOC( CELL_SIZE );
   BGL_INIT_CELL( cell, val );

   return BCELL( cell );
}

GC_API obj_t 
make_cell( obj_t val ) {
   obj_t cell;

   GC_INLINE_MALLOC( cell, CELL_SIZE, alloc_make_cell( val ) );
   BGL_INIT_CELL( cell, val );
   
   return BCELL( cell );
}

#endif

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_real ...                                                    */
/*---------------------------------------------------------------------*/
#ifndef BGL_MAKE_REAL
#define BGL_MAKE_REAL

#if( !BGL_NAN_TAGGING ) 
static obj_t
alloc_make_real( double d ) {
   obj_t real;

   real = (obj_t)GC_MALLOC_ATOMIC( REAL_SIZE );
   BGL_INIT_REAL( real, d );

   return BREAL( real );
}

GC_API obj_t
make_real( double d ) {
   obj_t real;

   GC_INLINE_MALLOC( real, REAL_SIZE, alloc_make_real( d ) );
   BGL_INIT_REAL( real, d );

   return BREAL( real );
}

#endif
#endif

/*---------------------------------------------------------------------*/
/*    alloc_make_belong ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_belong( long l ) {
   obj_t elong;

   elong = (obj_t)GC_MALLOC_ATOMIC( ELONG_SIZE );
   
   elong->elong.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
   elong->elong.val = l;

   return BREF( elong );
}

/*---------------------------------------------------------------------*/
/*    make_belong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_belong( long l ) {
   if(( BGL_ELONG_PREALLOC_MIN <= l) && (l < BGL_ELONG_PREALLOC_MAX) ) {
      return BREF( &belong_allocated[ (long)l - BGL_ELONG_PREALLOC_MIN ] );
   } else {
      obj_t elong;

      GC_INLINE_MALLOC( elong, ELONG_SIZE, alloc_make_belong( l ) );
      
      elong->elong.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
      elong->elong.val = l;

      return BREF( elong );
   }
}

/*---------------------------------------------------------------------*/
/*    alloc_make_bllong ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_bllong( BGL_LONGLONG_T l ) {
   obj_t llong;

   llong = (obj_t)GC_MALLOC_ATOMIC( LLONG_SIZE );
   
   llong->llong.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   llong->llong.val = l;

   return BREF( llong );
}

/*---------------------------------------------------------------------*/
/*    make_bllong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_bllong( BGL_LONGLONG_T l ) {
   obj_t llong;

   GC_INLINE_MALLOC( llong, LLONG_SIZE, alloc_make_bllong( l ) );

   llong->llong.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   llong->llong.val = l;

   return BREF( llong );
}

#if( !defined( BGL_CNST_SHIFT_INT32 ) )
/*---------------------------------------------------------------------*/
/*    alloc_make_bint32 ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_bint32( int32_t l ) {
   obj_t int32;

   int32 = (obj_t)GC_MALLOC_ATOMIC( BGL_INT32_SIZE );
   
   int32->sint32.header = MAKE_HEADER( INT32_TYPE, BGL_INT32_SIZE );
   int32->sint32.val = l;

   return BREF( int32 );
}

/*---------------------------------------------------------------------*/
/*    bgl_make_bint32 ...                                              */
/*---------------------------------------------------------------------*/
GC_API obj_t
bgl_make_bint32( int32_t l ) {
   if(( BGL_INT32_PREALLOC_MIN <= l) && (l < BGL_INT32_PREALLOC_MAX) ) {
      return BREF( &bint32_allocated[ l - BGL_INT32_PREALLOC_MIN ] );
   } else {
      obj_t int32;

      GC_INLINE_MALLOC( int32, BGL_INT32_SIZE, alloc_make_bint32( l ) );
      
      int32->sint32.header = MAKE_HEADER( INT32_TYPE, BGL_INT32_SIZE );
      int32->sint32.val = l;

      return BREF( int32 );
   }
}

/*---------------------------------------------------------------------*/
/*    alloc_make_buint32 ...                                           */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_buint32( uint32_t l ) {
   obj_t int32;

   int32 = (obj_t)GC_MALLOC_ATOMIC( BGL_UINT32_SIZE );
   
   int32->uint32.header = MAKE_HEADER( UINT32_TYPE, BGL_UINT32_SIZE );
   int32->uint32.val = l;

   return BREF( int32 );
}

/*---------------------------------------------------------------------*/
/*    bgl_make_buint32 ...                                             */
/*---------------------------------------------------------------------*/
GC_API obj_t
bgl_make_buint32( uint32_t l ) {
   if(l < BGL_UINT32_PREALLOC_MAX ) {
      return BREF( &buint32_allocated[ l ] );
   } else {
      obj_t int32;

      GC_INLINE_MALLOC( int32, BGL_UINT32_SIZE, alloc_make_buint32( l ) );
      
      int32->uint32.header = MAKE_HEADER( UINT32_TYPE, BGL_UINT32_SIZE );
      int32->uint32.val = l;

      return BREF( int32 );
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    alloc_make_bint64 ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_bint64( int64_t l ) {
   obj_t int64;

   int64 = (obj_t)GC_MALLOC_ATOMIC( BGL_INT64_SIZE );
   
   int64->sint64.header = MAKE_HEADER( INT64_TYPE, BGL_INT64_SIZE );
   int64->sint64.val = l;

   return BREF( int64 );
}

/*---------------------------------------------------------------------*/
/*    bgl_make_bint64 ...                                              */
/*---------------------------------------------------------------------*/
GC_API obj_t
bgl_make_bint64( int64_t l ) {
   obj_t int64;

   GC_INLINE_MALLOC( int64, BGL_INT64_SIZE, alloc_make_bint64( l ) );
      
   int64->sint64.header = MAKE_HEADER( INT64_TYPE, BGL_INT64_SIZE );
   int64->sint64.val = l;

   return BREF( int64 );
}

/*---------------------------------------------------------------------*/
/*    alloc_make_buint64 ...                                           */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_buint64( uint64_t l ) {
   obj_t int64;

   int64 = (obj_t)GC_MALLOC_ATOMIC( BGL_UINT64_SIZE );
   
   int64->uint64.header = MAKE_HEADER( UINT64_TYPE, BGL_UINT64_SIZE );
   int64->uint64.val = l;

   return BREF( int64 );
}

/*---------------------------------------------------------------------*/
/*    bgl_make_buint64 ...                                             */
/*---------------------------------------------------------------------*/
GC_API obj_t
bgl_make_buint64( uint64_t l ) {
   obj_t int64;

   GC_INLINE_MALLOC( int64, BGL_UINT64_SIZE, alloc_make_buint64( l ) );
      
   int64->uint64.header = MAKE_HEADER( UINT64_TYPE, BGL_UINT64_SIZE );
   int64->uint64.val = l;

   return BREF( int64 );
}
