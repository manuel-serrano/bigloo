/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/inline_alloc_bump.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 26 15:26:00 2017                          */
/*    Last change :  Thu Oct 26 22:50:50 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Bump gc allocations                                              */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    CARD_SIZE                                                        */
/*---------------------------------------------------------------------*/
#define BGL_BUMP_CARD_SIZE 128

/*---------------------------------------------------------------------*/
/*    Allocation pointers                                              */
/*---------------------------------------------------------------------*/
#if( !defined( BGL_GC_THREADS ) )
static char *bgl_gc_ptr_head = (char *)BGL_BUMP_CARD_SIZE;
static char *bgl_gc_ptr_base = (char *)BGL_BUMP_CARD_SIZE;
#else
static BGL_THREAD_DECL char *bgl_gc_ptr_head = (char *)BGL_BUMP_CARD_SIZE;
static BGL_THREAD_DECL char *bgl_gc_ptr_base = (char *)BGL_BUMP_CARD_SIZE;
#endif

#define BGL_GC_BUMP_MALLOC( sz ) \
  (bgl_gc_ptr_head -= sz, BGL_LIKELY( bgl_gc_ptr_head >= bgl_gc_ptr_base ))

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    alloc_make_pair ...                                              */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_pair( obj_t car, obj_t cdr ) {
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - PAIR_SIZE;
   
   BGL_INIT_PAIR( (obj_t)bgl_gc_ptr_head, car, cdr );

   return BPAIR( (obj_t)bgl_gc_ptr_head );
}

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_pair ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t 
make_pair( obj_t car, obj_t cdr ) {
   if( BGL_GC_BUMP_MALLOC( PAIR_SIZE ) ) {
      BGL_INIT_PAIR( (obj_t)bgl_gc_ptr_head, car, cdr );
      return BPAIR( (obj_t)bgl_gc_ptr_head );
   } else {
      return alloc_make_pair( car, cdr );
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    alloc_make_epair ...                                             */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_epair( obj_t car, obj_t cdr, obj_t cer ) {
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - EPAIR_SIZE;
   
   BGL_INIT_EPAIR( (obj_t)bgl_gc_ptr_head, car, cdr, cer );
   return BPAIR( (obj_t)bgl_gc_ptr_head );
}

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_epair ...                                                   */
/*---------------------------------------------------------------------*/
GC_API obj_t 
make_epair( obj_t car, obj_t cdr, obj_t cer ) {
   if( BGL_GC_BUMP_MALLOC( EPAIR_SIZE ) ) {
      BGL_INIT_EPAIR( (obj_t)bgl_gc_ptr_head, car, cdr, cer );
      return BPAIR( (obj_t)bgl_gc_ptr_head );
   } else {
      return alloc_make_epair( car, cdr, cer );
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    alloc_make_cell ...                                              */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_cell( obj_t val ) {
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - CELL_SIZE;
   
   BGL_INIT_CELL( (obj_t)bgl_gc_ptr_head, val );
   
   return BCELL( (obj_t)bgl_gc_ptr_head );
}

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_cell ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t 
make_cell( obj_t val ) {
   if( BGL_GC_BUMP_MALLOC( CELL_SIZE ) ) {
      BGL_INIT_CELL( (obj_t)bgl_gc_ptr_head, val );
      return BCELL( (obj_t)bgl_gc_ptr_head );
   } else {
      return alloc_make_cell( val );
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    alloc_make_real ...                                              */
/*---------------------------------------------------------------------*/
static obj_t
alloc_make_real( double d ) {
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - REAL_SIZE;
   
   BGL_INIT_REAL( (obj_t)bgl_gc_ptr_head, d );

   return BREAL( (obj_t)bgl_gc_ptr_head );
}

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_real ...                                                    */
/*---------------------------------------------------------------------*/
GC_API obj_t 
make_real( double d ) {
   if( BGL_GC_BUMP_MALLOC( REAL_SIZE ) ) {
      BGL_INIT_REAL( (obj_t)bgl_gc_ptr_head, d );
      return BREAL( (obj_t)bgl_gc_ptr_head );
   } else {
      return alloc_make_real( d );
   }
}

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_belong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_belong( long elong ) {
   obj_t a_elong;

   a_elong = GC_THREAD_MALLOC_ATOMIC( ELONG_SIZE );
   
   a_elong->elong_t.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
   a_elong->elong_t.elong = elong;
	
   return BREF( a_elong );
}

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_bllong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_bllong( BGL_LONGLONG_T llong ) {
   obj_t a_llong;

   a_llong = GC_THREAD_MALLOC_ATOMIC( LLONG_SIZE );
   
   a_llong->llong_t.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   a_llong->llong_t.llong = llong;
	
   return BREF( a_llong );
}

#if( !defined( BGL_CNST_SHIFT_INT32 ) )
/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    bgl_make_bint32 ...                                              */
/*---------------------------------------------------------------------*/
GC_API obj_t
bgl_make_bint32( int32_t l ) {
   obj_t a_sint32;

   a_sint32 = GC_THREAD_MALLOC_ATOMIC( BGL_INT32_SIZE );
   
   a_sint32->sint32_t.header = MAKE_HEADER( INT32_TYPE, BGL_INT32_SIZE );
   a_sint32->sint32_t.val = l;
	
   return BREF( a_sint32 );
}

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    bgl_make_buint32 ...                                             */
/*---------------------------------------------------------------------*/
GC_API obj_t
bgl_make_buint32( uint32_t l ) {
   obj_t a_uint32;

   a_uint32 = GC_THREAD_MALLOC_ATOMIC( BGL_UINT32_SIZE );
   
   a_uint32->uint32_t.header = MAKE_HEADER( UINT32_TYPE, BGL_UINT32_SIZE );
   a_uint32->uint32_t.val = l;
	
   return BREF( a_uint32 );
}
#endif

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    bgl_make_bint64 ...                                              */
/*---------------------------------------------------------------------*/
GC_API obj_t
bgl_make_bint64( int64_t l ) {
   obj_t a_sint64;

   a_sint64 = GC_THREAD_MALLOC_ATOMIC( BGL_INT64_SIZE );
   
   a_sint64->sint64_t.header = MAKE_HEADER( INT64_TYPE, BGL_INT64_SIZE );
   a_sint64->sint64_t.val = l;
	
   return BREF( a_sint64 );
}

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    bgl_make_buint64 ...                                             */
/*---------------------------------------------------------------------*/
GC_API obj_t
bgl_make_buint64( uint64_t l ) {
   obj_t a_uint64;

   a_uint64 = GC_THREAD_MALLOC_ATOMIC( BGL_UINT64_SIZE );
   
   a_uint64->uint64_t.header = MAKE_HEADER( UINT64_TYPE, BGL_UINT64_SIZE );
   a_uint64->uint64_t.val = l;
	
   return BREF( a_uint64 );
}

