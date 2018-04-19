/*=====================================================================*/
/*    .../project/bigloo/bigloo/runtime/Clib/inline_alloc_thread.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 26 15:50:11 2017                          */
/*    Last change :  Thu Apr 19 08:09:48 2018 (serrano)                */
/*    Copyright   :  2017-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Multi-threaded Boehm allocations                                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_pair ...                                                    */
/*---------------------------------------------------------------------*/
#ifndef BGL_MAKE_PAIR
#define BGL_MAKE_PAIR

GC_API obj_t
make_pair( obj_t car, obj_t cdr ) {
   obj_t pair;

   pair = GC_THREAD_MALLOC( PAIR_SIZE );
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

GC_API obj_t
make_epair( obj_t car, obj_t cdr, obj_t cer ) {
   obj_t pair;

   pair = GC_THREAD_MALLOC( EPAIR_SIZE );
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

GC_API obj_t
make_cell( obj_t val ) {
   obj_t cell;

   cell = GC_THREAD_MALLOC( CELL_SIZE );
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
GC_API obj_t
make_real( double real ) {
   obj_t a_real;

   a_real = GC_THREAD_MALLOC_ATOMIC( REAL_SIZE );
   BGL_INIT_REAL( a_real, real );

   return BREAL( a_real );
}
#endif

#endif

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    make_belong ...                                                  */
/*---------------------------------------------------------------------*/
GC_API obj_t
make_belong( long elong ) {
   obj_t a_elong;

   a_elong = GC_THREAD_MALLOC_ATOMIC( ELONG_SIZE );
   
   a_elong->elong.header = MAKE_HEADER( ELONG_TYPE, ELONG_SIZE );
   a_elong->elong.val = elong;
	
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
   
   a_llong->llong.header = MAKE_HEADER( LLONG_TYPE, LLONG_SIZE );
   a_llong->llong.val = llong;
	
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
   
   a_sint32->sint32.header = MAKE_HEADER( INT32_TYPE, BGL_INT32_SIZE );
   a_sint32->sint32.val = l;
	
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
   
   a_uint32->uint32.header = MAKE_HEADER( UINT32_TYPE, BGL_UINT32_SIZE );
   a_uint32->uint32.val = l;
	
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
   
   a_sint64->sint64.header = MAKE_HEADER( INT64_TYPE, BGL_INT64_SIZE );
   a_sint64->sint64.val = l;
	
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
   
   a_uint64->uint64.header = MAKE_HEADER( UINT64_TYPE, BGL_UINT64_SIZE );
   a_uint64->uint64.val = l;
	
   return BREF( a_uint64 );
}

