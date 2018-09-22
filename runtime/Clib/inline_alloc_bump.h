/*=====================================================================*/
/*    .../project/bigloo/bigloo/runtime/Clib/inline_alloc_bump.h       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Oct 26 15:26:00 2017                          */
/*    Last change :  Sat Sep 22 07:13:51 2018 (serrano)                */
/*    Copyright   :  2017-18 Manuel Serrano                            */
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
/*    GC_API obj_t                                                     */
/*    make_pair ...                                                    */
/*---------------------------------------------------------------------*/
#ifndef BGL_MAKE_PAIR
#define BGL_MAKE_PAIR

static obj_t
alloc_make_pair( obj_t car, obj_t cdr ) {
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - PAIR_SIZE;
   
   BGL_INIT_PAIR( (obj_t)bgl_gc_ptr_head, car, cdr );

   return BPAIR( (obj_t)bgl_gc_ptr_head );
}

GC_API obj_t 
make_pair( obj_t car, obj_t cdr ) {
   if( BGL_GC_BUMP_MALLOC( PAIR_SIZE ) ) {
      BGL_INIT_PAIR( (obj_t)bgl_gc_ptr_head, car, cdr );
      return BPAIR( (obj_t)bgl_gc_ptr_head );
   } else {
      return alloc_make_pair( car, cdr );
   }
}

#endif

/*---------------------------------------------------------------------*/
/*    GC_API obj_t                                                     */
/*    alloc_make_epair ...                                             */
/*---------------------------------------------------------------------*/
#ifndef BGL_MAKE_EPAIR
#define BGL_MAKE_EPAIR

static obj_t
alloc_make_epair( obj_t car, obj_t cdr, obj_t cer ) {
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - EPAIR_SIZE;
   
   BGL_INIT_EPAIR( (obj_t)bgl_gc_ptr_head, car, cdr, cer );
   return BPAIR( (obj_t)bgl_gc_ptr_head );
}

GC_API obj_t 
make_epair( obj_t car, obj_t cdr, obj_t cer ) {
   if( BGL_GC_BUMP_MALLOC( EPAIR_SIZE ) ) {
      BGL_INIT_EPAIR( (obj_t)bgl_gc_ptr_head, car, cdr, cer );
      return BPAIR( (obj_t)bgl_gc_ptr_head );
   } else {
      return alloc_make_epair( car, cdr, cer );
   }
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
   bgl_gc_ptr_base = (char *)GC_MALLOC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - CELL_SIZE;
   
   BGL_INIT_CELL( (obj_t)bgl_gc_ptr_head, val );
   
   return BCELL( (obj_t)bgl_gc_ptr_head );
}

GC_API obj_t 
make_cell( obj_t val ) {
   if( BGL_GC_BUMP_MALLOC( CELL_SIZE ) ) {
      BGL_INIT_CELL( (obj_t)bgl_gc_ptr_head, val );
      return BCELL( (obj_t)bgl_gc_ptr_head );
   } else {
      return alloc_make_cell( val );
   }
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
   bgl_gc_ptr_base = (char *)GC_MALLOC_ATOMIC( BGL_BUMP_CARD_SIZE );
   bgl_gc_ptr_head = bgl_gc_ptr_base + BGL_BUMP_CARD_SIZE - REAL_SIZE;
   
   BGL_INIT_REAL( (obj_t)bgl_gc_ptr_head, d );

   return BREAL( (obj_t)bgl_gc_ptr_head );
}

GC_API obj_t 
make_real( double d ) {
   if( BGL_GC_BUMP_MALLOC( REAL_SIZE ) ) {
      BGL_INIT_REAL( (obj_t)bgl_gc_ptr_head, d );
      return BREAL( (obj_t)bgl_gc_ptr_head );
   } else {
      return alloc_make_real( d );
   }
}
#endif

#endif

