/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/csaw.c                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Mar  3 17:05:58 2016                          */
/*    Last change :  Sun Mar  6 09:32:11 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    C Saw memory management.                                         */
/*=====================================================================*/
#if( BGL_SAW == 1 ) 

#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    constants                                                        */
/*---------------------------------------------------------------------*/
#define BGL_SAW_NURSERY_SIZE (1024 * 64 * OBJ_SIZE)

/*---------------------------------------------------------------------*/
/*    nursery                                                          */
/*---------------------------------------------------------------------*/
bgl_saw_nursery_t bgl_saw_nursery;
long bgl_saw_nursery_size = BGL_SAW_NURSERY_SIZE;

/*---------------------------------------------------------------------*/
/*    copiers                                                          */
/*---------------------------------------------------------------------*/
static long bgl_saw_copiers_size;
static bgl_saw_copier_t *bgl_saw_copiers;

static obj_t bgl_saw_pair_copy( obj_t );
static obj_t bgl_saw_real_copy( obj_t );

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_saw_init ...                                                 */
/*---------------------------------------------------------------------*/
void
bgl_saw_init() {
   /* alloc the nursery */
   char *heap = (char *)GC_MALLOC( bgl_saw_nursery_size );
   bgl_saw_nursery.size = bgl_saw_nursery_size;
   bgl_saw_nursery.heap = heap;
   bgl_saw_nursery.alloc = heap;
   bgl_saw_nursery.backpool = (obj_t **)(heap + bgl_saw_nursery_size - OBJ_SIZE);
   bgl_saw_nursery.backptr = bgl_saw_nursery.backpool;

   /* builtin type info */
   bgl_saw_copiers_size = OBJECT_TYPE;
   bgl_saw_copiers = malloc( sizeof( bgl_saw_copier_t ) * bgl_saw_copiers_size );
   
   bgl_saw_gc_add_copier( PAIR_TYPE, bgl_saw_pair_copy );
   bgl_saw_gc_add_copier( REAL_TYPE, bgl_saw_real_copy );
   bgl_saw_gc_add_copier( VECTOR_TYPE, bgl_saw_vector_copy );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_saw_gc_add_copier ...                                        */
/*---------------------------------------------------------------------*/
void
bgl_saw_gc_add_copier( long type, bgl_saw_copier_t copier ) {
   if( type > bgl_saw_copiers_size ) {
      /* enlarge the copiers on demand */
      long new_size = sizeof( bgl_saw_copier_t ) * (type + 10);
      long old_size = bgl_saw_copiers_size * sizeof( bgl_saw_copier_t );
      bgl_saw_copier_t *old = bgl_saw_copiers;
      bgl_saw_copiers = malloc( new_size );

      memcpy( bgl_saw_copiers, old, old_size );
      bgl_saw_copiers_size = new_size;
   }

   bgl_saw_copiers[ type ] = copier;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_saw_gc ...                                                   */
/*---------------------------------------------------------------------*/
void
bgl_saw_gc() {
   obj_t **ptr = bgl_saw_nursery.backpool;

   while( ptr > bgl_saw_nursery.backptr ) {
      obj_t *field = *ptr;
      if( BYOUNGP( *field ) ) *field = bgl_saw_gc_copy( *field );
      ptr--;
   }

   bgl_saw_nursery.backptr = bgl_saw_nursery.backpool;
   bgl_saw_nursery.alloc = bgl_saw_nursery.heap;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_gc_copy ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_gc_copy( obj_t obj ) {
   if( TYPE( obj ) != NO_TYPE ) {
      obj_t new = bgl_saw_copiers[ TYPE( obj ) ]( obj );
      TYPE( obj ) == NO_TYPE;
      CAR( obj ) = new;
   }

   return CAR( obj );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_pair ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_pair( obj_t a, obj_t d ) {
#if !defined( __GNUC__ )
#  define __GNUC__   
   obj_t an_object;
#endif
   
   bgl_saw_gc();
   return MAKE_PAIR( a, d );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_extended_pair ...                                   */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_extended_pair( obj_t a, obj_t d, obj_t e ) {
#if !defined( __GNUC__ )
#  define __GNUC__   
   obj_t an_object;
#endif
   
   bgl_saw_gc();
   return MAKE_EXTENDED_PAIR( a, d, e );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_saw_pair_copy ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
bgl_saw_pair_copy( obj_t pair ) {
   if( EXTENDED_PAIRP( pair ) ) {
      return make_extended_pair( bgl_saw_gc_copy( CAR( pair ) ),
				 bgl_saw_gc_copy( CDR( pair ) ),
				 bgl_saw_gc_copy( CER( pair ) ) );
   } else {
      return make_pair( bgl_saw_gc_copy( CAR( pair ) ),
			bgl_saw_gc_copy( CDR( pair ) ) );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_saw_make_real ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_saw_make_real( double d ) {
#if !defined( __GNUC__ )
#  define __GNUC__   
   obj_t an_object;
#endif

   bgl_saw_gc();
   return DOUBLE_TO_REAL( d );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_saw_real_copy ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
bgl_saw_real_copy( obj_t real ) {
   return make_real( REAL_TO_DOUBLE( real ) );
}


#endif
