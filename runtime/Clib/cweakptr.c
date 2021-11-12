/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cweakptr.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Stephane Epardaud                                 */
/*    Creation    :  Wed Dec 13 15:32:17 CET 2006                      */
/*    Last change :  Fri Nov 12 08:52:41 2021 (serrano)                */
/*    -------------------------------------------------------------    */
/*    C weak pointer management                                        */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    data_getter ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
data_getter( obj_t ptr ) {
   if( WEAKPTR( ptr ).data == NULL ) {
      return BUNSPEC;
   } else {
      return *(WEAKPTR( ptr ).data);
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    ref_getter ...                                                   */
/*---------------------------------------------------------------------*/
static obj_t
ref_getter( obj_t ptr ) {
   return WEAKPTR( ptr ).ref;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_weakptr ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_weakptr( obj_t data, obj_t ref ) {
   obj_t ptr = GC_MALLOC( WEAKPTR_SIZE );
   obj_t *pdata = (obj_t *)GC_MALLOC_ATOMIC( sizeof( obj_t ) );

   *pdata = data;
   
   ptr->weakptr.header = MAKE_HEADER( WEAKPTR_TYPE, 0 );
   ptr->weakptr.data = pdata;
   ptr->weakptr.ref = ref;
   
   // check if data has been allocated by the GC.
   // constants and ints are not pointers
   if( POINTERP( data ) && GC_base( CREFSLOW( data ) ) != NULL ) {
      GC_general_register_disappearing_link( (obj_t *)&(ptr->weakptr.data), 
					     GC_base( CREFSLOW( data ) ) );
      GC_general_register_disappearing_link( &(ptr->weakptr.ref), 
					     GC_base( CREFSLOW( data ) ) );
   }
   
   return BREF( ptr );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_weakptr_data_set ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_weakptr_data_set( obj_t ptr, obj_t data ) {
  obj_t old_data;
  
  // first unset it
  
  // FIXME: for strong pointers we could do without the lock
  old_data = GC_call_with_alloc_lock( data_getter, ptr );

  // check if data has been allocated by the GC.
  // constants and ints are not pointers
  // in theory if old_data is NULL the link was already unregistered by the GC
  if( POINTERP( old_data ) && GC_base( old_data ) != NULL ) {
      GC_unregister_disappearing_link( (obj_t *)&(WEAKPTR( ptr ).data) );
      GC_unregister_disappearing_link( &(WEAKPTR( ptr ).ref) );
  }

  // then set it
  *(WEAKPTR( ptr )).data = data;
  
  // check if data has been allocated by the GC.
  // constants and ints are not pointers
  if( POINTERP( data ) && GC_base( CREFSLOW( data ) ) != NULL ) {
     // it's a real weak pointer
     GC_general_register_disappearing_link( (obj_t *)&(WEAKPTR( ptr ).data), 
					    GC_base( CREFSLOW( data ) ) );
     GC_general_register_disappearing_link( &(WEAKPTR( ptr ).ref), 
					    GC_base( CREFSLOW( data ) ) );
  }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_weakptr_data ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_weakptr_data( obj_t ptr ) {
   obj_t data;

   return GC_call_with_alloc_lock( data_getter, ptr );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_weakptr_ref_set ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_weakptr_ref_set( obj_t ptr, obj_t ref ) {
  obj_t old_ref = GC_call_with_alloc_lock( ref_getter, ptr );
  obj_t old_data = GC_call_with_alloc_lock( data_getter, ptr );

  // check if ref has been allocated by the GC.
  // constants and ints are not pointers
  // in theory if old_ref is NULL the link was already unregistered by the GC
  if( POINTERP( old_ref ) && GC_base( old_ref ) != NULL ) {
      GC_unregister_disappearing_link( &(WEAKPTR( ptr ).ref) );
  }

  // then set it
  WEAKPTR( ptr ).ref = ref;
     
  // check if ref has been allocated by the GC.
  // constants and ints are not pointers
  if( POINTERP( old_data ) && GC_base( CREFSLOW( old_data ) ) != NULL ) {
     // it's a real weak pointer
     WEAKPTR( ptr ).ref = ref;
     GC_general_register_disappearing_link( &(WEAKPTR( ptr ).ref), 
					    GC_base( CREFSLOW( old_data ) ) );
  }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_weakptr_ref ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_weakptr_ref( obj_t ptr ) {
   obj_t ref;

   // FIXME: for strong pointers we could do without the lock
   ref = GC_call_with_alloc_lock( ref_getter, ptr );
   return ref == NULL ? BUNSPEC : ref;
}

