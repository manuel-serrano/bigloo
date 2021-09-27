/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cweakptr.c       */
/*    -------------------------------------------------------------    */
/*    Author      :  Stephane Epardaud                                 */
/*    Creation    :  Wed Dec 13 15:32:17 CET 2006                      */
/*    Last change :  Mon Sep 27 06:58:57 2021 (serrano)                */
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
   return WEAKPTR( ptr ).data;
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
   obj_t ptr = GC_MALLOC_ATOMIC( WEAKPTR_SIZE );

   ptr->weakptr.header = MAKE_HEADER( WEAKPTR_TYPE, 0 );
   ptr->weakptr.data = data;
   
   // check if data has been allocated by the GC.
   // constants and ints are not pointers
   if( POINTERP( data ) && GC_base( CREFSLOW( data ) ) != NULL ) {
      // make a real weak pointer
      GC_general_register_disappearing_link( &(ptr->weakptr.data), 
					     GC_base( CREFSLOW( data ) ) );
   }
   if( POINTERP( ref ) && GC_base( CREFSLOW( ref ) ) != NULL ) {
      // make a real weak pointer
      GC_general_register_disappearing_link( &(ptr->weakptr.ref), 
					     GC_base( CREFSLOW( ref ) ) );
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
      GC_unregister_disappearing_link( &(WEAKPTR( ptr ).data) );
  }

  // then set it
  // check if data has been allocated by the GC.
  // constants and ints are not pointers
  if( POINTERP( data ) && GC_base( CREFSLOW( data ) ) != NULL ) {
     // it's a real weak pointer
     WEAKPTR( ptr ).data = data;
     GC_general_register_disappearing_link( &(WEAKPTR( ptr ).data), 
					    GC_base( CREFSLOW( data ) ) );
  } else {
     // If not, we need to not tell the GC about the disappearing link, and our
     // weak pointer will just be a regular hard pointer.
     WEAKPTR( ptr ).data = data;
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

   // FIXME: for strong pointers we could do without the lock
   data = GC_call_with_alloc_lock( data_getter, ptr );
   return data == NULL ? BUNSPEC : data;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_weakptr_ref_set ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_weakptr_ref_set( obj_t ptr, obj_t ref ) {
  obj_t old_ref;
  
  // first unset it
  
  // FIXME: for strong pointers we could do without the lock
  old_ref = GC_call_with_alloc_lock( ref_getter, ptr );

  // check if ref has been allocated by the GC.
  // constants and ints are not pointers
  // in theory if old_ref is NULL the link was already unregistered by the GC
  if( POINTERP( old_ref ) && GC_base( old_ref ) != NULL ) {
      GC_unregister_disappearing_link( &(WEAKPTR( ptr ).ref) );
  }

  // then set it
  // check if ref has been allocated by the GC.
  // constants and ints are not pointers
  if( POINTERP( ref ) && GC_base( CREFSLOW( ref ) ) != NULL ) {
     // it's a real weak pointer
     WEAKPTR( ptr ).ref = ref;
     GC_general_register_disappearing_link( &(WEAKPTR( ptr ).ref), 
					    GC_base( CREFSLOW( ref ) ) );
  } else {
     // If not, we need to not tell the GC about the disappearing link, and our
     // weak pointer will just be a regular hard pointer.
     WEAKPTR( ptr ).ref = ref;
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

