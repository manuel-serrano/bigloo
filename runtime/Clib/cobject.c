/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cobject.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 28 08:08:56 2012                          */
/*    Last change :  Tue Apr 17 08:03:20 2018 (serrano)                */
/*    Copyright   :  2012-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C Bigloo object management.                                      */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bmem_set_allocation_type ...                                     */
/*---------------------------------------------------------------------*/
void
bmem_set_allocation_type( long tname, long offset ) {
   /* just a place holder for bmem */
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_class ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_make_class( obj_t name, obj_t module, long num,
		obj_t super, obj_t sub,
		obj_t alloc, long hash,
		obj_t fd, obj_t allfd,
		obj_t constr, obj_t virt, obj_t new, obj_t nil, obj_t shrink,
		long depth, 
		obj_t evdata ) {
   obj_t klass;
   long dspsz = depth < BGL_OBJECT_MIN_DISPLAY_SIZE ?
      BGL_OBJECT_MIN_DISPLAY_SIZE : depth;
   //dspsz = depth;

   klass = GC_MALLOC_UNCOLLECTABLE( BGL_CLASS_SIZE + (sizeof(obj_t) * dspsz) );

   klass->class.header = MAKE_HEADER( CLASS_TYPE, 0 );
   klass->class.name = name;

   klass->class.index = num;
   klass->class.super = super;
   klass->class.subclasses = sub;
   klass->class.alloc_fun = alloc;
   klass->class.hash = hash;
   klass->class.direct_fields = fd;
   klass->class.constructor = constr;
   klass->class.virtual_fields = virt;
   klass->class.new_fun = new;
   klass->class.nil_fun = nil;
   klass->class.shrink = shrink;
   klass->class.evdata = evdata;
   klass->class.all_fields = allfd;
   klass->class.module = module;
   klass->class.depth = depth;
   klass->class.nil = BFALSE;
   klass->class.depth = depth;

   if( depth > 0 ) {
      memcpy( &(klass->class.ancestor0),
	      &(BGL_CLASS_ANCESTORS_REF( super, 0 )),
	      sizeof( obj_t ) * (depth - 1) );
	 (&klass->class.ancestor0)[ depth - 1 ] = super;
      if( depth < BGL_OBJECT_MIN_DISPLAY_SIZE ) {
	 (&klass->class.ancestor0)[ depth ] = BREF( klass );
      }
   }

   return BREF( klass );
}
