/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cobject.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Oct 28 08:08:56 2012                          */
/*    Last change :  Sun Oct 28 19:09:51 2012 (serrano)                */
/*    Copyright   :  2012 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    C Bigloo object management.                                      */
/*=====================================================================*/
#include <bigloo.h>

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

   klass = GC_MALLOC_UNCOLLECTABLE( BGL_CLASS_SIZE + (sizeof( obj_t ) * depth) );

   klass->class_t.header = MAKE_HEADER( CLASS_TYPE, 0 );
   klass->class_t.name = name;

   klass->class_t.index = num;
   klass->class_t.super = super;
   klass->class_t.subclasses = sub;
   klass->class_t.alloc_fun = alloc;
   klass->class_t.hash = hash;
   klass->class_t.direct_fields = fd;
   klass->class_t.constructor = constr;
   klass->class_t.virtual_fields = virt;
   klass->class_t.new_fun = new;
   klass->class_t.nil_fun = nil;
   klass->class_t.shrink = shrink;
   klass->class_t.evdata = evdata;
   klass->class_t.all_fields = allfd;
   klass->class_t.module = module;
   klass->class_t.depth = depth;
   klass->class_t.nil = BFALSE;
   klass->class_t.depth = depth;

   if( depth > 0 ) {
      memcpy( &(klass->class_t.ancestor0),
	      &(BGL_CLASS_ANCESTORS_REF( super, 0 )),
	      sizeof( obj_t ) * (depth - 1) );
      (&klass->class_t.ancestor0)[ depth - 1 ] = super;
   }

   return BREF( klass );
}
