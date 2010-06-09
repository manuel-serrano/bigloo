/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/non_inline_alloc.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Apr 21 08:44:03 2007                          */
/*    Last change :  Tue Jun  8 06:57:12 2010 (serrano)                */
/*    Copyright   :  2007-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Non inlined allocated that are used when the GC is not           */
/*    the Boehm collector, the GC is not custom, the C compiler        */
/*    is not GCC.                                                      */
/*=====================================================================*/
#include <bigloo.h>

#define GC_PRIVATE_H
#define GC_API BGL_RUNTIME_DEF

#if( !BGL_GC_CUSTOM )
#  include "inline_alloc.c"
#endif
