/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/csrfi4.c                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov  7 11:58:06 2006                          */
/*    Last change :  Wed Jul  4 08:47:36 2012 (serrano)                */
/*    Copyright   :  2006-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C SRFI4 side                                                     */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    alloc_hvector ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
alloc_hvector( int len, int isize, int type ) {
   int byte_size = HVECTOR_SIZE + ( len * isize );
#if( defined( GC_THREADS ) && defined( THREAD_LOCAL_ALLOC ) )
   obj_t vector = GC_THREAD_MALLOC_ATOMIC( byte_size );
#else
   obj_t vector = GC_MALLOC_ATOMIC( byte_size );
#endif
    
   vector->hvector_t.header = MAKE_HEADER( type, 0 );
   vector->hvector_t.length = len;

   return BREF( vector );
}



