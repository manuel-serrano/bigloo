/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/csrfi4.c         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov  7 11:58:06 2006                          */
/*    Last change :  Tue Apr 17 08:01:33 2018 (serrano)                */
/*    Copyright   :  2006-18 Manuel Serrano                            */
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
    
   vector->hvector.header = MAKE_HEADER( type, 0 );
   vector->hvector.length = len;

   return BREF( vector );
}



