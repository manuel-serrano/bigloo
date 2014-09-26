/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cvector.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon May  8 14:16:24 1995                          */
/*    Last change :  Fri Sep 26 09:21:57 2014 (serrano)                */
/*    -------------------------------------------------------------    */
/*    C vector managment                                               */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_fill_vector ...                                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_fill_vector( obj_t bvector, long start, long end, obj_t init ) {
   obj_t *walker = (obj_t *)(&VECTOR_REF( bvector, start ));
   obj_t *stop = (obj_t *)(&VECTOR_REF( bvector, end ));

   while( walker < stop )
      *walker++ = init;

   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_fill_vector ...                                              */
/*---------------------------------------------------------------------*/
obj_t
fill_vector( obj_t bvector, long len, obj_t init ) {
   return bgl_fill_vector( bvector, 0, len, init );
}

/*---------------------------------------------------------------------*/
/*    create_vector ...                                                */
/*    -------------------------------------------------------------    */
/*    Any change to this function must be reflected in                 */
/*    ccontrol.c:opt_generic_entry                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
create_vector( int len ) {
   obj_t vector;

   if( len & ~(VECTOR_LENGTH_MASK) ) { 
      C_FAILURE( "create_vector", "vector too large", BINT( len ) );
      return BUNSPEC;
   } else {
      int byte_size;

      byte_size = VECTOR_SIZE + ( (len-1) * OBJ_SIZE );

      vector = GC_MALLOC( byte_size );

#if( !defined( TAG_VECTOR ) )
      vector->vector_t.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif		
      vector->vector_t.length = len;

      return BVECTOR( vector );
   }
}

/*---------------------------------------------------------------------*/
/*    create_vector_uncollectable ...                                  */
/*    -------------------------------------------------------------    */
/*    same as create_vector but the allocated vector is uncollectable  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
create_vector_uncollectable( int len ) {
   obj_t vector;
   
   if( len & ~(VECTOR_LENGTH_MASK) ) { 
      C_FAILURE( "create_vector", "vector too large", BINT( len ) );
      return BUNSPEC;
   } else {
      int byte_size;

      byte_size = VECTOR_SIZE + ( (len-1) * OBJ_SIZE );
      
      vector = GC_MALLOC_UNCOLLECTABLE( byte_size );

#if( !defined( TAG_VECTOR ) )
      vector->vector_t.header = MAKE_HEADER( VECTOR_TYPE, 0 );
#endif		
      vector->vector_t.length = len;

      return BVECTOR( vector );
   }
}

/*---------------------------------------------------------------------*/
/*    make_vector_uncollectable ...                                    */
/*    -------------------------------------------------------------    */
/*    same as create_vector but the allocated vector is uncollectable  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_vector_uncollectable( int len, obj_t init ) {
   obj_t vector;

   vector = create_vector_uncollectable( len );
   bgl_fill_vector( vector, 0, len, init );
	
   return vector;
}

/*---------------------------------------------------------------------*/
/*    make_vector_uncollectable ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_vector( int len, obj_t init ) {
   obj_t vector;

   vector = create_vector( len );
   bgl_fill_vector( vector, 0, len, init );
	
   return vector;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    sort_vector ...                                                  */
/*---------------------------------------------------------------------*/
obj_t
sort_vector( obj_t obj, obj_t proc ) {
   int i, j, incr, n;
   obj_t (*cb)();

   n = VECTOR_LENGTH( obj );

   if( VA_PROCEDUREP( proc ) )
      cb = (obj_t (*)())PROCEDURE_VA_ENTRY( proc );
   else
      cb = (obj_t (*)())PROCEDURE_ENTRY( proc );
   
   for( incr = n / 2; incr; incr /= 2 ) {
      for( i = incr; i < n; i++ ) {
	 for( j = i-incr; j >= 0; j -= incr ) {
	    if( PROCEDURE_ENTRY( proc )( proc,
					 VECTOR_REF( obj, j ),
					 VECTOR_REF( obj, j + incr ),
					 BEOA )
		!= BFALSE )
	       break;
	    else {
	       obj_t tmp = VECTOR_REF( obj, j + incr );
	       VECTOR_REF( obj, j + incr ) = VECTOR_REF( obj, j );
	       VECTOR_REF( obj, j ) = tmp;
	    }
	 }
      }
   }

   return obj;
}
