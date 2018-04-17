/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/cstruct.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 16 09:28:53 1992                          */
/*    Last change :  Tue Apr 17 07:58:52 2018 (serrano)                */
/*    Copyright   :  2000-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Structures constructions.                                        */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    set_struct ...                                                   */
/*---------------------------------------------------------------------*/
static obj_t
set_struct( obj_t heap, obj_t key, int len ) {
   obj_t structure = (obj_t)heap;
   
#if( !defined( TAG_STRUCTURE ) )
   structure->structure.header = MAKE_HEADER( STRUCT_TYPE, 0 );
#endif	

   structure->structure.key = key;
   structure->structure.length = len;
   
   return BSTRUCTURE( structure );
}
   
/*---------------------------------------------------------------------*/
/*    create_struct ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
create_struct( obj_t key, int len ) {
   int byte_size = STRUCT_SIZE + ( (len-1) * OBJ_SIZE );
   
   return set_struct( (obj_t)GC_MALLOC( byte_size ), key, len );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    fill_struct ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
fill_struct( obj_t structure, int len, obj_t init ) {
   int i;
   
   for( i = 0; i < len; i++ )
      STRUCT_SET( structure, i, init );

   return structure;
}

/*---------------------------------------------------------------------*/
/*    make_struct ...                                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
make_struct( obj_t key, int len, obj_t init ) {
   obj_t structure;

   structure = create_struct( key, len );
   fill_struct( structure, len, init );
   
   return structure;
}

